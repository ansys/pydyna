# Copyright (C) 2023 - 2026 Synopsys, Inc. and ANSYS, Inc. All rights reserved.
# SPDX-License-Identifier: MIT
#
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

"""
Row conditions: which rows of a table card group have a given sub-card.

The manifest gives a row condition as an expression tree over fields of earlier
sub-cards of the group, for example::

    {"op": "ne", "left": {"field": "n5"}, "right": {"literal": 0}}

Expressions:

- ``{"op": "and" | "or", "args": [expression, ...]}``
- ``{"op": "not", "arg": expression}``
- ``{"op": "is_set", "field": name}``
- ``{"op": "eq" | "ne" | "lt" | "le" | "gt" | "ge", "left": operand, "right": operand}``
- ``{"op": "in", "left": {"field": name}, "right": {"literal": [value, ...]}}``

where an operand is ``{"field": name}`` or ``{"literal": value}``. A comparison or
``in`` with a missing value is false; ``and``, ``or`` and ``not`` are classical on
top of that, so ``not`` of a comparison is true for a missing value.

``compile_row_condition`` produces the arguments of
``ansys.dyna.core.lib.row_condition.RowCondition``: a scalar expression over the
values of one row, used while reading, and a pandas expression over the whole
table, used for bulk operations such as writing.
"""

from dataclasses import dataclass
import json
import keyword
import math
from typing import Any, Dict, List, Tuple

_COMPARISONS = {"eq": "==", "ne": "!=", "lt": "<", "le": "<=", "gt": ">", "ge": ">="}
_MIRRORED = {"eq": "eq", "ne": "ne", "lt": "gt", "le": "ge", "gt": "lt", "ge": "le"}
_ORDERINGS = {"lt", "le", "gt", "ge"}
_NUMERIC_TYPES = {"int", "float"}

# Precedence levels, shared by the Python operators and their pandas counterparts.
_OR, _AND, _NOT, _ATOM = range(4)
_CONNECTIVES = {"and": (_AND, " and ", " & "), "or": (_OR, " or ", " | ")}

ROW_CONDITION_DEFS: Dict[str, Any] = {
    "row_condition": {
        "type": "object",
        "oneOf": [
            {
                "properties": {
                    "op": {"enum": ["and", "or"]},
                    "args": {"type": "array", "minItems": 2, "items": {"$ref": "#/$defs/row_condition"}},
                },
                "required": ["op", "args"],
                "additionalProperties": False,
            },
            {
                "properties": {"op": {"const": "not"}, "arg": {"$ref": "#/$defs/row_condition"}},
                "required": ["op", "arg"],
                "additionalProperties": False,
            },
            {
                "properties": {"op": {"const": "is_set"}, "field": {"type": "string"}},
                "required": ["op", "field"],
                "additionalProperties": False,
            },
            {
                "properties": {
                    "op": {"enum": [*_COMPARISONS, "in"]},
                    "left": {"$ref": "#/$defs/row_condition_operand"},
                    "right": {"$ref": "#/$defs/row_condition_operand"},
                },
                "required": ["op", "left", "right"],
                "additionalProperties": False,
            },
        ],
    },
    "row_condition_operand": {
        "type": "object",
        "oneOf": [
            {"properties": {"field": {"type": "string"}}, "required": ["field"], "additionalProperties": False},
            {"properties": {"literal": {}}, "required": ["literal"], "additionalProperties": False},
        ],
    },
}
"""JSON schema definitions of a row condition, referenced as ``#/$defs/row_condition``."""


@dataclass
class RowConditionCode:
    """Python source of a compiled row condition.

    Attributes
    ----------
        fields: Fields read by the condition, also the parameters of ``evaluate_row``
        evaluate_row: Expression over the values of ``fields`` in one row
        evaluate_all: Expression over the DataFrame ``table``, giving one numpy boolean per row
    """

    fields: List[str]
    evaluate_row: str
    evaluate_all: str


@dataclass
class _Code:
    row: str
    row_level: int
    table: str
    table_level: int


def compile_row_condition(expression: Dict[str, Any], field_types: Dict[str, str]) -> RowConditionCode:
    """Compile a row condition.

    Args:
        expression: Row condition from the manifest
        field_types: Python type (``int``, ``float``, ``str`` or ``bool``) of each field the condition may read

    Raises
    ------
        ValueError: If the expression is malformed, reads another field or compares incompatible types
    """
    compiler = _Compiler(field_types)
    code = compiler.compile(expression, "row condition")
    table = code.table if code.table_level == _ATOM else f"({code.table})"
    return RowConditionCode(list(compiler.fields), code.row, f"{table}.to_numpy(dtype=bool)")


def _literal(value: Any) -> str:
    return json.dumps(value) if isinstance(value, str) else repr(value)


def _in_connective(code: str, level: int, connective_level: int) -> str:
    return code if level == connective_level or level >= _NOT else f"({code})"


class _Compiler:
    def __init__(self, field_types: Dict[str, str]):
        self._field_types = field_types
        self.fields: Dict[str, None] = {}

    def compile(self, node: Any, path: str) -> _Code:
        op = node.get("op") if isinstance(node, dict) else None
        if isinstance(op, str):
            if op in _CONNECTIVES:
                return self._connective(op, node, path)
            if op == "not":
                return self._not(node, path)
            if op == "is_set":
                return self._is_set(node, path)
            if op in _COMPARISONS:
                return self._comparison(op, node, path)
            if op == "in":
                return self._membership(node, path)
        raise ValueError(f"{path}: expected an expression with a known 'op', got {node!r}")

    def _connective(self, op: str, node: Dict[str, Any], path: str) -> _Code:
        self._expect_keys(node, {"op", "args"}, path)
        args = node["args"]
        if not isinstance(args, list) or len(args) < 2:
            raise ValueError(f"{path}: '{op}' needs a list of at least two expressions in 'args'")
        level, row_separator, table_separator = _CONNECTIVES[op]
        codes = [self.compile(arg, f"{path}.args[{index}]") for index, arg in enumerate(args)]
        row = row_separator.join(_in_connective(code.row, code.row_level, level) for code in codes)
        table = table_separator.join(_in_connective(code.table, code.table_level, level) for code in codes)
        return _Code(row, level, table, level)

    def _not(self, node: Dict[str, Any], path: str) -> _Code:
        self._expect_keys(node, {"op", "arg"}, path)
        code = self.compile(node["arg"], f"{path}.arg")
        row = code.row if code.row_level >= _NOT else f"({code.row})"
        table = code.table if code.table_level == _ATOM else f"({code.table})"
        return _Code(f"not {row}", _NOT, f"~{table}", _NOT)

    def _is_set(self, node: Dict[str, Any], path: str) -> _Code:
        self._expect_keys(node, {"op", "field"}, path)
        name = self._field(node["field"], f"{path}.field")
        return _Code(f"not is_missing({name})", _NOT, f'table["{name}"].notna()', _ATOM)

    def _comparison(self, op: str, node: Dict[str, Any], path: str) -> _Code:
        self._expect_keys(node, {"op", "left", "right"}, path)
        left = self._operand(node["left"], f"{path}.left")
        right = self._operand(node["right"], f"{path}.right")
        if left[0] == "literal":
            if right[0] == "literal":
                raise ValueError(f"{path}: '{op}' compares two literals")
            left, right, op = right, left, _MIRRORED[op]
        name = left[1]
        field_type = self._field_types[name]
        if op in _ORDERINGS and field_type not in _NUMERIC_TYPES:
            raise ValueError(f"{path}: '{op}' needs a numeric field, {name!r} is {field_type}")
        if right[0] == "field":
            other = right[1]
            other_type = self._field_types[other]
            if field_type != other_type and not {field_type, other_type} <= _NUMERIC_TYPES:
                raise ValueError(f"{path}: cannot compare {name!r} ({field_type}) with {other!r} ({other_type})")
            guarded = list(dict.fromkeys([name, other]))
            row_right, table_right = other, f'table["{other}"]'
        else:
            self._check_literal(right[1], field_type, f"{path}.right.literal")
            guarded = [name]
            row_right = table_right = _literal(right[1])
        row_comparison = f"{name} {_COMPARISONS[op]} {row_right}"
        table_comparison = f'table["{name}"].{op}({table_right})'
        row = " and ".join([f"not is_missing({field})" for field in guarded] + [row_comparison])
        table = " & ".join([f'table["{field}"].notna()' for field in guarded] + [table_comparison])
        return _Code(row, _AND, table, _AND)

    def _membership(self, node: Dict[str, Any], path: str) -> _Code:
        self._expect_keys(node, {"op", "left", "right"}, path)
        kind, name = self._operand(node["left"], f"{path}.left")
        if kind != "field":
            raise ValueError(f"{path}.left: 'in' tests a field")
        kind, values = self._operand(node["right"], f"{path}.right")
        if kind != "literal" or not isinstance(values, list) or not values:
            raise ValueError(f"{path}.right: 'in' needs a non-empty list literal")
        for index, value in enumerate(values):
            self._check_literal(value, self._field_types[name], f"{path}.right.literal[{index}]")
        literals = ", ".join(_literal(value) for value in values)
        row_values = f"({literals},)" if len(values) == 1 else f"({literals})"
        row = f"not is_missing({name}) and {name} in {row_values}"
        return _Code(row, _AND, f'table["{name}"].isin([{literals}])', _ATOM)

    def _operand(self, node: Any, path: str) -> Tuple[str, Any]:
        if isinstance(node, dict) and len(node) == 1:
            if "field" in node:
                return "field", self._field(node["field"], f"{path}.field")
            if "literal" in node:
                return "literal", node["literal"]
        raise ValueError(f'{path}: expected {{"field": name}} or {{"literal": value}}, got {node!r}')

    def _field(self, name: Any, path: str) -> str:
        if not isinstance(name, str) or name not in self._field_types:
            raise ValueError(
                f"{path}: {name!r} is not a field of an earlier sub-card, which are {sorted(self._field_types)}"
            )
        if not name.isidentifier() or keyword.iskeyword(name):
            raise ValueError(f"{path}: field {name!r} cannot be a lambda parameter")
        self.fields.setdefault(name)
        return name

    @staticmethod
    def _check_literal(value: Any, field_type: str, path: str) -> None:
        if field_type in _NUMERIC_TYPES:
            valid = isinstance(value, (int, float)) and not isinstance(value, bool) and math.isfinite(value)
        elif field_type == "str":
            valid = isinstance(value, str)
        else:
            valid = field_type == "bool" and isinstance(value, bool)
        if not valid:
            raise ValueError(f"{path}: {value!r} is not a literal of type {field_type}")

    @staticmethod
    def _expect_keys(node: Dict[str, Any], keys: set, path: str) -> None:
        if set(node) != keys:
            raise ValueError(f"{path}: '{node['op']}' takes the keys {sorted(keys)}, got {sorted(node)}")
