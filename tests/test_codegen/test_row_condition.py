# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
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

"""Tests for the row conditions of table card groups: compiler, compile step and template."""

from dataclasses import asdict
import io
import pathlib

from jinja2 import Environment, FileSystemLoader
import numpy as np
import pandas as pd
import pytest

from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.row_condition import RowCondition, is_missing
from ansys.dyna.core.lib.table_card_group import TableCardGroup
from keyword_generation.data_model.keyword_data import Card, Field, KeywordData
from keyword_generation.data_model.row_condition import RowConditionCode, compile_row_condition
from keyword_generation.generators.class_generator import _compile_row_conditions, _transform_data

_FIELD_TYPES = {"n": "int", "m": "int", "x": "float", "s": "str", "flag": "bool"}

_TABLE = pd.DataFrame(
    {
        "n": pd.array([0, 1, 2, None, 1, None], dtype="Int32"),
        "m": pd.array([0, 2, 2, 1, None, None], dtype="Int32"),
        "x": [0.0, 0.5, np.nan, 2.0, 1.0, np.nan],
        "s": pd.Series(["VEC3", None, "RX", np.nan, "MAT", None], dtype=object),
    }
)


def _field(name):
    return {"field": name}


def _literal(value):
    return {"literal": value}


def _compare(op, left, right):
    return {"op": op, "left": left, "right": right}


def _is_set(name):
    return {"op": "is_set", "field": name}


def _build(code: RowConditionCode) -> RowCondition:
    namespace = {"is_missing": is_missing}
    evaluate_row = eval(f"lambda {', '.join(code.fields)}: {code.evaluate_row}", namespace)
    evaluate_all = eval(f"lambda table: {code.evaluate_all}", namespace)
    return RowCondition(tuple(code.fields), evaluate_row, evaluate_all)


class TestSource:
    @pytest.mark.parametrize(
        "expression, fields, evaluate_row, evaluate_all",
        [
            pytest.param(
                _compare("ne", _field("n"), _literal(0)),
                ["n"],
                "not is_missing(n) and n != 0",
                '(table["n"].notna() & table["n"].ne(0)).to_numpy(dtype=bool)',
                id="ne",
            ),
            pytest.param(
                {"op": "in", "left": _field("s"), "right": _literal(["VEC3", "MAT"])},
                ["s"],
                'not is_missing(s) and s in ("VEC3", "MAT")',
                'table["s"].isin(["VEC3", "MAT"]).to_numpy(dtype=bool)',
                id="in",
            ),
            pytest.param(
                {"op": "in", "left": _field("s"), "right": _literal(["VEC3"])},
                ["s"],
                'not is_missing(s) and s in ("VEC3",)',
                'table["s"].isin(["VEC3"]).to_numpy(dtype=bool)',
                id="in one value",
            ),
            pytest.param(
                _compare("lt", _literal(0), _field("x")),
                ["x"],
                "not is_missing(x) and x > 0",
                '(table["x"].notna() & table["x"].gt(0)).to_numpy(dtype=bool)',
                id="literal on the left",
            ),
            pytest.param(
                _compare("le", _field("n"), _field("x")),
                ["n", "x"],
                "not is_missing(n) and not is_missing(x) and n <= x",
                '(table["n"].notna() & table["x"].notna() & table["n"].le(table["x"])).to_numpy(dtype=bool)',
                id="two fields",
            ),
            pytest.param(
                _is_set("s"),
                ["s"],
                "not is_missing(s)",
                'table["s"].notna().to_numpy(dtype=bool)',
                id="is_set",
            ),
            pytest.param(
                {"op": "not", "arg": _compare("eq", _field("n"), _literal(1))},
                ["n"],
                "not (not is_missing(n) and n == 1)",
                '(~(table["n"].notna() & table["n"].eq(1))).to_numpy(dtype=bool)',
                id="not",
            ),
            pytest.param(
                {"op": "or", "args": [_compare("eq", _field("n"), _literal(1)), _is_set("s")]},
                ["n", "s"],
                "(not is_missing(n) and n == 1) or not is_missing(s)",
                '((table["n"].notna() & table["n"].eq(1)) | table["s"].notna()).to_numpy(dtype=bool)',
                id="or",
            ),
            pytest.param(
                {
                    "op": "and",
                    "args": [
                        {"op": "or", "args": [_is_set("n"), _is_set("x")]},
                        _compare("eq", _field("s"), _literal("A")),
                    ],
                },
                ["n", "x", "s"],
                '(not is_missing(n) or not is_missing(x)) and not is_missing(s) and s == "A"',
                '((table["n"].notna() | table["x"].notna()) & table["s"].notna() & table["s"].eq("A"))'
                ".to_numpy(dtype=bool)",
                id="and of or",
            ),
        ],
    )
    def test_source(self, expression, fields, evaluate_row, evaluate_all):
        code = compile_row_condition(expression, _FIELD_TYPES)
        assert code == RowConditionCode(fields, evaluate_row, evaluate_all)

    def test_field_read_twice_is_one_parameter(self):
        bounds = [_compare("gt", _field("n"), _literal(0)), _compare("lt", _field("n"), _literal(9))]
        assert compile_row_condition({"op": "and", "args": bounds}, _FIELD_TYPES).fields == ["n"]


_EXPRESSIONS = [
    pytest.param(_compare("ne", _field("n"), _literal(0)), [False, True, True, False, True, False], id="ne"),
    pytest.param(_compare("eq", _field("n"), _field("m")), [True, False, True, False, False, False], id="two fields"),
    pytest.param(_compare("ge", _field("x"), _literal(1)), [False, False, False, True, True, False], id="ge"),
    pytest.param(
        {"op": "not", "arg": _compare("eq", _field("n"), _literal(1))},
        [True, False, True, True, False, True],
        id="not",
    ),
    pytest.param(
        {"op": "in", "left": _field("s"), "right": _literal(["VEC3", "MAT"])},
        [True, False, False, False, True, False],
        id="in",
    ),
    pytest.param(_is_set("s"), [True, False, True, False, True, False], id="is_set"),
    pytest.param(
        {"op": "or", "args": [_compare("eq", _field("n"), _literal(2)), _is_set("s")]},
        [True, False, True, False, True, False],
        id="or",
    ),
    pytest.param(
        {"op": "and", "args": [_compare("gt", _field("x"), _literal(0)), {"op": "not", "arg": _is_set("m")}]},
        [False, False, False, False, True, False],
        id="and",
    ),
]


class TestSemantics:
    """The scalar and vectorized forms give the same, documented answer, with missing values."""

    @pytest.mark.parametrize("expression, expected", _EXPRESSIONS)
    def test_vectorized(self, expression, expected):
        condition = _build(compile_row_condition(expression, _FIELD_TYPES))
        assert condition.evaluate(_TABLE).tolist() == expected

    @pytest.mark.parametrize("expression, expected", _EXPRESSIONS)
    def test_row_by_row(self, expression, expected):
        condition = _build(compile_row_condition(expression, _FIELD_TYPES))
        assert RowCondition(condition.fields, condition.evaluate_row).evaluate(_TABLE).tolist() == expected

    @pytest.mark.parametrize("expression, expected", _EXPRESSIONS)
    def test_row_by_row_with_none_for_blank_fields(self, expression, expected):
        condition = _build(compile_row_condition(expression, _FIELD_TYPES))
        rows = zip(*[_TABLE[name].tolist() for name in condition.fields])
        values = [[None if is_missing(value) else value for value in row] for row in rows]
        assert [condition.evaluate_row(*row) for row in values] == expected


class TestErrors:
    @pytest.mark.parametrize(
        "expression, message",
        [
            pytest.param("n != 0", "known 'op'", id="not an expression"),
            pytest.param({"op": "xor", "args": []}, "known 'op'", id="unknown op"),
            pytest.param({"op": "and", "args": [_is_set("n")]}, "at least two", id="one argument"),
            pytest.param({"op": "is_set", "field": "n", "value": 0}, "takes the keys", id="extra key"),
            pytest.param(_is_set("n9"), "not a field of an earlier sub-card", id="unknown field"),
            pytest.param(_compare("eq", _field("n"), _literal("0")), "not a literal of type int", id="str for int"),
            pytest.param(_compare("eq", _field("n"), _literal(True)), "not a literal of type int", id="bool for int"),
            pytest.param(_compare("eq", _field("x"), _literal(float("nan"))), "not a literal of type float", id="nan"),
            pytest.param(_compare("eq", _field("flag"), _literal(1)), "not a literal of type bool", id="int for bool"),
            pytest.param(_compare("lt", _field("s"), _literal("A")), "needs a numeric field", id="ordered str"),
            pytest.param(_compare("eq", _literal(1), _literal(1)), "two literals", id="two literals"),
            pytest.param(_compare("eq", _field("n"), _field("s")), "cannot compare", id="int with str"),
            pytest.param(_compare("eq", _field("n"), {"value": 0}), "expected", id="bad operand"),
            pytest.param({"op": "in", "left": _field("s"), "right": _literal([])}, "non-empty list", id="empty in"),
            pytest.param({"op": "in", "left": _field("s"), "right": _literal("VEC3")}, "non-empty list", id="in str"),
            pytest.param(
                {"op": "in", "left": _literal("VEC3"), "right": _literal(["VEC3"])}, "tests a field", id="in literal"
            ),
            pytest.param(
                {"op": "in", "left": _field("s"), "right": _literal(["VEC3", 3])},
                r"literal\[1\]: 3 is not a literal of type str",
                id="in mixed",
            ),
        ],
    )
    def test_invalid(self, expression, message):
        with pytest.raises(ValueError, match=message):
            compile_row_condition(expression, _FIELD_TYPES)

    def test_field_must_be_a_lambda_parameter(self):
        with pytest.raises(ValueError, match="cannot be a lambda parameter"):
            compile_row_condition(_is_set("if"), {"if": "int"})


def _shell_group(row_active_if) -> KeywordData:
    element = Card(
        index=0,
        fields=[
            Field(name="EID", type="integer", position=0, width=8),
            Field(name="N5", type="integer", position=8, width=8),
        ],
    )
    midside = Card(
        index=1,
        fields=[Field(name="THIC5", type="real", position=0, width=16)],
        row_active_if=row_active_if,
    )
    group = Card(index=0, table_group=True, sub_cards=[element, midside], overall_name="elements")
    kwd_data = KeywordData(keyword="ELEMENT", subkeyword="SHELL", title="*ELEMENT_SHELL", cards=[group])
    _transform_data(kwd_data)
    return kwd_data


class TestCompileStep:
    def test_compiles_with_normalized_names(self):
        kwd_data = _shell_group(_compare("ne", _field("n5"), _literal(0)))
        _compile_row_conditions(kwd_data)
        element, midside = kwd_data.cards[0].sub_cards
        assert kwd_data.row_conditions
        assert element.row_condition is None
        assert midside.row_condition.fields == ["n5"]

    def test_no_row_conditions(self):
        kwd_data = _shell_group(None)
        _compile_row_conditions(kwd_data)
        assert not kwd_data.row_conditions

    def test_condition_reads_earlier_sub_cards_only(self):
        kwd_data = _shell_group(None)
        kwd_data.cards[0].sub_cards[0].row_active_if = _is_set("thic5")
        with pytest.raises(ValueError, match="table-card-group 'elements'.*not a field of an earlier sub-card"):
            _compile_row_conditions(kwd_data)


class TestTemplate:
    """The rendered group, run against data with and without the conditional line."""

    @pytest.fixture
    def source(self) -> str:
        kwd_data = _shell_group({"op": "and", "args": [_compare("ne", _field("n5"), _literal(0)), _is_set("eid")]})
        _compile_row_conditions(kwd_data)
        templates = pathlib.Path(__file__).parents[2] / "codegen" / "templates"
        env = Environment(loader=FileSystemLoader(str(templates)), trim_blocks=True, lstrip_blocks=True)
        template = env.get_template("keyword/card_types/table_card_group.j2")
        return template.render(card=asdict(kwd_data.cards[0]))

    def test_source(self, source):
        lines = [line.strip() for line in source.splitlines()]
        assert '("n5", "eid"),' in lines
        assert "lambda n5, eid: not is_missing(n5) and n5 != 0 and not is_missing(eid)," in lines

    def test_read_and_write(self, source):
        namespace = {
            "TableCardGroup": TableCardGroup,
            "FieldSchema": FieldSchema,
            "RowCondition": RowCondition,
            "is_missing": is_missing,
            "kwargs": {},
        }
        group = eval(source.strip().rstrip(","), namespace)
        lines = ["       1       5", "             1.5", "       2        "]
        group.read(io.StringIO("\n".join(lines)))
        assert group.table["eid"].tolist() == [1, 2]
        np.testing.assert_array_equal(group.table["thic5"].to_numpy(), [1.5, np.nan])
        assert group.write(comment=False) == "\n".join(lines)
