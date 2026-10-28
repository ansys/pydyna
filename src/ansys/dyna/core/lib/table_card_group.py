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

"""Module for handling groups of table cards.

Each row of a group is written as one data line per sub-card, sub-card by sub-card.
A sub-card can be switched off for the whole group (``card_active_funcs``), or for
individual rows (``row_conditions``).
"""

import io
import math
import typing

import numpy as np
import pandas as pd

from ansys.dyna.core.lib.card_interface import CardInterface, ReadResult
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core.lib.io_utils import write_or_return
from ansys.dyna.core.lib.kwd_line_formatter import (
    _is_comma_delimited,
    _normalize_dyna_float,
    buffer_to_lines,
    load_dataline,
    read_line,
)
from ansys.dyna.core.lib.parameters import ParameterSet
from ansys.dyna.core.lib.row_condition import RowCondition
from ansys.dyna.core.lib.table_card import TableCard, _get_first_row_from_schemas, try_initialize_table

# Lines read for each sub-card, the row each of those lines belongs to, and the number of rows.
_Layout = typing.Tuple[typing.List[typing.List[str]], typing.List[typing.Sequence[int]], int]


def _check_row_conditions(
    card_schemas: typing.List[typing.Tuple[FieldSchema, ...]],
    row_conditions: typing.Optional[typing.List[typing.Optional[RowCondition]]],
) -> typing.List[typing.Optional[RowCondition]]:
    if row_conditions is None:
        return [None] * len(card_schemas)
    if len(row_conditions) != len(card_schemas):
        raise ValueError(f"Expected one row condition per sub-card ({len(card_schemas)}), got {len(row_conditions)}.")
    earlier_fields = set()
    for index, (schemas, condition) in enumerate(zip(card_schemas, row_conditions)):
        if condition is not None:
            unknown = [name for name in condition.fields if name not in earlier_fields]
            if unknown:
                raise ValueError(
                    f"The row condition of sub-card {index} reads {unknown}, "
                    "which are not fields of an earlier sub-card."
                )
        earlier_fields.update(schema.name for schema in schemas)
    return list(row_conditions)


def _parse_fixed_width(text: str, field_type: type) -> typing.Any:
    """Parse one fixed-width field of a data line, giving ``None`` for a blank field."""
    if not text or text.isspace():
        return None
    if field_type is str:
        return text.strip()
    value = float(_normalize_dyna_float(text))
    return int(value) if field_type is int else value


class TableCardGroup(CardInterface):
    def __init__(
        self,
        card_schemas: typing.List[typing.Tuple[FieldSchema, ...]],
        length_func: typing.Callable,
        active_func: typing.Optional[typing.Callable] = None,
        name: str = None,
        format: format_type = format_type.default,
        card_active_funcs: typing.Optional[typing.List[typing.Callable]] = None,
        row_conditions: typing.Optional[typing.List[typing.Optional[RowCondition]]] = None,
        **kwargs,
    ):
        """Initialize a TableCardGroup from field schemas.

        Parameters
        ----------
        card_schemas : List[Tuple[FieldSchema, ...]]
            List of FieldSchema tuples, one for each sub-card in the group.
        length_func : callable
            Function returning the number of rows, or None for unbounded.
        active_func : callable, optional
            Function returning bool to determine if card group is active.
        name : str, optional
            Name for table initialization from kwargs.
        format : format_type, optional
            The format type (default, standard, or long).
        card_active_funcs : List[callable], optional
            List of active functions for each sub-card. If provided, must have
            the same length as card_schemas. An inactive sub-card has no line in any row.
        row_conditions : List[RowCondition or None], optional
            One entry per sub-card. An active sub-card with a condition has a line only in
            the rows where its condition holds. A condition reads fields of earlier sub-cards.
        """
        if card_active_funcs is None:
            self._cards = [
                TableCard.from_field_schemas(schemas, length_func, format=format) for schemas in card_schemas
            ]
        else:
            self._cards = [
                TableCard.from_field_schemas(schemas, length_func, active_func=af, format=format)
                for schemas, af in zip(card_schemas, card_active_funcs)
            ]
        self._row_conditions = _check_row_conditions(card_schemas, row_conditions)
        field_cards = {}
        for index, schemas in enumerate(card_schemas):
            for schema in schemas:
                field_cards.setdefault(schema.name, index)
        condition_fields = dict.fromkeys(name for c in self._row_conditions if c is not None for name in c.fields)
        # Sub-card holding each field that a row condition reads.
        self._condition_field_cards: typing.Dict[str, int] = {name: field_cards[name] for name in condition_fields}
        self._length_func = length_func
        self._active_func = active_func
        self.format = format
        if length_func is None:
            self._bounded = False
            self._length_func = self._get_unbounded_length
        else:
            self._bounded = True
            self._length_func = length_func
        self._initialized = try_initialize_table(self, name, **kwargs)
        if not self._initialized:
            # Collect all field schemas for first row lookup
            all_schemas = tuple(fs for schemas in card_schemas for fs in schemas)
            first_row = _get_first_row_from_schemas(all_schemas, **kwargs)
            for card in self._cards:
                card._first_row = first_row

    def _get_fields(self):
        fields = []
        for card in self._cards:
            fields.extend(card._fields)
        return fields

    def _initialize(self) -> None:
        if self._initialized:
            return
        [card._initialize(True) for card in self._cards]
        data = pd.concat([card.table for card in self._cards], axis=1)
        self._table = data
        self._initialized = True

    @property
    def table(self) -> pd.DataFrame:
        """Get the table as a pandas DataFrame."""
        self._initialize()
        return self._table

    @table.setter
    def table(self, value: pd.DataFrame):
        """Set the table from a pandas DataFrame."""
        # remove duplicate columns
        value = value.loc[:, ~value.columns.duplicated()]
        # store the table
        self._table = value
        # propagate table to child duplicate card objects
        self._propagate()
        self._initialized = True

    @property
    def format(self) -> format_type:
        """Format type of the table card group."""
        return self._format

    def _propagate(self) -> None:
        """Propagate view of data frame to all child cards."""
        for card in self._cards:
            card.table = self._table

    @format.setter
    def format(self, value: format_type) -> None:
        self._format = value
        for card in self._cards:
            card.format = value

    def read(self, buf: typing.TextIO, parameter_set: ParameterSet = None) -> ReadResult:
        """Read the table card group from a buffer.

        Returns
        -------
        ReadResult
            Result containing warnings.
        """
        if self._has_row_conditions():
            layout = self._scan_lines(buf, parameter_set)
        else:
            layout = self._split_lines(buf)
        result = ReadResult()
        result.warnings.extend(self._load_layout(*layout, parameter_set))
        return result

    def _split_lines(self, buf: typing.TextIO) -> _Layout:
        """Read the lines of a group without row conditions, where every row has a line per active sub-card."""
        active = [index for index, card in enumerate(self._cards) if self._is_card_active(card)]
        card_lines = [[] for _ in self._cards]
        card_rows = [range(0) for _ in self._cards]
        if not active:
            return card_lines, card_rows, 0
        data_lines = buffer_to_lines(buf, self._length_func() * len(active) if self.bounded else -1)
        for position, index in enumerate(active):
            card_lines[index] = data_lines[position :: len(active)]
            card_rows[index] = range(len(card_lines[index]))
        return card_lines, card_rows, math.ceil(len(data_lines) / len(active))

    def _scan_lines(self, buf: typing.TextIO, parameter_set: ParameterSet) -> _Layout:
        """Read the lines row by row, using the values read so far in a row to decide its next line."""
        active = [self._is_card_active(card) for card in self._cards]
        readers = {index: self._condition_value_reader(index) for index in set(self._condition_field_cards.values())}
        card_lines = [[] for _ in self._cards]
        card_rows = [[] for _ in self._cards]
        max_rows = self._length_func() if self.bounded else None
        num_rows = 0
        while max_rows is None or num_rows < max_rows:
            values = {}
            row_started = False
            for index, condition in enumerate(self._row_conditions):
                if not active[index]:
                    continue
                if condition is not None and not condition.evaluate_row(*[values.get(f) for f in condition.fields]):
                    continue
                line, end_of_data = read_line(buf)
                if end_of_data:
                    break
                row_started = True
                card_lines[index].append(line)
                card_rows[index].append(num_rows)
                if index in readers:
                    values.update(readers[index](line, num_rows, parameter_set))
            if not row_started:
                break
            num_rows += 1
        return card_lines, card_rows, num_rows

    def _condition_value_reader(
        self, index: int
    ) -> typing.Callable[[str, int, ParameterSet], typing.Dict[str, typing.Any]]:
        """Return a function reading, from a line of sub-card ``index``, the fields that row conditions read."""
        fields = self._cards[index]._get_fields()
        spec = [(field.offset, field.width, field.type) for field in fields]
        wanted = [
            (pos, field) for pos, field in enumerate(fields) if self._condition_field_cards.get(field.name) == index
        ]

        def read_values(line: str, row: int, parameter_set: ParameterSet) -> typing.Dict[str, typing.Any]:
            if "&" not in line and not _is_comma_delimited(line, len(fields)):
                return {
                    field.name: _parse_fixed_width(line[field.offset : field.offset + field.width], field.type)
                    for _, field in wanted
                }
            if parameter_set is None:
                values, _ = load_dataline(spec, line)
            else:
                # The sub-card parses this line again under the same scope, recording the same refs.
                with parameter_set.scope(f"subcard{index}"), parameter_set.scope(f"row{row}"):
                    values, _ = load_dataline(spec, line, parameter_set)
            return {field.name: values[pos] for pos, field in wanted}

        return read_values

    def _load_layout(
        self,
        card_lines: typing.List[typing.List[str]],
        card_rows: typing.List[typing.Sequence[int]],
        num_rows: int,
        parameter_set: ParameterSet,
    ) -> typing.List[str]:
        """Load the lines of each sub-card, and assemble them into rows of the table.

        For parameter retention, each sub-card's refs are scoped with `subcard{index}`
        so URIs are: uri_prefix/subcard{subcard_index}/row{row_index}/{field_index}

        Returns
        -------
        list of str
            Warning messages.
        """
        all_warnings = []
        tables = []
        for index, (card, lines, rows) in enumerate(zip(self._cards, card_lines, card_rows)):
            # Scope by subcard index so refs from different sub-cards don't overlap
            if parameter_set is not None:
                with parameter_set.scope(f"subcard{index}"):
                    warnings_list = card._load_lines(lines, parameter_set, rows)
            else:
                warnings_list = card._load_lines(lines, parameter_set, rows)
            all_warnings.extend(warnings_list)
            table = card.table
            if len(rows) != num_rows:
                table = table.set_axis(rows).reindex(pd.RangeIndex(num_rows))
            tables.append(table)
        self.table = pd.concat(tables, axis=1)
        return all_warnings

    def write(
        self,
        format: typing.Optional[format_type] = None,
        buf: typing.Optional[typing.TextIO] = None,
        comment: typing.Optional[bool] = True,
        retain_parameters: bool = False,
        parameter_set: ParameterSet = None,
        uri_prefix: str = None,
        **kwargs,
    ) -> str:
        """Write the table card group to a string or buffer."""
        if format == None:
            format = self.format

        def _write(buf: typing.TextIO):
            if self.active:
                self._initialize()
                self._propagate()
                buf.write("\n".join(self._data_lines(format, comment, retain_parameters, parameter_set, uri_prefix)))

        return write_or_return(buf, _write)

    def _data_lines(
        self,
        format: format_type,
        comment: bool,
        retain_parameters: bool,
        parameter_set: ParameterSet,
        uri_prefix: str,
    ) -> typing.List[str]:
        """Render the rows as data lines, preceding each sub-card's first line with its comment line."""
        presence = self._row_presence()
        num_rows = len(self._table)
        # The sub-card of each output line: row by row, and within a row, sub-card by sub-card.
        line_cards = np.tile(np.arange(len(self._cards)), num_rows)[np.column_stack(presence).ravel()]
        lines = np.empty(len(line_cards), dtype=object)
        comment_lines = []
        for index, (card, present) in enumerate(zip(self._cards, presence)):
            rows = np.flatnonzero(present)
            if len(rows) == 0:
                continue
            slots = np.flatnonzero(line_cards == index)
            lines[slots] = self._card_lines(index, rows, format, retain_parameters, parameter_set, uri_prefix)
            if comment:
                comment_lines.append((slots[0], card._get_comment(format)))
        lines = lines.tolist()
        for position, comment_line in sorted(comment_lines, reverse=True):
            lines.insert(position, comment_line)
        return lines

    def _card_lines(
        self,
        index: int,
        rows: np.ndarray,
        format: format_type,
        retain_parameters: bool,
        parameter_set: ParameterSet,
        uri_prefix: str,
    ) -> typing.List[str]:
        """Render the data lines of sub-card ``index`` for the given rows."""
        card = self._cards[index]
        table = card.table if len(rows) == len(card.table) else card.table.iloc[rows]
        # Build sub-card URI prefix for parameter retention
        # Join segments properly, filtering out empty strings
        subcard_uri_prefix = None
        if retain_parameters and uri_prefix is not None:
            subcard_uri_prefix = "/".join(s for s in [uri_prefix, f"subcard{index}"] if s)
        card_buf = io.StringIO()
        card._write_rows(card_buf, format, table, rows, retain_parameters, parameter_set, subcard_uri_prefix)
        return card_buf.getvalue().split("\n")[: len(rows)]

    def _row_presence(self) -> typing.List[np.ndarray]:
        """Return, for each sub-card, one boolean per row telling whether the sub-card has a line in that row.

        Requires the sub-card tables to be up to date with the group table (see ``_propagate``).
        """
        num_rows = len(self._table)
        condition_table = None
        if self._has_row_conditions():
            condition_table = pd.DataFrame(
                {name: self._cards[index].table[name] for name, index in self._condition_field_cards.items()}
            )
        presence = []
        for card, condition in zip(self._cards, self._row_conditions):
            if not self._is_card_active(card):
                presence.append(np.zeros(num_rows, dtype=bool))
            elif condition is None:
                presence.append(np.ones(num_rows, dtype=bool))
            else:
                presence.append(condition.evaluate(condition_table))
        return presence

    def _has_row_conditions(self) -> bool:
        return bool(self._condition_field_cards)

    @property
    def active(self) -> bool:
        """Indicates whether the card group is active."""
        if self._active_func == None:
            return True
        return self._active_func()

    def _is_card_active(self, card) -> bool:
        if card._active_func != None:
            return card._active_func()
        return True

    def _get_active_cards(self) -> typing.List[TableCard]:
        return [card for card in self._cards if self._is_card_active(card)]

    def _get_unbounded_length(self) -> int:
        """The unbounded length is the minimum of all sub-card's unbounded length"""
        self._initialize()  # Need to initialize first, so that the sub card can calculate num_rows
        lens = [card._num_rows() for card in self._get_active_cards()]
        return min(lens)

    @property
    def bounded(self) -> bool:
        """Indicates whether the card group is bounded."""
        return self._bounded

    def _num_rows(self) -> int:
        """Return the number of data lines of the group."""
        if not self.active:
            return 0
        if not self._has_row_conditions():
            return self._length_func() * len(self._get_active_cards())
        self._initialize()
        self._propagate()
        return int(sum(present.sum() for present in self._row_presence()))

    def __repr__(self) -> str:
        """Returns a console-friendly representation of the desired parameters for the card"""
        self._propagate()
        content_lines = []
        for card in self._get_active_cards():
            content_lines.append(card._get_comment(self._format))
        output = "\n".join(content_lines)
        return "TableCardGroup: \n" + output
