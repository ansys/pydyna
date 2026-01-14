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
"""Module for handling table cards."""

import io
import typing

import numpy as np
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field
from ansys.dyna.core.lib.field_writer import write_c_dataframe
from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core.lib.io_utils import is_dataframe, write_or_return
from ansys.dyna.core.lib.kwd_line_formatter import buffer_to_lines
from ansys.dyna.core.lib.parameters import ParameterSet

CHECK_TYPE = True


def _check_type(value):
    global CHECK_TYPE
    if CHECK_TYPE:
        if not is_dataframe(value):
            raise TypeError("value must be a DataFrame")


def try_initialize_table(card, name: str, **kwargs):
    """Card is a TableCard or a TableCardGroup"""
    if name is not None:
        data = kwargs.get(name, None)
        if data is not None:
            card.table = data
            return True
    return False


def get_first_row(fields: typing.List[Field], **kwargs) -> typing.Dict[str, typing.Any]:
    """Get the first row data from the kwargs."""
    result = dict()
    for field in fields:
        if field.name in kwargs:
            result[field.name] = kwargs[field.name]
    if len(result) == 0:
        return None
    return result


class TableCard(Card):
    def __init__(
        self,
        fields: typing.List[Field],
        length_func,
        active_func: typing.Callable = None,
        name: str = None,
        format: format_type = format_type.default,
        **kwargs,
    ):
        super().__init__(fields, active_func)
        self._format = [(field.offset, field.width) for field in self._fields]
        if length_func == None:
            self._bounded = False
            self._length_func = lambda: len(self.table)
        else:
            self._bounded = True
            self._length_func = length_func

        self._format_type = format
        self._initialized = try_initialize_table(self, name, **kwargs)
        if not self._initialized:
            self._first_row = get_first_row(self._fields, **kwargs)

    def _initialize(self, check: bool = False):
        """Lazy initialization routine for the data frame.

        This can not be done in the constructor because the length function
        might be unusable until after the keyword is fully initialized.

        If the card is bounded, the table should be initialized with the
        bounded length

        If the card is not bounded, the table should be initialized either
        using the input data, if it is given, or with exactly one row
        if the constructor **kwargs included any of the fields used by
        the table. This is called the "first row"

        Since TableCard is used internally by TableCardGroup,
        it could be initialized by the table card group, the first
        row may contain fields used by other tables in the group.
        """
        handle_first_row = self._first_row is not None
        if self._bounded:
            length = self._length_func()
            self._initialize_data(length)
            if length == 0:
                handle_first_row = False
        else:
            initial_size = 1 if handle_first_row else 0
            self._initialize_data(initial_size)

        if handle_first_row:
            for k, v in self._first_row.items():
                if k in self._table:
                    self._table.loc[0, k] = v

        self._first_row = None
        self._initialized = True

    @property
    def table(self) -> pd.DataFrame:
        if not self._initialized:
            self._initialize()
        return self._table

    @table.setter
    def table(self, value: pd.DataFrame):
        _check_type(value)
        # Build columns dict first, then create DataFrame (pandas 2.3+ compatibility)
        columns = {}
        for field in self._fields:
            if field.name in value:
                field_type = field.type
                if field_type == float:
                    field_type = np.float64
                elif field_type == int:
                    field_type = pd.Int32Dtype()
                columns[field.name] = value[field.name].astype(field_type)
            else:
                columns[field.name] = self._make_column(field, len(value))
        self._table = pd.DataFrame(columns)
        self._initialized = True

    @property
    def format(self) -> format_type:
        return self._format_type

    @format.setter
    def format(self, value: format_type) -> None:
        self._format_type = value

    def _get_default_value(self, field: Field) -> typing.Optional[typing.Any]:
        if field.value is not None:
            return field.value
        if field.type == float:
            return np.nan
        return None

    def _make_column(self, field, length):
        default_value = self._get_default_value(field)
        if field.type == float:
            arr = np.empty((length,))
            arr[:] = default_value
            return arr
        elif field.type == str:
            return [default_value] * length
        elif field.type == int:
            return pd.Series([default_value] * length, dtype=pd.Int32Dtype())
        raise Exception("unexpected type")

    def _initialize_data(self, length):
        data = {}
        num_fields = len(self._fields)
        column_names = np.ndarray(num_fields, "object")
        for index in range(num_fields):
            field = self._fields[index]
            value = self._make_column(field, length)
            column_names[index] = field.name
            data[field.name] = value
        self._table = pd.DataFrame(data, columns=column_names)

    def _get_row_values(self, index: int) -> list:
        # Used by Duplicate Card Group only
        if index >= len(self.table):
            return [None] * len(self._fields)
        values = []
        for key in self.table.keys():
            col = self.table[key]
            val = col[index]
            values.append(val)
        return values

    def _get_read_options(self):
        fields = self._get_fields()
        colspecs = [(field.offset, field.offset + field.width) for field in fields]
        type_mapping = {float: np.float64, int: pd.Int32Dtype(), str: str}
        dtype = {field.name: type_mapping[field.type] for field in fields}
        names = [field.name for field in fields]
        options = {"names": names, "colspecs": colspecs, "dtype": dtype, "comment": "$"}
        return options

    def _read_buffer_as_dataframe(
        self, buffer: typing.TextIO, fields: typing.Iterable[Field], parameter_set: ParameterSet
    ) -> pd.DataFrame:
        read_options = self._get_read_options()
        df = pd.read_fwf(buffer, **read_options)
        return df

    def _get_fields(self) -> typing.List[Field]:
        fields = self._fields
        if self.format == format_type.long:
            fields = self._convert_fields_to_long_format()
        return fields

    def _load_bounded_from_buffer(self, buf: typing.TextIO, parameter_set: ParameterSet) -> None:
        # For bounded cards, read all lines and use same logic as unbounded
        data_lines = buffer_to_lines(buf, self._num_rows())
        self._load_lines(data_lines, parameter_set)

    def _load_unbounded_from_buffer(self, buf: typing.TextIO, parameter_set: ParameterSet) -> None:
        data_lines = buffer_to_lines(buf)
        self._load_lines(data_lines, parameter_set)

    def read(self, buf: typing.TextIO, parameter_set: ParameterSet = None) -> None:
        if self.bounded:
            self._initialized = True
            self._load_bounded_from_buffer(buf, parameter_set)
        else:
            self._initialize_data(0)
            self._initialized = True
            self._load_unbounded_from_buffer(buf, parameter_set)

    def _has_parameters(self, data_lines: typing.List[str]) -> bool:
        """Check if any data lines contain parameter references."""
        return any("&" in line for line in data_lines)

    def _has_csv_format(self, data_lines: typing.List[str]) -> bool:
        """Check if any data lines use comma-delimited format.

        Uses the same detection logic as load_dataline to avoid false positives
        on lines that contain commas but aren't CSV format (e.g., expressions
        like min(1,2,3) in PARAMETER_EXPRESSION).
        """
        from ansys.dyna.core.lib.kwd_line_formatter import _is_comma_delimited

        num_fields = len(self._fields)
        return any(_is_comma_delimited(line, num_fields) for line in data_lines)

    def _load_lines_with_parameters(self, data_lines: typing.List[str], parameter_set: ParameterSet) -> None:
        """Load lines using load_dataline for parameter support.

        This method processes each line individually using load_dataline(),
        which handles parameter substitution. It's used when parameters are
        detected in the data.
        """
        from ansys.dyna.core.lib.kwd_line_formatter import load_dataline

        fields = self._get_fields()
        format_spec = [(f.offset, f.width, f.type) for f in fields]

        rows = []
        for line in data_lines:
            values = load_dataline(format_spec, line, parameter_set)
            row_dict = {field.name: value for field, value in zip(fields, values)}
            rows.append(row_dict)

        self._table = pd.DataFrame(rows)
        self._initialized = True

    def _load_lines(self, data_lines: typing.List[str], parameter_set: ParameterSet) -> None:
        # Use parameter-aware loading if parameters or CSV format is present
        # CSV format must go through load_dataline since pd.read_fwf doesn't support it
        has_params = parameter_set is not None and self._has_parameters(data_lines)
        has_csv = self._has_csv_format(data_lines)
        if has_params or has_csv:
            self._load_lines_with_parameters(data_lines, parameter_set)
        else:
            # Use fast pandas path when no parameters present and all lines are fixed-width
            fields = self._get_fields()
            buffer = io.StringIO()
            [(buffer.write(line), buffer.write("\n")) for line in data_lines]
            buffer.seek(0)
            self._table = self._read_buffer_as_dataframe(buffer, fields, parameter_set)
            self._initialized = True

    def write(
        self,
        format: typing.Optional[format_type] = None,
        buf: typing.Optional[typing.TextIO] = None,
        comment: typing.Optional[bool] = True,
    ) -> str:
        if format == None:
            format = self._format_type

        def _write(buf: typing.TextIO):
            if self._num_rows() > 0:
                if comment:
                    buf.write(self._get_comment(format))
                    buf.write("\n")
                write_c_dataframe(buf, self._fields, self.table, format)

        return write_or_return(buf, _write)

    @property
    def bounded(self) -> bool:
        return self._bounded

    def _num_rows(self) -> int:
        if not self.active:
            return 0
        return self._length_func()

    def __repr__(self) -> str:
        """Returns a console-friendly representation of the desired parameters for the card"""
        content_lines = []
        content_lines.append(self._get_comment(self._format_type))
        output = "\n".join(content_lines)
        return "TableCard: \n" + output
