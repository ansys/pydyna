# Copyright (C) 2021 - 2024 ANSYS, Inc. and/or its affiliates.
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

import io
import typing

import numpy as np
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field
from ansys.dyna.core.lib.field_writer import write_c_dataframe
from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core.lib.io_utils import write_or_return
from ansys.dyna.core.lib.kwd_line_formatter import buffer_to_lines

CHECK_TYPE = True


def _check_type(value):
    global CHECK_TYPE
    if CHECK_TYPE:
        assert isinstance(value, pd.DataFrame), "value must be a DataFrame"


class DuplicateCard(Card):
    def __init__(
        self, fields: typing.List[Field], length_func, active_func=None, data=None, format=format_type.default
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
        self._initialized = False
        if data is not None:
            self.table = data

    def _initialize(self):
        if self._bounded:
            self._initialize_data(self._length_func())
        else:
            self._initialize_data(0)
        self._initialized = True

    @property
    def table(self):
        if not self._initialized:
            self._initialize()
        return self._table

    @table.setter
    def table(self, value: pd.DataFrame):
        _check_type(value)
        self._table = pd.DataFrame()
        for field in self._fields:
            if field.name in value:
                field_type = field.type
                if field_type == float:
                    field_type = np.float64
                elif field_type == int:
                    field_type = pd.Int32Dtype()
                self._table[field.name] = value[field.name].astype(field_type)
            else:
                self._table[field.name] = self._make_column(field.type, len(value))
        self._initialized = True

    @property
    def format(self):
        return self._format_type

    @format.setter
    def format(self, value: format_type) -> None:
        self._format_type = value

    def _make_column(self, type, length):
        if type == float:
            arr = np.empty((length,))
            arr[:] = np.nan
            return arr
        elif type == str:
            return [None] * length
        elif type == int:
            return pd.Series([None] * length, dtype=pd.Int32Dtype())
        raise Exception("unexpected type")

    def _initialize_data(self, length):
        data = {}
        num_fields = len(self._fields)
        column_names = np.ndarray(num_fields, "object")
        for index in range(num_fields):
            field = self._fields[index]
            value = self._make_column(field.type, length)
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

    def _read_buffer_as_dataframe(self, buffer: typing.TextIO, fields: typing.Iterable[Field]) -> pd.DataFrame:
        read_options = self._get_read_options()
        df = pd.read_fwf(buffer, **read_options)
        return df

    def _get_fields(self) -> typing.List[Field]:
        fields = self._fields
        if self.format == format_type.long:
            fields = self._convert_fields_to_long_format()
        return fields

    def _load_bounded_from_buffer(self, buf: typing.TextIO) -> None:
        read_options = self._get_read_options()
        read_options["nrows"] = self._num_rows()
        df = pd.read_fwf(buf, **read_options)
        self._table = df
        self._initialized = True

    def _load_unbounded_from_buffer(self, buf: typing.TextIO) -> None:
        data_lines = buffer_to_lines(buf)
        self._load_lines(data_lines)

    def read(self, buf: typing.TextIO) -> None:
        if self.bounded:
            self._initialized = True
            self._load_bounded_from_buffer(buf)
        else:
            self._initialize_data(0)
            self._initialized = True
            self._load_unbounded_from_buffer(buf)

    def _load_lines(self, data_lines: typing.List[str]) -> None:
        fields = self._get_fields()
        buffer = io.StringIO()
        [(buffer.write(line), buffer.write("\n")) for line in data_lines]
        buffer.seek(0)
        self._table = self._read_buffer_as_dataframe(buffer, fields)
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
        if not self._is_active():
            return 0
        return self._length_func()

    def __repr__(self) -> str:
        """Returns a console-friendly representation of the desired parameters for the card"""
        content_lines = []
        content_lines.append(self._get_comment(self._format_type))
        output = "\n".join(content_lines)
        return "DuplicateCard: \n" + output
