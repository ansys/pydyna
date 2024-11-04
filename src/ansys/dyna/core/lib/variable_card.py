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
import math
import typing

import ansys.dyna.core.lib.array as arr
from ansys.dyna.core.lib.card_interface import CardInterface
from ansys.dyna.core.lib.field import Field
from ansys.dyna.core.lib.field_writer import write_fields
from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core.lib.kwd_line_formatter import load_dataline, read_line


class VariableCard(CardInterface):
    """Variable length card"""

    def __init__(
        self,
        name: str,
        card_size: int,
        element_width: int,
        type: type,
        length_func: typing.Callable = None,
        active_fn: typing.Callable = None,
        data=None,
        format: format_type = format_type.default,
    ):
        self._name = name
        self._card_size = card_size
        self._element_width = element_width
        self._active_func = active_fn
        self._type = type
        self._initialize_data(0)
        if isinstance(data, list):
            self.data = data
        if length_func == None:
            self._bounded = False
            self._length_func = lambda: len(self._data)
        else:
            self._bounded = True
            self._length_func = length_func
        self._format_type = format

    @property
    def format(self) -> format_type:
        return self._format_type

    @format.setter
    def format(self, value: format_type) -> None:
        self._format_type = value

    def _initialize_data(self, length):
        self._data = arr.array(self._type, length)

    def _is_last_card(self, card_index: int):
        count = self._length_func()
        start = card_index * self._card_size
        end = start + self._card_size
        return end >= count

    def _get_card_range(self, card_index: int):
        start_index = self._card_size * card_index
        if self._is_last_card(card_index):
            # last card, only use the remainder for number of fields
            remainder = self._length_func() % self._card_size
            if remainder == 0:
                remainder = self._card_size
            end_index = start_index + remainder
        else:
            # not last card, use all fields
            end_index = start_index + self._card_size
        return start_index, end_index

    def _get_comment(self, format: format_type) -> str:
        if not self._is_last_card(0):
            count = self._card_size
        else:
            # TODO test case when amount == card_size
            count = self._length_func() % self._card_size or self._card_size
        element_width = self._get_width(format)
        element = " " * element_width
        assert len(self._name) <= element_width - 2, "name of variable field is too long"
        element = element[: -len(self._name)] + self._name
        array = element * count
        return "$#" + array[2:]

    def __get_value(self, index: int):
        if index < len(self._data):
            return self._data[index]
        return self.__get_null_value()

    # TODO this should be on an Array class
    def __get_null_value(self):
        if self._type == float:
            return math.nan
        return None

    def _is_active(self) -> bool:
        if self._active_func == None:
            return True
        return self._active_func()

    def _num_rows(self):
        return math.ceil(self._length_func() / self._card_size)

    def __getitem__(self, index):
        err_string = f"get indexer for VariableCard must be of the form [index] or [start:end].  End must be greater than start"  # noqa : E501
        assert type(index) in (slice, int), err_string
        if type(index) == int:
            return self.__get_value(index)
        assert index.stop > index.start and index.step == None, err_string
        start = index.start
        end = index.stop
        assert type(start) == int, err_string
        assert type(end) == int, err_string
        assert end > start, err_string
        return [self.__get_value(i) for i in range(start, end)]

    def __setitem__(self, index: int, value):
        # first resize up to index.  TODO this should be on an Array class
        while len(self._data) <= index:
            self._data.append(self.__get_null_value())
        # then set value at the specified index
        self._data[index] = value

    def append(self, value) -> None:
        self._data.append(value)

    def extend(self, valuelist) -> None:
        self._data.extend(valuelist)

    def _get_width(self, format: typing.Optional[format_type] = None):
        if format == None:
            format = self.format
        width = self._element_width
        if format == format_type.long:
            width = 20
        return width

    def _load_bounded_from_buffer(self, buf: typing.TextIO) -> None:
        width = self._get_width()
        num_lines = self._num_rows()
        for index in range(num_lines):
            line, exit_loop = read_line(buf)
            if exit_loop:
                break
            start, end = self._get_card_range(index)
            size = end - start
            read_format = [(i * width, width, self._type) for i in range(size)]
            values = load_dataline(read_format, line)
            for j, value in zip(range(start, end), values):
                self[j] = value

    def _load_unbounded_from_buffer(self, buf: typing.TextIO) -> None:
        width = self._get_width()
        self._initialize_data(0)
        while True:
            line, exit_loop = read_line(buf)
            if exit_loop:
                break
            # this is going to be slower... because we don't know how many
            # lines there are going to be.
            size = math.ceil(len(line) / width)
            trailing_spaces = len(line) - width * size
            if trailing_spaces > 0:
                print("Trailing spaces, TODO - write a test!")
                line = line + " " * trailing_spaces
            read_format = [(i * width, width, self._type) for i in range(size)]
            values = load_dataline(read_format, line)
            self.extend(values)

    def read(self, buf: typing.TextIO) -> bool:
        if self.bounded:
            self._load_bounded_from_buffer(buf)
            return False
        else:
            self._load_unbounded_from_buffer(buf)
            return True

    def _get_lines(self, format: typing.Optional[format_type], comment: bool) -> typing.List[str]:
        if self._num_rows() == 0:
            return []
        if format == None:
            format = self._format_type
        content_lines = []
        if comment:
            content_lines.append(self._get_comment(format))
        for i in range(self._num_rows()):
            content_lines.append(self._get_row_data(i, format))
        return content_lines

    def write(
        self,
        format: typing.Optional[format_type] = None,
        buf: typing.Optional[typing.TextIO] = None,
        comment: typing.Optional[bool] = True,
    ) -> str:
        if format == None:
            format = self._format_type
        output = ""
        if self._is_active():
            lines = [row for row in self._get_lines(format, comment) if row]
            output = "\n".join(lines)
        if buf == None:
            return output
        buf.write(output)

    def _write_row(self, format: format_type, start_index: int, end_index: int) -> str:
        """Fields aren't really the right abstraction for a variable card,
        but its an easy way to reuse the code in write_fields so we create fields
        on the fly here. TODO - reuse less of the code without creating fields on the fly"""
        row_values = self[start_index:end_index]
        width = self._element_width
        size = end_index - start_index
        row_fields = [Field(self._name, self._type, i * width, width) for i in range(size)]
        s = io.StringIO()
        write_fields(s, row_fields, row_values, format)
        return s.getvalue()

    def _get_row_data(self, index: int, format: format_type) -> str:
        start_index, end_index = self._get_card_range(index)
        return self._write_row(format, start_index, end_index)

    def __len__(self) -> int:
        return self._length_func()

    @property
    def bounded(self) -> bool:
        return self._bounded

    def __repr__(self) -> str:
        """Returns a console-friendly representation of the desired parameters for the card"""
        content_lines = []
        content_lines.append(self._get_comment(self._format_type))
        output = "\n".join(content_lines)
        return "VariableCard: \n" + output

    @property
    def data(self):
        """Gets or sets the data list of parameter values"""
        return self._data

    @data.setter
    def data(self, vallist: typing.List) -> None:
        self._initialize_data(0)
        self._data.extend(vallist)

    def extend(self, values: typing.List) -> None:
        self._data.extend(values)
