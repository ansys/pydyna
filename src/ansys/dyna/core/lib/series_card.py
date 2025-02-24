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

import dataclasses
import io
import math
import typing

import ansys.dyna.core.lib.array as arr
from ansys.dyna.core.lib.card_interface import CardInterface
from ansys.dyna.core.lib.field import Field
from ansys.dyna.core.lib.field_writer import write_comment_line, write_fields
from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core.lib.kwd_line_formatter import load_dataline, read_line
from ansys.dyna.core.lib.parameters import ParameterSet


class SeriesCard(CardInterface):
    """Card holding a series of values."""

    def __init__(
        self,
        name: str,
        fields_per_card: int,
        element_width: int,
        input_type: typing.Union[type, typing.List[type]],
        length_func: typing.Callable = None,
        active_fn: typing.Callable = None,
        type_names: typing.Optional[typing.List[str]] = None,
        data=None,
        format: format_type = format_type.default,
    ):
        self._name = name
        self._fields_per_card = fields_per_card
        self._element_width = element_width
        self._active_func = active_fn
        if isinstance(input_type, list):
            self._type = self._make_struct_datatype(type_names, input_type)
        else:
            self._type = input_type
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

    def _make_struct_datatype(self, type_names, type_types):
        assert type_names is not None, "Type names are required if a list of types is used!"
        assert len(type_types) == len(type_names), "The `type_names` and `input_type` lists have to be the same length"
        dataclass_spec = []
        for type_name, type_type in zip(type_names, type_types):
            dataclass_spec.append((type_name, type_type))
        return dataclasses.make_dataclass(self._name, dataclass_spec)

    def __iter__(self) -> typing.Iterable:
        return iter(self._data)

    @property
    def format(self) -> format_type:
        return self._format_type

    @format.setter
    def format(self, value: format_type) -> None:
        self._format_type = value

    def _uses_structure(self):
        return dataclasses.is_dataclass(self._type)

    def _initialize_data(self, length: int) -> None:
        self._data = arr.array(self._type, length)

    def _get_element_width(self) -> int:
        return self._element_width * self._num_fields()

    def _get_fields_per_card(self) -> int:
        num = self._fields_per_card / self._num_fields()
        int_num = int(num)
        assert num - int_num == 0, "Error"
        return int_num

    def _is_last_card(self, card_index: int) -> bool:
        count = self._length_func()
        fields_per_card = self._get_fields_per_card()
        start = card_index * fields_per_card
        end = start + fields_per_card
        result = end >= count
        return result

    def _get_card_range(self, card_index: int) -> typing.Tuple[int, int]:
        fields_per_card = self._get_fields_per_card()
        start_index = fields_per_card * card_index
        if self._is_last_card(card_index):
            # last card, only use the remainder for number of fields
            remainder = self._length_func() % fields_per_card
            if remainder == 0:
                remainder = fields_per_card
            end_index = start_index + remainder
        else:
            # not last card, use all fields
            end_index = start_index + fields_per_card
        return start_index, end_index

    def _get_comment_struct(self, count: int, format: format_type) -> str:
        s = io.StringIO()
        comment_fields = []
        width = self._get_width(format)
        offset = 0
        for _ in range(count):
            for field in dataclasses.fields(self._type):
                comment_fields.append(Field(field.name, field.type, offset, width))
                offset += width
        write_comment_line(s, comment_fields, format)
        return s.getvalue()

    def _get_comment_scalar(self, count: int, format: format_type) -> str:
        element_width = self._get_width(format)
        element = " " * element_width
        assert len(self._name) <= element_width - 2, "name of variable field is too long"
        element = element[: -len(self._name)] + self._name
        array = element * count
        return "$#" + array[2:]

    def _get_comment(self, format: format_type) -> str:
        fields_per_card = self._get_fields_per_card()
        if not self._is_last_card(0):
            count = fields_per_card
        else:
            # TODO test case when amount == card_size
            count = self._length_func() % fields_per_card or fields_per_card

        if dataclasses.is_dataclass(self._type):
            return self._get_comment_struct(count, format)
        else:
            return self._get_comment_scalar(count, format)

    def __get_value(self, index: int):
        if index < len(self._data):
            return self._data[index]
        if self._bounded:
            return self.__get_null_value()
        else:
            raise IndexError(f"Accessing index {index} out of bound")

    # TODO this should be on an Array class
    def __get_null_value(self):
        if self._type == float:
            return math.nan
        return None

    @property
    def active(self) -> bool:
        if self._active_func == None:
            return True
        return self._active_func()

    def _num_rows(self):
        fields_per_card = self._get_fields_per_card()
        return math.ceil(self._length_func() / fields_per_card)

    def __getitem__(self, index):
        err_string = f"get indexer for SeriesCard must be of the form [index] or [start:end].  End must be greater than start"  # noqa : E501
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

    def _wrap_value(self, value):
        if isinstance(value, self._type):
            return value
        else:
            if self._uses_structure():
                # allow setting struct with tuple or list
                return self._type(*value)
            else:
                return self._type(value)

    def __setitem__(self, index: int, value):
        # first resize up to index.  TODO this should be on an Array class
        while len(self._data) <= index:
            self._data.append(self.__get_null_value())
        # then set value at the specified index
        self._data[index] = self._wrap_value(value)

    def append(self, value) -> None:
        self._data.append(self._wrap_value(value))

    def extend(self, valuelist: typing.Iterable) -> None:
        values = [self._wrap_value(value) for value in valuelist]
        self._data.extend(values)

    def _num_fields(self):
        if dataclasses.is_dataclass(self._type):
            return len(dataclasses.fields(self._type))
        return 1

    def _get_width(self, format: typing.Optional[format_type] = None):
        if format == None:
            format = self.format
        width = self._element_width
        if format == format_type.long:
            width = 20
        return width

    def _read_line(self, size, line):
        num_fields = self._num_fields()
        width = self._get_width()
        read_format = [(i * width * num_fields, width, self._type) for i in range(size)]
        values = load_dataline(read_format, line)
        return values

    def _load_bounded_from_buffer(self, buf: typing.TextIO) -> None:
        num_lines = self._num_rows()
        for index in range(num_lines):
            line, exit_loop = read_line(buf)
            if exit_loop:
                break
            start, end = self._get_card_range(index)
            size = end - start
            values = self._read_line(size, line)
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
            num_fields = self._num_fields()
            text_width = width * num_fields
            size = math.ceil(len(line) / (text_width))
            trailing_spaces = len(line) - text_width * size
            if trailing_spaces > 0:
                print("Trailing spaces, TODO - write a test!")
                line = line + " " * trailing_spaces
            max_amount = min(size, self._get_fields_per_card())
            values = self._read_line(max_amount, line)
            self.extend(values)

    def read(self, buf: typing.TextIO, parameter_set: ParameterSet = None) -> bool:
        # parameter sets are ignored for variable cards
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
        if self.active:
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
        field_width = self._get_element_width()
        element_width = self._element_width
        size = end_index - start_index

        # the field writer uses the element width, not the struct width, for structured types
        # the offset, however, takes into account the struct width.
        row_fields = [Field(self._name, self._type, i * field_width, element_width) for i in range(size)]
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
        return "SeriesCard: \n" + output

    @property
    def data(self):
        """Gets or sets the data list of parameter values"""
        return self._data

    @data.setter
    def data(self, vallist: typing.List) -> None:
        self._initialize_data(0)
        self.extend(vallist)
