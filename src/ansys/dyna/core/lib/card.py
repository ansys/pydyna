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

from ansys.dyna.core.lib.card_interface import CardInterface
from ansys.dyna.core.lib.field import Field, Flag, to_long  # noqa: F401
from ansys.dyna.core.lib.field_writer import write_comment_line, write_fields
from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core.lib.io_utils import write_or_return
from ansys.dyna.core.lib.kwd_line_formatter import load_dataline, read_line


class Card(CardInterface):
    def __init__(self, fields: typing.List[Field], active_func=None, format=format_type.default):
        self._fields = fields
        self._active_func = active_func
        self._format_type = format

    @property
    def format(self):
        return self._format_type

    @format.setter
    def format(self, value: format_type) -> None:
        self._format_type = value

    def _convert_fields_to_long_format(self) -> typing.List[Field]:
        fields = []
        offset = 0
        for field in self._fields:
            new_field = to_long(field, offset)
            offset += new_field.width
            fields.append(new_field)
        return fields

    def read(self, buf: typing.TextIO) -> bool:
        if not self._is_active():
            return False
        line, to_exit = read_line(buf)
        if to_exit:
            return True
        self._load(line)
        return False

    def _load(self, data_line: str) -> None:
        """loads the card data from a list of strings"""
        fields = self._fields
        if self.format == format_type.long:
            fields = self._convert_fields_to_long_format()
        format = [(field.offset, field.width, field.type) for field in fields]
        values = load_dataline(format, data_line)
        num_fields = len(fields)
        for field_index in range(num_fields):
            self._fields[field_index].value = values[field_index]

    def write(
        self,
        format: typing.Optional[format_type] = None,
        buf: typing.Optional[typing.TextIO] = None,
        comment: typing.Optional[bool] = True,
    ) -> typing.Union[str, None]:
        if format == None:
            format = self._format_type

        def _write(buf: typing.TextIO):
            if self._is_active():
                if comment:
                    write_comment_line(buf, self._fields, format)
                    buf.write("\n")
                write_fields(buf, self._fields, None, format)

        return write_or_return(buf, _write)

    def _is_active(self) -> bool:
        if self._active_func == None:
            return True
        return True if self._active_func() else False

    # only used by tests, TODO move to conftest
    def _get_comment(self, format: format_type) -> str:
        s = io.StringIO()
        write_comment_line(s, self._fields, format)
        return s.getvalue()

    def _get_field_by_name(self, prop: str) -> Field:
        return [f for f in self._fields if f.name == prop][0]

    # not needed by subclasses - only used by methods on keyword classes
    def get_value(self, prop: str) -> typing.Any:
        """gets the value of the field in the card"""
        field = self._get_field_by_name(prop)
        return field.value

    def set_value(self, prop: str, value: typing.Any) -> None:
        """sets the value of the field in the card"""
        self._get_field_by_name(prop).value = value

    def __repr__(self) -> str:
        """Returns a console-friendly representation of the desired parameters for the card"""
        content_lines = []
        content_lines.append(self._get_comment(self._format_type))
        output = "\n".join(content_lines)
        return "StandardCard:" + output
