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

import io
import typing

from ansys.dyna.core.lib.card_interface import CardInterface
from ansys.dyna.core.lib.field import Field, Flag, to_long  # noqa: F401
from ansys.dyna.core.lib.field_writer import write_comment_line, write_fields, write_fields_csv
from ansys.dyna.core.lib.format_type import card_format, format_type
from ansys.dyna.core.lib.io_utils import write_or_return
from ansys.dyna.core.lib.kwd_line_formatter import load_dataline_with_format, read_line
from ansys.dyna.core.lib.parameters import ParameterSet


class Card(CardInterface):
    def __init__(self, fields: typing.List[Field], active_func=None, format=format_type.default):
        self._fields = fields
        self._active_func = active_func
        self._format_type = format
        self._card_format = card_format.fixed

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

    def read(self, buf: typing.TextIO, parameter_set: ParameterSet = None) -> bool:
        if not self.active:
            return False
        line, to_exit = read_line(buf)
        if to_exit:
            return True
        self._load(line, parameter_set)
        return False

    def _get_format_spec(self, fields):
        format_spec = []
        for field in fields:
            if field._is_flag():
                field_type = field._value
            else:
                field_type = field.type
            format_spec.append((field.offset, field.width, field_type))
        return format_spec

    def _load(self, data_line: str, parameter_set: ParameterSet) -> None:
        """Loads the card data from a list of strings"""
        fields = self._fields
        if self.format == format_type.long:
            fields = self._convert_fields_to_long_format()
        format_spec = self._get_format_spec(fields)
        values, detected_format = load_dataline_with_format(format_spec, data_line, parameter_set)
        self._card_format = detected_format
        num_fields = len(fields)
        for field_index in range(num_fields):
            self._fields[field_index].value = values[field_index]

    def write(
        self,
        format: typing.Optional[format_type] = None,
        buf: typing.Optional[typing.TextIO] = None,
        comment: typing.Optional[bool] = True,
        output_format: typing.Optional[str] = None,
    ) -> typing.Union[str, None]:
        """Write the card to a buffer or return as string.

        Parameters
        ----------
        format : format_type, optional
            Field width format (default, standard, or long).
        buf : TextIO, optional
            Buffer to write to. If None, returns the output as a string.
        comment : bool, optional
            Whether to include the comment line with field names. Default is True.
        output_format : str, optional
            Card serialization format: card_format.fixed or card_format.csv.
            If None (default), uses fixed unless the card was originally read as csv.

        Returns
        -------
        str or None
            The card as a string if buf is None, otherwise None.
        """
        if format is None:
            format = self._format_type
        if output_format is None:
            output_format = self._card_format

        def _write(buf: typing.TextIO):
            if self.active:
                if output_format == card_format.csv:
                    # CSV format: no comment line, comma-separated values
                    write_fields_csv(buf, self._fields)
                else:
                    # Fixed-width format (default)
                    if comment:
                        write_comment_line(buf, self._fields, format)
                        buf.write("\n")
                    write_fields(buf, self._fields, None, format)

        return write_or_return(buf, _write)

    @property
    def active(self) -> bool:
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
        """Gets the value of the field in the card"""
        field = self._get_field_by_name(prop)
        return field.value

    def set_value(self, prop: str, value: typing.Any) -> None:
        """Sets the value of the field in the card"""
        self._get_field_by_name(prop).value = value

    def __repr__(self) -> str:
        """Returns a console-friendly representation of the desired parameters for the card"""
        content_lines = []
        content_lines.append(self._get_comment(self._format_type))
        output = "\n".join(content_lines)
        return "StandardCard:" + output
