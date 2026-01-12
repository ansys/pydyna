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

import typing

from ansys.dyna.core.lib.card import Card, Field
from ansys.dyna.core.lib.field_schema import CardSchema, FieldSchema
from ansys.dyna.core.lib.field_writer import write_comment_line
from ansys.dyna.core.lib.format_type import card_format, format_type
from ansys.dyna.core.lib.io_utils import write_or_return
from ansys.dyna.core.lib.kwd_line_formatter import read_line

## TODO codegen - add a codegen option to use a custom card
#                 provide the class name, filename and mixin name


class IncludeCard(Card):
    def __init__(self, **kwargs):
        # Inline Card initialization (avoid deprecated constructor path)
        fields = [Field("filename", str, 0, 80, kwargs.get("filename"))]
        field_schemas = tuple(FieldSchema.from_field(f) for f in fields)
        name_to_index = {f.name: i for i, f in enumerate(fields)}
        self._schema = CardSchema(field_schemas, name_to_index)
        self._signature = id(self._schema)
        self._values = [f.value for f in fields]
        self._active_func = None
        self._format_type = format_type.default
        self._card_format = card_format.fixed

    def _read_line(self, buf: typing.TextIO) -> str:
        line, to_exit = read_line(buf)
        if to_exit:
            raise RuntimeError("*INCLUDE card missing filename")
        return line

    def read(self, buf: typing.TextIO, parameter_set) -> None:
        """Reads the card data from an input text buffer."""
        line = self._read_line(buf)
        filename = line
        if filename.endswith(" +"):
            filename = filename[:-2]
            line = self._read_line(buf)
            filename += line
            if filename.endswith(" +"):
                filename = filename[:-2]
                line = self._read_line(buf)
                filename += line
        else:
            filename = filename.strip()
        self.set_value("filename", filename)
        return False

    def write(
        self,
        format: typing.Optional[format_type] = None,
        buf: typing.Optional[typing.TextIO] = None,
        comment: typing.Optional[bool] = True,
    ) -> typing.Union[str, None]:
        if format == None:
            format = self._format_type

        def _write(buf: typing.TextIO):
            if self.active:
                if comment:
                    write_comment_line(buf, self._fields, format)
                    buf.write("\n")
                filename = self.get_value("filename")
                if len(filename) > 236:
                    raise Exception("Maximum filename length is 236 characters")
                if len(filename) <= 80:
                    right_justified_filename = f"{{0:<80}}".format(filename)
                    buf.write(right_justified_filename)
                else:
                    buf.write(filename[0:78])
                    buf.write(" +\n")
                    if len(filename) <= 158:
                        buf.write(filename[78:])
                    else:
                        buf.write(filename[78:156])
                        buf.write(" +\n")
                        buf.write(filename[156:])

        return write_or_return(buf, _write)


class IncludeCardMixin:
    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the File name of file to be included in this keyword file.
        Maximum 80 characters. If the STAMPED_PART option is active, this is the
        DYNAIN file containing the results from metal stamping.
        """  # nopep8
        return self._cards[0].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        self._cards[0].set_value("filename", value)
