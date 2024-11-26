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

import typing

from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core.lib.kwd_line_formatter import read_line

from .card_interface import CardInterface

# TODO - should TextCard do anything special for long format?


class TextCard(CardInterface):
    def __init__(self, name: str, content: str = None, format=format_type.default):
        self.value = content
        self._name = name
        self._format_type = format

    @property
    def bounded(self) -> bool:
        """Text cards are always unbounded."""
        return False

    @property
    def format(self) -> format_type:
        return self._format_type

    @format.setter
    def format(self, value: format_type) -> None:
        self._format_type = value

    def _get_comment(self, format: typing.Optional[format_type]):
        if format == None:
            format = self._format_type
        if format != format_type.long:
            return "$#" + f"{{0:>{78}}}".format(self._name)
        else:
            return "$#" + f"{{0:>{158}}}".format(self._name)

    def read(self, buf: typing.TextIO) -> None:
        self._content_lines = []
        while True:
            line, exit_loop = read_line(buf)
            if exit_loop:
                break
            self._content_lines.append(line)

    def write(
        self,
        format: typing.Optional[format_type] = None,
        buf: typing.Optional[typing.TextIO] = None,
        comment: typing.Optional[bool] = True,
    ) -> str:
        if format == None:
            format = self._format_type
        rows = []
        if comment:
            rows.append(self._get_comment(format))
        rows.extend(self._content_lines)
        lines = [row for row in rows if row]
        output = "\n".join(lines)
        if buf == None:
            return output
        buf.write(output)

    @property
    def value(self) -> str:
        return "\n".join(self._content_lines)

    @value.setter
    def value(self, value: str):
        if value == None:
            self._content_lines = []
        else:
            self._content_lines = value.split("\n")
