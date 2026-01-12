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

"""Contains the ElementSolid class."""

import typing

from ansys.dyna.core.lib.field import Field
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.kwd_line_formatter import buffer_to_lines
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.table_card_group import TableCardGroup

# Schema definitions for optimized Card creation
_ELEMENTSOLID_CARD0 = (
    FieldSchema("eid", int, 0, 8, None),
    FieldSchema("pid", int, 8, 8, None),
)

_ELEMENTSOLID_CARD1 = (
    FieldSchema("n1", int, 0, 8, None),
    FieldSchema("n2", int, 8, 8, None),
    FieldSchema("n3", int, 16, 8, None),
    FieldSchema("n4", int, 24, 8, None),
    FieldSchema("n5", int, 32, 8, None),
    FieldSchema("n6", int, 40, 8, None),
    FieldSchema("n7", int, 48, 8, None),
    FieldSchema("n8", int, 56, 8, None),
    FieldSchema("n9", int, 64, 8, None),
    FieldSchema("n10", int, 72, 8, None),
)


class ElementSolid(KeywordBase):
    """DYNA ELEMENT_SOLID (ten nodes format) keyword"""

    keyword = "ELEMENT"
    subkeyword = "SOLID"

    def __init__(self, **kwargs):
        """Initialize the ElementSolid class"""
        super().__init__(**kwargs)
        self._cards = [
            TableCardGroup(
                [
                    _ELEMENTSOLID_CARD0,
                    _ELEMENTSOLID_CARD1,
                ],
                None,
                data=kwargs.get("elements"),
            ),
        ]

    def before_read(self, buf: typing.TextIO) -> None:
        """Reads the first line of the buffer to determine the format."""
        pos = buf.tell()

        lines = buffer_to_lines(buf)
        if len(lines) < 2:
            return
        if len(lines[0].strip().split()) > 2:
            self.set_legacy_format()

        buf.seek(pos)

    def set_legacy_format(self):
        """Sets the legacy format."""
        self._cards = [
            TableCard(
                [
                    Field("eid", int, 0, 8),
                    Field("pid", int, 8, 8),
                    Field("n1", int, 16, 8),
                    Field("n2", int, 24, 8),
                    Field("n3", int, 32, 8),
                    Field("n4", int, 40, 8),
                    Field("n5", int, 48, 8),
                    Field("n6", int, 56, 8),
                    Field("n7", int, 64, 8),
                    Field("n8", int, 72, 8),
                ],
                None,
                data=self.elements,
            ),
        ]

    @property
    def elements(self):
        """Gets the full table of elements"""
        return self._cards[0].table

    @elements.setter
    def elements(self, df):
        """Sets elements from the dataframe df"""
        self._cards[0].table = df
