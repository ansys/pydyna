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

"""Module providing the IgaIncludeBezier class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_IGAINCLUDEBEZIER_CARD0 = (
    FieldSchema("filename", str, 0, 256, None),
)

_IGAINCLUDEBEZIER_CARD1 = (
    FieldSchema("filetype", int, 0, 10, None),
    FieldSchema("pid", int, 10, 10, None),
    FieldSchema("dim", int, 20, 10, None),
)

class IgaIncludeBezier(KeywordBase):
    """DYNA IGA_INCLUDE_BEZIER keyword"""

    keyword = "IGA"
    subkeyword = "INCLUDE_BEZIER"

    def __init__(self, **kwargs):
        """Initialize the IgaIncludeBezier class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _IGAINCLUDEBEZIER_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _IGAINCLUDEBEZIER_CARD1,
                **kwargs,
            ),        ]
    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the Name of the file to be included;
        """ # nopep8
        return self._cards[0].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        """Set the filename property."""
        self._cards[0].set_value("filename", value)

    @property
    def filetype(self) -> typing.Optional[int]:
        """Get or set the Type of the file to be included:EQ.1:	ASCII
        """ # nopep8
        return self._cards[1].get_value("filetype")

    @filetype.setter
    def filetype(self, value: int) -> None:
        """Set the filetype property."""
        self._cards[1].set_value("filetype", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID
        """ # nopep8
        return self._cards[1].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[1].set_value("pid", value)

    @property
    def dim(self) -> typing.Optional[int]:
        """Get or set the Parametric dimension:
        EQ.2:	Surface
        EQ.3 : Volume
        """ # nopep8
        return self._cards[1].get_value("dim")

    @dim.setter
    def dim(self, value: int) -> None:
        """Set the dim property."""
        self._cards[1].set_value("dim", value)

