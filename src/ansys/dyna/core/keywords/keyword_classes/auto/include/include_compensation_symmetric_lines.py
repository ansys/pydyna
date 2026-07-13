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

"""Module providing the IncludeCompensationSymmetricLines class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_INCLUDECOMPENSATIONSYMMETRICLINES_CARD0 = (
    FieldSchema("symid", int, 0, 10, None),
    FieldSchema("symxy", int, 10, 10, None),
    FieldSchema("x0", float, 20, 10, None),
    FieldSchema("y0", float, 30, 10, None),
)

class IncludeCompensationSymmetricLines(KeywordBase):
    """DYNA INCLUDE_COMPENSATION_SYMMETRIC_LINES keyword"""

    keyword = "INCLUDE"
    subkeyword = "COMPENSATION_SYMMETRIC_LINES"

    def __init__(self, **kwargs):
        """Initialize the IncludeCompensationSymmetricLines class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INCLUDECOMPENSATIONSYMMETRICLINES_CARD0,
                **kwargs,
            ),
        ]
    @property
    def symid(self) -> typing.Optional[int]:
        """Get or set the ID of the symmetry condition being defined
        """ # nopep8
        return self._cards[0].get_value("symid")

    @symid.setter
    def symid(self, value: int) -> None:
        """Set the symid property."""
        self._cards[0].set_value("symid", value)

    @property
    def symxy(self) -> typing.Optional[int]:
        """Get or set the Symmetry boundary condition:
        EQ.1:	Symmetrical about the y - axis
        EQ.2 : Symmetrical about the x - axis
        """ # nopep8
        return self._cards[0].get_value("symxy")

    @symxy.setter
    def symxy(self, value: int) -> None:
        """Set the symxy property."""
        self._cards[0].set_value("symxy", value)

    @property
    def x0(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point on the symmetry plane
        """ # nopep8
        return self._cards[0].get_value("x0")

    @x0.setter
    def x0(self, value: float) -> None:
        """Set the x0 property."""
        self._cards[0].set_value("x0", value)

    @property
    def y0(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point on the symmetry plane
        """ # nopep8
        return self._cards[0].get_value("y0")

    @y0.setter
    def y0(self, value: float) -> None:
        """Set the y0 property."""
        self._cards[0].set_value("y0", value)

