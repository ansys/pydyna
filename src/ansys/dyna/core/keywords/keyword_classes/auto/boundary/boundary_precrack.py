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

"""Module providing the BoundaryPrecrack class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BOUNDARYPRECRACK_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("ctype", int, 10, 10, 1),
    FieldSchema("np", int, 20, 10, None),
)

_BOUNDARYPRECRACK_CARD1 = (
    FieldSchema("x", float, 0, 10, None),
    FieldSchema("y", float, 10, 10, None),
    FieldSchema("z", float, 20, 10, None),
)

class BoundaryPrecrack(KeywordBase):
    """DYNA BOUNDARY_PRECRACK keyword"""

    keyword = "BOUNDARY"
    subkeyword = "PRECRACK"

    def __init__(self, **kwargs):
        """Initialize the BoundaryPrecrack class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYPRECRACK_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _BOUNDARYPRECRACK_CARD1,
                **kwargs,
            ),        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID where the pre-crack is located
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def ctype(self) -> int:
        """Get or set the Type of pre-crack:
        EQ.1: straight line
        """ # nopep8
        return self._cards[0].get_value("ctype")

    @ctype.setter
    def ctype(self, value: int) -> None:
        """Set the ctype property."""
        self._cards[0].set_value("ctype", value)

    @property
    def np(self) -> typing.Optional[int]:
        """Get or set the Number of points defining the pre-crack
        """ # nopep8
        return self._cards[0].get_value("np")

    @np.setter
    def np(self, value: int) -> None:
        """Set the np property."""
        self._cards[0].set_value("np", value)

    @property
    def x(self) -> typing.Optional[float]:
        """Get or set the Coordinates of the points defining the pre-crack.  It is recommended that these points be defined such that the pre-crack does not coincide with mesh lines.   A pre-crack coinciding with mesh lines will be automatically moved with sometimes unexpected results, e.g., the moved pre-crack location does not lie on part PID and the pre-crack cannot be created
        """ # nopep8
        return self._cards[1].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        """Set the x property."""
        self._cards[1].set_value("x", value)

    @property
    def y(self) -> typing.Optional[float]:
        """Get or set the Coordinates of the points defining the pre-crack.  It is recommended that these points be defined such that the pre-crack does not coincide with mesh lines.   A pre-crack coinciding with mesh lines will be automatically moved with sometimes unexpected results, e.g., the moved pre-crack location does not lie on part PID and the pre-crack cannot be created
        """ # nopep8
        return self._cards[1].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        """Set the y property."""
        self._cards[1].set_value("y", value)

    @property
    def z(self) -> typing.Optional[float]:
        """Get or set the Coordinates of the points defining the pre-crack.  It is recommended that these points be defined such that the pre-crack does not coincide with mesh lines.   A pre-crack coinciding with mesh lines will be automatically moved with sometimes unexpected results, e.g., the moved pre-crack location does not lie on part PID and the pre-crack cannot be created
        """ # nopep8
        return self._cards[1].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        """Set the z property."""
        self._cards[1].set_value("z", value)

