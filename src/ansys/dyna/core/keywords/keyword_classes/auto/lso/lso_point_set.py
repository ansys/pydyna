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

"""Module providing the LsoPointSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_LSOPOINTSET_CARD0 = (
    FieldSchema("setid", int, 0, 10, None),
    FieldSchema("use", int, 10, 10, 1),
)

_LSOPOINTSET_CARD1 = (
    FieldSchema("x", float, 0, 10, None),
    FieldSchema("y", float, 10, 10, None),
    FieldSchema("z", float, 20, 10, None),
)

class LsoPointSet(KeywordBase):
    """DYNA LSO_POINT_SET keyword"""

    keyword = "LSO"
    subkeyword = "POINT_SET"

    def __init__(self, **kwargs):
        """Initialize the LsoPointSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LSOPOINTSET_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LSOPOINTSET_CARD1,
                **kwargs,
            ),        ]
    @property
    def setid(self) -> typing.Optional[int]:
        """Get or set the Identifier for this point set. Called by *LSO_DOMAIN.
        """ # nopep8
        return self._cards[0].get_value("setid")

    @setid.setter
    def setid(self, value: int) -> None:
        """Set the setid property."""
        self._cards[0].set_value("setid", value)

    @property
    def use(self) -> int:
        """Get or set the Points in this set are used as:
        EQ.1: Fixed time history points (default)
        EQ.2: Positions of tracer particles
        """ # nopep8
        return self._cards[0].get_value("use")

    @use.setter
    def use(self, value: int) -> None:
        """Set the use property."""
        if value not in [1, 2, None]:
            raise Exception("""use must be `None` or one of {1,2}.""")
        self._cards[0].set_value("use", value)

    @property
    def x(self) -> typing.Optional[float]:
        """Get or set the Coordinates of the point.
        """ # nopep8
        return self._cards[1].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        """Set the x property."""
        self._cards[1].set_value("x", value)

    @property
    def y(self) -> typing.Optional[float]:
        """Get or set the Coordinates of the point.
        """ # nopep8
        return self._cards[1].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        """Set the y property."""
        self._cards[1].set_value("y", value)

    @property
    def z(self) -> typing.Optional[float]:
        """Get or set the Coordinates of the point.
        """ # nopep8
        return self._cards[1].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        """Set the z property."""
        self._cards[1].set_value("z", value)

