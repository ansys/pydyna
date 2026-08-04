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

"""Module providing the DualcesePointset class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DUALCESEPOINTSET_CARD0 = (
    FieldSchema("pntsid", int, 0, 10, None),
)

_DUALCESEPOINTSET_CARD1 = (
    FieldSchema("x", float, 0, 10, None),
    FieldSchema("y", float, 10, 10, None),
    FieldSchema("z", float, 20, 10, None),
)

class DualcesePointset(KeywordBase):
    """DYNA DUALCESE_POINTSET keyword"""

    keyword = "DUALCESE"
    subkeyword = "POINTSET"

    def __init__(self, **kwargs):
        """Initialize the DualcesePointset class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEPOINTSET_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DUALCESEPOINTSET_CARD1,
                **kwargs,
            ),
        ]
    @property
    def pntsid(self) -> typing.Optional[int]:
        """Get or set the ID for this point set which can be used by *DUALCESE_DATABASE_HISTORY_POINT_SET or *DUALCESE_ENSIGHT_DOMAIN
        """ # nopep8
        return self._cards[0].get_value("pntsid")

    @pntsid.setter
    def pntsid(self, value: int) -> None:
        """Set the pntsid property."""
        self._cards[0].set_value("pntsid", value)

    @property
    def x(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point. As many points as desired can be specified
        """ # nopep8
        return self._cards[1].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        """Set the x property."""
        self._cards[1].set_value("x", value)

    @property
    def y(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point. As many points as desired can be specified
        """ # nopep8
        return self._cards[1].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        """Set the y property."""
        self._cards[1].set_value("y", value)

    @property
    def z(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point. As many points as desired can be specified
        """ # nopep8
        return self._cards[1].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        """Set the z property."""
        self._cards[1].set_value("z", value)

