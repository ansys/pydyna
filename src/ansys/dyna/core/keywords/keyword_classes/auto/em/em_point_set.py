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

"""Module providing the EmPointSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EMPOINTSET_CARD0 = (
    FieldSchema("ptsid", int, 0, 10, 0),
    FieldSchema("ptstype", int, 10, 10, 0),
    FieldSchema("vx", float, 20, 10, 0.0),
    FieldSchema("vy", float, 30, 10, 0.0),
    FieldSchema("vz", float, 40, 10, 0.0),
)

_EMPOINTSET_CARD1 = (
    FieldSchema("ptid", int, 0, 10, None),
    FieldSchema("x", float, 10, 10, None),
    FieldSchema("y", float, 20, 10, None),
    FieldSchema("z", float, 30, 10, None),
    FieldSchema("ipos", int, 40, 10, 0),
)

class EmPointSet(KeywordBase):
    """DYNA EM_POINT_SET keyword"""

    keyword = "EM"
    subkeyword = "POINT_SET"

    def __init__(self, **kwargs):
        """Initialize the EmPointSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMPOINTSET_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EMPOINTSET_CARD1,
                **kwargs,
            ),
        ]
    @property
    def ptsid(self) -> int:
        """Get or set the Point set ID.
        """ # nopep8
        return self._cards[0].get_value("ptsid")

    @ptsid.setter
    def ptsid(self, value: int) -> None:
        """Set the ptsid property."""
        self._cards[0].set_value("ptsid", value)

    @property
    def ptstype(self) -> int:
        """Get or set the Point set type:
        EQ.0: Fixed points.
        EQ.1: Tracer points using prescribed velocity.
        """ # nopep8
        return self._cards[0].get_value("ptstype")

    @ptstype.setter
    def ptstype(self, value: int) -> None:
        """Set the ptstype property."""
        self._cards[0].set_value("ptstype", value)

    @property
    def vx(self) -> float:
        """Get or set the Constant velocities used when PTSTYPE = 1
        """ # nopep8
        return self._cards[0].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        """Set the vx property."""
        self._cards[0].set_value("vx", value)

    @property
    def vy(self) -> float:
        """Get or set the Constant velocities used when PTSTYPE = 1
        """ # nopep8
        return self._cards[0].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        """Set the vy property."""
        self._cards[0].set_value("vy", value)

    @property
    def vz(self) -> float:
        """Get or set the Constant velocities used when PTSTYPE = 1
        """ # nopep8
        return self._cards[0].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        """Set the vz property."""
        self._cards[0].set_value("vz", value)

    @property
    def ptid(self) -> typing.Optional[int]:
        """Get or set the Point ID.
        """ # nopep8
        return self._cards[1].get_value("ptid")

    @ptid.setter
    def ptid(self, value: int) -> None:
        """Set the ptid property."""
        self._cards[1].set_value("ptid", value)

    @property
    def x(self) -> typing.Optional[float]:
        """Get or set the Point initial coordinates
        """ # nopep8
        return self._cards[1].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        """Set the x property."""
        self._cards[1].set_value("x", value)

    @property
    def y(self) -> typing.Optional[float]:
        """Get or set the Point initial coordinates
        """ # nopep8
        return self._cards[1].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        """Set the y property."""
        self._cards[1].set_value("y", value)

    @property
    def z(self) -> typing.Optional[float]:
        """Get or set the Point initial coordinates
        """ # nopep8
        return self._cards[1].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        """Set the z property."""
        self._cards[1].set_value("z", value)

    @property
    def ipos(self) -> int:
        """Get or set the Position flag:
        EQ.0:The solver determines if the point is inside or outside of the conductors.
        EQ.1: The point outside of the conductors during the entire simulation.The solver does not check; hence a gain in computation time.
        """ # nopep8
        return self._cards[1].get_value("ipos")

    @ipos.setter
    def ipos(self, value: int) -> None:
        """Set the ipos property."""
        if value not in [0, 1, None]:
            raise Exception("""ipos must be `None` or one of {0,1}.""")
        self._cards[1].set_value("ipos", value)

