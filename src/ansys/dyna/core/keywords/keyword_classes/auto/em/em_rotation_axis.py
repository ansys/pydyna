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

"""Module providing the EmRotationAxis class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EMROTATIONAXIS_CARD0 = (
    FieldSchema("xp", float, 0, 10, None),
    FieldSchema("yp", float, 10, 10, None),
    FieldSchema("zp", float, 20, 10, None),
    FieldSchema("xd", float, 30, 10, None),
    FieldSchema("yd", float, 40, 10, None),
    FieldSchema("zd", float, 50, 10, None),
    FieldSchema("numsec", int, 60, 10, None),
)

class EmRotationAxis(KeywordBase):
    """DYNA EM_ROTATION_AXIS keyword"""

    keyword = "EM"
    subkeyword = "ROTATION_AXIS"

    def __init__(self, **kwargs):
        """Initialize the EmRotationAxis class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMROTATIONAXIS_CARD0,
                **kwargs,
            ),        ]
    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the x coordinate of the point
        """ # nopep8
        return self._cards[0].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[0].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the y coordinate of the point
        """ # nopep8
        return self._cards[0].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[0].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the z coordinate of the point
        """ # nopep8
        return self._cards[0].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[0].set_value("zp", value)

    @property
    def xd(self) -> typing.Optional[float]:
        """Get or set the x coordinate of the direction of the axis
        """ # nopep8
        return self._cards[0].get_value("xd")

    @xd.setter
    def xd(self, value: float) -> None:
        """Set the xd property."""
        self._cards[0].set_value("xd", value)

    @property
    def yd(self) -> typing.Optional[float]:
        """Get or set the y coordinate of the direction of the axis
        """ # nopep8
        return self._cards[0].get_value("yd")

    @yd.setter
    def yd(self, value: float) -> None:
        """Set the yd property."""
        self._cards[0].set_value("yd", value)

    @property
    def zd(self) -> typing.Optional[float]:
        """Get or set the z coordinate of the direction of the axis
        """ # nopep8
        return self._cards[0].get_value("zd")

    @zd.setter
    def zd(self, value: float) -> None:
        """Set the zd property."""
        self._cards[0].set_value("zd", value)

    @property
    def numsec(self) -> typing.Optional[int]:
        """Get or set the Number of Sectors. This field gives the ratio of the full circle to the angular extension of the mesh.This has to be a power of two. For example, NUMSEC = 4 means that the mesh of the part represents one fourth of the total circle.If NUMSEC = 0 for *EM_2DAXI, the solver will replace it with this value.
        """ # nopep8
        return self._cards[0].get_value("numsec")

    @numsec.setter
    def numsec(self, value: int) -> None:
        """Set the numsec property."""
        self._cards[0].set_value("numsec", value)

