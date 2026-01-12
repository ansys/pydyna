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

"""Module providing the IcfdDefinePoint class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDDEFINEPOINT_CARD0 = (
    FieldSchema("poid", int, 0, 10, None),
    FieldSchema("x", float, 10, 10, None),
    FieldSchema("y", float, 20, 10, None),
    FieldSchema("z", float, 30, 10, None),
    FieldSchema("constpid", int, 40, 10, None),
)

_ICFDDEFINEPOINT_CARD1 = (
    FieldSchema("lcidx", int, 0, 10, None),
    FieldSchema("lcidy", int, 10, 10, None),
    FieldSchema("lcidz", int, 20, 10, None),
)

_ICFDDEFINEPOINT_CARD2 = (
    FieldSchema("lcidw", int, 0, 10, None),
    FieldSchema("xt", float, 10, 10, None),
    FieldSchema("yt", float, 20, 10, None),
    FieldSchema("zt", float, 30, 10, None),
    FieldSchema("xh", float, 40, 10, None),
    FieldSchema("yh", float, 50, 10, None),
    FieldSchema("zh", float, 60, 10, None),
)

class IcfdDefinePoint(KeywordBase):
    """DYNA ICFD_DEFINE_POINT keyword"""

    keyword = "ICFD"
    subkeyword = "DEFINE_POINT"

    def __init__(self, **kwargs):
        """Initialize the IcfdDefinePoint class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDDEFINEPOINT_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ICFDDEFINEPOINT_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ICFDDEFINEPOINT_CARD2,
                **kwargs,
            ),        ]
    @property
    def poid(self) -> typing.Optional[int]:
        """Get or set the Point ID.
        """ # nopep8
        return self._cards[0].get_value("poid")

    @poid.setter
    def poid(self, value: int) -> None:
        """Set the poid property."""
        self._cards[0].set_value("poid", value)

    @property
    def x(self) -> typing.Optional[float]:
        """Get or set the x coordinate for the point.
        """ # nopep8
        return self._cards[0].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        """Set the x property."""
        self._cards[0].set_value("x", value)

    @property
    def y(self) -> typing.Optional[float]:
        """Get or set the y coordinate for the point.
        """ # nopep8
        return self._cards[0].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        """Set the y property."""
        self._cards[0].set_value("y", value)

    @property
    def z(self) -> typing.Optional[float]:
        """Get or set the z coordinate for the point.
        """ # nopep8
        return self._cards[0].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        """Set the z property."""
        self._cards[0].set_value("z", value)

    @property
    def constpid(self) -> typing.Optional[int]:
        """Get or set the Surface Part ID to which the point is constrained. This means that if the selected surface moves, then the localization of the point will update as well.
        """ # nopep8
        return self._cards[0].get_value("constpid")

    @constpid.setter
    def constpid(self, value: int) -> None:
        """Set the constpid property."""
        self._cards[0].set_value("constpid", value)

    @property
    def lcidx(self) -> typing.Optional[int]:
        """Get or set the The point can be made to translate.three load curve IDs for the three translation components.
        """ # nopep8
        return self._cards[1].get_value("lcidx")

    @lcidx.setter
    def lcidx(self, value: int) -> None:
        """Set the lcidx property."""
        self._cards[1].set_value("lcidx", value)

    @property
    def lcidy(self) -> typing.Optional[int]:
        """Get or set the The point can be made to translate.three load curve IDs for the three translation components.
        """ # nopep8
        return self._cards[1].get_value("lcidy")

    @lcidy.setter
    def lcidy(self, value: int) -> None:
        """Set the lcidy property."""
        self._cards[1].set_value("lcidy", value)

    @property
    def lcidz(self) -> typing.Optional[int]:
        """Get or set the The point can be made to translate.three load curve IDs for the three translation components.
        """ # nopep8
        return self._cards[1].get_value("lcidz")

    @lcidz.setter
    def lcidz(self, value: int) -> None:
        """Set the lcidz property."""
        self._cards[1].set_value("lcidz", value)

    @property
    def lcidw(self) -> typing.Optional[int]:
        """Get or set the The point can also be made to rotate. This load curve specifies the angular velocity.
        """ # nopep8
        return self._cards[2].get_value("lcidw")

    @lcidw.setter
    def lcidw(self, value: int) -> None:
        """Set the lcidw property."""
        self._cards[2].set_value("lcidw", value)

    @property
    def xt(self) -> typing.Optional[float]:
        """Get or set the Rotation axis tail point coordinates.
        """ # nopep8
        return self._cards[2].get_value("xt")

    @xt.setter
    def xt(self, value: float) -> None:
        """Set the xt property."""
        self._cards[2].set_value("xt", value)

    @property
    def yt(self) -> typing.Optional[float]:
        """Get or set the Rotation axis tail point coordinates.
        """ # nopep8
        return self._cards[2].get_value("yt")

    @yt.setter
    def yt(self, value: float) -> None:
        """Set the yt property."""
        self._cards[2].set_value("yt", value)

    @property
    def zt(self) -> typing.Optional[float]:
        """Get or set the Rotation axis tail point coordinates.
        """ # nopep8
        return self._cards[2].get_value("zt")

    @zt.setter
    def zt(self, value: float) -> None:
        """Set the zt property."""
        self._cards[2].set_value("zt", value)

    @property
    def xh(self) -> typing.Optional[float]:
        """Get or set the Rotation axis head point coordinates.
        """ # nopep8
        return self._cards[2].get_value("xh")

    @xh.setter
    def xh(self, value: float) -> None:
        """Set the xh property."""
        self._cards[2].set_value("xh", value)

    @property
    def yh(self) -> typing.Optional[float]:
        """Get or set the Rotation axis head point coordinates.
        """ # nopep8
        return self._cards[2].get_value("yh")

    @yh.setter
    def yh(self, value: float) -> None:
        """Set the yh property."""
        self._cards[2].set_value("yh", value)

    @property
    def zh(self) -> typing.Optional[float]:
        """Get or set the Rotation axis head point coordinates.
        """ # nopep8
        return self._cards[2].get_value("zh")

    @zh.setter
    def zh(self, value: float) -> None:
        """Set the zh property."""
        self._cards[2].set_value("zh", value)

