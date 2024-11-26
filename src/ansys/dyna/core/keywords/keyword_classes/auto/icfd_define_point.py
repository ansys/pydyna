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
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class IcfdDefinePoint(KeywordBase):
    """DYNA ICFD_DEFINE_POINT keyword"""

    keyword = "ICFD"
    subkeyword = "DEFINE_POINT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "poid",
                        int,
                        0,
                        10,
                        kwargs.get("poid")
                    ),
                    Field(
                        "x",
                        float,
                        10,
                        10,
                        kwargs.get("x")
                    ),
                    Field(
                        "y",
                        float,
                        20,
                        10,
                        kwargs.get("y")
                    ),
                    Field(
                        "z",
                        float,
                        30,
                        10,
                        kwargs.get("z")
                    ),
                    Field(
                        "constpid",
                        int,
                        40,
                        10,
                        kwargs.get("constpid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcidx",
                        int,
                        0,
                        10,
                        kwargs.get("lcidx")
                    ),
                    Field(
                        "lcidy",
                        int,
                        10,
                        10,
                        kwargs.get("lcidy")
                    ),
                    Field(
                        "lcidz",
                        int,
                        20,
                        10,
                        kwargs.get("lcidz")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcidw",
                        int,
                        0,
                        10,
                        kwargs.get("lcidw")
                    ),
                    Field(
                        "xt",
                        float,
                        10,
                        10,
                        kwargs.get("xt")
                    ),
                    Field(
                        "yt",
                        float,
                        20,
                        10,
                        kwargs.get("yt")
                    ),
                    Field(
                        "zt",
                        float,
                        30,
                        10,
                        kwargs.get("zt")
                    ),
                    Field(
                        "xh",
                        float,
                        40,
                        10,
                        kwargs.get("xh")
                    ),
                    Field(
                        "yh",
                        float,
                        50,
                        10,
                        kwargs.get("yh")
                    ),
                    Field(
                        "zh",
                        float,
                        60,
                        10,
                        kwargs.get("zh")
                    ),
                ],
            ),
        ]

    @property
    def poid(self) -> typing.Optional[int]:
        """Get or set the Point ID.
        """ # nopep8
        return self._cards[0].get_value("poid")

    @poid.setter
    def poid(self, value: int) -> None:
        self._cards[0].set_value("poid", value)

    @property
    def x(self) -> typing.Optional[float]:
        """Get or set the x coordinate for the point.
        """ # nopep8
        return self._cards[0].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        self._cards[0].set_value("x", value)

    @property
    def y(self) -> typing.Optional[float]:
        """Get or set the y coordinate for the point.
        """ # nopep8
        return self._cards[0].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        self._cards[0].set_value("y", value)

    @property
    def z(self) -> typing.Optional[float]:
        """Get or set the z coordinate for the point.
        """ # nopep8
        return self._cards[0].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        self._cards[0].set_value("z", value)

    @property
    def constpid(self) -> typing.Optional[int]:
        """Get or set the Surface Part ID to which the point is constrained. This means that if the selected surface moves, then the localization of the point will update as well.
        """ # nopep8
        return self._cards[0].get_value("constpid")

    @constpid.setter
    def constpid(self, value: int) -> None:
        self._cards[0].set_value("constpid", value)

    @property
    def lcidx(self) -> typing.Optional[int]:
        """Get or set the The point can be made to translate.three load curve IDs for the three translation components.
        """ # nopep8
        return self._cards[1].get_value("lcidx")

    @lcidx.setter
    def lcidx(self, value: int) -> None:
        self._cards[1].set_value("lcidx", value)

    @property
    def lcidy(self) -> typing.Optional[int]:
        """Get or set the The point can be made to translate.three load curve IDs for the three translation components.
        """ # nopep8
        return self._cards[1].get_value("lcidy")

    @lcidy.setter
    def lcidy(self, value: int) -> None:
        self._cards[1].set_value("lcidy", value)

    @property
    def lcidz(self) -> typing.Optional[int]:
        """Get or set the The point can be made to translate.three load curve IDs for the three translation components.
        """ # nopep8
        return self._cards[1].get_value("lcidz")

    @lcidz.setter
    def lcidz(self, value: int) -> None:
        self._cards[1].set_value("lcidz", value)

    @property
    def lcidw(self) -> typing.Optional[int]:
        """Get or set the The point can also be made to rotate. This load curve specifies the angular velocity.
        """ # nopep8
        return self._cards[2].get_value("lcidw")

    @lcidw.setter
    def lcidw(self, value: int) -> None:
        self._cards[2].set_value("lcidw", value)

    @property
    def xt(self) -> typing.Optional[float]:
        """Get or set the Rotation axis tail point coordinates.
        """ # nopep8
        return self._cards[2].get_value("xt")

    @xt.setter
    def xt(self, value: float) -> None:
        self._cards[2].set_value("xt", value)

    @property
    def yt(self) -> typing.Optional[float]:
        """Get or set the Rotation axis tail point coordinates.
        """ # nopep8
        return self._cards[2].get_value("yt")

    @yt.setter
    def yt(self, value: float) -> None:
        self._cards[2].set_value("yt", value)

    @property
    def zt(self) -> typing.Optional[float]:
        """Get or set the Rotation axis tail point coordinates.
        """ # nopep8
        return self._cards[2].get_value("zt")

    @zt.setter
    def zt(self, value: float) -> None:
        self._cards[2].set_value("zt", value)

    @property
    def xh(self) -> typing.Optional[float]:
        """Get or set the Rotation axis head point coordinates.
        """ # nopep8
        return self._cards[2].get_value("xh")

    @xh.setter
    def xh(self, value: float) -> None:
        self._cards[2].set_value("xh", value)

    @property
    def yh(self) -> typing.Optional[float]:
        """Get or set the Rotation axis head point coordinates.
        """ # nopep8
        return self._cards[2].get_value("yh")

    @yh.setter
    def yh(self, value: float) -> None:
        self._cards[2].set_value("yh", value)

    @property
    def zh(self) -> typing.Optional[float]:
        """Get or set the Rotation axis head point coordinates.
        """ # nopep8
        return self._cards[2].get_value("zh")

    @zh.setter
    def zh(self, value: float) -> None:
        self._cards[2].set_value("zh", value)

