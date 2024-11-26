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

class ControlFormingPreBending(KeywordBase):
    """DYNA CONTROL_FORMING_PRE_BENDING keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_PRE_BENDING"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "pset",
                        int,
                        0,
                        10,
                        kwargs.get("pset")
                    ),
                    Field(
                        "radius",
                        float,
                        10,
                        10,
                        kwargs.get("radius")
                    ),
                    Field(
                        "vx",
                        float,
                        20,
                        10,
                        kwargs.get("vx")
                    ),
                    Field(
                        "vy",
                        float,
                        30,
                        10,
                        kwargs.get("vy")
                    ),
                    Field(
                        "vz",
                        float,
                        40,
                        10,
                        kwargs.get("vz")
                    ),
                    Field(
                        "xc",
                        float,
                        50,
                        10,
                        kwargs.get("xc")
                    ),
                    Field(
                        "yc",
                        float,
                        60,
                        10,
                        kwargs.get("yc")
                    ),
                    Field(
                        "zc",
                        float,
                        70,
                        10,
                        kwargs.get("zc")
                    ),
                ],
            ),
        ]

    @property
    def pset(self) -> typing.Optional[int]:
        """Get or set the Part set ID to be included in the pre-bending.
        """ # nopep8
        return self._cards[0].get_value("pset")

    @pset.setter
    def pset(self, value: int) -> None:
        self._cards[0].set_value("pset", value)

    @property
    def radius(self) -> typing.Optional[float]:
        """Get or set the Radius of the pre-bending.
        GT.0.0: bending center is on the same side as the element normals
        LT.0.0: bending center is on the reverse side of the element normals.
        See figure below for more information.
        """ # nopep8
        return self._cards[0].get_value("radius")

    @radius.setter
    def radius(self, value: float) -> None:
        self._cards[0].set_value("radius", value)

    @property
    def vx(self) -> typing.Optional[float]:
        """Get or set the x Vector components of an axis about which the flat blank will be bent.
        """ # nopep8
        return self._cards[0].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        self._cards[0].set_value("vx", value)

    @property
    def vy(self) -> typing.Optional[float]:
        """Get or set the y Vector components of an axis about which the flat blank will be bent.
        """ # nopep8
        return self._cards[0].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        self._cards[0].set_value("vy", value)

    @property
    def vz(self) -> typing.Optional[float]:
        """Get or set the z Vector components of an axis about which the flat blank will be bent.
        """ # nopep8
        return self._cards[0].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        self._cards[0].set_value("vz", value)

    @property
    def xc(self) -> typing.Optional[float]:
        """Get or set the X coordinates of the center of most-bent location. If undefined,center of gravity of the blank will be used as a default.
        """ # nopep8
        return self._cards[0].get_value("xc")

    @xc.setter
    def xc(self, value: float) -> None:
        self._cards[0].set_value("xc", value)

    @property
    def yc(self) -> typing.Optional[float]:
        """Get or set the Y coordinates of the center of most-bent location. If undefined,center of gravity of the blank will be used as a default..
        """ # nopep8
        return self._cards[0].get_value("yc")

    @yc.setter
    def yc(self, value: float) -> None:
        self._cards[0].set_value("yc", value)

    @property
    def zc(self) -> typing.Optional[float]:
        """Get or set the Z coordinates of the center of most-bent location. If undefined,center of gravity of the blank will be used as a default..
        """ # nopep8
        return self._cards[0].get_value("zc")

    @zc.setter
    def zc(self, value: float) -> None:
        self._cards[0].set_value("zc", value)

