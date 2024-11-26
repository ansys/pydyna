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

class IcfdDefineHeatsource(KeywordBase):
    """DYNA ICFD_DEFINE_HEATSOURCE keyword"""

    keyword = "ICFD"
    subkeyword = "DEFINE_HEATSOURCE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "hsid",
                        int,
                        0,
                        10,
                        kwargs.get("hsid")
                    ),
                    Field(
                        "lcid",
                        int,
                        10,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "shape",
                        int,
                        20,
                        10,
                        kwargs.get("shape")
                    ),
                    Field(
                        "r",
                        float,
                        30,
                        10,
                        kwargs.get("r")
                    ),
                    Field(
                        "ptid1",
                        int,
                        40,
                        10,
                        kwargs.get("ptid1")
                    ),
                    Field(
                        "ptid2",
                        int,
                        50,
                        10,
                        kwargs.get("ptid2")
                    ),
                ],
            ),
        ]

    @property
    def hsid(self) -> typing.Optional[int]:
        """Get or set the Heat source ID
        """ # nopep8
        return self._cards[0].get_value("hsid")

    @hsid.setter
    def hsid(self, value: int) -> None:
        self._cards[0].set_value("hsid", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying the evolution of the heat source term function of time for the X, Y and Z dofs, see *DEFINE_CURVE,*DEFINE_CURVE_FUNCTION or *DEFINE_FUNCTION. If a DEFINE_FUNCTION is used, the following parameters are allowed:  f(x,y,z,vx,vy,vz,temp,pres,time).
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def shape(self) -> typing.Optional[int]:
        """Get or set the Shape of the volumetric heat source:
        EQ.1 :	Box shape
        EQ.2 :	Cylinder shape
        EQ.3 :	Sphere shape
        """ # nopep8
        return self._cards[0].get_value("shape")

    @shape.setter
    def shape(self, value: int) -> None:
        self._cards[0].set_value("shape", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Radius of the sphere is SHAPE=3
        """ # nopep8
        return self._cards[0].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        self._cards[0].set_value("r", value)

    @property
    def ptid1(self) -> typing.Optional[int]:
        """Get or set the ID of point (See ICFD_DEFINE_POINT) of minimum coordinates if SHAPE=1, tail point if SHAPE=2, origin if SHAPE=3
        """ # nopep8
        return self._cards[0].get_value("ptid1")

    @ptid1.setter
    def ptid1(self, value: int) -> None:
        self._cards[0].set_value("ptid1", value)

    @property
    def ptid2(self) -> typing.Optional[int]:
        """Get or set the ID of point of maximum coordinates if SHAPE=2, head point if SHAPE=2.
        """ # nopep8
        return self._cards[0].get_value("ptid2")

    @ptid2.setter
    def ptid2(self, value: int) -> None:
        self._cards[0].set_value("ptid2", value)

