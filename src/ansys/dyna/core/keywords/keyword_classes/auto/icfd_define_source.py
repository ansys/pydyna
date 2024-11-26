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

class IcfdDefineSource(KeywordBase):
    """DYNA ICFD_DEFINE_SOURCE keyword"""

    keyword = "ICFD"
    subkeyword = "DEFINE_SOURCE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "sid",
                        int,
                        0,
                        10,
                        kwargs.get("sid")
                    ),
                    Field(
                        "lcidk",
                        int,
                        10,
                        10,
                        kwargs.get("lcidk")
                    ),
                    Field(
                        "shape",
                        int,
                        20,
                        10,
                        kwargs.get("shape", 1)
                    ),
                    Field(
                        "r",
                        float,
                        30,
                        10,
                        kwargs.get("r")
                    ),
                    Field(
                        "pid1",
                        int,
                        40,
                        10,
                        kwargs.get("pid1")
                    ),
                    Field(
                        "pid2",
                        int,
                        50,
                        10,
                        kwargs.get("pid2")
                    ),
                ],
            ),
        ]

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the source ID
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[0].set_value("sid", value)

    @property
    def lcidk(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying the evolution of the external source term function of time for the turbulent kinetic energy k equation, see *DEFINE_CURVE,*DEFINE_CURVE_FUNCTION or *DEFINE_FUNCTION. If a DEFINE_FUNCTION is used, the following parameters are allowed:  f(x,y,z,vx,vy,vz,temp,pres,time
        """ # nopep8
        return self._cards[0].get_value("lcidk")

    @lcidk.setter
    def lcidk(self, value: int) -> None:
        self._cards[0].set_value("lcidk", value)

    @property
    def shape(self) -> int:
        """Get or set the Shape of the external source:
        EQ.1 :	Box shape
        EQ.2 :	Cylinder shape
        EQ.3 :	Sphere shape
        """ # nopep8
        return self._cards[0].get_value("shape")

    @shape.setter
    def shape(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""shape must be one of {1,2,3}""")
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
    def pid1(self) -> typing.Optional[int]:
        """Get or set the ID of point (See ICFD_DEFINE_POINT) of minimum coordinates if SHAPE=1, tail point if SHAPE=2, origin if SHAPE=3.
        """ # nopep8
        return self._cards[0].get_value("pid1")

    @pid1.setter
    def pid1(self, value: int) -> None:
        self._cards[0].set_value("pid1", value)

    @property
    def pid2(self) -> typing.Optional[int]:
        """Get or set the ID of point of maximum coordinates if SHAPE=2, head point if SHAPE=2.
        """ # nopep8
        return self._cards[0].get_value("pid2")

    @pid2.setter
    def pid2(self, value: int) -> None:
        self._cards[0].set_value("pid2", value)

