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

class IcfdBoundaryConvectionTemp(KeywordBase):
    """DYNA ICFD_BOUNDARY_CONVECTION_TEMP keyword"""

    keyword = "ICFD"
    subkeyword = "BOUNDARY_CONVECTION_TEMP"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "hlcid",
                        int,
                        10,
                        10,
                        kwargs.get("hlcid")
                    ),
                    Field(
                        "hsf",
                        float,
                        20,
                        10,
                        kwargs.get("hsf", 1.0)
                    ),
                    Field(
                        "tblcid",
                        int,
                        30,
                        10,
                        kwargs.get("tblcid")
                    ),
                    Field(
                        "tbsf",
                        float,
                        40,
                        10,
                        kwargs.get("tbsf", 1.0)
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the PID for a fluid surface.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def hlcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the heat transfer coefficient value versus time, see *DEFINE_CURVE,*DEFINE_CURVE_FUNCTION or *DEFINE_FUNCTION. If a DEFINE_FUNCTION is used, the following parameters are allowed:  f(x,y,z,vx,vy,vz,temp,pres,time).
        """ # nopep8
        return self._cards[0].get_value("hlcid")

    @hlcid.setter
    def hlcid(self, value: int) -> None:
        self._cards[0].set_value("hlcid", value)

    @property
    def hsf(self) -> float:
        """Get or set the Load curve scale factor applied on the heat transfer coefficient value.  (default=1.0)
        """ # nopep8
        return self._cards[0].get_value("hsf")

    @hsf.setter
    def hsf(self, value: float) -> None:
        self._cards[0].set_value("hsf", value)

    @property
    def tblcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the environment (i.e bulk) temperature value versus time, see *DEFINE_CURVE,*DEFINE_CURVE_FUNCTION or *DEFINE_FUNCTION. If a DEFINE_FUNCTION is used, the following parameters are allowed:  f(x,y,z,vx,vy,vz,temp,pres,time).
        """ # nopep8
        return self._cards[0].get_value("tblcid")

    @tblcid.setter
    def tblcid(self, value: int) -> None:
        self._cards[0].set_value("tblcid", value)

    @property
    def tbsf(self) -> float:
        """Get or set the Load curve scale factor applied on the environment value.  (default=1.0)
        """ # nopep8
        return self._cards[0].get_value("tbsf")

    @tbsf.setter
    def tbsf(self, value: float) -> None:
        self._cards[0].set_value("tbsf", value)

