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

class IcfdBoundaryConjHeat(KeywordBase):
    """DYNA ICFD_BOUNDARY_CONJ_HEAT keyword"""

    keyword = "ICFD"
    subkeyword = "BOUNDARY_CONJ_HEAT"

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
                        "ctype",
                        int,
                        10,
                        10,
                        kwargs.get("ctype", 0)
                    ),
                    Field(
                        "val",
                        float,
                        20,
                        10,
                        kwargs.get("val", 0.0)
                    ),
                    Field(
                        "sflcid",
                        float,
                        30,
                        10,
                        kwargs.get("sflcid")
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the PID of the fluid surface in contact with the solid
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def ctype(self) -> int:
        """Get or set the Contact type:
        EQ.0:	Constraint approach.
        EQ.1 : Mortar contact
        """ # nopep8
        return self._cards[0].get_value("ctype")

    @ctype.setter
    def ctype(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ctype must be one of {0,1}""")
        self._cards[0].set_value("ctype", value)

    @property
    def val(self) -> float:
        """Get or set the Optional Temperature drop if CTYPE=0 or Interface Heat Transfer Coefficient if CTYPE=1 (high value by default to insure perfect contact)
        """ # nopep8
        return self._cards[0].get_value("val")

    @val.setter
    def val(self, value: float) -> None:
        self._cards[0].set_value("val", value)

    @property
    def sflcid(self) -> typing.Optional[float]:
        """Get or set the Load curve ID used to describe scale factor on VAL value versus time, see *DEFINE_CURVE, *DEFINE_CURVE_FUNCTION, or *DEFINE_FUNCTION.  If a DEFINE_FUNCTION is used, the following parameters are allowed: f(x,y,z,vx,vy,vz,temp,pres,time).
        """ # nopep8
        return self._cards[0].get_value("sflcid")

    @sflcid.setter
    def sflcid(self, value: float) -> None:
        self._cards[0].set_value("sflcid", value)

