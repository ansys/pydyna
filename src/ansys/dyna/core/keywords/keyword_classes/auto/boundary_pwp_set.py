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

class BoundaryPwpSet(KeywordBase):
    """DYNA BOUNDARY_PWP_SET keyword"""

    keyword = "BOUNDARY"
    subkeyword = "PWP_SET"

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
                        "lc",
                        int,
                        10,
                        10,
                        kwargs.get("lc")
                    ),
                    Field(
                        "cmult",
                        float,
                        20,
                        10,
                        kwargs.get("cmult", 0.0)
                    ),
                    Field(
                        "lcdr",
                        int,
                        30,
                        10,
                        kwargs.get("lcdr")
                    ),
                    Field(
                        "tbirth",
                        float,
                        40,
                        10,
                        kwargs.get("tbirth", 0.0)
                    ),
                    Field(
                        "tdeath",
                        float,
                        50,
                        10,
                        kwargs.get("tdeath", 1.0E20)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "iphre",
                        int,
                        0,
                        10,
                        kwargs.get("iphre", 0)
                    ),
                    Field(
                        "itotex",
                        int,
                        10,
                        10,
                        kwargs.get("itotex", 0)
                    ),
                    Field(
                        "idrflag",
                        int,
                        20,
                        10,
                        kwargs.get("idrflag", 0)
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "lcleak",
                        int,
                        40,
                        10,
                        kwargs.get("lcleak")
                    ),
                    Field(
                        "cleak",
                        float,
                        50,
                        10,
                        kwargs.get("cleak")
                    ),
                    Field(
                        "lcpum",
                        int,
                        60,
                        10,
                        kwargs.get("lcpum")
                    ),
                ],
            ),
        ]

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Node SET ID.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[0].set_value("sid", value)

    @property
    def lc(self) -> typing.Optional[int]:
        """Get or set the Load curve giving pore water pressure head (length units) vs time. =0: constant pressure head assumed equal to CMULT(leave blank for TABLE option)
        """ # nopep8
        return self._cards[0].get_value("lc")

    @lc.setter
    def lc(self, value: int) -> None:
        self._cards[0].set_value("lc", value)

    @property
    def cmult(self) -> float:
        """Get or set the Factor on curve or constant pressure head if LC=0
        """ # nopep8
        return self._cards[0].get_value("cmult")

    @cmult.setter
    def cmult(self, value: float) -> None:
        self._cards[0].set_value("cmult", value)

    @property
    def lcdr(self) -> typing.Optional[int]:
        """Get or set the Load curve giving pore water pressure head during dynamic relaxation.
        """ # nopep8
        return self._cards[0].get_value("lcdr")

    @lcdr.setter
    def lcdr(self, value: int) -> None:
        self._cards[0].set_value("lcdr", value)

    @property
    def tbirth(self) -> float:
        """Get or set the Time at which boundary condition becomes active
        """ # nopep8
        return self._cards[0].get_value("tbirth")

    @tbirth.setter
    def tbirth(self, value: float) -> None:
        self._cards[0].set_value("tbirth", value)

    @property
    def tdeath(self) -> float:
        """Get or set the Time at which boundary condition becomes inactive
        """ # nopep8
        return self._cards[0].get_value("tdeath")

    @tdeath.setter
    def tdeath(self, value: float) -> None:
        self._cards[0].set_value("tdeath", value)

    @property
    def iphre(self) -> int:
        """Get or set the Flag =1 for phreatic behaviour (water can be removed by the boundary condition but not added, e.g. at a sloping free surface). Not applicable to TABLE option.
        """ # nopep8
        return self._cards[1].get_value("iphre")

    @iphre.setter
    def iphre(self, value: int) -> None:
        self._cards[1].set_value("iphre", value)

    @property
    def itotex(self) -> int:
        """Get or set the Flag for type of pressure boundary condition: (see notes)
        =0: 	Total head
        =1: 	Excess head
        =2:	Hydraulic head
        =4:	Z-coord where head=0 (piezometric level)
        """ # nopep8
        return self._cards[1].get_value("itotex")

    @itotex.setter
    def itotex(self, value: int) -> None:
        if value not in [0, 1, 2, 4]:
            raise Exception("""itotex must be one of {0,1,2,4}""")
        self._cards[1].set_value("itotex", value)

    @property
    def idrflag(self) -> int:
        """Get or set the Active flag:
        =0:	Active only in transient analysis
        =1:	Active only in dynamic relaxation
        =2:	Active in all analysis phases(leave blank for TABLE option)
        """ # nopep8
        return self._cards[1].get_value("idrflag")

    @idrflag.setter
    def idrflag(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""idrflag must be one of {0,1,2}""")
        self._cards[1].set_value("idrflag", value)

    @property
    def lcleak(self) -> typing.Optional[int]:
        """Get or set the Optional load curve ID (see *DEFINE_CURVE) applicable to IPHRE = 1 only, giving area of the hole through which pore fluid leaks to the zero pressure boundary condition. See Remark 9.
        """ # nopep8
        return self._cards[1].get_value("lcleak")

    @lcleak.setter
    def lcleak(self, value: int) -> None:
        self._cards[1].set_value("lcleak", value)

    @property
    def cleak(self) -> typing.Optional[float]:
        """Get or set the Discharge coefficient, applicable when LCLEAK is nonzero
        """ # nopep8
        return self._cards[1].get_value("cleak")

    @cleak.setter
    def cleak(self, value: float) -> None:
        self._cards[1].set_value("cleak", value)

    @property
    def lcpum(self) -> typing.Optional[int]:
        """Get or set the Optional load curve ID (see *DEFINE_CURVE) giving volumetric outflow rate per node. The curve x-axis is time while the y-axis is in units of volume per unit time. If defined, LCPUMP overrides all other input fields on Card 2.  See Remark 11
        """ # nopep8
        return self._cards[1].get_value("lcpum")

    @lcpum.setter
    def lcpum(self, value: int) -> None:
        self._cards[1].set_value("lcpum", value)

