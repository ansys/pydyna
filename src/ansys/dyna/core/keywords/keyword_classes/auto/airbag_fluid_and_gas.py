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

class AirbagFluidAndGas(KeywordBase):
    """DYNA AIRBAG_FLUID_AND_GAS keyword"""

    keyword = "AIRBAG"
    subkeyword = "FLUID_AND_GAS"

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
                        "sidtyp",
                        int,
                        10,
                        10,
                        kwargs.get("sidtyp", 0)
                    ),
                    Field(
                        "rbid",
                        int,
                        20,
                        10,
                        kwargs.get("rbid", 0)
                    ),
                    Field(
                        "vsca",
                        float,
                        30,
                        10,
                        kwargs.get("vsca", 1.0)
                    ),
                    Field(
                        "psca",
                        float,
                        40,
                        10,
                        kwargs.get("psca", 1.0)
                    ),
                    Field(
                        "vini",
                        float,
                        50,
                        10,
                        kwargs.get("vini", 0.0)
                    ),
                    Field(
                        "mwd",
                        float,
                        60,
                        10,
                        kwargs.get("mwd", 0.0)
                    ),
                    Field(
                        "spsf",
                        float,
                        70,
                        10,
                        kwargs.get("spsf", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xwini",
                        float,
                        0,
                        10,
                        kwargs.get("xwini")
                    ),
                    Field(
                        "xwadd",
                        float,
                        10,
                        10,
                        kwargs.get("xwadd")
                    ),
                    Field(
                        "xw",
                        float,
                        20,
                        10,
                        kwargs.get("xw")
                    ),
                    Field(
                        "p",
                        float,
                        30,
                        10,
                        kwargs.get("p")
                    ),
                    Field(
                        "tend",
                        float,
                        40,
                        10,
                        kwargs.get("tend")
                    ),
                    Field(
                        "rho",
                        float,
                        50,
                        10,
                        kwargs.get("rho")
                    ),
                    Field(
                        "lcxw",
                        int,
                        60,
                        10,
                        kwargs.get("lcxw")
                    ),
                    Field(
                        "lcp",
                        int,
                        70,
                        10,
                        kwargs.get("lcp")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "gdir",
                        float,
                        0,
                        10,
                        kwargs.get("gdir")
                    ),
                    Field(
                        "nproj",
                        int,
                        10,
                        10,
                        kwargs.get("nproj", 3)
                    ),
                    Field(
                        "idir",
                        int,
                        20,
                        10,
                        kwargs.get("idir")
                    ),
                    Field(
                        "iidir",
                        int,
                        30,
                        10,
                        kwargs.get("iidir")
                    ),
                    Field(
                        "kappa",
                        float,
                        40,
                        10,
                        kwargs.get("kappa", 1.0)
                    ),
                    Field(
                        "kbm",
                        float,
                        50,
                        10,
                        kwargs.get("kbm")
                    ),
                ],
            ),
        ]

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Set ID.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[0].set_value("sid", value)

    @property
    def sidtyp(self) -> int:
        """Get or set the Set type:
        EQ.0: segment,
        EQ.1: part IDs.
        """ # nopep8
        return self._cards[0].get_value("sidtyp")

    @sidtyp.setter
    def sidtyp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""sidtyp must be one of {0,1}""")
        self._cards[0].set_value("sidtyp", value)

    @property
    def rbid(self) -> int:
        """Get or set the Rigid body part ID for user defined activation subroutine:
        EQ.-RBID: sensor subroutine flags initiates the inflator. Load curves are offset by initiation time,
        EQ.0: the control volume is active from time zero,
        EQ.RBID: user sensor subroutine flags the start of the inflation. Load curves are offset by initiation time.
        """ # nopep8
        return self._cards[0].get_value("rbid")

    @rbid.setter
    def rbid(self, value: int) -> None:
        self._cards[0].set_value("rbid", value)

    @property
    def vsca(self) -> float:
        """Get or set the Volume scale factor, V-sca (default=1.0).
        """ # nopep8
        return self._cards[0].get_value("vsca")

    @vsca.setter
    def vsca(self, value: float) -> None:
        self._cards[0].set_value("vsca", value)

    @property
    def psca(self) -> float:
        """Get or set the Pressure scale factor, P-sca (default=1.0).
        """ # nopep8
        return self._cards[0].get_value("psca")

    @psca.setter
    def psca(self, value: float) -> None:
        self._cards[0].set_value("psca", value)

    @property
    def vini(self) -> float:
        """Get or set the Initial filled volume, V-ini (default=0.0).
        """ # nopep8
        return self._cards[0].get_value("vini")

    @vini.setter
    def vini(self, value: float) -> None:
        self._cards[0].set_value("vini", value)

    @property
    def mwd(self) -> float:
        """Get or set the Mass weighted damping factor, D (default=0.0).
        """ # nopep8
        return self._cards[0].get_value("mwd")

    @mwd.setter
    def mwd(self, value: float) -> None:
        self._cards[0].set_value("mwd", value)

    @property
    def spsf(self) -> float:
        """Get or set the Stagnation pressure scale factor, 0.0 <= gamma <= 1.0.
        """ # nopep8
        return self._cards[0].get_value("spsf")

    @spsf.setter
    def spsf(self, value: float) -> None:
        self._cards[0].set_value("spsf", value)

    @property
    def xwini(self) -> typing.Optional[float]:
        """Get or set the Fluid level at time t = 0 in |GDIR| direction.
        """ # nopep8
        return self._cards[1].get_value("xwini")

    @xwini.setter
    def xwini(self, value: float) -> None:
        self._cards[1].set_value("xwini", value)

    @property
    def xwadd(self) -> typing.Optional[float]:
        """Get or set the Fluid level filling increment per time step.
        """ # nopep8
        return self._cards[1].get_value("xwadd")

    @xwadd.setter
    def xwadd(self, value: float) -> None:
        self._cards[1].set_value("xwadd", value)

    @property
    def xw(self) -> typing.Optional[float]:
        """Get or set the Final fluid level in filling process.
        """ # nopep8
        return self._cards[1].get_value("xw")

    @xw.setter
    def xw(self, value: float) -> None:
        self._cards[1].set_value("xw", value)

    @property
    def p(self) -> typing.Optional[float]:
        """Get or set the Gas pressure at time t = TEND.
        """ # nopep8
        return self._cards[1].get_value("p")

    @p.setter
    def p(self, value: float) -> None:
        self._cards[1].set_value("p", value)

    @property
    def tend(self) -> typing.Optional[float]:
        """Get or set the Time when gas pressure P is reached.
        """ # nopep8
        return self._cards[1].get_value("tend")

    @tend.setter
    def tend(self, value: float) -> None:
        self._cards[1].set_value("tend", value)

    @property
    def rho(self) -> typing.Optional[float]:
        """Get or set the Density of the fluid (e.g. for water, RHO is about 1.0 kg/m3).
        """ # nopep8
        return self._cards[1].get_value("rho")

    @rho.setter
    def rho(self, value: float) -> None:
        self._cards[1].set_value("rho", value)

    @property
    def lcxw(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for fluid level vs. time. XW, XWADD, and XWINI are with this option.
        """ # nopep8
        return self._cards[1].get_value("lcxw")

    @lcxw.setter
    def lcxw(self, value: int) -> None:
        self._cards[1].set_value("lcxw", value)

    @property
    def lcp(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for gas pressure vs. time. P and TEND are ignored with this option.
        """ # nopep8
        return self._cards[1].get_value("lcp")

    @lcp.setter
    def lcp(self, value: int) -> None:
        self._cards[1].set_value("lcp", value)

    @property
    def gdir(self) -> typing.Optional[float]:
        """Get or set the Global direction of gravity (e.g. -3.0 for negative global z-axis).
        EQ.1.0: global x-direction,
        EQ.2.0: global y-direction,
        EQ.3.0: global z-direction.
        """ # nopep8
        return self._cards[2].get_value("gdir")

    @gdir.setter
    def gdir(self, value: float) -> None:
        self._cards[2].set_value("gdir", value)

    @property
    def nproj(self) -> int:
        """Get or set the Number of projection directions (only global axis) for volume calculation.
        """ # nopep8
        return self._cards[2].get_value("nproj")

    @nproj.setter
    def nproj(self, value: int) -> None:
        self._cards[2].set_value("nproj", value)

    @property
    def idir(self) -> typing.Optional[int]:
        """Get or set the First direction of projection (if |NPROJ| != 3), only global axis.
        """ # nopep8
        return self._cards[2].get_value("idir")

    @idir.setter
    def idir(self, value: int) -> None:
        self._cards[2].set_value("idir", value)

    @property
    def iidir(self) -> typing.Optional[int]:
        """Get or set the Second direction of projection (if |NPROJ| = 2), only global axis.
        """ # nopep8
        return self._cards[2].get_value("iidir")

    @iidir.setter
    def iidir(self, value: int) -> None:
        self._cards[2].set_value("iidir", value)

    @property
    def kappa(self) -> float:
        """Get or set the Adiabatic exponent.
        """ # nopep8
        return self._cards[2].get_value("kappa")

    @kappa.setter
    def kappa(self, value: float) -> None:
        self._cards[2].set_value("kappa", value)

    @property
    def kbm(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus of the fluid (e.g. for water, BKM is about 2080 N/mm2).
        """ # nopep8
        return self._cards[2].get_value("kbm")

    @kbm.setter
    def kbm(self, value: float) -> None:
        self._cards[2].set_value("kbm", value)

