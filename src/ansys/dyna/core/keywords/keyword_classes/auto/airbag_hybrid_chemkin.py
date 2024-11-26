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

class AirbagHybridChemkin(KeywordBase):
    """DYNA AIRBAG_HYBRID_CHEMKIN keyword"""

    keyword = "AIRBAG"
    subkeyword = "HYBRID_CHEMKIN"

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
                        "lcidm",
                        int,
                        0,
                        10,
                        kwargs.get("lcidm")
                    ),
                    Field(
                        "lcidt",
                        int,
                        10,
                        10,
                        kwargs.get("lcidt")
                    ),
                    Field(
                        "ngas",
                        int,
                        20,
                        10,
                        kwargs.get("ngas")
                    ),
                    Field(
                        "data",
                        int,
                        30,
                        10,
                        kwargs.get("data", 0)
                    ),
                    Field(
                        "atmt",
                        float,
                        40,
                        10,
                        kwargs.get("atmt")
                    ),
                    Field(
                        "atmp",
                        float,
                        50,
                        10,
                        kwargs.get("atmp")
                    ),
                    Field(
                        "rg",
                        float,
                        60,
                        10,
                        kwargs.get("rg")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "hconv",
                        float,
                        0,
                        10,
                        kwargs.get("hconv", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c23",
                        float,
                        0,
                        10,
                        kwargs.get("c23", 0.0)
                    ),
                    Field(
                        "a23",
                        float,
                        10,
                        10,
                        kwargs.get("a23", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "chname",
                        str,
                        0,
                        10,
                        kwargs.get("chname")
                    ),
                    Field(
                        "mw",
                        float,
                        10,
                        10,
                        kwargs.get("mw")
                    ),
                    Field(
                        "lcidn",
                        int,
                        20,
                        10,
                        kwargs.get("lcidn", 0)
                    ),
                    Field(
                        "fmole",
                        float,
                        30,
                        10,
                        kwargs.get("fmole")
                    ),
                    Field(
                        "fmolet",
                        float,
                        40,
                        10,
                        kwargs.get("fmolet", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "tlow",
                        float,
                        0,
                        10,
                        kwargs.get("tlow")
                    ),
                    Field(
                        "tmid",
                        float,
                        10,
                        10,
                        kwargs.get("tmid")
                    ),
                    Field(
                        "thigh",
                        float,
                        20,
                        10,
                        kwargs.get("thigh")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "alow",
                        float,
                        0,
                        10,
                        kwargs.get("alow")
                    ),
                    Field(
                        "blow",
                        float,
                        10,
                        10,
                        kwargs.get("blow")
                    ),
                    Field(
                        "clow",
                        float,
                        20,
                        10,
                        kwargs.get("clow")
                    ),
                    Field(
                        "dlow",
                        float,
                        30,
                        10,
                        kwargs.get("dlow")
                    ),
                    Field(
                        "elow",
                        float,
                        40,
                        10,
                        kwargs.get("elow")
                    ),
                    Field(
                        "flow",
                        float,
                        50,
                        10,
                        kwargs.get("flow")
                    ),
                    Field(
                        "glow",
                        float,
                        60,
                        10,
                        kwargs.get("glow")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ahigh",
                        float,
                        0,
                        10,
                        kwargs.get("ahigh")
                    ),
                    Field(
                        "bhigh",
                        float,
                        10,
                        10,
                        kwargs.get("bhigh")
                    ),
                    Field(
                        "chigh",
                        float,
                        20,
                        10,
                        kwargs.get("chigh")
                    ),
                    Field(
                        "dhigh",
                        float,
                        30,
                        10,
                        kwargs.get("dhigh")
                    ),
                    Field(
                        "ehigh",
                        float,
                        40,
                        10,
                        kwargs.get("ehigh")
                    ),
                    Field(
                        "fhigh",
                        float,
                        50,
                        10,
                        kwargs.get("fhigh")
                    ),
                    Field(
                        "ghigh",
                        float,
                        60,
                        10,
                        kwargs.get("ghigh")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "a",
                        float,
                        0,
                        10,
                        kwargs.get("a")
                    ),
                    Field(
                        "b",
                        float,
                        10,
                        10,
                        kwargs.get("b", 0.0)
                    ),
                    Field(
                        "c",
                        float,
                        20,
                        10,
                        kwargs.get("c", 0.0)
                    ),
                    Field(
                        "d",
                        float,
                        30,
                        10,
                        kwargs.get("d", 0.0)
                    ),
                    Field(
                        "e",
                        float,
                        40,
                        10,
                        kwargs.get("e", 0.0)
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
    def lcidm(self) -> typing.Optional[int]:
        """Get or set the Load curve specifying input mass flow rate versus time.
        GT.0: piece wise linear interpolation
        LT.0: cubic spline interpolation
        """ # nopep8
        return self._cards[1].get_value("lcidm")

    @lcidm.setter
    def lcidm(self, value: int) -> None:
        self._cards[1].set_value("lcidm", value)

    @property
    def lcidt(self) -> typing.Optional[int]:
        """Get or set the Load curve specifying input gas temperature versus time.
        GT.0: piece wise linear interpolation
        LT.0: cubic spline interpolation
        """ # nopep8
        return self._cards[1].get_value("lcidt")

    @lcidt.setter
    def lcidt(self, value: int) -> None:
        self._cards[1].set_value("lcidt", value)

    @property
    def ngas(self) -> typing.Optional[int]:
        """Get or set the Number of gas inputs to be defined below (including initial air).
        """ # nopep8
        return self._cards[1].get_value("ngas")

    @ngas.setter
    def ngas(self, value: int) -> None:
        self._cards[1].set_value("ngas", value)

    @property
    def data(self) -> int:
        """Get or set the Thermodynamic database.
        EQ.1: NIST database (3 additional property cards are required below),
        EQ.2: CHEMKIN database (no additional property cards are required),
        EQ.3: Polynomial data (1 additional property card is required below).
        """ # nopep8
        return self._cards[1].get_value("data")

    @data.setter
    def data(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""data must be one of {0,1,2,3}""")
        self._cards[1].set_value("data", value)

    @property
    def atmt(self) -> typing.Optional[float]:
        """Get or set the Atmospheric temperature.
        """ # nopep8
        return self._cards[1].get_value("atmt")

    @atmt.setter
    def atmt(self, value: float) -> None:
        self._cards[1].set_value("atmt", value)

    @property
    def atmp(self) -> typing.Optional[float]:
        """Get or set the Atmospheric pressure.
        """ # nopep8
        return self._cards[1].get_value("atmp")

    @atmp.setter
    def atmp(self, value: float) -> None:
        self._cards[1].set_value("atmp", value)

    @property
    def rg(self) -> typing.Optional[float]:
        """Get or set the Universal gas constant.
        """ # nopep8
        return self._cards[1].get_value("rg")

    @rg.setter
    def rg(self, value: float) -> None:
        self._cards[1].set_value("rg", value)

    @property
    def hconv(self) -> float:
        """Get or set the Convection heat transfer coefficient
        """ # nopep8
        return self._cards[2].get_value("hconv")

    @hconv.setter
    def hconv(self, value: float) -> None:
        self._cards[2].set_value("hconv", value)

    @property
    def c23(self) -> float:
        """Get or set the Vent orifice coefficient
        """ # nopep8
        return self._cards[3].get_value("c23")

    @c23.setter
    def c23(self, value: float) -> None:
        self._cards[3].set_value("c23", value)

    @property
    def a23(self) -> float:
        """Get or set the Vent orifice area
        """ # nopep8
        return self._cards[3].get_value("a23")

    @a23.setter
    def a23(self, value: float) -> None:
        self._cards[3].set_value("a23", value)

    @property
    def chname(self) -> typing.Optional[str]:
        """Get or set the Chemical symbol for this gas species (e.g., N2 for nitrogen, AR for argon).
        Required for DATA=2 (CHEMKIN), optional for DATA=1 or DATA=3.
        """ # nopep8
        return self._cards[4].get_value("chname")

    @chname.setter
    def chname(self, value: str) -> None:
        self._cards[4].set_value("chname", value)

    @property
    def mw(self) -> typing.Optional[float]:
        """Get or set the Molecular weight of this gas species.
        """ # nopep8
        return self._cards[4].get_value("mw")

    @mw.setter
    def mw(self, value: float) -> None:
        self._cards[4].set_value("mw", value)

    @property
    def lcidn(self) -> int:
        """Get or set the Load curve specifying the input mole fraction versus time for this gas species. If >0, FMOLE is not used.
        """ # nopep8
        return self._cards[4].get_value("lcidn")

    @lcidn.setter
    def lcidn(self, value: int) -> None:
        self._cards[4].set_value("lcidn", value)

    @property
    def fmole(self) -> typing.Optional[float]:
        """Get or set the Mole fraction of this gas species in the inlet stream.
        """ # nopep8
        return self._cards[4].get_value("fmole")

    @fmole.setter
    def fmole(self, value: float) -> None:
        self._cards[4].set_value("fmole", value)

    @property
    def fmolet(self) -> float:
        """Get or set the Initial mole fraction of this gas species in the tank.
        """ # nopep8
        return self._cards[4].get_value("fmolet")

    @fmolet.setter
    def fmolet(self, value: float) -> None:
        self._cards[4].set_value("fmolet", value)

    @property
    def tlow(self) -> typing.Optional[float]:
        """Get or set the Curve fit low temperature limit.
        """ # nopep8
        return self._cards[5].get_value("tlow")

    @tlow.setter
    def tlow(self, value: float) -> None:
        self._cards[5].set_value("tlow", value)

    @property
    def tmid(self) -> typing.Optional[float]:
        """Get or set the Curve fit low-to-high transition temperature.
        """ # nopep8
        return self._cards[5].get_value("tmid")

    @tmid.setter
    def tmid(self, value: float) -> None:
        self._cards[5].set_value("tmid", value)

    @property
    def thigh(self) -> typing.Optional[float]:
        """Get or set the Curve fit high temperature limit.
        """ # nopep8
        return self._cards[5].get_value("thigh")

    @thigh.setter
    def thigh(self, value: float) -> None:
        self._cards[5].set_value("thigh", value)

    @property
    def alow(self) -> typing.Optional[float]:
        """Get or set the Low temperature range NIST polynomial curve fit coefficient.
        """ # nopep8
        return self._cards[6].get_value("alow")

    @alow.setter
    def alow(self, value: float) -> None:
        self._cards[6].set_value("alow", value)

    @property
    def blow(self) -> typing.Optional[float]:
        """Get or set the Low temperature range NIST polynomial curve fit coefficient.
        """ # nopep8
        return self._cards[6].get_value("blow")

    @blow.setter
    def blow(self, value: float) -> None:
        self._cards[6].set_value("blow", value)

    @property
    def clow(self) -> typing.Optional[float]:
        """Get or set the Low temperature range NIST polynomial curve fit coefficient.
        """ # nopep8
        return self._cards[6].get_value("clow")

    @clow.setter
    def clow(self, value: float) -> None:
        self._cards[6].set_value("clow", value)

    @property
    def dlow(self) -> typing.Optional[float]:
        """Get or set the Low temperature range NIST polynomial curve fit coefficient.
        """ # nopep8
        return self._cards[6].get_value("dlow")

    @dlow.setter
    def dlow(self, value: float) -> None:
        self._cards[6].set_value("dlow", value)

    @property
    def elow(self) -> typing.Optional[float]:
        """Get or set the Low temperature range NIST polynomial curve fit coefficient.
        """ # nopep8
        return self._cards[6].get_value("elow")

    @elow.setter
    def elow(self, value: float) -> None:
        self._cards[6].set_value("elow", value)

    @property
    def flow(self) -> typing.Optional[float]:
        """Get or set the Low temperature range NIST polynomial curve fit coefficient.
        """ # nopep8
        return self._cards[6].get_value("flow")

    @flow.setter
    def flow(self, value: float) -> None:
        self._cards[6].set_value("flow", value)

    @property
    def glow(self) -> typing.Optional[float]:
        """Get or set the Low temperature range NIST polynomial curve fit coefficient.
        """ # nopep8
        return self._cards[6].get_value("glow")

    @glow.setter
    def glow(self, value: float) -> None:
        self._cards[6].set_value("glow", value)

    @property
    def ahigh(self) -> typing.Optional[float]:
        """Get or set the High temperature range NIST polynomial curve fit coefficient.
        """ # nopep8
        return self._cards[7].get_value("ahigh")

    @ahigh.setter
    def ahigh(self, value: float) -> None:
        self._cards[7].set_value("ahigh", value)

    @property
    def bhigh(self) -> typing.Optional[float]:
        """Get or set the High temperature range NIST polynomial curve fit coefficient.
        """ # nopep8
        return self._cards[7].get_value("bhigh")

    @bhigh.setter
    def bhigh(self, value: float) -> None:
        self._cards[7].set_value("bhigh", value)

    @property
    def chigh(self) -> typing.Optional[float]:
        """Get or set the High temperature range NIST polynomial curve fit coefficient.
        """ # nopep8
        return self._cards[7].get_value("chigh")

    @chigh.setter
    def chigh(self, value: float) -> None:
        self._cards[7].set_value("chigh", value)

    @property
    def dhigh(self) -> typing.Optional[float]:
        """Get or set the High temperature range NIST polynomial curve fit coefficient.
        """ # nopep8
        return self._cards[7].get_value("dhigh")

    @dhigh.setter
    def dhigh(self, value: float) -> None:
        self._cards[7].set_value("dhigh", value)

    @property
    def ehigh(self) -> typing.Optional[float]:
        """Get or set the High temperature range NIST polynomial curve fit coefficient.
        """ # nopep8
        return self._cards[7].get_value("ehigh")

    @ehigh.setter
    def ehigh(self, value: float) -> None:
        self._cards[7].set_value("ehigh", value)

    @property
    def fhigh(self) -> typing.Optional[float]:
        """Get or set the High temperature range NIST polynomial curve fit coefficient.
        """ # nopep8
        return self._cards[7].get_value("fhigh")

    @fhigh.setter
    def fhigh(self, value: float) -> None:
        self._cards[7].set_value("fhigh", value)

    @property
    def ghigh(self) -> typing.Optional[float]:
        """Get or set the High temperature range NIST polynomial curve fit coefficient.
        """ # nopep8
        return self._cards[7].get_value("ghigh")

    @ghigh.setter
    def ghigh(self, value: float) -> None:
        self._cards[7].set_value("ghigh", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Coefficient A, in the polynomial curve fit for heat capacity given by the equation:
        c-p = 1/MW (A + BT + CT^2 + DT^3 + ET^4).
        """ # nopep8
        return self._cards[8].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        self._cards[8].set_value("a", value)

    @property
    def b(self) -> float:
        """Get or set the Coefficient B, in the polynomial curve fit for heat capacity given by the equation:
        c-p = 1/MW (A + BT + CT^2 + DT^3 + ET^4).
        """ # nopep8
        return self._cards[8].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        self._cards[8].set_value("b", value)

    @property
    def c(self) -> float:
        """Get or set the Coefficient C, in the polynomial curve fit for heat capacity given by the equation:
        c-p = 1/MW (A + BT + CT^2 + DT^3 + ET^4).
        """ # nopep8
        return self._cards[8].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        self._cards[8].set_value("c", value)

    @property
    def d(self) -> float:
        """Get or set the Coefficient D, in the polynomial curve fit for heat capacity given by the equation:
        c-p = 1/MW (A + BT + CT^2 + DT^3 + ET^4).
        """ # nopep8
        return self._cards[8].get_value("d")

    @d.setter
    def d(self, value: float) -> None:
        self._cards[8].set_value("d", value)

    @property
    def e(self) -> float:
        """Get or set the Coefficient E, in the polynomial curve fit for heat capacity given by the equation:
        c-p = 1/MW (A + BT + CT^2 + DT^3 + ET^4).
        """ # nopep8
        return self._cards[8].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        self._cards[8].set_value("e", value)

