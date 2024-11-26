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
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class Mat248(KeywordBase):
    """DYNA MAT_248 keyword"""

    keyword = "MAT"
    subkeyword = "248"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "mid",
                        int,
                        0,
                        10,
                        kwargs.get("mid")
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        kwargs.get("ro")
                    ),
                    Field(
                        "e",
                        float,
                        20,
                        10,
                        kwargs.get("e")
                    ),
                    Field(
                        "pr",
                        float,
                        30,
                        10,
                        kwargs.get("pr")
                    ),
                    Field(
                        "tunit",
                        float,
                        40,
                        10,
                        kwargs.get("tunit", 3600)
                    ),
                    Field(
                        "trip",
                        int,
                        50,
                        10,
                        kwargs.get("trip", 0)
                    ),
                    Field(
                        "phase",
                        int,
                        60,
                        10,
                        kwargs.get("phase", 0)
                    ),
                    Field(
                        "heat",
                        int,
                        70,
                        10,
                        kwargs.get("heat", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcy1",
                        int,
                        0,
                        10,
                        kwargs.get("lcy1")
                    ),
                    Field(
                        "lcy2",
                        int,
                        10,
                        10,
                        kwargs.get("lcy2")
                    ),
                    Field(
                        "lcy3",
                        int,
                        20,
                        10,
                        kwargs.get("lcy3")
                    ),
                    Field(
                        "lcy4",
                        int,
                        30,
                        10,
                        kwargs.get("lcy4")
                    ),
                    Field(
                        "lcy5",
                        int,
                        40,
                        10,
                        kwargs.get("lcy5")
                    ),
                    Field(
                        "c_f",
                        float,
                        50,
                        10,
                        kwargs.get("c_f")
                    ),
                    Field(
                        "c_p",
                        float,
                        60,
                        10,
                        kwargs.get("c_p")
                    ),
                    Field(
                        "c_b",
                        float,
                        70,
                        10,
                        kwargs.get("c_b")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c",
                        float,
                        0,
                        10,
                        kwargs.get("c")
                    ),
                    Field(
                        "co",
                        float,
                        10,
                        10,
                        kwargs.get("co")
                    ),
                    Field(
                        "mo",
                        float,
                        20,
                        10,
                        kwargs.get("mo")
                    ),
                    Field(
                        "cr",
                        float,
                        30,
                        10,
                        kwargs.get("cr")
                    ),
                    Field(
                        "ni",
                        float,
                        40,
                        10,
                        kwargs.get("ni")
                    ),
                    Field(
                        "mn",
                        float,
                        50,
                        10,
                        kwargs.get("mn")
                    ),
                    Field(
                        "si",
                        float,
                        60,
                        10,
                        kwargs.get("si")
                    ),
                    Field(
                        "v",
                        float,
                        70,
                        10,
                        kwargs.get("v")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "w",
                        float,
                        0,
                        10,
                        kwargs.get("w")
                    ),
                    Field(
                        "cu",
                        float,
                        10,
                        10,
                        kwargs.get("cu")
                    ),
                    Field(
                        "p",
                        float,
                        20,
                        10,
                        kwargs.get("p")
                    ),
                    Field(
                        "ai",
                        float,
                        30,
                        10,
                        kwargs.get("ai")
                    ),
                    Field(
                        "as",
                        float,
                        40,
                        10,
                        kwargs.get("as")
                    ),
                    Field(
                        "ti",
                        float,
                        50,
                        10,
                        kwargs.get("ti")
                    ),
                    Field(
                        "b",
                        float,
                        60,
                        10,
                        kwargs.get("b")
                    ),
                    Field(
                        "unused",
                        int,
                        70,
                        10,
                        kwargs.get("unused")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "unused",
                        int,
                        0,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "tabrho",
                        int,
                        20,
                        10,
                        kwargs.get("tabrho")
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "tref",
                        float,
                        40,
                        10,
                        kwargs.get("tref")
                    ),
                    Field(
                        "lat1",
                        float,
                        50,
                        10,
                        kwargs.get("lat1")
                    ),
                    Field(
                        "lat5",
                        float,
                        60,
                        10,
                        kwargs.get("lat5")
                    ),
                    Field(
                        "tabth",
                        int,
                        70,
                        10,
                        kwargs.get("tabth")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "qr2",
                        float,
                        0,
                        10,
                        kwargs.get("qr2")
                    ),
                    Field(
                        "qr3",
                        float,
                        10,
                        10,
                        kwargs.get("qr3")
                    ),
                    Field(
                        "qr4",
                        float,
                        20,
                        10,
                        kwargs.get("qr4")
                    ),
                    Field(
                        "alpha",
                        float,
                        30,
                        10,
                        kwargs.get("alpha")
                    ),
                    Field(
                        "grain",
                        float,
                        40,
                        10,
                        kwargs.get("grain")
                    ),
                    Field(
                        "toffe",
                        float,
                        50,
                        10,
                        kwargs.get("toffe")
                    ),
                    Field(
                        "tofpe",
                        float,
                        60,
                        10,
                        kwargs.get("tofpe")
                    ),
                    Field(
                        "tofba",
                        float,
                        70,
                        10,
                        kwargs.get("tofba")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "plmem2",
                        float,
                        0,
                        10,
                        kwargs.get("plmem2")
                    ),
                    Field(
                        "plmem3",
                        float,
                        10,
                        10,
                        kwargs.get("plmem3")
                    ),
                    Field(
                        "plmem4",
                        float,
                        20,
                        10,
                        kwargs.get("plmem4")
                    ),
                    Field(
                        "plmem5",
                        float,
                        30,
                        10,
                        kwargs.get("plmem5")
                    ),
                    Field(
                        "strc",
                        float,
                        40,
                        10,
                        kwargs.get("strc")
                    ),
                    Field(
                        "strp",
                        float,
                        50,
                        10,
                        kwargs.get("strp")
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        70,
                        10,
                        kwargs.get("unused")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "fs",
                        float,
                        0,
                        10,
                        kwargs.get("fs")
                    ),
                    Field(
                        "ps",
                        float,
                        10,
                        10,
                        kwargs.get("ps")
                    ),
                    Field(
                        "bs",
                        float,
                        20,
                        10,
                        kwargs.get("bs")
                    ),
                    Field(
                        "ms",
                        float,
                        30,
                        10,
                        kwargs.get("ms")
                    ),
                    Field(
                        "msig",
                        float,
                        40,
                        10,
                        kwargs.get("msig")
                    ),
                    Field(
                        "lceps23",
                        int,
                        50,
                        10,
                        kwargs.get("lceps23")
                    ),
                    Field(
                        "lceps4",
                        int,
                        60,
                        10,
                        kwargs.get("lceps4")
                    ),
                    Field(
                        "lceps5",
                        int,
                        70,
                        10,
                        kwargs.get("lceps5")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lch4",
                        int,
                        0,
                        10,
                        kwargs.get("lch4")
                    ),
                    Field(
                        "lch5",
                        int,
                        10,
                        10,
                        kwargs.get("lch5")
                    ),
                    Field(
                        "dtcrit",
                        float,
                        20,
                        10,
                        kwargs.get("dtcrit")
                    ),
                    Field(
                        "tsamp",
                        float,
                        30,
                        10,
                        kwargs.get("tsamp")
                    ),
                    Field(
                        "islc",
                        int,
                        40,
                        10,
                        kwargs.get("islc", 0)
                    ),
                    Field(
                        "iextra",
                        int,
                        50,
                        10,
                        kwargs.get("iextra", 0)
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        70,
                        10,
                        kwargs.get("unused")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "alph_m",
                        float,
                        0,
                        10,
                        kwargs.get("alph_m", 0.0428)
                    ),
                    Field(
                        "n_m",
                        float,
                        10,
                        10,
                        kwargs.get("n_m", 0.191)
                    ),
                    Field(
                        "phi_m",
                        float,
                        20,
                        10,
                        kwargs.get("phi_m", 0.382)
                    ),
                    Field(
                        "psi_m",
                        float,
                        30,
                        10,
                        kwargs.get("psi_m", 2.421)
                    ),
                    Field(
                        "omg_f",
                        float,
                        40,
                        10,
                        kwargs.get("omg_f", 0.41)
                    ),
                    Field(
                        "phi_f",
                        float,
                        50,
                        10,
                        kwargs.get("phi_f", 0.4)
                    ),
                    Field(
                        "psi_f",
                        float,
                        60,
                        10,
                        kwargs.get("psi_f", 0.4)
                    ),
                    Field(
                        "cr_f",
                        float,
                        70,
                        10,
                        kwargs.get("cr_f")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "omg_p",
                        float,
                        0,
                        10,
                        kwargs.get("omg_p", 0.32)
                    ),
                    Field(
                        "phi_p",
                        float,
                        10,
                        10,
                        kwargs.get("phi_p", 0.4)
                    ),
                    Field(
                        "psi_p",
                        float,
                        20,
                        10,
                        kwargs.get("psi_p", 0.4)
                    ),
                    Field(
                        "cr_p",
                        float,
                        30,
                        10,
                        kwargs.get("cr_p")
                    ),
                    Field(
                        "omg_b",
                        float,
                        40,
                        10,
                        kwargs.get("omg_b", 0.29)
                    ),
                    Field(
                        "phi_b",
                        float,
                        50,
                        10,
                        kwargs.get("phi_b", 0.4)
                    ),
                    Field(
                        "psi_b",
                        float,
                        60,
                        10,
                        kwargs.get("psi_b", 0.4)
                    ),
                    Field(
                        "cr_b",
                        float,
                        70,
                        10,
                        kwargs.get("cr_b")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "aust",
                        float,
                        0,
                        10,
                        kwargs.get("aust")
                    ),
                    Field(
                        "ferr",
                        float,
                        10,
                        10,
                        kwargs.get("ferr")
                    ),
                    Field(
                        "pear",
                        float,
                        20,
                        10,
                        kwargs.get("pear")
                    ),
                    Field(
                        "bain",
                        float,
                        30,
                        10,
                        kwargs.get("bain")
                    ),
                    Field(
                        "mart",
                        float,
                        40,
                        10,
                        kwargs.get("mart")
                    ),
                    Field(
                        "grk",
                        float,
                        50,
                        10,
                        kwargs.get("grk")
                    ),
                    Field(
                        "grqr",
                        float,
                        60,
                        10,
                        kwargs.get("grqr")
                    ),
                    Field(
                        "tau1",
                        float,
                        70,
                        10,
                        kwargs.get("tau1", 2.08E+8)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "gra",
                        float,
                        0,
                        10,
                        kwargs.get("gra", 3.11)
                    ),
                    Field(
                        "grb",
                        float,
                        10,
                        10,
                        kwargs.get("grb", 7520.)
                    ),
                    Field(
                        "expa",
                        float,
                        20,
                        10,
                        kwargs.get("expa", 1.0)
                    ),
                    Field(
                        "expb",
                        float,
                        30,
                        10,
                        kwargs.get("expb", 1.0)
                    ),
                    Field(
                        "grcc",
                        float,
                        40,
                        10,
                        kwargs.get("grcc")
                    ),
                    Field(
                        "grcm",
                        float,
                        50,
                        10,
                        kwargs.get("grcm")
                    ),
                    Field(
                        "heatn",
                        float,
                        60,
                        10,
                        kwargs.get("heatn", 1.0)
                    ),
                    Field(
                        "tau2",
                        float,
                        70,
                        10,
                        kwargs.get("tau2", 4.806)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "funca",
                        int,
                        0,
                        10,
                        kwargs.get("funca")
                    ),
                    Field(
                        "funcb",
                        int,
                        10,
                        10,
                        kwargs.get("funcb")
                    ),
                    Field(
                        "funcm",
                        int,
                        20,
                        10,
                        kwargs.get("funcm")
                    ),
                    Field(
                        "tcvup",
                        float,
                        30,
                        10,
                        kwargs.get("tcvup", 0.0)
                    ),
                    Field(
                        "tcvlo",
                        float,
                        40,
                        10,
                        kwargs.get("tcvlo", 0.0)
                    ),
                    Field(
                        "cvcrit",
                        float,
                        50,
                        10,
                        kwargs.get("cvcrit", 0.0)
                    ),
                    Field(
                        "tcvsl",
                        float,
                        60,
                        10,
                        kwargs.get("tcvsl", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "epsp",
                        float,
                        0,
                        10,
                        kwargs.get("epsp", 0.0)
                    ),
                    Field(
                        "expon",
                        float,
                        10,
                        10,
                        kwargs.get("expon", 0.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat248.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material ID, a unique number has to be chosen.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Material density at room temperature (necessary for calculating transformation induced strains).
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Youngs' modulus:
        GT.0.0:	constant value is used
        LT.0.0:	LCID or TABID.  Temperature dependent Young's modulus given by load curve or table ID = -E. When using a table to describe the Young's modulus see Remark 10 for more information..
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio:
        GT.0.0:	constant value is used
        LT.0.0:	LCID or TABID.  Temperature dependent Poisson's ratio given by load curve or table ID = -PR. The table input is described in Remark 10.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def tunit(self) -> float:
        """Get or set the Number of time units per hour. Default is seconds, that is 3600 time units per hour. It is used only for hardness calculations.
        """ # nopep8
        return self._cards[0].get_value("tunit")

    @tunit.setter
    def tunit(self, value: float) -> None:
        self._cards[0].set_value("tunit", value)

    @property
    def trip(self) -> int:
        """Get or set the Flag to activate (0) or deactivate (1) trip effect calculation.
        """ # nopep8
        return self._cards[0].get_value("trip")

    @trip.setter
    def trip(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""trip must be one of {0,1}""")
        self._cards[0].set_value("trip", value)

    @property
    def phase(self) -> int:
        """Get or set the Switch to exclude middle phases from the simulation.
        EQ.0:	all phases active (default)
        EQ.1:	pearlite and bainite active
        EQ.2:	bainite active
        EQ.3:	ferrite and pearlite active
        EQ.4:	ferrite and bainite active
        EQ.5:	no active middle phases (only austenite → martensite)
        """ # nopep8
        return self._cards[0].get_value("phase")

    @phase.setter
    def phase(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5]:
            raise Exception("""phase must be one of {0,1,2,3,4,5}""")
        self._cards[0].set_value("phase", value)

    @property
    def heat(self) -> int:
        """Get or set the Heat flag as in MAT_244, see there for details.
        EQ.0:	Heating is not activated.
        EQ.1:	Heating is activated.
        EQ.2:	Automatic switching between cooling and heating.
        LT.0:	Switch between cooling and heating is defined by a time dependent load curve with id ABS(HEAT).
        """ # nopep8
        return self._cards[0].get_value("heat")

    @heat.setter
    def heat(self, value: int) -> None:
        self._cards[0].set_value("heat", value)

    @property
    def lcy1(self) -> typing.Optional[int]:
        """Get or set the Load curve or Table ID for austenite hardening.
        if LCID
        input yield stress versus effective plastic strain.
        if TABID.GT.0:
        2D table. Input temperatures as table values and hardening curves as targets for those temperatures (see *DEFINE_TABLE)
        if TABID.LT.0:
        3D table. Input temperatures as main table values and strain rates as values for the sub tables, and hardening curves as targets for those strain rates..
        """ # nopep8
        return self._cards[1].get_value("lcy1")

    @lcy1.setter
    def lcy1(self, value: int) -> None:
        self._cards[1].set_value("lcy1", value)

    @property
    def lcy2(self) -> typing.Optional[int]:
        """Get or set the Load curve or Table ID for ferrite. See LCY1 for description.
        """ # nopep8
        return self._cards[1].get_value("lcy2")

    @lcy2.setter
    def lcy2(self, value: int) -> None:
        self._cards[1].set_value("lcy2", value)

    @property
    def lcy3(self) -> typing.Optional[int]:
        """Get or set the Load curve or Table ID for pearlite. See LCY1 for description.
        """ # nopep8
        return self._cards[1].get_value("lcy3")

    @lcy3.setter
    def lcy3(self, value: int) -> None:
        self._cards[1].set_value("lcy3", value)

    @property
    def lcy4(self) -> typing.Optional[int]:
        """Get or set the Load curve or Table ID for bainite. See LCY1 for description.
        """ # nopep8
        return self._cards[1].get_value("lcy4")

    @lcy4.setter
    def lcy4(self, value: int) -> None:
        self._cards[1].set_value("lcy4", value)

    @property
    def lcy5(self) -> typing.Optional[int]:
        """Get or set the Load curve or Table ID for martensite. See LCY1 for description.
        """ # nopep8
        return self._cards[1].get_value("lcy5")

    @lcy5.setter
    def lcy5(self, value: int) -> None:
        self._cards[1].set_value("lcy5", value)

    @property
    def c_f(self) -> typing.Optional[float]:
        """Get or set the Alloy dependent factor  for ferrite (controls the alloying effects beside of Boron on the time-temperature-transformation start line of ferrite).
        """ # nopep8
        return self._cards[1].get_value("c_f")

    @c_f.setter
    def c_f(self, value: float) -> None:
        self._cards[1].set_value("c_f", value)

    @property
    def c_p(self) -> typing.Optional[float]:
        """Get or set the Alloy dependent factor  for pearlite (see C_F for description).
        """ # nopep8
        return self._cards[1].get_value("c_p")

    @c_p.setter
    def c_p(self, value: float) -> None:
        self._cards[1].set_value("c_p", value)

    @property
    def c_b(self) -> typing.Optional[float]:
        """Get or set the Alloy dependent factor  for bainite (see C_F for description).
        """ # nopep8
        return self._cards[1].get_value("c_b")

    @c_b.setter
    def c_b(self, value: float) -> None:
        self._cards[1].set_value("c_b", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Carbon [weight %].
        """ # nopep8
        return self._cards[2].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        self._cards[2].set_value("c", value)

    @property
    def co(self) -> typing.Optional[float]:
        """Get or set the Cobolt [weight %].
        """ # nopep8
        return self._cards[2].get_value("co")

    @co.setter
    def co(self, value: float) -> None:
        self._cards[2].set_value("co", value)

    @property
    def mo(self) -> typing.Optional[float]:
        """Get or set the Molybdenum [weight %].
        """ # nopep8
        return self._cards[2].get_value("mo")

    @mo.setter
    def mo(self, value: float) -> None:
        self._cards[2].set_value("mo", value)

    @property
    def cr(self) -> typing.Optional[float]:
        """Get or set the Chromium [weight %].
        """ # nopep8
        return self._cards[2].get_value("cr")

    @cr.setter
    def cr(self, value: float) -> None:
        self._cards[2].set_value("cr", value)

    @property
    def ni(self) -> typing.Optional[float]:
        """Get or set the Nickel [weight %]
        """ # nopep8
        return self._cards[2].get_value("ni")

    @ni.setter
    def ni(self, value: float) -> None:
        self._cards[2].set_value("ni", value)

    @property
    def mn(self) -> typing.Optional[float]:
        """Get or set the Manganese [weight %]
        """ # nopep8
        return self._cards[2].get_value("mn")

    @mn.setter
    def mn(self, value: float) -> None:
        self._cards[2].set_value("mn", value)

    @property
    def si(self) -> typing.Optional[float]:
        """Get or set the Silicon [weight %]
        """ # nopep8
        return self._cards[2].get_value("si")

    @si.setter
    def si(self, value: float) -> None:
        self._cards[2].set_value("si", value)

    @property
    def v(self) -> typing.Optional[float]:
        """Get or set the Vanadium [weight %]
        """ # nopep8
        return self._cards[2].get_value("v")

    @v.setter
    def v(self, value: float) -> None:
        self._cards[2].set_value("v", value)

    @property
    def w(self) -> typing.Optional[float]:
        """Get or set the Tungsten [weight %].
        """ # nopep8
        return self._cards[3].get_value("w")

    @w.setter
    def w(self, value: float) -> None:
        self._cards[3].set_value("w", value)

    @property
    def cu(self) -> typing.Optional[float]:
        """Get or set the Copper [weight %].
        """ # nopep8
        return self._cards[3].get_value("cu")

    @cu.setter
    def cu(self, value: float) -> None:
        self._cards[3].set_value("cu", value)

    @property
    def p(self) -> typing.Optional[float]:
        """Get or set the Phosphorous [weight %].
        """ # nopep8
        return self._cards[3].get_value("p")

    @p.setter
    def p(self, value: float) -> None:
        self._cards[3].set_value("p", value)

    @property
    def ai(self) -> typing.Optional[float]:
        """Get or set the Aluminium [weight %].
        """ # nopep8
        return self._cards[3].get_value("ai")

    @ai.setter
    def ai(self, value: float) -> None:
        self._cards[3].set_value("ai", value)

    @property
    def as_(self) -> typing.Optional[float]:
        """Get or set the Arsenic [weight %]
        """ # nopep8
        return self._cards[3].get_value("as")

    @as_.setter
    def as_(self, value: float) -> None:
        self._cards[3].set_value("as", value)

    @property
    def ti(self) -> typing.Optional[float]:
        """Get or set the Titanium [weight %]
        """ # nopep8
        return self._cards[3].get_value("ti")

    @ti.setter
    def ti(self, value: float) -> None:
        self._cards[3].set_value("ti", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Boron [weight %]
        """ # nopep8
        return self._cards[3].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        self._cards[3].set_value("b", value)

    @property
    def tabrho(self) -> typing.Optional[int]:
        """Get or set the Table definition for phase and temperature dependent densities. Needed for calculation of transformation induced strains.
        """ # nopep8
        return self._cards[4].get_value("tabrho")

    @tabrho.setter
    def tabrho(self, value: int) -> None:
        self._cards[4].set_value("tabrho", value)

    @property
    def tref(self) -> typing.Optional[float]:
        """Get or set the Reference temperature for thermal expansion (only necessary for thermal expansion calculation with the secant method).
        """ # nopep8
        return self._cards[4].get_value("tref")

    @tref.setter
    def tref(self, value: float) -> None:
        self._cards[4].set_value("tref", value)

    @property
    def lat1(self) -> typing.Optional[float]:
        """Get or set the Latent heat for the decomposition of austenite into ferrite, pearlite and bainite.
        GT.0.0:	Constant value
        LT.0.0:	Curve ID or Table ID: See remark 11 for more information.
        """ # nopep8
        return self._cards[4].get_value("lat1")

    @lat1.setter
    def lat1(self, value: float) -> None:
        self._cards[4].set_value("lat1", value)

    @property
    def lat5(self) -> typing.Optional[float]:
        """Get or set the Latent heat for the decomposition of austenite into martensite.
        GT.0.0:	Constant value
        LT.0.0:	Curve ID:	Note that LAT 5 is ignored if a Table ID is used in LAT1.
        """ # nopep8
        return self._cards[4].get_value("lat5")

    @lat5.setter
    def lat5(self, value: float) -> None:
        self._cards[4].set_value("lat5", value)

    @property
    def tabth(self) -> typing.Optional[int]:
        """Get or set the Table definition for thermal expansion coefficient. See remarks for more information how to input this table.
        GT.0:	A table for instantaneous thermal expansion (TREF is ignored).
        LT.0:	A table with thermal expansion with reference to TREF.
        """ # nopep8
        return self._cards[4].get_value("tabth")

    @tabth.setter
    def tabth(self, value: int) -> None:
        self._cards[4].set_value("tabth", value)

    @property
    def qr2(self) -> typing.Optional[float]:
        """Get or set the Activation energy divided by the universal gas constant for the diffusion reaction of the austenite-ferrite reaction: Q2/R. R= 8.314472 [J/mol K].Load curve ID if ISLC=2 (function of cooling rate)
        """ # nopep8
        return self._cards[5].get_value("qr2")

    @qr2.setter
    def qr2(self, value: float) -> None:
        self._cards[5].set_value("qr2", value)

    @property
    def qr3(self) -> typing.Optional[float]:
        """Get or set the Activation energy divided by the universal gas constant for the diffusion reaction for the austenite-pearlite reaction: Q3/R. R=8.314472 [J/mol K].Load curve ID if ISLC=2 (function of cooling rate).
        """ # nopep8
        return self._cards[5].get_value("qr3")

    @qr3.setter
    def qr3(self, value: float) -> None:
        self._cards[5].set_value("qr3", value)

    @property
    def qr4(self) -> typing.Optional[float]:
        """Get or set the Activation energy divided by the universal gas constant for the diffusion reaction for the austenite-bainite reaction: Q4/R. R=8.314472 [J/mol K].Load curve ID if ISLC=2 (function of cooling rate).
        """ # nopep8
        return self._cards[5].get_value("qr4")

    @qr4.setter
    def qr4(self, value: float) -> None:
        self._cards[5].set_value("qr4", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Material constant for the martensite phase. A value of 0.011 means that 90% of the available austenite is transformed into martensite at 210 degrees below the martensite start temperature (see message file for information), whereas a value of 0.033 means a 99.9% transformation.
        """ # nopep8
        return self._cards[5].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        self._cards[5].set_value("alpha", value)

    @property
    def grain(self) -> typing.Optional[float]:
        """Get or set the ASTM grain size number  for austenite, usually a number between 7 and 11.
        """ # nopep8
        return self._cards[5].get_value("grain")

    @grain.setter
    def grain(self, value: float) -> None:
        self._cards[5].set_value("grain", value)

    @property
    def toffe(self) -> typing.Optional[float]:
        """Get or set the Number of degrees that the ferrite is bleeding over into the pearlite reaction: .
        """ # nopep8
        return self._cards[5].get_value("toffe")

    @toffe.setter
    def toffe(self, value: float) -> None:
        self._cards[5].set_value("toffe", value)

    @property
    def tofpe(self) -> typing.Optional[float]:
        """Get or set the Number of degrees that the pearlite is bleeding over into the bainite reaction: .
        """ # nopep8
        return self._cards[5].get_value("tofpe")

    @tofpe.setter
    def tofpe(self, value: float) -> None:
        self._cards[5].set_value("tofpe", value)

    @property
    def tofba(self) -> typing.Optional[float]:
        """Get or set the Number of degrees that the bainite is bleeding over into the martensite reaction: .
        """ # nopep8
        return self._cards[5].get_value("tofba")

    @tofba.setter
    def tofba(self, value: float) -> None:
        self._cards[5].set_value("tofba", value)

    @property
    def plmem2(self) -> typing.Optional[float]:
        """Get or set the Memory coefficient for the plastic strain that is carried over from the austenite. A value of 1 means that all plastic strains from austenite is transferred to the ferrite phase and a value of 0 means that nothing is transferred.
        """ # nopep8
        return self._cards[6].get_value("plmem2")

    @plmem2.setter
    def plmem2(self, value: float) -> None:
        self._cards[6].set_value("plmem2", value)

    @property
    def plmem3(self) -> typing.Optional[float]:
        """Get or set the Same as PLMEM2 but between austenite and pearlite.
        """ # nopep8
        return self._cards[6].get_value("plmem3")

    @plmem3.setter
    def plmem3(self, value: float) -> None:
        self._cards[6].set_value("plmem3", value)

    @property
    def plmem4(self) -> typing.Optional[float]:
        """Get or set the Same as PLMEM2 but between austenite and bainite.
        """ # nopep8
        return self._cards[6].get_value("plmem4")

    @plmem4.setter
    def plmem4(self, value: float) -> None:
        self._cards[6].set_value("plmem4", value)

    @property
    def plmem5(self) -> typing.Optional[float]:
        """Get or set the Same as PLMEM3 but between austenite and martensite.
        """ # nopep8
        return self._cards[6].get_value("plmem5")

    @plmem5.setter
    def plmem5(self, value: float) -> None:
        self._cards[6].set_value("plmem5", value)

    @property
    def strc(self) -> typing.Optional[float]:
        """Get or set the Cowper and Symonds strain rate parameter .
        STRC.LT.0.0:	load curve id = -STRC
        STRC.GT.0.0:	constant value
        STRC.EQ.0.0:	strain rate NOT active
        """ # nopep8
        return self._cards[6].get_value("strc")

    @strc.setter
    def strc(self, value: float) -> None:
        self._cards[6].set_value("strc", value)

    @property
    def strp(self) -> typing.Optional[float]:
        """Get or set the Cowper and Symonds strain rate parameter P.
        STRP.LT.0.0:	load curve id = -STRP
        STRP.GT.0.0:	constant value
        STRP.EQ.0.0:	strain rate NOT active
        """ # nopep8
        return self._cards[6].get_value("strp")

    @strp.setter
    def strp(self, value: float) -> None:
        self._cards[6].set_value("strp", value)

    @property
    def fs(self) -> typing.Optional[float]:
        """Get or set the Manual start temperature ferrite, .
        GT.0.0:	Same temperature is used for heating and cooling.
        LT.0.0:	Curve ID:	Different start temperatures for cooling and heating given by load curve ID= -FS. First ordinate value is used for cooling, last ordinate value for heating..
        """ # nopep8
        return self._cards[7].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        self._cards[7].set_value("fs", value)

    @property
    def ps(self) -> typing.Optional[float]:
        """Get or set the Manual start temperature pearlite, . See FS for description.
        """ # nopep8
        return self._cards[7].get_value("ps")

    @ps.setter
    def ps(self, value: float) -> None:
        self._cards[7].set_value("ps", value)

    @property
    def bs(self) -> typing.Optional[float]:
        """Get or set the Manual start temperature bainite, . See FS for description.
        """ # nopep8
        return self._cards[7].get_value("bs")

    @bs.setter
    def bs(self, value: float) -> None:
        self._cards[7].set_value("bs", value)

    @property
    def ms(self) -> typing.Optional[float]:
        """Get or set the Manual start temperature martensite, . See FS for description.
        """ # nopep8
        return self._cards[7].get_value("ms")

    @ms.setter
    def ms(self, value: float) -> None:
        self._cards[7].set_value("ms", value)

    @property
    def msig(self) -> typing.Optional[float]:
        """Get or set the Describes the increase of martensite start temperature for cooling due to applied stress.
        LT.0:	Load Curve ID describes MSIG as a function of triaxiality (pressure / effective stress).
        """ # nopep8
        return self._cards[7].get_value("msig")

    @msig.setter
    def msig(self, value: float) -> None:
        self._cards[7].set_value("msig", value)

    @property
    def lceps23(self) -> typing.Optional[int]:
        """Get or set the Load Curve ID dependent on plastic strain that scales the activation energy QR2 and QR3.
        """ # nopep8
        return self._cards[7].get_value("lceps23")

    @lceps23.setter
    def lceps23(self, value: int) -> None:
        self._cards[7].set_value("lceps23", value)

    @property
    def lceps4(self) -> typing.Optional[int]:
        """Get or set the Load Curve ID dependent on plastic strain that scales the activation energy QR4.
        """ # nopep8
        return self._cards[7].get_value("lceps4")

    @lceps4.setter
    def lceps4(self, value: int) -> None:
        self._cards[7].set_value("lceps4", value)

    @property
    def lceps5(self) -> typing.Optional[int]:
        """Get or set the Load Curve ID which describe the increase of the martensite start temperature for cooling as a function of plastic strain.	MS*= MS+ MSIG+LCEPS5()
        """ # nopep8
        return self._cards[7].get_value("lceps5")

    @lceps5.setter
    def lceps5(self, value: int) -> None:
        self._cards[7].set_value("lceps5", value)

    @property
    def lch4(self) -> typing.Optional[int]:
        """Get or set the Load curve ID of Vickers hardness vs. temperature for bainite hardness calculation.
        """ # nopep8
        return self._cards[8].get_value("lch4")

    @lch4.setter
    def lch4(self, value: int) -> None:
        self._cards[8].set_value("lch4", value)

    @property
    def lch5(self) -> typing.Optional[int]:
        """Get or set the Load curve ID of Vickers hardness vs. temperature for martensite hardness calculation.
        """ # nopep8
        return self._cards[8].get_value("lch5")

    @lch5.setter
    def lch5(self, value: int) -> None:
        self._cards[8].set_value("lch5", value)

    @property
    def dtcrit(self) -> typing.Optional[float]:
        """Get or set the Critical cooling rate to detect holding phase.
        """ # nopep8
        return self._cards[8].get_value("dtcrit")

    @dtcrit.setter
    def dtcrit(self, value: float) -> None:
        self._cards[8].set_value("dtcrit", value)

    @property
    def tsamp(self) -> typing.Optional[float]:
        """Get or set the Sampling interval for temperature rate monitoring to detect the holding phase
        """ # nopep8
        return self._cards[8].get_value("tsamp")

    @tsamp.setter
    def tsamp(self, value: float) -> None:
        self._cards[8].set_value("tsamp", value)

    @property
    def islc(self) -> int:
        """Get or set the Flag for definition of evolution parameters on Cards 10 and 11.
        EQ.0.0:	All 16 fields on Cards 10 and 11 are constant values.
        EQ.1.0 : PHI_‌F, CR_‌F, PHI_‌P, CR_‌P, PHI_‌B,and CR_‌B are load curves defining values as functions of cooling rate.The remaining 10 fields on Cards 10 and 11 are constant values.
        EQ.2.0 : QR2, QR3, QR4 from Card 6 and allAll 16 fields on Cards 10 and 11 are load curves defining values as functions of cooling rate.
        """ # nopep8
        return self._cards[8].get_value("islc")

    @islc.setter
    def islc(self, value: int) -> None:
        self._cards[8].set_value("islc", value)

    @property
    def iextra(self) -> int:
        """Get or set the Flag to read extra cards (see Cards 14 and 15)
        """ # nopep8
        return self._cards[8].get_value("iextra")

    @iextra.setter
    def iextra(self, value: int) -> None:
        self._cards[8].set_value("iextra", value)

    @property
    def alph_m(self) -> float:
        """Get or set the Martensite evolution parameter .
        """ # nopep8
        return self._cards[9].get_value("alph_m")

    @alph_m.setter
    def alph_m(self, value: float) -> None:
        self._cards[9].set_value("alph_m", value)

    @property
    def n_m(self) -> float:
        """Get or set the Martensite evolution parameter.
        """ # nopep8
        return self._cards[9].get_value("n_m")

    @n_m.setter
    def n_m(self, value: float) -> None:
        self._cards[9].set_value("n_m", value)

    @property
    def phi_m(self) -> float:
        """Get or set the Martensite evolution parameter .
        """ # nopep8
        return self._cards[9].get_value("phi_m")

    @phi_m.setter
    def phi_m(self, value: float) -> None:
        self._cards[9].set_value("phi_m", value)

    @property
    def psi_m(self) -> float:
        """Get or set the Martensite evolution exponent , if  then .
        """ # nopep8
        return self._cards[9].get_value("psi_m")

    @psi_m.setter
    def psi_m(self, value: float) -> None:
        self._cards[9].set_value("psi_m", value)

    @property
    def omg_f(self) -> float:
        """Get or set the Ferrite grain size factor  (mainly controls the alloying effect of Boron on the time-temperature-transformation start line of ferrite)
        """ # nopep8
        return self._cards[9].get_value("omg_f")

    @omg_f.setter
    def omg_f(self, value: float) -> None:
        self._cards[9].set_value("omg_f", value)

    @property
    def phi_f(self) -> float:
        """Get or set the Ferrite evolution parameter  (controls the incubation time till 1vol% of ferrite is built)
        """ # nopep8
        return self._cards[9].get_value("phi_f")

    @phi_f.setter
    def phi_f(self, value: float) -> None:
        self._cards[9].set_value("phi_f", value)

    @property
    def psi_f(self) -> float:
        """Get or set the Ferrite evolution parameter  (controls the time till 99vol% of ferrite is built without effect on the incubation time)
        """ # nopep8
        return self._cards[9].get_value("psi_f")

    @psi_f.setter
    def psi_f(self, value: float) -> None:
        self._cards[9].set_value("psi_f", value)

    @property
    def cr_f(self) -> typing.Optional[float]:
        """Get or set the Ferrite evolution parameter  (retardation coefficient to influence the kinetics of phase transformation of ferrite, should be determined at slow cooling conditions, can also be defined in dependency to the cooling rate)
        """ # nopep8
        return self._cards[9].get_value("cr_f")

    @cr_f.setter
    def cr_f(self, value: float) -> None:
        self._cards[9].set_value("cr_f", value)

    @property
    def omg_p(self) -> float:
        """Get or set the Pearlite grain size factor  (see OMG_F for description).
        """ # nopep8
        return self._cards[10].get_value("omg_p")

    @omg_p.setter
    def omg_p(self, value: float) -> None:
        self._cards[10].set_value("omg_p", value)

    @property
    def phi_p(self) -> float:
        """Get or set the Pearlite evolution parameter  (see PHI_F for description).
        """ # nopep8
        return self._cards[10].get_value("phi_p")

    @phi_p.setter
    def phi_p(self, value: float) -> None:
        self._cards[10].set_value("phi_p", value)

    @property
    def psi_p(self) -> float:
        """Get or set the Pearlite evolution parameter  (see PSI_F for description).
        """ # nopep8
        return self._cards[10].get_value("psi_p")

    @psi_p.setter
    def psi_p(self, value: float) -> None:
        self._cards[10].set_value("psi_p", value)

    @property
    def cr_p(self) -> typing.Optional[float]:
        """Get or set the Pearlite evolution parameter  (see CR_F for description).
        """ # nopep8
        return self._cards[10].get_value("cr_p")

    @cr_p.setter
    def cr_p(self, value: float) -> None:
        self._cards[10].set_value("cr_p", value)

    @property
    def omg_b(self) -> float:
        """Get or set the Bainite grain size factor  (see OMG_F for description)
        """ # nopep8
        return self._cards[10].get_value("omg_b")

    @omg_b.setter
    def omg_b(self, value: float) -> None:
        self._cards[10].set_value("omg_b", value)

    @property
    def phi_b(self) -> float:
        """Get or set the Bainite evolution parameter  (see PHI_F for description)
        """ # nopep8
        return self._cards[10].get_value("phi_b")

    @phi_b.setter
    def phi_b(self, value: float) -> None:
        self._cards[10].set_value("phi_b", value)

    @property
    def psi_b(self) -> float:
        """Get or set the Bainite evolution parameter  (see PSI_F for description)
        """ # nopep8
        return self._cards[10].get_value("psi_b")

    @psi_b.setter
    def psi_b(self, value: float) -> None:
        self._cards[10].set_value("psi_b", value)

    @property
    def cr_b(self) -> typing.Optional[float]:
        """Get or set the Bainite evolution parameter  (see CR_F for description)
        """ # nopep8
        return self._cards[10].get_value("cr_b")

    @cr_b.setter
    def cr_b(self, value: float) -> None:
        self._cards[10].set_value("cr_b", value)

    @property
    def aust(self) -> typing.Optional[float]:
        """Get or set the If a heating process is initiated at t = 0 this parameters sets the initial amount of austenite in the blank. If heating is activated at t > 0 during a simulation this value is ignored.
        """ # nopep8
        return self._cards[11].get_value("aust")

    @aust.setter
    def aust(self, value: float) -> None:
        self._cards[11].set_value("aust", value)

    @property
    def ferr(self) -> typing.Optional[float]:
        """Get or set the See AUST for description.
        """ # nopep8
        return self._cards[11].get_value("ferr")

    @ferr.setter
    def ferr(self, value: float) -> None:
        self._cards[11].set_value("ferr", value)

    @property
    def pear(self) -> typing.Optional[float]:
        """Get or set the See AUST for description.
        """ # nopep8
        return self._cards[11].get_value("pear")

    @pear.setter
    def pear(self, value: float) -> None:
        self._cards[11].set_value("pear", value)

    @property
    def bain(self) -> typing.Optional[float]:
        """Get or set the See AUST for description.
        """ # nopep8
        return self._cards[11].get_value("bain")

    @bain.setter
    def bain(self, value: float) -> None:
        self._cards[11].set_value("bain", value)

    @property
    def mart(self) -> typing.Optional[float]:
        """Get or set the See AUST for description
        """ # nopep8
        return self._cards[11].get_value("mart")

    @mart.setter
    def mart(self, value: float) -> None:
        self._cards[11].set_value("mart", value)

    @property
    def grk(self) -> typing.Optional[float]:
        """Get or set the Growth parameter k (μm2/sec)
        """ # nopep8
        return self._cards[11].get_value("grk")

    @grk.setter
    def grk(self, value: float) -> None:
        self._cards[11].set_value("grk", value)

    @property
    def grqr(self) -> typing.Optional[float]:
        """Get or set the Grain growth activation energy (J/mol) divided by the universal gas constant. Q/R where R=8.314472 (J/mol K)
        """ # nopep8
        return self._cards[11].get_value("grqr")

    @grqr.setter
    def grqr(self, value: float) -> None:
        self._cards[11].set_value("grqr", value)

    @property
    def tau1(self) -> float:
        """Get or set the Empirical grain growth parameter  describing the function τ(T)
        """ # nopep8
        return self._cards[11].get_value("tau1")

    @tau1.setter
    def tau1(self, value: float) -> None:
        self._cards[11].set_value("tau1", value)

    @property
    def gra(self) -> float:
        """Get or set the Grain growth parameter A.
        """ # nopep8
        return self._cards[12].get_value("gra")

    @gra.setter
    def gra(self, value: float) -> None:
        self._cards[12].set_value("gra", value)

    @property
    def grb(self) -> float:
        """Get or set the Grain growth parameter B. A table of recommended values of GRA and GRB is included in Remark 7 of *MAT_244..
        """ # nopep8
        return self._cards[12].get_value("grb")

    @grb.setter
    def grb(self, value: float) -> None:
        self._cards[12].set_value("grb", value)

    @property
    def expa(self) -> float:
        """Get or set the Grain growth parameter .
        """ # nopep8
        return self._cards[12].get_value("expa")

    @expa.setter
    def expa(self, value: float) -> None:
        self._cards[12].set_value("expa", value)

    @property
    def expb(self) -> float:
        """Get or set the Grain growth parameter .
        """ # nopep8
        return self._cards[12].get_value("expb")

    @expb.setter
    def expb(self, value: float) -> None:
        self._cards[12].set_value("expb", value)

    @property
    def grcc(self) -> typing.Optional[float]:
        """Get or set the Grain growth parameter with the concentration of non metals in the blank, weight% of C or N
        """ # nopep8
        return self._cards[12].get_value("grcc")

    @grcc.setter
    def grcc(self, value: float) -> None:
        self._cards[12].set_value("grcc", value)

    @property
    def grcm(self) -> typing.Optional[float]:
        """Get or set the Grain growth parameter with the concentration of metals in the blank, lowest weight% of Cr, V, Nb, Ti, Al.
        """ # nopep8
        return self._cards[12].get_value("grcm")

    @grcm.setter
    def grcm(self, value: float) -> None:
        self._cards[12].set_value("grcm", value)

    @property
    def heatn(self) -> float:
        """Get or set the Grain growth parameter  for the austenite formation
        """ # nopep8
        return self._cards[12].get_value("heatn")

    @heatn.setter
    def heatn(self, value: float) -> None:
        self._cards[12].set_value("heatn", value)

    @property
    def tau2(self) -> float:
        """Get or set the Empirical grain growth parameter  describing the function τ(T)
        """ # nopep8
        return self._cards[12].get_value("tau2")

    @tau2.setter
    def tau2(self, value: float) -> None:
        self._cards[12].set_value("tau2", value)

    @property
    def funca(self) -> typing.Optional[int]:
        """Get or set the ID of a *DEFINE_FUNCTION for saturation stress A (Hockett-Sherby approach)
        """ # nopep8
        return self._cards[13].get_value("funca")

    @funca.setter
    def funca(self, value: int) -> None:
        self._cards[13].set_value("funca", value)

    @property
    def funcb(self) -> typing.Optional[int]:
        """Get or set the ID of a *DEFINE_FUNCTION for initial yield stress B (Hockett-Sherby approach)
        """ # nopep8
        return self._cards[13].get_value("funcb")

    @funcb.setter
    def funcb(self, value: int) -> None:
        self._cards[13].set_value("funcb", value)

    @property
    def funcm(self) -> typing.Optional[int]:
        """Get or set the ID of a *DEFINE_FUNCTION for saturation rate M (Hockett-Sherby approach)
        """ # nopep8
        return self._cards[13].get_value("funcm")

    @funcm.setter
    def funcm(self, value: int) -> None:
        self._cards[13].set_value("funcm", value)

    @property
    def tcvup(self) -> float:
        """Get or set the Upper temperature for determination of average cooling velocity
        """ # nopep8
        return self._cards[13].get_value("tcvup")

    @tcvup.setter
    def tcvup(self, value: float) -> None:
        self._cards[13].set_value("tcvup", value)

    @property
    def tcvlo(self) -> float:
        """Get or set the Lower temperature for determination of average cooling velocity
        """ # nopep8
        return self._cards[13].get_value("tcvlo")

    @tcvlo.setter
    def tcvlo(self, value: float) -> None:
        self._cards[13].set_value("tcvlo", value)

    @property
    def cvcrit(self) -> float:
        """Get or set the Critical cooling velocity. If the average cooling velocity is less than or equal to CVCRIT, the cooling rate at temperature TCVSL is used
        """ # nopep8
        return self._cards[13].get_value("cvcrit")

    @cvcrit.setter
    def cvcrit(self, value: float) -> None:
        self._cards[13].set_value("cvcrit", value)

    @property
    def tcvsl(self) -> float:
        """Get or set the Temperature for determination of cooling velocity for small cooling velocities
        """ # nopep8
        return self._cards[13].get_value("tcvsl")

    @tcvsl.setter
    def tcvsl(self, value: float) -> None:
        self._cards[13].set_value("tcvsl", value)

    @property
    def epsp(self) -> float:
        """Get or set the Plastic strain in Hockett-Sherby approach
        """ # nopep8
        return self._cards[14].get_value("epsp")

    @epsp.setter
    def epsp(self, value: float) -> None:
        self._cards[14].set_value("epsp", value)

    @property
    def expon(self) -> float:
        """Get or set the Exponent in Hockett-Sherby approach
        """ # nopep8
        return self._cards[14].get_value("expon")

    @expon.setter
    def expon(self, value: float) -> None:
        self._cards[14].set_value("expon", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[15].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[15].cards[0].set_value("title", value)

