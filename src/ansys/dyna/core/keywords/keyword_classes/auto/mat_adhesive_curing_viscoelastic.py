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

class MatAdhesiveCuringViscoelastic(KeywordBase):
    """DYNA MAT_ADHESIVE_CURING_VISCOELASTIC keyword"""

    keyword = "MAT"
    subkeyword = "ADHESIVE_CURING_VISCOELASTIC"
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
                        "k1",
                        float,
                        20,
                        10,
                        kwargs.get("k1")
                    ),
                    Field(
                        "k2",
                        float,
                        30,
                        10,
                        kwargs.get("k2")
                    ),
                    Field(
                        "c1",
                        float,
                        40,
                        10,
                        kwargs.get("c1")
                    ),
                    Field(
                        "c2",
                        float,
                        50,
                        10,
                        kwargs.get("c2")
                    ),
                    Field(
                        "m",
                        float,
                        60,
                        10,
                        kwargs.get("m")
                    ),
                    Field(
                        "n",
                        float,
                        70,
                        10,
                        kwargs.get("n")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "chexp1",
                        float,
                        0,
                        10,
                        kwargs.get("chexp1")
                    ),
                    Field(
                        "chexp2",
                        float,
                        10,
                        10,
                        kwargs.get("chexp2")
                    ),
                    Field(
                        "chexp3",
                        float,
                        20,
                        10,
                        kwargs.get("chexp3")
                    ),
                    Field(
                        "lcchexp",
                        int,
                        30,
                        10,
                        kwargs.get("lcchexp")
                    ),
                    Field(
                        "lcthexp",
                        int,
                        40,
                        10,
                        kwargs.get("lcthexp")
                    ),
                    Field(
                        "r",
                        float,
                        50,
                        10,
                        kwargs.get("r")
                    ),
                    Field(
                        "trefexp",
                        float,
                        60,
                        10,
                        kwargs.get("trefexp")
                    ),
                    Field(
                        "docrefexp",
                        float,
                        70,
                        10,
                        kwargs.get("docrefexp")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "wlftref",
                        float,
                        0,
                        10,
                        kwargs.get("wlftref")
                    ),
                    Field(
                        "wlfa",
                        float,
                        10,
                        10,
                        kwargs.get("wlfa")
                    ),
                    Field(
                        "wlfb",
                        float,
                        20,
                        10,
                        kwargs.get("wlfb")
                    ),
                    Field(
                        "lcg0",
                        int,
                        30,
                        10,
                        kwargs.get("lcg0")
                    ),
                    Field(
                        "lck0",
                        int,
                        40,
                        10,
                        kwargs.get("lck0")
                    ),
                    Field(
                        "idoc",
                        float,
                        50,
                        10,
                        kwargs.get("idoc")
                    ),
                    Field(
                        "incr",
                        int,
                        60,
                        10,
                        kwargs.get("incr", 0)
                    ),
                    Field(
                        "qcure",
                        float,
                        70,
                        10,
                        kwargs.get("qcure")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "gi",
                        float,
                        0,
                        10,
                        kwargs.get("gi")
                    ),
                    Field(
                        "betagi",
                        float,
                        10,
                        10,
                        kwargs.get("betagi")
                    ),
                    Field(
                        "ki",
                        float,
                        20,
                        10,
                        kwargs.get("ki")
                    ),
                    Field(
                        "betaki",
                        float,
                        30,
                        10,
                        kwargs.get("betaki")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatAdhesiveCuringViscoelastic.option_specs[0],
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
        """Get or set the Material identification. A unique number or label must be specified.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def k1(self) -> typing.Optional[float]:
        """Get or set the Parameter K1 for Kamal model.
        """ # nopep8
        return self._cards[0].get_value("k1")

    @k1.setter
    def k1(self, value: float) -> None:
        self._cards[0].set_value("k1", value)

    @property
    def k2(self) -> typing.Optional[float]:
        """Get or set the Parameter K2 for Kamal model.
        """ # nopep8
        return self._cards[0].get_value("k2")

    @k2.setter
    def k2(self, value: float) -> None:
        self._cards[0].set_value("k2", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the Parameter C1 for Kamal model.
        """ # nopep8
        return self._cards[0].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        self._cards[0].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the Parameter C2 for Kamal model.
        """ # nopep8
        return self._cards[0].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        self._cards[0].set_value("c2", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Exponent m for Kamal model.
        """ # nopep8
        return self._cards[0].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        self._cards[0].set_value("m", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Exponent n for Kamal model.
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        self._cards[0].set_value("n", value)

    @property
    def chexp1(self) -> typing.Optional[float]:
        """Get or set the Quadratic parameter 𝛾2 for chemical shrinkage.
        """ # nopep8
        return self._cards[1].get_value("chexp1")

    @chexp1.setter
    def chexp1(self, value: float) -> None:
        self._cards[1].set_value("chexp1", value)

    @property
    def chexp2(self) -> typing.Optional[float]:
        """Get or set the Linear parameter 𝛾1 for chemical shrinkage.
        """ # nopep8
        return self._cards[1].get_value("chexp2")

    @chexp2.setter
    def chexp2(self, value: float) -> None:
        self._cards[1].set_value("chexp2", value)

    @property
    def chexp3(self) -> typing.Optional[float]:
        """Get or set the Constant parameter 𝛾0 for chemical shrinkage.
        """ # nopep8
        return self._cards[1].get_value("chexp3")

    @chexp3.setter
    def chexp3(self, value: float) -> None:
        self._cards[1].set_value("chexp3", value)

    @property
    def lcchexp(self) -> typing.Optional[int]:
        """Get or set the |LCCHEXP| is Load curve ID to define the coefficient for chemical shrinkage 𝛾(𝛼)
        as a function of the state of cure 𝛼. If set, parameters CHEXP1,
        CHEXP2 and CHEXP3 are ignored.
        """ # nopep8
        return self._cards[1].get_value("lcchexp")

    @lcchexp.setter
    def lcchexp(self, value: int) -> None:
        self._cards[1].set_value("lcchexp", value)

    @property
    def lcthexp(self) -> typing.Optional[int]:
        """Get or set the |LCTHEXP| is Load curve ID or table ID defining the coefficient of
        thermal expansion 𝛽(𝛼, 𝑇) as a function of cure 𝛼 and temperature 𝑇.
        If referring to a load curve, parameter 𝛽(𝑇) is a function of
        temperature 𝑇.
        """ # nopep8
        return self._cards[1].get_value("lcthexp")

    @lcthexp.setter
    def lcthexp(self, value: int) -> None:
        self._cards[1].set_value("lcthexp", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Gas constant 𝑅 for Kamal model.
        """ # nopep8
        return self._cards[1].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        self._cards[1].set_value("r", value)

    @property
    def trefexp(self) -> typing.Optional[float]:
        """Get or set the Reference temperature 𝑇0 for secant form of thermal expansion. See Remarks below.
        """ # nopep8
        return self._cards[1].get_value("trefexp")

    @trefexp.setter
    def trefexp(self, value: float) -> None:
        self._cards[1].set_value("trefexp", value)

    @property
    def docrefexp(self) -> typing.Optional[float]:
        """Get or set the Reference degree of cure 𝛼0 for sequential form of chemical	expansion. See Remarks below.
        """ # nopep8
        return self._cards[1].get_value("docrefexp")

    @docrefexp.setter
    def docrefexp(self, value: float) -> None:
        self._cards[1].set_value("docrefexp", value)

    @property
    def wlftref(self) -> typing.Optional[float]:
        """Get or set the Reference temperature for WLF shift function.
        """ # nopep8
        return self._cards[2].get_value("wlftref")

    @wlftref.setter
    def wlftref(self, value: float) -> None:
        self._cards[2].set_value("wlftref", value)

    @property
    def wlfa(self) -> typing.Optional[float]:
        """Get or set the Parameter A for WLF shift function.
        """ # nopep8
        return self._cards[2].get_value("wlfa")

    @wlfa.setter
    def wlfa(self, value: float) -> None:
        self._cards[2].set_value("wlfa", value)

    @property
    def wlfb(self) -> typing.Optional[float]:
        """Get or set the Parameter B for WLF shift function.
        """ # nopep8
        return self._cards[2].get_value("wlfb")

    @wlfb.setter
    def wlfb(self, value: float) -> None:
        self._cards[2].set_value("wlfb", value)

    @property
    def lcg0(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining the instantaneous shear modulus G0 as a function of state of cure.
        """ # nopep8
        return self._cards[2].get_value("lcg0")

    @lcg0.setter
    def lcg0(self, value: int) -> None:
        self._cards[2].set_value("lcg0", value)

    @property
    def lck0(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining the instantaneous bulk modulus K0 as a function of state of cure.
        """ # nopep8
        return self._cards[2].get_value("lck0")

    @lck0.setter
    def lck0(self, value: int) -> None:
        self._cards[2].set_value("lck0", value)

    @property
    def idoc(self) -> typing.Optional[float]:
        """Get or set the Initial degree of cure.
        """ # nopep8
        return self._cards[2].get_value("idoc")

    @idoc.setter
    def idoc(self, value: float) -> None:
        self._cards[2].set_value("idoc", value)

    @property
    def incr(self) -> int:
        """Get or set the Switch between incremental and total stress formulation.
        EQ.0: total form: (DEFAULT)
        EQ.1: incremental form: (recommended).
        """ # nopep8
        return self._cards[2].get_value("incr")

    @incr.setter
    def incr(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""incr must be one of {0,1}""")
        self._cards[2].set_value("incr", value)

    @property
    def qcure(self) -> typing.Optional[float]:
        """Get or set the Heat generation factor, relating the heat generated in one time step with the increment of the degree of cure in that step
        """ # nopep8
        return self._cards[2].get_value("qcure")

    @qcure.setter
    def qcure(self, value: float) -> None:
        self._cards[2].set_value("qcure", value)

    @property
    def gi(self) -> typing.Optional[float]:
        """Get or set the Shear relaxation modulus for the ith term for fully cured material.
        """ # nopep8
        return self._cards[3].get_value("gi")

    @gi.setter
    def gi(self, value: float) -> None:
        self._cards[3].set_value("gi", value)

    @property
    def betagi(self) -> typing.Optional[float]:
        """Get or set the Shear decay constant for the ith term for fully cured material.
        """ # nopep8
        return self._cards[3].get_value("betagi")

    @betagi.setter
    def betagi(self, value: float) -> None:
        self._cards[3].set_value("betagi", value)

    @property
    def ki(self) -> typing.Optional[float]:
        """Get or set the Bulk relaxation modulus for the ith term for fully cured material.
        """ # nopep8
        return self._cards[3].get_value("ki")

    @ki.setter
    def ki(self, value: float) -> None:
        self._cards[3].set_value("ki", value)

    @property
    def betaki(self) -> typing.Optional[float]:
        """Get or set the Bulk decay constant for the ith term for fully cured material.
        """ # nopep8
        return self._cards[3].get_value("betaki")

    @betaki.setter
    def betaki(self, value: float) -> None:
        self._cards[3].set_value("betaki", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[4].cards[0].set_value("title", value)

