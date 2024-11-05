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

class MatElasticViscoplasticThermal(KeywordBase):
    """DYNA MAT_ELASTIC_VISCOPLASTIC_THERMAL keyword"""

    keyword = "MAT"
    subkeyword = "ELASTIC_VISCOPLASTIC_THERMAL"
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
                        "sigy",
                        float,
                        40,
                        10,
                        kwargs.get("sigy")
                    ),
                    Field(
                        "alpha",
                        float,
                        50,
                        10,
                        kwargs.get("alpha")
                    ),
                    Field(
                        "lcss",
                        int,
                        60,
                        10,
                        kwargs.get("lcss")
                    ),
                    Field(
                        "fail",
                        float,
                        70,
                        10,
                        kwargs.get("fail")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "qr1",
                        float,
                        0,
                        10,
                        kwargs.get("qr1")
                    ),
                    Field(
                        "cr1",
                        float,
                        10,
                        10,
                        kwargs.get("cr1")
                    ),
                    Field(
                        "qr2",
                        float,
                        20,
                        10,
                        kwargs.get("qr2")
                    ),
                    Field(
                        "cr2",
                        float,
                        30,
                        10,
                        kwargs.get("cr2")
                    ),
                    Field(
                        "qx1",
                        float,
                        40,
                        10,
                        kwargs.get("qx1")
                    ),
                    Field(
                        "cx1",
                        float,
                        50,
                        10,
                        kwargs.get("cx1")
                    ),
                    Field(
                        "qx2",
                        float,
                        60,
                        10,
                        kwargs.get("qx2")
                    ),
                    Field(
                        "cx2",
                        float,
                        70,
                        10,
                        kwargs.get("cx2")
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
                        "p",
                        float,
                        10,
                        10,
                        kwargs.get("p")
                    ),
                    Field(
                        "lce",
                        float,
                        20,
                        10,
                        kwargs.get("lce")
                    ),
                    Field(
                        "lcpr",
                        float,
                        30,
                        10,
                        kwargs.get("lcpr")
                    ),
                    Field(
                        "lcsigy",
                        float,
                        40,
                        10,
                        kwargs.get("lcsigy")
                    ),
                    Field(
                        "lcr",
                        float,
                        50,
                        10,
                        kwargs.get("lcr")
                    ),
                    Field(
                        "lcx",
                        float,
                        60,
                        10,
                        kwargs.get("lcx")
                    ),
                    Field(
                        "lcalph",
                        float,
                        70,
                        10,
                        kwargs.get("lcalph")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcc",
                        int,
                        0,
                        10,
                        kwargs.get("lcc")
                    ),
                    Field(
                        "lcp",
                        int,
                        10,
                        10,
                        kwargs.get("lcp")
                    ),
                    Field(
                        "tref",
                        float,
                        20,
                        10,
                        kwargs.get("tref")
                    ),
                    Field(
                        "lcfail",
                        float,
                        30,
                        10,
                        kwargs.get("lcfail")
                    ),
                    Field(
                        "nuhis",
                        int,
                        40,
                        10,
                        kwargs.get("nuhis")
                    ),
                    Field(
                        "t1phas",
                        float,
                        50,
                        10,
                        kwargs.get("t1phas")
                    ),
                    Field(
                        "t2phas",
                        float,
                        60,
                        10,
                        kwargs.get("t2phas")
                    ),
                    Field(
                        "tol",
                        float,
                        70,
                        10,
                        kwargs.get("tol")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "fushi1",
                        int,
                        0,
                        10,
                        kwargs.get("fushi1")
                    ),
                    Field(
                        "fushi2",
                        int,
                        10,
                        10,
                        kwargs.get("fushi2")
                    ),
                    Field(
                        "fushi3",
                        int,
                        20,
                        10,
                        kwargs.get("fushi3")
                    ),
                    Field(
                        "fushi4",
                        int,
                        30,
                        10,
                        kwargs.get("fushi4")
                    ),
                    Field(
                        "fushi5",
                        int,
                        40,
                        10,
                        kwargs.get("fushi5")
                    ),
                    Field(
                        "fushi6",
                        int,
                        50,
                        10,
                        kwargs.get("fushi6")
                    ),
                    Field(
                        "fushi7",
                        int,
                        60,
                        10,
                        kwargs.get("fushi7")
                    ),
                    Field(
                        "fushi8",
                        int,
                        70,
                        10,
                        kwargs.get("fushi8")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatElasticViscoplasticThermal.option_specs[0],
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
        """Get or set the Material identification. A unique number has to be used.
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
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def sigy(self) -> typing.Optional[float]:
        """Get or set the Initial yield stress.
        """ # nopep8
        return self._cards[0].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        self._cards[0].set_value("sigy", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Coefficient of thermal expansion.
        """ # nopep8
        return self._cards[0].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        self._cards[0].set_value("alpha", value)

    @property
    def lcss(self) -> typing.Optional[int]:
        """Get or set the Load curve ID. The load curve ID defines effective stress versus effective plastic strain.
        Card 2 is ignored with this option.
        """ # nopep8
        return self._cards[0].get_value("lcss")

    @lcss.setter
    def lcss(self, value: int) -> None:
        self._cards[0].set_value("lcss", value)

    @property
    def fail(self) -> typing.Optional[float]:
        """Get or set the Effective plastic failure strain for erosion.
        """ # nopep8
        return self._cards[0].get_value("fail")

    @fail.setter
    def fail(self, value: float) -> None:
        self._cards[0].set_value("fail", value)

    @property
    def qr1(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter QR1.
        """ # nopep8
        return self._cards[1].get_value("qr1")

    @qr1.setter
    def qr1(self, value: float) -> None:
        self._cards[1].set_value("qr1", value)

    @property
    def cr1(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter CR1.
        """ # nopep8
        return self._cards[1].get_value("cr1")

    @cr1.setter
    def cr1(self, value: float) -> None:
        self._cards[1].set_value("cr1", value)

    @property
    def qr2(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter QR2.
        """ # nopep8
        return self._cards[1].get_value("qr2")

    @qr2.setter
    def qr2(self, value: float) -> None:
        self._cards[1].set_value("qr2", value)

    @property
    def cr2(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter CR2.
        """ # nopep8
        return self._cards[1].get_value("cr2")

    @cr2.setter
    def cr2(self, value: float) -> None:
        self._cards[1].set_value("cr2", value)

    @property
    def qx1(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening parameter QX1.
        """ # nopep8
        return self._cards[1].get_value("qx1")

    @qx1.setter
    def qx1(self, value: float) -> None:
        self._cards[1].set_value("qx1", value)

    @property
    def cx1(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening parameter CX1.
        """ # nopep8
        return self._cards[1].get_value("cx1")

    @cx1.setter
    def cx1(self, value: float) -> None:
        self._cards[1].set_value("cx1", value)

    @property
    def qx2(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening parameter QX2.
        """ # nopep8
        return self._cards[1].get_value("qx2")

    @qx2.setter
    def qx2(self, value: float) -> None:
        self._cards[1].set_value("qx2", value)

    @property
    def cx2(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening parameter CX2.
        """ # nopep8
        return self._cards[1].get_value("cx2")

    @cx2.setter
    def cx2(self, value: float) -> None:
        self._cards[1].set_value("cx2", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Viscous material parameter C.
        """ # nopep8
        return self._cards[2].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        self._cards[2].set_value("c", value)

    @property
    def p(self) -> typing.Optional[float]:
        """Get or set the Viscous material parameter P.
        """ # nopep8
        return self._cards[2].get_value("p")

    @p.setter
    def p(self, value: float) -> None:
        self._cards[2].set_value("p", value)

    @property
    def lce(self) -> typing.Optional[float]:
        """Get or set the Load curve defining Young's modulus as a function of temperature.
        E on card 1 is ignored with this option.
        """ # nopep8
        return self._cards[2].get_value("lce")

    @lce.setter
    def lce(self, value: float) -> None:
        self._cards[2].set_value("lce", value)

    @property
    def lcpr(self) -> typing.Optional[float]:
        """Get or set the Load curve defining Poisson's ratio as a function of temperature.
        PR on card 1 is ignored with this option.
        """ # nopep8
        return self._cards[2].get_value("lcpr")

    @lcpr.setter
    def lcpr(self, value: float) -> None:
        self._cards[2].set_value("lcpr", value)

    @property
    def lcsigy(self) -> typing.Optional[float]:
        """Get or set the Load curve defining the initial yield stress as a function of temperature.
        SIGY on card 1 is ignored with this option.
        """ # nopep8
        return self._cards[2].get_value("lcsigy")

    @lcsigy.setter
    def lcsigy(self, value: float) -> None:
        self._cards[2].set_value("lcsigy", value)

    @property
    def lcr(self) -> typing.Optional[float]:
        """Get or set the Load curve for scaling the isotropic hardening parameters QR1 and QR2 or the stress given by the load curve LCSS as a function of temperature.
        """ # nopep8
        return self._cards[2].get_value("lcr")

    @lcr.setter
    def lcr(self, value: float) -> None:
        self._cards[2].set_value("lcr", value)

    @property
    def lcx(self) -> typing.Optional[float]:
        """Get or set the Load curve for scaling the isotropic hardening parameters QX1 and QX2 as a function of temperature.
        """ # nopep8
        return self._cards[2].get_value("lcx")

    @lcx.setter
    def lcx(self, value: float) -> None:
        self._cards[2].set_value("lcx", value)

    @property
    def lcalph(self) -> typing.Optional[float]:
        """Get or set the Load curve defining the coefficient of thermal expansion as a function of temperature.
        ALPHA on card 1 is ignored with this option.
        """ # nopep8
        return self._cards[2].get_value("lcalph")

    @lcalph.setter
    def lcalph(self, value: float) -> None:
        self._cards[2].set_value("lcalph", value)

    @property
    def lcc(self) -> typing.Optional[int]:
        """Get or set the Load curve for scaling the viscous materal parameter C as a function of temperature.
        """ # nopep8
        return self._cards[3].get_value("lcc")

    @lcc.setter
    def lcc(self, value: int) -> None:
        self._cards[3].set_value("lcc", value)

    @property
    def lcp(self) -> typing.Optional[int]:
        """Get or set the Load curve for scaling the viscous material parameter P as a function of temperature.
        """ # nopep8
        return self._cards[3].get_value("lcp")

    @lcp.setter
    def lcp(self, value: int) -> None:
        self._cards[3].set_value("lcp", value)

    @property
    def tref(self) -> typing.Optional[float]:
        """Get or set the Reference temperature required if and only if LCALPH is given with a negative curve ID.
        """ # nopep8
        return self._cards[3].get_value("tref")

    @tref.setter
    def tref(self, value: float) -> None:
        self._cards[3].set_value("tref", value)

    @property
    def lcfail(self) -> typing.Optional[float]:
        """Get or set the Load curve defining the plastic failure strain as a function of temperature. FAIL on card 1 is ignored with this option.
        """ # nopep8
        return self._cards[3].get_value("lcfail")

    @lcfail.setter
    def lcfail(self, value: float) -> None:
        self._cards[3].set_value("lcfail", value)

    @property
    def nuhis(self) -> typing.Optional[int]:
        """Get or set the Number of additional user defined history variables
        """ # nopep8
        return self._cards[3].get_value("nuhis")

    @nuhis.setter
    def nuhis(self, value: int) -> None:
        self._cards[3].set_value("nuhis", value)

    @property
    def t1phas(self) -> typing.Optional[float]:
        """Get or set the Lower temperature limit for cooling rate evaluation.  Cooling rate can be used as input for user defined variables
        """ # nopep8
        return self._cards[3].get_value("t1phas")

    @t1phas.setter
    def t1phas(self, value: float) -> None:
        self._cards[3].set_value("t1phas", value)

    @property
    def t2phas(self) -> typing.Optional[float]:
        """Get or set the Upper temperature limit for cooling rate evaluation.  Cooling rate can be used as input for user defined variables
        """ # nopep8
        return self._cards[3].get_value("t2phas")

    @t2phas.setter
    def t2phas(self, value: float) -> None:
        self._cards[3].set_value("t2phas", value)

    @property
    def tol(self) -> typing.Optional[float]:
        """Get or set the Optional tolerance for plasticity update. The default is 10-6 for solid elements and 10-3 for shells. This parameter overrides the default tolerance for all element types.
        """ # nopep8
        return self._cards[3].get_value("tol")

    @tol.setter
    def tol(self, value: float) -> None:
        self._cards[3].set_value("tol", value)

    @property
    def fushi1(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables
        """ # nopep8
        return self._cards[4].get_value("fushi1")

    @fushi1.setter
    def fushi1(self, value: int) -> None:
        self._cards[4].set_value("fushi1", value)

    @property
    def fushi2(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables
        """ # nopep8
        return self._cards[4].get_value("fushi2")

    @fushi2.setter
    def fushi2(self, value: int) -> None:
        self._cards[4].set_value("fushi2", value)

    @property
    def fushi3(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables
        """ # nopep8
        return self._cards[4].get_value("fushi3")

    @fushi3.setter
    def fushi3(self, value: int) -> None:
        self._cards[4].set_value("fushi3", value)

    @property
    def fushi4(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables
        """ # nopep8
        return self._cards[4].get_value("fushi4")

    @fushi4.setter
    def fushi4(self, value: int) -> None:
        self._cards[4].set_value("fushi4", value)

    @property
    def fushi5(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables
        """ # nopep8
        return self._cards[4].get_value("fushi5")

    @fushi5.setter
    def fushi5(self, value: int) -> None:
        self._cards[4].set_value("fushi5", value)

    @property
    def fushi6(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables
        """ # nopep8
        return self._cards[4].get_value("fushi6")

    @fushi6.setter
    def fushi6(self, value: int) -> None:
        self._cards[4].set_value("fushi6", value)

    @property
    def fushi7(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables
        """ # nopep8
        return self._cards[4].get_value("fushi7")

    @fushi7.setter
    def fushi7(self, value: int) -> None:
        self._cards[4].set_value("fushi7", value)

    @property
    def fushi8(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables
        """ # nopep8
        return self._cards[4].get_value("fushi8")

    @fushi8.setter
    def fushi8(self, value: int) -> None:
        self._cards[4].set_value("fushi8", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[5].cards[0].set_value("title", value)

