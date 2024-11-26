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

class MatJohnsonCookStochastic(KeywordBase):
    """DYNA MAT_JOHNSON_COOK_STOCHASTIC keyword"""

    keyword = "MAT"
    subkeyword = "JOHNSON_COOK_STOCHASTIC"
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
                        "g",
                        float,
                        20,
                        10,
                        kwargs.get("g")
                    ),
                    Field(
                        "e",
                        float,
                        30,
                        10,
                        kwargs.get("e")
                    ),
                    Field(
                        "pr",
                        float,
                        40,
                        10,
                        kwargs.get("pr")
                    ),
                    Field(
                        "dtf",
                        float,
                        50,
                        10,
                        kwargs.get("dtf")
                    ),
                    Field(
                        "vp",
                        float,
                        60,
                        10,
                        kwargs.get("vp", 0.0)
                    ),
                    Field(
                        "rateop",
                        float,
                        70,
                        10,
                        kwargs.get("rateop", 0.0)
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
                        kwargs.get("b")
                    ),
                    Field(
                        "n",
                        float,
                        20,
                        10,
                        kwargs.get("n")
                    ),
                    Field(
                        "c",
                        float,
                        30,
                        10,
                        kwargs.get("c")
                    ),
                    Field(
                        "m",
                        float,
                        40,
                        10,
                        kwargs.get("m")
                    ),
                    Field(
                        "tm",
                        float,
                        50,
                        10,
                        kwargs.get("tm")
                    ),
                    Field(
                        "tr",
                        float,
                        60,
                        10,
                        kwargs.get("tr")
                    ),
                    Field(
                        "epso",
                        float,
                        70,
                        10,
                        kwargs.get("epso")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cp",
                        float,
                        0,
                        10,
                        kwargs.get("cp")
                    ),
                    Field(
                        "pc",
                        float,
                        10,
                        10,
                        kwargs.get("pc")
                    ),
                    Field(
                        "spall",
                        float,
                        20,
                        10,
                        kwargs.get("spall", 2.0)
                    ),
                    Field(
                        "it",
                        float,
                        30,
                        10,
                        kwargs.get("it", 0.0)
                    ),
                    Field(
                        "d1",
                        float,
                        40,
                        10,
                        kwargs.get("d1")
                    ),
                    Field(
                        "d2",
                        float,
                        50,
                        10,
                        kwargs.get("d2")
                    ),
                    Field(
                        "d3",
                        float,
                        60,
                        10,
                        kwargs.get("d3")
                    ),
                    Field(
                        "d4",
                        float,
                        70,
                        10,
                        kwargs.get("d4")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "d5",
                        float,
                        0,
                        10,
                        kwargs.get("d5")
                    ),
                    Field(
                        "c2/p/xnp/d",
                        float,
                        10,
                        10,
                        kwargs.get("c2/p/xnp/d")
                    ),
                    Field(
                        "erod",
                        float,
                        20,
                        10,
                        kwargs.get("erod")
                    ),
                    Field(
                        "efmin",
                        float,
                        30,
                        10,
                        kwargs.get("efmin", 0.000001)
                    ),
                    Field(
                        "numint",
                        float,
                        40,
                        10,
                        kwargs.get("numint")
                    ),
                    Field(
                        "k",
                        float,
                        50,
                        10,
                        kwargs.get("k")
                    ),
                    Field(
                        "eps1",
                        float,
                        60,
                        10,
                        kwargs.get("eps1")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatJohnsonCookStochastic.option_specs[0],
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
    def g(self) -> typing.Optional[float]:
        """Get or set the Shear modulus.
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        self._cards[0].set_value("g", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's Modulus (shell elements only).
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio (shell elements only).
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def dtf(self) -> typing.Optional[float]:
        """Get or set the Minimum time step size for automatic element deletion (shell elements).
        """ # nopep8
        return self._cards[0].get_value("dtf")

    @dtf.setter
    def dtf(self, value: float) -> None:
        self._cards[0].set_value("dtf", value)

    @property
    def vp(self) -> float:
        """Get or set the Formulation for rate effects:
        EQ.0.0: Scale yield stress (default),
        EQ.1.0: Viscoplastic formulation.
        """ # nopep8
        return self._cards[0].get_value("vp")

    @vp.setter
    def vp(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""vp must be one of {0.0,1.0}""")
        self._cards[0].set_value("vp", value)

    @property
    def rateop(self) -> float:
        """Get or set the Optional forms of strain-rate term:
        EQ.0.0:  Log-Linear Johnson-Cook (default),
        EQ.1.0:  Log-Quadratic Huh-Kang (2 parameters),
        EQ.2.0:  Exponential Allen-Rule-Jones,
        EQ.3.0:  Exponential Cowper-Symonds (2 parameters).
        EQ.4.0:	nonlinear rate coefficient (2 parameters)
        EQ.5.0:	log - exponential Couque(4 parameters)
        """ # nopep8
        return self._cards[0].get_value("rateop")

    @rateop.setter
    def rateop(self, value: float) -> None:
        if value not in [0.0, 1.0, 2.0, 3.0, 4.0, 5.0]:
            raise Exception("""rateop must be one of {0.0,1.0,2.0,3.0,4.0,5.0}""")
        self._cards[0].set_value("rateop", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the See equations in the keyword manual page 62 (volume two).
        """ # nopep8
        return self._cards[1].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        self._cards[1].set_value("a", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the See equations in the keyword manual page 62 (volume two).
        """ # nopep8
        return self._cards[1].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        self._cards[1].set_value("b", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the See equations in the keyword manual page 62 (volume two).
        """ # nopep8
        return self._cards[1].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        self._cards[1].set_value("n", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the See equations in the keyword manual page 62 (volume two).
        """ # nopep8
        return self._cards[1].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        self._cards[1].set_value("c", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the See equations in the keyword manual page 62 (volume two).
        """ # nopep8
        return self._cards[1].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        self._cards[1].set_value("m", value)

    @property
    def tm(self) -> typing.Optional[float]:
        """Get or set the Melt temperature.
        """ # nopep8
        return self._cards[1].get_value("tm")

    @tm.setter
    def tm(self, value: float) -> None:
        self._cards[1].set_value("tm", value)

    @property
    def tr(self) -> typing.Optional[float]:
        """Get or set the Room temperature.
        """ # nopep8
        return self._cards[1].get_value("tr")

    @tr.setter
    def tr(self, value: float) -> None:
        self._cards[1].set_value("tr", value)

    @property
    def epso(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain rate. This value depends on the time units.  Typically, input 1 for units of seconds, 0.001 for units of milliseconds, 0.000001 for microseconds, etc.
        """ # nopep8
        return self._cards[1].get_value("epso")

    @epso.setter
    def epso(self, value: float) -> None:
        self._cards[1].set_value("epso", value)

    @property
    def cp(self) -> typing.Optional[float]:
        """Get or set the Specific heat.
        """ # nopep8
        return self._cards[2].get_value("cp")

    @cp.setter
    def cp(self, value: float) -> None:
        self._cards[2].set_value("cp", value)

    @property
    def pc(self) -> typing.Optional[float]:
        """Get or set the Failure stress or pressure cutoff (pmin < 0.0).
        """ # nopep8
        return self._cards[2].get_value("pc")

    @pc.setter
    def pc(self, value: float) -> None:
        self._cards[2].set_value("pc", value)

    @property
    def spall(self) -> float:
        """Get or set the Spall type:
        EQ.0.0: default is set to 2.0,
        EQ. 1.0: p => pmin ,
        EQ. 2.0: if sigma-max  => -pmin element spalls and tension, p < 0, is never allowed (default),
        EQ. 3.0: p < -pmin element spalls and tension, p < 0, is never allowed.
        """ # nopep8
        return self._cards[2].get_value("spall")

    @spall.setter
    def spall(self, value: float) -> None:
        self._cards[2].set_value("spall", value)

    @property
    def it(self) -> float:
        """Get or set the Plastic strain iteration options. This input applies to solid elements only since it is always necessary to iterate for the shell element plane stress condition.
        EQ. 0.0: no iterations (default),
        EQ. 1.0: accurate iterative solution for plastic strain. Much more expensive than default.
        """ # nopep8
        return self._cards[2].get_value("it")

    @it.setter
    def it(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""it must be one of {0.0,1.0}""")
        self._cards[2].set_value("it", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Failure parameter. See equations in the keyword manual page 62 (volume two).
        """ # nopep8
        return self._cards[2].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[2].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Failure parameter. See equations in the keyword manual page 62 (volume two).
        """ # nopep8
        return self._cards[2].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[2].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Failure parameter. See equations in the keyword manual page 62 (volume two).
        """ # nopep8
        return self._cards[2].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[2].set_value("d3", value)

    @property
    def d4(self) -> typing.Optional[float]:
        """Get or set the Failure parameter. See equations in the keyword manual page 62 (volume two).
        """ # nopep8
        return self._cards[2].get_value("d4")

    @d4.setter
    def d4(self, value: float) -> None:
        self._cards[2].set_value("d4", value)

    @property
    def d5(self) -> typing.Optional[float]:
        """Get or set the Failure parameter. Please see equations in the keyword manual page 62 (volume two).
        """ # nopep8
        return self._cards[3].get_value("d5")

    @d5.setter
    def d5(self, value: float) -> None:
        self._cards[3].set_value("d5", value)

    @property
    def c2_p_xnp_d(self) -> typing.Optional[float]:
        """Get or set the Optional strain-rate parameter for Huh-Kang (C2) or Cowper-Symonds (P) forms; see equations below
        """ # nopep8
        return self._cards[3].get_value("c2/p/xnp/d")

    @c2_p_xnp_d.setter
    def c2_p_xnp_d(self, value: float) -> None:
        self._cards[3].set_value("c2/p/xnp/d", value)

    @property
    def erod(self) -> typing.Optional[float]:
        """Get or set the Erosion flag:
        EQ.0.0:	element erosion allowed(default).
        NE.0.0 : element does not erode; deviatoric stresses set to zero when element fails.
        """ # nopep8
        return self._cards[3].get_value("erod")

    @erod.setter
    def erod(self, value: float) -> None:
        self._cards[3].set_value("erod", value)

    @property
    def efmin(self) -> float:
        """Get or set the The lower bound for calculated strain at fracture
        """ # nopep8
        return self._cards[3].get_value("efmin")

    @efmin.setter
    def efmin(self, value: float) -> None:
        self._cards[3].set_value("efmin", value)

    @property
    def numint(self) -> typing.Optional[float]:
        """Get or set the Number of through thickness integration points which must fail before the shell element is deleted. (If zero, all points must fail.)
        Since nodal fiber rotations limit strains at active integration points, the default, which is to require that all integration points fail, is not recommended, because elements undergoing large strain are often not deleted using this criterion.Better results may be obtained when NUMINT is set to 1 or a number less than one half of the number of through thickness points.
        """ # nopep8
        return self._cards[3].get_value("numint")

    @numint.setter
    def numint(self, value: float) -> None:
        self._cards[3].set_value("numint", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the Optional strain-rate parameter for Couque term
        """ # nopep8
        return self._cards[3].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        self._cards[3].set_value("k", value)

    @property
    def eps1(self) -> typing.Optional[float]:
        """Get or set the Optional reference strain rate for Couque term, characterizing the transition between the thermally activated regime and the viscous regime. Input in units of [time ]^(-1)
        """ # nopep8
        return self._cards[3].get_value("eps1")

    @eps1.setter
    def eps1(self, value: float) -> None:
        self._cards[3].set_value("eps1", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[4].cards[0].set_value("title", value)

