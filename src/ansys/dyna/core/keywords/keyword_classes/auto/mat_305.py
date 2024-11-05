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

class Mat305(KeywordBase):
    """DYNA MAT_305 keyword"""

    keyword = "MAT"
    subkeyword = "305"
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
                        "alphat",
                        float,
                        40,
                        10,
                        kwargs.get("alphat")
                    ),
                    Field(
                        "beta",
                        float,
                        50,
                        10,
                        kwargs.get("beta", 0.0)
                    ),
                    Field(
                        "vp",
                        float,
                        60,
                        10,
                        kwargs.get("vp", 0.0)
                    ),
                    Field(
                        "tol",
                        float,
                        70,
                        10,
                        kwargs.get("tol", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "yb",
                        float,
                        0,
                        10,
                        kwargs.get("yb")
                    ),
                    Field(
                        "qdef",
                        float,
                        10,
                        10,
                        kwargs.get("qdef")
                    ),
                    Field(
                        "r",
                        float,
                        20,
                        10,
                        kwargs.get("r")
                    ),
                    Field(
                        "a",
                        float,
                        30,
                        10,
                        kwargs.get("a")
                    ),
                    Field(
                        "b",
                        float,
                        40,
                        10,
                        kwargs.get("b")
                    ),
                    Field(
                        "minrt",
                        float,
                        50,
                        10,
                        kwargs.get("minrt")
                    ),
                    Field(
                        "post",
                        float,
                        60,
                        10,
                        kwargs.get("post")
                    ),
                    Field(
                        "odesol",
                        float,
                        70,
                        10,
                        kwargs.get("odesol")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "asig0",
                        float,
                        0,
                        10,
                        kwargs.get("asig0")
                    ),
                    Field(
                        "bsig0",
                        float,
                        10,
                        10,
                        kwargs.get("bsig0")
                    ),
                    Field(
                        "asigs",
                        float,
                        20,
                        10,
                        kwargs.get("asigs")
                    ),
                    Field(
                        "bsigs",
                        float,
                        30,
                        10,
                        kwargs.get("bsigs")
                    ),
                    Field(
                        "asigss",
                        float,
                        40,
                        10,
                        kwargs.get("asigss")
                    ),
                    Field(
                        "bsigss",
                        float,
                        50,
                        10,
                        kwargs.get("bsigss")
                    ),
                    Field(
                        "aeps",
                        float,
                        60,
                        10,
                        kwargs.get("aeps")
                    ),
                    Field(
                        "beps",
                        float,
                        70,
                        10,
                        kwargs.get("beps")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "thres",
                        float,
                        0,
                        10,
                        kwargs.get("thres")
                    ),
                    Field(
                        "m",
                        float,
                        10,
                        10,
                        kwargs.get("m")
                    ),
                    Field(
                        "alpha",
                        float,
                        20,
                        10,
                        kwargs.get("alpha")
                    ),
                    Field(
                        "nud",
                        float,
                        30,
                        10,
                        kwargs.get("nud")
                    ),
                    Field(
                        "u0",
                        float,
                        40,
                        10,
                        kwargs.get("u0")
                    ),
                    Field(
                        "k",
                        float,
                        50,
                        10,
                        kwargs.get("k")
                    ),
                    Field(
                        "nu",
                        float,
                        60,
                        10,
                        kwargs.get("nu")
                    ),
                    Field(
                        "bnu",
                        float,
                        70,
                        10,
                        kwargs.get("bnu")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "t50",
                        float,
                        0,
                        10,
                        kwargs.get("t50")
                    ),
                    Field(
                        "n",
                        float,
                        10,
                        10,
                        kwargs.get("n")
                    ),
                    Field(
                        "a50",
                        float,
                        20,
                        10,
                        kwargs.get("a50")
                    ),
                    Field(
                        "d",
                        float,
                        30,
                        10,
                        kwargs.get("d")
                    ),
                    Field(
                        "gsf",
                        float,
                        40,
                        10,
                        kwargs.get("gsf")
                    ),
                    Field(
                        "p",
                        float,
                        50,
                        10,
                        kwargs.get("p")
                    ),
                    Field(
                        "q",
                        float,
                        60,
                        10,
                        kwargs.get("q")
                    ),
                    Field(
                        "qrex",
                        float,
                        70,
                        10,
                        kwargs.get("qrex")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat305.option_specs[0],
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
    def alphat(self) -> typing.Optional[float]:
        """Get or set the Thermal expansion coefficient
        """ # nopep8
        return self._cards[0].get_value("alphat")

    @alphat.setter
    def alphat(self, value: float) -> None:
        self._cards[0].set_value("alphat", value)

    @property
    def beta(self) -> float:
        """Get or set the Mixed hardening parameter, 0≤β≤1. β=0 for isotropic and β=1 for kinematic hardening.
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[0].set_value("beta", value)

    @property
    def vp(self) -> float:
        """Get or set the Formulation for rate effects in plasticity update:
        EQ.0.0: No plastic strain rate dependence in yield stress(default)
        EQ.1.0 : Plastic strain rate dependence in yield stress. Slower but more stable(recommended)
        """ # nopep8
        return self._cards[0].get_value("vp")

    @vp.setter
    def vp(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""vp must be one of {0.0,1.0}""")
        self._cards[0].set_value("vp", value)

    @property
    def tol(self) -> float:
        """Get or set the Multiplication factor(must be >0) on tolerance criteria for plasticity and annealing iterations
        LT.1.0:	Increases accuracy at greater computational cost
        EQ.1.0:	Default value
        GT.1.0 : Decreases accuracy at less computational cost
        """ # nopep8
        return self._cards[0].get_value("tol")

    @tol.setter
    def tol(self, value: float) -> None:
        self._cards[0].set_value("tol", value)

    @property
    def yb(self) -> typing.Optional[float]:
        """Get or set the Work hardening parameter B_y. See Work Hardening and Dynamic Softening
        """ # nopep8
        return self._cards[1].get_value("yb")

    @yb.setter
    def yb(self, value: float) -> None:
        self._cards[1].set_value("yb", value)

    @property
    def qdef(self) -> typing.Optional[float]:
        """Get or set the Work hardening activation energy,Q_def . See Work Hardening and Dynamic Softening
        """ # nopep8
        return self._cards[1].get_value("qdef")

    @qdef.setter
    def qdef(self, value: float) -> None:
        self._cards[1].set_value("qdef", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Work hardening gas constant.R. See Work Hardening and Dynamic Softening.
        """ # nopep8
        return self._cards[1].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        self._cards[1].set_value("r", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Dynamic softening parameter a.See Work Hardening and Dynamic Softening.
        """ # nopep8
        return self._cards[1].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        self._cards[1].set_value("a", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Dynamic softening parameter b.See Work Hardening and Dynamic Softening.
        """ # nopep8
        return self._cards[1].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        self._cards[1].set_value("b", value)

    @property
    def minrt(self) -> typing.Optional[float]:
        """Get or set the Work hardening minimum (plastic) strain rate ε ̇_min in Zener-Hollomon parameter
        """ # nopep8
        return self._cards[1].get_value("minrt")

    @minrt.setter
    def minrt(self, value: float) -> None:
        self._cards[1].set_value("minrt", value)

    @property
    def post(self) -> typing.Optional[float]:
        """Get or set the Save additional history variables for post-processing with POST=1
        """ # nopep8
        return self._cards[1].get_value("post")

    @post.setter
    def post(self, value: float) -> None:
        self._cards[1].set_value("post", value)

    @property
    def odesol(self) -> typing.Optional[float]:
        """Get or set the Solver for static recovery stress:
        EQ.0.0:	Trapezoidal rule(default)
        EQ.1.0 : Heun’s method.Faster but less stable
        """ # nopep8
        return self._cards[1].get_value("odesol")

    @odesol.setter
    def odesol(self, value: float) -> None:
        self._cards[1].set_value("odesol", value)

    @property
    def asig0(self) -> typing.Optional[float]:
        """Get or set the Parameters a_i, i=0,s,ss, to calculate σ_0, σ_s, and σ_ss, respectively, from the Zener-Hollomon parameter.  See Work Hardening and Dynamic Softening
        """ # nopep8
        return self._cards[2].get_value("asig0")

    @asig0.setter
    def asig0(self, value: float) -> None:
        self._cards[2].set_value("asig0", value)

    @property
    def bsig0(self) -> typing.Optional[float]:
        """Get or set the Parameters b_i, i=0,s,ss, to calculate σ_0, σ_s, and σ_ss, respectively, from the Zener-Hollomon parameter.  See Work Hardening and Dynamic Softening.
        """ # nopep8
        return self._cards[2].get_value("bsig0")

    @bsig0.setter
    def bsig0(self, value: float) -> None:
        self._cards[2].set_value("bsig0", value)

    @property
    def asigs(self) -> typing.Optional[float]:
        """Get or set the Parameters a_i, i=0,s,ss, to calculate σ_0, σ_s, and σ_ss, respectively, from the Zener-Hollomon parameter.  See Work Hardening and Dynamic Softening
        """ # nopep8
        return self._cards[2].get_value("asigs")

    @asigs.setter
    def asigs(self, value: float) -> None:
        self._cards[2].set_value("asigs", value)

    @property
    def bsigs(self) -> typing.Optional[float]:
        """Get or set the Parameters b_i, i=0,s,ss, to calculate σ_0, σ_s, and σ_ss, respectively, from the Zener-Hollomon parameter.  See Work Hardening and Dynamic Softening.
        """ # nopep8
        return self._cards[2].get_value("bsigs")

    @bsigs.setter
    def bsigs(self, value: float) -> None:
        self._cards[2].set_value("bsigs", value)

    @property
    def asigss(self) -> typing.Optional[float]:
        """Get or set the Parameters a_i, i=0,s,ss, to calculate σ_0, σ_s, and σ_ss, respectively, from the Zener-Hollomon parameter.  See Work Hardening and Dynamic Softening
        """ # nopep8
        return self._cards[2].get_value("asigss")

    @asigss.setter
    def asigss(self, value: float) -> None:
        self._cards[2].set_value("asigss", value)

    @property
    def bsigss(self) -> typing.Optional[float]:
        """Get or set the Parameters b_i, i=0,s,ss, to calculate σ_0, σ_s, and σ_ss, respectively, from the Zener-Hollomon parameter.  See Work Hardening and Dynamic Softening.
        """ # nopep8
        return self._cards[2].get_value("bsigss")

    @bsigss.setter
    def bsigss(self, value: float) -> None:
        self._cards[2].set_value("bsigss", value)

    @property
    def aeps(self) -> typing.Optional[float]:
        """Get or set the Parameter a_(ε_s ) used to calculate the saturation strain, ε_s, for dynamic relaxation from the Zener-Hollomon parameter. See Work Hardening and Dynamic Softening
        """ # nopep8
        return self._cards[2].get_value("aeps")

    @aeps.setter
    def aeps(self, value: float) -> None:
        self._cards[2].set_value("aeps", value)

    @property
    def beps(self) -> typing.Optional[float]:
        """Get or set the Parameter a_(ε_s ) used to calculate the saturation strain, ε_s, for dynamic relaxation from the Zener-Hollomon parameter. See Work Hardening and Dynamic Softening
        """ # nopep8
        return self._cards[2].get_value("beps")

    @beps.setter
    def beps(self, value: float) -> None:
        self._cards[2].set_value("beps", value)

    @property
    def thres(self) -> typing.Optional[float]:
        """Get or set the Static recovery strain rate threshold.THRES>0 turns off dynamic softening, meaning. sets A=0.
        """ # nopep8
        return self._cards[3].get_value("thres")

    @thres.setter
    def thres(self, value: float) -> None:
        self._cards[3].set_value("thres", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Taylor factor M for static recovery stress
        """ # nopep8
        return self._cards[3].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        self._cards[3].set_value("m", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the α parameter for static recovery stress
        """ # nopep8
        return self._cards[3].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        self._cards[3].set_value("alpha", value)

    @property
    def nud(self) -> typing.Optional[float]:
        """Get or set the Debye frequency Vd for static recovery stress
        """ # nopep8
        return self._cards[3].get_value("nud")

    @nud.setter
    def nud(self, value: float) -> None:
        self._cards[3].set_value("nud", value)

    @property
    def u0(self) -> typing.Optional[float]:
        """Get or set the Activation energy U_0 for static recovery stress
        """ # nopep8
        return self._cards[3].get_value("u0")

    @u0.setter
    def u0(self, value: float) -> None:
        self._cards[3].set_value("u0", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the Bolzmann constant
        """ # nopep8
        return self._cards[3].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        self._cards[3].set_value("k", value)

    @property
    def nu(self) -> typing.Optional[float]:
        """Get or set the Interaction volume ν for static recovery stress
        """ # nopep8
        return self._cards[3].get_value("nu")

    @nu.setter
    def nu(self, value: float) -> None:
        self._cards[3].set_value("nu", value)

    @property
    def bnu(self) -> typing.Optional[float]:
        """Get or set the Burger’s vector b_ν for static recovery stress
        """ # nopep8
        return self._cards[3].get_value("bnu")

    @bnu.setter
    def bnu(self, value: float) -> None:
        self._cards[3].set_value("bnu", value)

    @property
    def t50(self) -> typing.Optional[float]:
        """Get or set the Time required to reach 50 % static recrystallization
        """ # nopep8
        return self._cards[4].get_value("t50")

    @t50.setter
    def t50(self, value: float) -> None:
        self._cards[4].set_value("t50", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Static recrystallization exponent
        """ # nopep8
        return self._cards[4].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        self._cards[4].set_value("n", value)

    @property
    def a50(self) -> typing.Optional[float]:
        """Get or set the Scale parameter for strain dependent recrystallization time
        EQ.0.0:	T50 parameter is used for recrystallization time
        GT.0.0 : T50 parameter is ignored.Recrystallization time is calculated using A50, D, GSF, P, Q, QREX
        LT.0.0 : T50 parameter is used.Recrystallization factor and combined recovery stress calculated using A50, D, GSF, P, Q, QREX is added to history variables if POST = 1
        """ # nopep8
        return self._cards[4].get_value("a50")

    @a50.setter
    def a50(self, value: float) -> None:
        self._cards[4].set_value("a50", value)

    @property
    def d(self) -> typing.Optional[float]:
        """Get or set the Length parameter for strain dependent recrystallization time
        """ # nopep8
        return self._cards[4].get_value("d")

    @d.setter
    def d(self, value: float) -> None:
        self._cards[4].set_value("d", value)

    @property
    def gsf(self) -> typing.Optional[float]:
        """Get or set the Exponent for strain dependent recrystallization time
        """ # nopep8
        return self._cards[4].get_value("gsf")

    @gsf.setter
    def gsf(self, value: float) -> None:
        self._cards[4].set_value("gsf", value)

    @property
    def p(self) -> typing.Optional[float]:
        """Get or set the Exponent for strain dependent recrystallization time
        """ # nopep8
        return self._cards[4].get_value("p")

    @p.setter
    def p(self, value: float) -> None:
        self._cards[4].set_value("p", value)

    @property
    def q(self) -> typing.Optional[float]:
        """Get or set the Exponent for strain dependent recrystallization time
        """ # nopep8
        return self._cards[4].get_value("q")

    @q.setter
    def q(self, value: float) -> None:
        self._cards[4].set_value("q", value)

    @property
    def qrex(self) -> typing.Optional[float]:
        """Get or set the Activation energy for strain dependent recrystallization time
        """ # nopep8
        return self._cards[4].get_value("qrex")

    @qrex.setter
    def qrex(self, value: float) -> None:
        self._cards[4].set_value("qrex", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[5].cards[0].set_value("title", value)

