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

class MatCohesiveMixedModeElastoplasticRate(KeywordBase):
    """DYNA MAT_COHESIVE_MIXED_MODE_ELASTOPLASTIC_RATE keyword"""

    keyword = "MAT"
    subkeyword = "COHESIVE_MIXED_MODE_ELASTOPLASTIC_RATE"
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
                        "roflg",
                        int,
                        20,
                        10,
                        kwargs.get("roflg", 0)
                    ),
                    Field(
                        "intfail",
                        float,
                        30,
                        10,
                        kwargs.get("intfail")
                    ),
                    Field(
                        "emod",
                        float,
                        40,
                        10,
                        kwargs.get("emod")
                    ),
                    Field(
                        "gmod",
                        float,
                        50,
                        10,
                        kwargs.get("gmod")
                    ),
                    Field(
                        "thick",
                        float,
                        60,
                        10,
                        kwargs.get("thick")
                    ),
                    Field(
                        "inicrt",
                        float,
                        70,
                        10,
                        kwargs.get("inicrt", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "g1c_0",
                        float,
                        0,
                        10,
                        kwargs.get("g1c_0")
                    ),
                    Field(
                        "g1c_inf",
                        float,
                        10,
                        10,
                        kwargs.get("g1c_inf")
                    ),
                    Field(
                        "edot_g1",
                        float,
                        20,
                        10,
                        kwargs.get("edot_g1")
                    ),
                    Field(
                        "t0",
                        float,
                        30,
                        10,
                        kwargs.get("t0")
                    ),
                    Field(
                        "t1",
                        float,
                        40,
                        10,
                        kwargs.get("t1")
                    ),
                    Field(
                        "edot_t",
                        float,
                        50,
                        10,
                        kwargs.get("edot_t")
                    ),
                    Field(
                        "fg1",
                        float,
                        60,
                        10,
                        kwargs.get("fg1")
                    ),
                    Field(
                        "lcg1c",
                        int,
                        70,
                        10,
                        kwargs.get("lcg1c")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "g2c_0",
                        float,
                        0,
                        10,
                        kwargs.get("g2c_0")
                    ),
                    Field(
                        "g2c_inf",
                        float,
                        10,
                        10,
                        kwargs.get("g2c_inf")
                    ),
                    Field(
                        "edot_g2",
                        float,
                        20,
                        10,
                        kwargs.get("edot_g2")
                    ),
                    Field(
                        "s0",
                        float,
                        30,
                        10,
                        kwargs.get("s0")
                    ),
                    Field(
                        "s1",
                        float,
                        40,
                        10,
                        kwargs.get("s1")
                    ),
                    Field(
                        "edot_s",
                        float,
                        50,
                        10,
                        kwargs.get("edot_s")
                    ),
                    Field(
                        "fg2",
                        float,
                        60,
                        10,
                        kwargs.get("fg2")
                    ),
                    Field(
                        "lcg2c",
                        int,
                        70,
                        10,
                        kwargs.get("lcg2c")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "rfiltf",
                        float,
                        0,
                        10,
                        kwargs.get("rfiltf")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatCohesiveMixedModeElastoplasticRate.option_specs[0],
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
    def roflg(self) -> int:
        """Get or set the Flag for whether density is specified per unit area or volume. ROFLG=0 specified density per unit volume (default), and ROFLG=1 specifies the density is per unit area for controlling the mass of cohesive elements with an initial volume of zero.
        """ # nopep8
        return self._cards[0].get_value("roflg")

    @roflg.setter
    def roflg(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""roflg must be one of {0,1}""")
        self._cards[0].set_value("roflg", value)

    @property
    def intfail(self) -> typing.Optional[float]:
        """Get or set the The number of integration points required for the cohesive element to be deleted. The value of INTFAIL may range from 1 to 4 with 1 the recommended value.
        LT.0.0:	Employs a Newton - Cotes integration scheme. The element will be deleted when | INTFAIL | integration points have failed.
        EQ.0.0 : Employs a Newton - Cotes integration scheme. The element will not be deleted even if it satisfies the failure criterion.
        GT.0.0 : Employs a Gauss integration scheme. The element will be deleted when INTFAIL integration points have failed.
        """ # nopep8
        return self._cards[0].get_value("intfail")

    @intfail.setter
    def intfail(self, value: float) -> None:
        self._cards[0].set_value("intfail", value)

    @property
    def emod(self) -> typing.Optional[float]:
        """Get or set the Young’s modulus of the material (Mode I). It is a curve ID if the THERMAL keyword option is used. It is a function ID if the FUNCTIONS keyword option is used.
        """ # nopep8
        return self._cards[0].get_value("emod")

    @emod.setter
    def emod(self, value: float) -> None:
        self._cards[0].set_value("emod", value)

    @property
    def gmod(self) -> typing.Optional[float]:
        """Get or set the The shear modulus of the material (Mode II). Curve ID for THERMAL keyword option. GMOD is a function ID for the FUNCTIONS keyword option
        """ # nopep8
        return self._cards[0].get_value("gmod")

    @gmod.setter
    def gmod(self, value: float) -> None:
        self._cards[0].set_value("gmod", value)

    @property
    def thick(self) -> typing.Optional[float]:
        """Get or set the GT.0.0: Cohesive thickness. LE.0.0: Initial thickness is calculated from nodal coordinates.
        """ # nopep8
        return self._cards[0].get_value("thick")

    @thick.setter
    def thick(self, value: float) -> None:
        self._cards[0].set_value("thick", value)

    @property
    def inicrt(self) -> float:
        """Get or set the Yield and damage initiation criterion:
        EQ.0.0:	quadratic nominal stress(default)
        EQ.1.0 : maximum nominal stress.
        EQ.2.0:	maximum nominal stress ( same as INICRT=1.0). Additionally flags outputting the maximum strain as history variable #15
        LT.0.0: mixed mode with flexible exponent | INICRT |
        """ # nopep8
        return self._cards[0].get_value("inicrt")

    @inicrt.setter
    def inicrt(self, value: float) -> None:
        self._cards[0].set_value("inicrt", value)

    @property
    def g1c_0(self) -> typing.Optional[float]:
        """Get or set the GT 0.0: Energy release rate GIC in Mode I. LE. 0.0: Lower bound value of rate-dependent GIC.
        """ # nopep8
        return self._cards[1].get_value("g1c_0")

    @g1c_0.setter
    def g1c_0(self, value: float) -> None:
        self._cards[1].set_value("g1c_0", value)

    @property
    def g1c_inf(self) -> typing.Optional[float]:
        """Get or set the Upper bound value of rate-dependent GIC (only considered if G1C_0<0).
        """ # nopep8
        return self._cards[1].get_value("g1c_inf")

    @g1c_inf.setter
    def g1c_inf(self, value: float) -> None:
        self._cards[1].set_value("g1c_inf", value)

    @property
    def edot_g1(self) -> typing.Optional[float]:
        """Get or set the Equivalent strain rate at yield initiation to describe the rate dependency of GIC (only considered if G1C_0<0).
        """ # nopep8
        return self._cards[1].get_value("edot_g1")

    @edot_g1.setter
    def edot_g1(self, value: float) -> None:
        self._cards[1].set_value("edot_g1", value)

    @property
    def t0(self) -> typing.Optional[float]:
        """Get or set the GT.0.0: Yield stress in Mode I
        LT.0.0: Rate-dependency is considered, Parameter T0.
        """ # nopep8
        return self._cards[1].get_value("t0")

    @t0.setter
    def t0(self, value: float) -> None:
        self._cards[1].set_value("t0", value)

    @property
    def t1(self) -> typing.Optional[float]:
        """Get or set the Parameter T1, only considered if T0 < 0:
        GT.0.0: Quadratic logarithmic model
        LT.0.0: Linear logarithmic model.
        """ # nopep8
        return self._cards[1].get_value("t1")

    @t1.setter
    def t1(self, value: float) -> None:
        self._cards[1].set_value("t1", value)

    @property
    def edot_t(self) -> typing.Optional[float]:
        """Get or set the Equivalent strain rate at yield initiation to describe the rate dependency of the yield stress in Mode I (only considered if T0<0).
        """ # nopep8
        return self._cards[1].get_value("edot_t")

    @edot_t.setter
    def edot_t(self, value: float) -> None:
        self._cards[1].set_value("edot_t", value)

    @property
    def fg1(self) -> typing.Optional[float]:
        """Get or set the Parameter fG1 to describe the tri-linear shape of the traction-separation law in Mode I.
        """ # nopep8
        return self._cards[1].get_value("fg1")

    @fg1.setter
    def fg1(self, value: float) -> None:
        self._cards[1].set_value("fg1", value)

    @property
    def lcg1c(self) -> typing.Optional[int]:
        """Get or set the Load curve ID which defines fracture energy GIC as a function of cohesive element thickness. G1C_‌0 and G1C_‌INF are ignored in this case.
        """ # nopep8
        return self._cards[1].get_value("lcg1c")

    @lcg1c.setter
    def lcg1c(self, value: int) -> None:
        self._cards[1].set_value("lcg1c", value)

    @property
    def g2c_0(self) -> typing.Optional[float]:
        """Get or set the GT.0.0: Energy release rate GIIC in Mode II
        LE.0.0: Lower bound value of rate-dependent GIIC.
        """ # nopep8
        return self._cards[2].get_value("g2c_0")

    @g2c_0.setter
    def g2c_0(self, value: float) -> None:
        self._cards[2].set_value("g2c_0", value)

    @property
    def g2c_inf(self) -> typing.Optional[float]:
        """Get or set the Upper bound value of GIIC (only considered if G2C_0<0).
        """ # nopep8
        return self._cards[2].get_value("g2c_inf")

    @g2c_inf.setter
    def g2c_inf(self, value: float) -> None:
        self._cards[2].set_value("g2c_inf", value)

    @property
    def edot_g2(self) -> typing.Optional[float]:
        """Get or set the Equivalent strain rate at yield initiation to describe the rate dependency of GIIC (only considered if G2C_0<0).
        """ # nopep8
        return self._cards[2].get_value("edot_g2")

    @edot_g2.setter
    def edot_g2(self, value: float) -> None:
        self._cards[2].set_value("edot_g2", value)

    @property
    def s0(self) -> typing.Optional[float]:
        """Get or set the GT.0.0: Yield stress in Mode II
        LT.0.0: Rate-dependency is considered, Parameter S0.
        """ # nopep8
        return self._cards[2].get_value("s0")

    @s0.setter
    def s0(self, value: float) -> None:
        self._cards[2].set_value("s0", value)

    @property
    def s1(self) -> typing.Optional[float]:
        """Get or set the Parameter S1, only considered if S0<0:
        GT.0.0: Quadratic logarithmic model is applied
        LT.0.0: Linear logarithmic model is applied.
        """ # nopep8
        return self._cards[2].get_value("s1")

    @s1.setter
    def s1(self, value: float) -> None:
        self._cards[2].set_value("s1", value)

    @property
    def edot_s(self) -> typing.Optional[float]:
        """Get or set the Equivalent strain rate at yield initiation to describe the rate dependency of the yield stress in Mode II (only considered if S0<0).
        """ # nopep8
        return self._cards[2].get_value("edot_s")

    @edot_s.setter
    def edot_s(self, value: float) -> None:
        self._cards[2].set_value("edot_s", value)

    @property
    def fg2(self) -> typing.Optional[float]:
        """Get or set the Parameter fG2 to describe the tri-linear shape of the traction-separation law in Mode II.
        """ # nopep8
        return self._cards[2].get_value("fg2")

    @fg2.setter
    def fg2(self, value: float) -> None:
        self._cards[2].set_value("fg2", value)

    @property
    def lcg2c(self) -> typing.Optional[int]:
        """Get or set the Load curve ID which defines fracture energy GIIC as a function of cohesive element thickness. G2C_‌0 and G2C_‌INF are ignored in that case.
        """ # nopep8
        return self._cards[2].get_value("lcg2c")

    @lcg2c.setter
    def lcg2c(self, value: int) -> None:
        self._cards[2].set_value("lcg2c", value)

    @property
    def rfiltf(self) -> typing.Optional[float]:
        """Get or set the Smoothing factor on the equivalent strain rate using an exponential moving average method:
        This option invokes a modified handling of strain rates, see Remarks.
        GT.0.0:	RFILTF applied on the equivalent plastic strain rate
        LT.0.0 : | RFILTF | applied on the equivalent total strain rate
        """ # nopep8
        return self._cards[3].get_value("rfiltf")

    @rfiltf.setter
    def rfiltf(self, value: float) -> None:
        self._cards[3].set_value("rfiltf", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[4].cards[0].set_value("title", value)

