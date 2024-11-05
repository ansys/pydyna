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

class MatModifiedPiecewiseLinearPlasticityRate(KeywordBase):
    """DYNA MAT_MODIFIED_PIECEWISE_LINEAR_PLASTICITY_RATE keyword"""

    keyword = "MAT"
    subkeyword = "MODIFIED_PIECEWISE_LINEAR_PLASTICITY_RATE"
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
                        "etan",
                        float,
                        50,
                        10,
                        kwargs.get("etan")
                    ),
                    Field(
                        "fail",
                        float,
                        60,
                        10,
                        kwargs.get("fail", 10.E+20)
                    ),
                    Field(
                        "tdel",
                        float,
                        70,
                        10,
                        kwargs.get("tdel")
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
                        "lcss",
                        int,
                        20,
                        10,
                        kwargs.get("lcss", 0)
                    ),
                    Field(
                        "lcsr",
                        int,
                        30,
                        10,
                        kwargs.get("lcsr", 0)
                    ),
                    Field(
                        "vp",
                        float,
                        40,
                        10,
                        kwargs.get("vp", 0.0)
                    ),
                    Field(
                        "epsthin",
                        float,
                        50,
                        10,
                        kwargs.get("epsthin")
                    ),
                    Field(
                        "epsmaj",
                        float,
                        60,
                        10,
                        kwargs.get("epsmaj")
                    ),
                    Field(
                        "numint",
                        float,
                        70,
                        10,
                        kwargs.get("numint", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "eps1",
                        float,
                        0,
                        10,
                        kwargs.get("eps1")
                    ),
                    Field(
                        "eps2",
                        float,
                        10,
                        10,
                        kwargs.get("eps2")
                    ),
                    Field(
                        "eps3",
                        float,
                        20,
                        10,
                        kwargs.get("eps3")
                    ),
                    Field(
                        "eps4",
                        float,
                        30,
                        10,
                        kwargs.get("eps4")
                    ),
                    Field(
                        "eps5",
                        float,
                        40,
                        10,
                        kwargs.get("eps5")
                    ),
                    Field(
                        "eps6",
                        float,
                        50,
                        10,
                        kwargs.get("eps6")
                    ),
                    Field(
                        "eps7",
                        float,
                        60,
                        10,
                        kwargs.get("eps7")
                    ),
                    Field(
                        "eps8",
                        float,
                        70,
                        10,
                        kwargs.get("eps8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "es1",
                        float,
                        0,
                        10,
                        kwargs.get("es1")
                    ),
                    Field(
                        "es2",
                        float,
                        10,
                        10,
                        kwargs.get("es2")
                    ),
                    Field(
                        "es3",
                        float,
                        20,
                        10,
                        kwargs.get("es3")
                    ),
                    Field(
                        "es4",
                        float,
                        30,
                        10,
                        kwargs.get("es4")
                    ),
                    Field(
                        "es5",
                        float,
                        40,
                        10,
                        kwargs.get("es5")
                    ),
                    Field(
                        "es6",
                        float,
                        50,
                        10,
                        kwargs.get("es6")
                    ),
                    Field(
                        "es7",
                        float,
                        60,
                        10,
                        kwargs.get("es7")
                    ),
                    Field(
                        "es8",
                        float,
                        70,
                        10,
                        kwargs.get("es8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lctsrf",
                        int,
                        0,
                        10,
                        kwargs.get("lctsrf", 0)
                    ),
                    Field(
                        "eps0",
                        float,
                        10,
                        10,
                        kwargs.get("eps0", 0)
                    ),
                    Field(
                        "triax",
                        float,
                        20,
                        10,
                        kwargs.get("triax", 0)
                    ),
                    Field(
                        "ips",
                        int,
                        30,
                        10,
                        kwargs.get("ips", 0)
                    ),
                    Field(
                        "lcemod",
                        int,
                        40,
                        10,
                        kwargs.get("lcemod", 0)
                    ),
                    Field(
                        "beta",
                        float,
                        50,
                        10,
                        kwargs.get("beta", 0.0)
                    ),
                    Field(
                        "rfiltf",
                        float,
                        60,
                        10,
                        kwargs.get("rfiltf", 0.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatModifiedPiecewiseLinearPlasticityRate.option_specs[0],
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
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[0].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        self._cards[0].set_value("sigy", value)

    @property
    def etan(self) -> typing.Optional[float]:
        """Get or set the Tangent modulus, ignored if (LCSS.GT.0) is defined.
        """ # nopep8
        return self._cards[0].get_value("etan")

    @etan.setter
    def etan(self, value: float) -> None:
        self._cards[0].set_value("etan", value)

    @property
    def fail(self) -> float:
        """Get or set the Failure flag:
        LT.0.0: User defined failure subroutine is called to determine failure
        EQ.0.0: Failure is not considered. Recommended if failure is not of interest.
        GT.0.0: Plastic strain to failure. When the plastic strain reaches this value, the element is deleted from the calculation.
        """ # nopep8
        return self._cards[0].get_value("fail")

    @fail.setter
    def fail(self, value: float) -> None:
        self._cards[0].set_value("fail", value)

    @property
    def tdel(self) -> typing.Optional[float]:
        """Get or set the Minimum time step size for automatic element deletion.
        """ # nopep8
        return self._cards[0].get_value("tdel")

    @tdel.setter
    def tdel(self, value: float) -> None:
        self._cards[0].set_value("tdel", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Strain rate parameter, C, see formula in keyword manual page 98 (volume two).
        """ # nopep8
        return self._cards[1].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        self._cards[1].set_value("c", value)

    @property
    def p(self) -> typing.Optional[float]:
        """Get or set the Strain rate parameter, P, see formula in keyword manual page 98 (volume two).
        """ # nopep8
        return self._cards[1].get_value("p")

    @p.setter
    def p(self, value: float) -> None:
        self._cards[1].set_value("p", value)

    @property
    def lcss(self) -> int:
        """Get or set the Load curve ID or Table ID.
        Load curve ID defining effective stress versus effective plastic strain. If defined EPS1-EPS8 and ES1-ES8 are ignored.
        The table ID defines for each strain rate value a load curve ID giving the stress versus effective plastic strain for that rate. The stress versus effective plastic strain curve for the lowest value of strain rate is used if the strain rate falls below the minmimum value. Likewise, the stress versus effective plastic strain curve for the highest value of strain rate is used if the strain rate exceeds the maximum value. If defined C, P,curve ID, LCSR, EPS1-EPS8 and ES1-ES8 are ignored.
        """ # nopep8
        return self._cards[1].get_value("lcss")

    @lcss.setter
    def lcss(self, value: int) -> None:
        self._cards[1].set_value("lcss", value)

    @property
    def lcsr(self) -> int:
        """Get or set the Load curve ID defining strain rate scaling effect on yield stress.
        """ # nopep8
        return self._cards[1].get_value("lcsr")

    @lcsr.setter
    def lcsr(self, value: int) -> None:
        self._cards[1].set_value("lcsr", value)

    @property
    def vp(self) -> float:
        """Get or set the Formulation for rate effects (currently not used with this model).
        """ # nopep8
        return self._cards[1].get_value("vp")

    @vp.setter
    def vp(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""vp must be one of {0.0,1.0}""")
        self._cards[1].set_value("vp", value)

    @property
    def epsthin(self) -> typing.Optional[float]:
        """Get or set the Thinning plastic strain at failure. This number should be given as positive number
        """ # nopep8
        return self._cards[1].get_value("epsthin")

    @epsthin.setter
    def epsthin(self, value: float) -> None:
        self._cards[1].set_value("epsthin", value)

    @property
    def epsmaj(self) -> typing.Optional[float]:
        """Get or set the Major in plane strain at failure.
        """ # nopep8
        return self._cards[1].get_value("epsmaj")

    @epsmaj.setter
    def epsmaj(self, value: float) -> None:
        self._cards[1].set_value("epsmaj", value)

    @property
    def numint(self) -> float:
        """Get or set the No. of through thickness integration points which must fail before the element is deleted.(if zero, all points must fail)
        """ # nopep8
        return self._cards[1].get_value("numint")

    @numint.setter
    def numint(self, value: float) -> None:
        self._cards[1].set_value("numint", value)

    @property
    def eps1(self) -> typing.Optional[float]:
        """Get or set the First effective plastic strain value (optional if SIGY is defined). At least 2 points should be defined. The first point must be zero corresponding to the initial yield stress.
        WARNING: If the first point is nonzero the yield stress is extrapolated to determine the initial yield. If this option is used SIGY and ETAN are ignored and may be input as zero.
        """ # nopep8
        return self._cards[2].get_value("eps1")

    @eps1.setter
    def eps1(self, value: float) -> None:
        self._cards[2].set_value("eps1", value)

    @property
    def eps2(self) -> typing.Optional[float]:
        """Get or set the Second effective plastic strain value (optional if SIGY is defined). At least 2 points should be defined. The first point must be zero corresponding to the initial yield stress.
        """ # nopep8
        return self._cards[2].get_value("eps2")

    @eps2.setter
    def eps2(self, value: float) -> None:
        self._cards[2].set_value("eps2", value)

    @property
    def eps3(self) -> typing.Optional[float]:
        """Get or set the Third effective plastic strain value (optional if SIGY is defined).
        """ # nopep8
        return self._cards[2].get_value("eps3")

    @eps3.setter
    def eps3(self, value: float) -> None:
        self._cards[2].set_value("eps3", value)

    @property
    def eps4(self) -> typing.Optional[float]:
        """Get or set the Fourth effective plastic strain value (optional if SIGY is defined).
        """ # nopep8
        return self._cards[2].get_value("eps4")

    @eps4.setter
    def eps4(self, value: float) -> None:
        self._cards[2].set_value("eps4", value)

    @property
    def eps5(self) -> typing.Optional[float]:
        """Get or set the Fifth effective plastic strain value (optional if SIGY is defined).
        """ # nopep8
        return self._cards[2].get_value("eps5")

    @eps5.setter
    def eps5(self, value: float) -> None:
        self._cards[2].set_value("eps5", value)

    @property
    def eps6(self) -> typing.Optional[float]:
        """Get or set the Sixth effective plastic strain value (optional if SIGY is defined).
        """ # nopep8
        return self._cards[2].get_value("eps6")

    @eps6.setter
    def eps6(self, value: float) -> None:
        self._cards[2].set_value("eps6", value)

    @property
    def eps7(self) -> typing.Optional[float]:
        """Get or set the Seventh effective plastic strain value (optiona l if SIGY is defined).
        """ # nopep8
        return self._cards[2].get_value("eps7")

    @eps7.setter
    def eps7(self, value: float) -> None:
        self._cards[2].set_value("eps7", value)

    @property
    def eps8(self) -> typing.Optional[float]:
        """Get or set the Eighth effective plastic strain value (optional if SIGY is defined).
        """ # nopep8
        return self._cards[2].get_value("eps8")

    @eps8.setter
    def eps8(self, value: float) -> None:
        self._cards[2].set_value("eps8", value)

    @property
    def es1(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress value to EPS1
        """ # nopep8
        return self._cards[3].get_value("es1")

    @es1.setter
    def es1(self, value: float) -> None:
        self._cards[3].set_value("es1", value)

    @property
    def es2(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress value to EPS2
        """ # nopep8
        return self._cards[3].get_value("es2")

    @es2.setter
    def es2(self, value: float) -> None:
        self._cards[3].set_value("es2", value)

    @property
    def es3(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress value to EPS3
        """ # nopep8
        return self._cards[3].get_value("es3")

    @es3.setter
    def es3(self, value: float) -> None:
        self._cards[3].set_value("es3", value)

    @property
    def es4(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress value to EPS4
        """ # nopep8
        return self._cards[3].get_value("es4")

    @es4.setter
    def es4(self, value: float) -> None:
        self._cards[3].set_value("es4", value)

    @property
    def es5(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress value to EPS5
        """ # nopep8
        return self._cards[3].get_value("es5")

    @es5.setter
    def es5(self, value: float) -> None:
        self._cards[3].set_value("es5", value)

    @property
    def es6(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress value to EPS6
        """ # nopep8
        return self._cards[3].get_value("es6")

    @es6.setter
    def es6(self, value: float) -> None:
        self._cards[3].set_value("es6", value)

    @property
    def es7(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress value to EPS7
        """ # nopep8
        return self._cards[3].get_value("es7")

    @es7.setter
    def es7(self, value: float) -> None:
        self._cards[3].set_value("es7", value)

    @property
    def es8(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress value to EPS8
        """ # nopep8
        return self._cards[3].get_value("es8")

    @es8.setter
    def es8(self, value: float) -> None:
        self._cards[3].set_value("es8", value)

    @property
    def lctsrf(self) -> int:
        """Get or set the Load curve that defines the thinning plastic strain at failure as a function of the plastic strain rate
        """ # nopep8
        return self._cards[4].get_value("lctsrf")

    @lctsrf.setter
    def lctsrf(self, value: int) -> None:
        self._cards[4].set_value("lctsrf", value)

    @property
    def eps0(self) -> float:
        """Get or set the EPS0 parameter for RTCL damage.
        EQ.0.0: (default) RTCL damage is inactive.
        GT.0.0: RTCL damage is active
        """ # nopep8
        return self._cards[4].get_value("eps0")

    @eps0.setter
    def eps0(self, value: float) -> None:
        self._cards[4].set_value("eps0", value)

    @property
    def triax(self) -> float:
        """Get or set the RTCL damage triaxiality limit.
        EQ.0.0: (default) No limit.
        GT.0.0: Damage does not accumulate when triaxiality exceeds TRIAX.
        """ # nopep8
        return self._cards[4].get_value("triax")

    @triax.setter
    def triax(self, value: float) -> None:
        self._cards[4].set_value("triax", value)

    @property
    def ips(self) -> int:
        """Get or set the Flag to add prestrain when checking for major strain failure (see EPSMAJ above on Card 2) for the PRESTRAIN keyword option:
        EQ.0:	No prestrain added(default)
        EQ.1 : Initial strain set with * INITIAL_STRAIN_SHELL will be used as a prestrain when checking for major strain failure(VP = 0 and shells only).
        """ # nopep8
        return self._cards[4].get_value("ips")

    @ips.setter
    def ips(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ips must be one of {0,1}""")
        self._cards[4].set_value("ips", value)

    @property
    def lcemod(self) -> int:
        """Get or set the Load curve ID defining Young’s modulus as function of effective strain rate. LCEMOD ≠ 0 activates viscoelasticity. See *MAT_187L for details. The parameters BETA and RFILTF have to be defined too.
        (If LCEMOD ≠ 0 is used, VP = 1 should be defined and failure options EPSTHIN, EPSMAJ,and RTCL are currently not available.)
        """ # nopep8
        return self._cards[4].get_value("lcemod")

    @lcemod.setter
    def lcemod(self, value: int) -> None:
        self._cards[4].set_value("lcemod", value)

    @property
    def beta(self) -> float:
        """Get or set the Decay constant in viscoelastic law. BETA has the unit [1/time]. If LCEMOD > 0 is used, a non-zero value for BETA is mandatory.
        """ # nopep8
        return self._cards[4].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[4].set_value("beta", value)

    @property
    def rfiltf(self) -> float:
        """Get or set the Smoothing factor on the effective strain rate (default is 0.95). The filtered strain rate is used for the viscoelasticity (LCEMOD > 0).
        """ # nopep8
        return self._cards[4].get_value("rfiltf")

    @rfiltf.setter
    def rfiltf(self, value: float) -> None:
        self._cards[4].set_value("rfiltf", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[5].cards[0].set_value("title", value)

