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

class Mat166(KeywordBase):
    """DYNA MAT_166 keyword"""

    keyword = "MAT"
    subkeyword = "166"
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
                        "elaf",
                        int,
                        30,
                        10,
                        kwargs.get("elaf")
                    ),
                    Field(
                        "epflg",
                        int,
                        40,
                        10,
                        kwargs.get("epflg", 0)
                    ),
                    Field(
                        "cta",
                        float,
                        50,
                        10,
                        kwargs.get("cta")
                    ),
                    Field(
                        "ctb",
                        float,
                        60,
                        10,
                        kwargs.get("ctb")
                    ),
                    Field(
                        "ctt",
                        float,
                        70,
                        10,
                        kwargs.get("ctt")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "n1",
                        float,
                        0,
                        10,
                        kwargs.get("n1")
                    ),
                    Field(
                        "n2",
                        float,
                        10,
                        10,
                        kwargs.get("n2")
                    ),
                    Field(
                        "n3",
                        float,
                        20,
                        10,
                        kwargs.get("n3")
                    ),
                    Field(
                        "n4",
                        float,
                        30,
                        10,
                        kwargs.get("n4")
                    ),
                    Field(
                        "n5",
                        float,
                        40,
                        10,
                        kwargs.get("n5")
                    ),
                    Field(
                        "n6",
                        float,
                        50,
                        10,
                        kwargs.get("n6")
                    ),
                    Field(
                        "n7",
                        float,
                        60,
                        10,
                        kwargs.get("n7")
                    ),
                    Field(
                        "n8",
                        float,
                        70,
                        10,
                        kwargs.get("n8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcms1",
                        int,
                        0,
                        10,
                        kwargs.get("lcms1")
                    ),
                    Field(
                        "lcms2",
                        int,
                        10,
                        10,
                        kwargs.get("lcms2")
                    ),
                    Field(
                        "lcms3",
                        int,
                        20,
                        10,
                        kwargs.get("lcms3")
                    ),
                    Field(
                        "lcms4",
                        int,
                        30,
                        10,
                        kwargs.get("lcms4")
                    ),
                    Field(
                        "lcms5",
                        int,
                        40,
                        10,
                        kwargs.get("lcms5")
                    ),
                    Field(
                        "lcms6",
                        int,
                        50,
                        10,
                        kwargs.get("lcms6")
                    ),
                    Field(
                        "lcms7",
                        int,
                        60,
                        10,
                        kwargs.get("lcms7")
                    ),
                    Field(
                        "lcms8",
                        int,
                        70,
                        10,
                        kwargs.get("lcms8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcmt1",
                        int,
                        0,
                        10,
                        kwargs.get("lcmt1")
                    ),
                    Field(
                        "lcmt2",
                        int,
                        10,
                        10,
                        kwargs.get("lcmt2")
                    ),
                    Field(
                        "lcmt3",
                        int,
                        20,
                        10,
                        kwargs.get("lcmt3")
                    ),
                    Field(
                        "lcmt4",
                        int,
                        30,
                        10,
                        kwargs.get("lcmt4")
                    ),
                    Field(
                        "lcmt5",
                        int,
                        40,
                        10,
                        kwargs.get("lcmt5")
                    ),
                    Field(
                        "lcmt6",
                        int,
                        50,
                        10,
                        kwargs.get("lcmt6")
                    ),
                    Field(
                        "lcmt7",
                        int,
                        60,
                        10,
                        kwargs.get("lcmt7")
                    ),
                    Field(
                        "lcmt8",
                        int,
                        70,
                        10,
                        kwargs.get("lcmt8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lct1",
                        int,
                        0,
                        10,
                        kwargs.get("lct1")
                    ),
                    Field(
                        "lct2",
                        int,
                        10,
                        10,
                        kwargs.get("lct2")
                    ),
                    Field(
                        "lct3",
                        int,
                        20,
                        10,
                        kwargs.get("lct3")
                    ),
                    Field(
                        "lct4",
                        int,
                        30,
                        10,
                        kwargs.get("lct4")
                    ),
                    Field(
                        "lct5",
                        int,
                        40,
                        10,
                        kwargs.get("lct5")
                    ),
                    Field(
                        "lct6",
                        int,
                        50,
                        10,
                        kwargs.get("lct6")
                    ),
                    Field(
                        "lct7",
                        int,
                        60,
                        10,
                        kwargs.get("lct7")
                    ),
                    Field(
                        "lct8",
                        int,
                        70,
                        10,
                        kwargs.get("lct8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cfa",
                        float,
                        0,
                        10,
                        kwargs.get("cfa", 1.0)
                    ),
                    Field(
                        "cfb",
                        float,
                        10,
                        10,
                        kwargs.get("cfb", 1.0)
                    ),
                    Field(
                        "cft",
                        float,
                        20,
                        10,
                        kwargs.get("cft", 1.0)
                    ),
                    Field(
                        "hrule",
                        float,
                        30,
                        10,
                        kwargs.get("hrule")
                    ),
                    Field(
                        "reps",
                        float,
                        40,
                        10,
                        kwargs.get("reps", 1.0E+20)
                    ),
                    Field(
                        "rbeta",
                        float,
                        50,
                        10,
                        kwargs.get("rbeta", 1.0E+20)
                    ),
                    Field(
                        "rcapay",
                        float,
                        60,
                        10,
                        kwargs.get("rcapay", 1.0E+20)
                    ),
                    Field(
                        "rcapaz",
                        float,
                        70,
                        10,
                        kwargs.get("rcapaz", 1.0E+20)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat166.option_specs[0],
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
        """Get or set the Young's modulus.  This variable controls the time step size and must be chosen carefully.  Increasing the value of E will decrease the time step size.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        self._cards[0].set_value("e", value)

    @property
    def elaf(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for the axial force-strain curve
        """ # nopep8
        return self._cards[0].get_value("elaf")

    @elaf.setter
    def elaf(self, value: int) -> None:
        self._cards[0].set_value("elaf", value)

    @property
    def epflg(self) -> int:
        """Get or set the Function flag
        EQ.0.0: nonlinear elastic analysis
        EQ.1.0: multi-linear plastic analysis.
        """ # nopep8
        return self._cards[0].get_value("epflg")

    @epflg.setter
    def epflg(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""epflg must be one of {0,1}""")
        self._cards[0].set_value("epflg", value)

    @property
    def cta(self) -> typing.Optional[float]:
        """Get or set the Type of axial force-strain, moment-curvature, and torque-twist rate curves
        EQ.0.0: curve is symmetric
        EQ.1.0: curve is asymmetric
        For symmetric curves, all data point must be in the first quadrant and at least three data points need to be given, starting from the origin, ensued by the yield point.
        For asymmetric curves, at least five data points are needed and exactly one point must be at the origin.  The two points on both sides of the origin record the positive and negative yield points.
        The last data point(s) has no physical meaning: it serves only as a control point for inter or extrapolation.
        The curves are input by the user and treated in LS-DYNA as a linearly piecewise function.  The curves must be monotonically increasing, while the slopes must be monotonically decreasing.
        """ # nopep8
        return self._cards[0].get_value("cta")

    @cta.setter
    def cta(self, value: float) -> None:
        self._cards[0].set_value("cta", value)

    @property
    def ctb(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("ctb")

    @ctb.setter
    def ctb(self, value: float) -> None:
        self._cards[0].set_value("ctb", value)

    @property
    def ctt(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("ctt")

    @ctt.setter
    def ctt(self, value: float) -> None:
        self._cards[0].set_value("ctt", value)

    @property
    def n1(self) -> typing.Optional[float]:
        """Get or set the Axial forces at which moment-curvature curves are given	The axial forces must be ordered monotonically increasing.  At least two axial forces must be defined if the curves are symmetric.  At least three axial forces must be defined if the curves are asymmetric.
        """ # nopep8
        return self._cards[1].get_value("n1")

    @n1.setter
    def n1(self, value: float) -> None:
        self._cards[1].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[float]:
        """Get or set the Axial forces at which moment-curvature curves are given	The axial forces must be ordered monotonically increasing.  At least two axial forces must be defined if the curves are symmetric.  At least three axial forces must be defined if the curves are asymmetric.
        """ # nopep8
        return self._cards[1].get_value("n2")

    @n2.setter
    def n2(self, value: float) -> None:
        self._cards[1].set_value("n2", value)

    @property
    def n3(self) -> typing.Optional[float]:
        """Get or set the Axial forces at which moment-curvature curves are given	The axial forces must be ordered monotonically increasing.  At least two axial forces must be defined if the curves are symmetric.  At least three axial forces must be defined if the curves are asymmetric.
        """ # nopep8
        return self._cards[1].get_value("n3")

    @n3.setter
    def n3(self, value: float) -> None:
        self._cards[1].set_value("n3", value)

    @property
    def n4(self) -> typing.Optional[float]:
        """Get or set the Axial forces at which moment-curvature curves are given	The axial forces must be ordered monotonically increasing.  At least two axial forces must be defined if the curves are symmetric.  At least three axial forces must be defined if the curves are asymmetric..
        """ # nopep8
        return self._cards[1].get_value("n4")

    @n4.setter
    def n4(self, value: float) -> None:
        self._cards[1].set_value("n4", value)

    @property
    def n5(self) -> typing.Optional[float]:
        """Get or set the Axial forces at which moment-curvature curves are given	The axial forces must be ordered monotonically increasing.  At least two axial forces must be defined if the curves are symmetric.  At least three axial forces must be defined if the curves are asymmetric.
        """ # nopep8
        return self._cards[1].get_value("n5")

    @n5.setter
    def n5(self, value: float) -> None:
        self._cards[1].set_value("n5", value)

    @property
    def n6(self) -> typing.Optional[float]:
        """Get or set the Axial forces at which moment-curvature curves are given	The axial forces must be ordered monotonically increasing.  At least two axial forces must be defined if the curves are symmetric.  At least three axial forces must be defined if the curves are asymmetric.
        """ # nopep8
        return self._cards[1].get_value("n6")

    @n6.setter
    def n6(self, value: float) -> None:
        self._cards[1].set_value("n6", value)

    @property
    def n7(self) -> typing.Optional[float]:
        """Get or set the Axial forces at which moment-curvature curves are given	The axial forces must be ordered monotonically increasing.  At least two axial forces must be defined if the curves are symmetric.  At least three axial forces must be defined if the curves are asymmetric.
        """ # nopep8
        return self._cards[1].get_value("n7")

    @n7.setter
    def n7(self, value: float) -> None:
        self._cards[1].set_value("n7", value)

    @property
    def n8(self) -> typing.Optional[float]:
        """Get or set the Axial forces at which moment-curvature curves are given	The axial forces must be ordered monotonically increasing.  At least two axial forces must be defined if the curves are symmetric.  At least three axial forces must be defined if the curves are asymmetric.
        """ # nopep8
        return self._cards[1].get_value("n8")

    @n8.setter
    def n8(self, value: float) -> None:
        self._cards[1].set_value("n8", value)

    @property
    def lcms1(self) -> typing.Optional[int]:
        """Get or set the Load curve IDs for the moment-curvature curves about axis S under corresponding axial forces..
        """ # nopep8
        return self._cards[2].get_value("lcms1")

    @lcms1.setter
    def lcms1(self, value: int) -> None:
        self._cards[2].set_value("lcms1", value)

    @property
    def lcms2(self) -> typing.Optional[int]:
        """Get or set the Load curve IDs for the moment-curvature curves about axis S under corresponding axial forces.
        """ # nopep8
        return self._cards[2].get_value("lcms2")

    @lcms2.setter
    def lcms2(self, value: int) -> None:
        self._cards[2].set_value("lcms2", value)

    @property
    def lcms3(self) -> typing.Optional[int]:
        """Get or set the Load curve IDs for the moment-curvature curves about axis S under corresponding axial forces.
        """ # nopep8
        return self._cards[2].get_value("lcms3")

    @lcms3.setter
    def lcms3(self, value: int) -> None:
        self._cards[2].set_value("lcms3", value)

    @property
    def lcms4(self) -> typing.Optional[int]:
        """Get or set the Load curve IDs for the moment-curvature curves about axis S under corresponding axial forces..
        """ # nopep8
        return self._cards[2].get_value("lcms4")

    @lcms4.setter
    def lcms4(self, value: int) -> None:
        self._cards[2].set_value("lcms4", value)

    @property
    def lcms5(self) -> typing.Optional[int]:
        """Get or set the Load curve IDs for the moment-curvature curves about axis S under corresponding axial forces.
        """ # nopep8
        return self._cards[2].get_value("lcms5")

    @lcms5.setter
    def lcms5(self, value: int) -> None:
        self._cards[2].set_value("lcms5", value)

    @property
    def lcms6(self) -> typing.Optional[int]:
        """Get or set the Load curve IDs for the moment-curvature curves about axis S under corresponding axial forces.
        """ # nopep8
        return self._cards[2].get_value("lcms6")

    @lcms6.setter
    def lcms6(self, value: int) -> None:
        self._cards[2].set_value("lcms6", value)

    @property
    def lcms7(self) -> typing.Optional[int]:
        """Get or set the Load curve IDs for the moment-curvature curves about axis S under corresponding axial forces.
        """ # nopep8
        return self._cards[2].get_value("lcms7")

    @lcms7.setter
    def lcms7(self, value: int) -> None:
        self._cards[2].set_value("lcms7", value)

    @property
    def lcms8(self) -> typing.Optional[int]:
        """Get or set the Load curve IDs for the moment-curvature curves about axis S under corresponding axial forces.
        """ # nopep8
        return self._cards[2].get_value("lcms8")

    @lcms8.setter
    def lcms8(self, value: int) -> None:
        self._cards[2].set_value("lcms8", value)

    @property
    def lcmt1(self) -> typing.Optional[int]:
        """Get or set the Load curve IDs for the moment-curvature curves about axis T under corresponding axial forces..
        """ # nopep8
        return self._cards[3].get_value("lcmt1")

    @lcmt1.setter
    def lcmt1(self, value: int) -> None:
        self._cards[3].set_value("lcmt1", value)

    @property
    def lcmt2(self) -> typing.Optional[int]:
        """Get or set the Load curve IDs for the moment-curvature curves about axis T under corresponding axial forces.
        """ # nopep8
        return self._cards[3].get_value("lcmt2")

    @lcmt2.setter
    def lcmt2(self, value: int) -> None:
        self._cards[3].set_value("lcmt2", value)

    @property
    def lcmt3(self) -> typing.Optional[int]:
        """Get or set the Load curve IDs for the moment-curvature curves about axis T under corresponding axial forces.
        """ # nopep8
        return self._cards[3].get_value("lcmt3")

    @lcmt3.setter
    def lcmt3(self, value: int) -> None:
        self._cards[3].set_value("lcmt3", value)

    @property
    def lcmt4(self) -> typing.Optional[int]:
        """Get or set the Load curve IDs for the moment-curvature curves about axis T under corresponding axial forces..
        """ # nopep8
        return self._cards[3].get_value("lcmt4")

    @lcmt4.setter
    def lcmt4(self, value: int) -> None:
        self._cards[3].set_value("lcmt4", value)

    @property
    def lcmt5(self) -> typing.Optional[int]:
        """Get or set the Load curve IDs for the moment-curvature curves about axis T under corresponding axial forces.
        """ # nopep8
        return self._cards[3].get_value("lcmt5")

    @lcmt5.setter
    def lcmt5(self, value: int) -> None:
        self._cards[3].set_value("lcmt5", value)

    @property
    def lcmt6(self) -> typing.Optional[int]:
        """Get or set the Load curve IDs for the moment-curvature curves about axis T under corresponding axial forces.
        """ # nopep8
        return self._cards[3].get_value("lcmt6")

    @lcmt6.setter
    def lcmt6(self, value: int) -> None:
        self._cards[3].set_value("lcmt6", value)

    @property
    def lcmt7(self) -> typing.Optional[int]:
        """Get or set the Load curve IDs for the moment-curvature curves about axis T under corresponding axial forces.
        """ # nopep8
        return self._cards[3].get_value("lcmt7")

    @lcmt7.setter
    def lcmt7(self, value: int) -> None:
        self._cards[3].set_value("lcmt7", value)

    @property
    def lcmt8(self) -> typing.Optional[int]:
        """Get or set the Load curve IDs for the moment-curvature curves about axis T under corresponding axial forces.
        """ # nopep8
        return self._cards[3].get_value("lcmt8")

    @lcmt8.setter
    def lcmt8(self, value: int) -> None:
        self._cards[3].set_value("lcmt8", value)

    @property
    def lct1(self) -> typing.Optional[int]:
        """Get or set the Load curve IDs for the torque-twist rate curves under corresponding axial forces..
        """ # nopep8
        return self._cards[4].get_value("lct1")

    @lct1.setter
    def lct1(self, value: int) -> None:
        self._cards[4].set_value("lct1", value)

    @property
    def lct2(self) -> typing.Optional[int]:
        """Get or set the Load curve IDs for the torque-twist rate curves under corresponding axial forces.
        """ # nopep8
        return self._cards[4].get_value("lct2")

    @lct2.setter
    def lct2(self, value: int) -> None:
        self._cards[4].set_value("lct2", value)

    @property
    def lct3(self) -> typing.Optional[int]:
        """Get or set the Load curve IDs for the torque-twist rate curves under corresponding axial forces.
        """ # nopep8
        return self._cards[4].get_value("lct3")

    @lct3.setter
    def lct3(self, value: int) -> None:
        self._cards[4].set_value("lct3", value)

    @property
    def lct4(self) -> typing.Optional[int]:
        """Get or set the Load curve IDs for the torque-twist rate curves under corresponding axial forces..
        """ # nopep8
        return self._cards[4].get_value("lct4")

    @lct4.setter
    def lct4(self, value: int) -> None:
        self._cards[4].set_value("lct4", value)

    @property
    def lct5(self) -> typing.Optional[int]:
        """Get or set the Load curve IDs for the torque-twist rate curves under corresponding axial forces.
        """ # nopep8
        return self._cards[4].get_value("lct5")

    @lct5.setter
    def lct5(self, value: int) -> None:
        self._cards[4].set_value("lct5", value)

    @property
    def lct6(self) -> typing.Optional[int]:
        """Get or set the Load curve IDs for the torque-twist rate curves under corresponding axial forces.
        """ # nopep8
        return self._cards[4].get_value("lct6")

    @lct6.setter
    def lct6(self, value: int) -> None:
        self._cards[4].set_value("lct6", value)

    @property
    def lct7(self) -> typing.Optional[int]:
        """Get or set the Load curve IDs for the torque-twist rate curves under corresponding axial forces.
        """ # nopep8
        return self._cards[4].get_value("lct7")

    @lct7.setter
    def lct7(self, value: int) -> None:
        self._cards[4].set_value("lct7", value)

    @property
    def lct8(self) -> typing.Optional[int]:
        """Get or set the Load curve IDs for the torque-twist rate curves under corresponding axial forces.
        """ # nopep8
        return self._cards[4].get_value("lct8")

    @lct8.setter
    def lct8(self, value: int) -> None:
        self._cards[4].set_value("lct8", value)

    @property
    def cfa(self) -> float:
        """Get or set the For multi-linear plastic analysis only.	Ratio of axial, bending and torsional elastic rigidities to their initial values, no less than 1.0 in value.
        """ # nopep8
        return self._cards[5].get_value("cfa")

    @cfa.setter
    def cfa(self, value: float) -> None:
        self._cards[5].set_value("cfa", value)

    @property
    def cfb(self) -> float:
        """Get or set the For multi-linear plastic analysis only.	Ratio of axial, bending and torsional elastic rigidities to their initial values, no less than 1.0 in value
        """ # nopep8
        return self._cards[5].get_value("cfb")

    @cfb.setter
    def cfb(self, value: float) -> None:
        self._cards[5].set_value("cfb", value)

    @property
    def cft(self) -> float:
        """Get or set the For multi-linear plastic analysis only.	Ratio of axial, bending and torsional elastic rigidities to their initial values, no less than 1.0 in value
        """ # nopep8
        return self._cards[5].get_value("cft")

    @cft.setter
    def cft(self, value: float) -> None:
        self._cards[5].set_value("cft", value)

    @property
    def hrule(self) -> typing.Optional[float]:
        """Get or set the Hardening rule, for multi-linear plastic analysis only.
        EQ.0.0: isotropic hardening
        EQ.1.0: kinematic hardening
        In between: mixed hardening.
        """ # nopep8
        return self._cards[5].get_value("hrule")

    @hrule.setter
    def hrule(self, value: float) -> None:
        self._cards[5].set_value("hrule", value)

    @property
    def reps(self) -> float:
        """Get or set the Rupture effective plastic axial strain
        """ # nopep8
        return self._cards[5].get_value("reps")

    @reps.setter
    def reps(self, value: float) -> None:
        self._cards[5].set_value("reps", value)

    @property
    def rbeta(self) -> float:
        """Get or set the Rupture effective plastic twist rate
        """ # nopep8
        return self._cards[5].get_value("rbeta")

    @rbeta.setter
    def rbeta(self, value: float) -> None:
        self._cards[5].set_value("rbeta", value)

    @property
    def rcapay(self) -> float:
        """Get or set the Rupture effective plastic curvature about axis S
        """ # nopep8
        return self._cards[5].get_value("rcapay")

    @rcapay.setter
    def rcapay(self, value: float) -> None:
        self._cards[5].set_value("rcapay", value)

    @property
    def rcapaz(self) -> float:
        """Get or set the Rupture effective plastic curvature about axis T
        """ # nopep8
        return self._cards[5].get_value("rcapaz")

    @rcapaz.setter
    def rcapaz(self, value: float) -> None:
        self._cards[5].set_value("rcapaz", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[6].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[6].cards[0].set_value("title", value)

