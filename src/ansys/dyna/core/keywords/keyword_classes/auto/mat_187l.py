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

class Mat187L(KeywordBase):
    """DYNA MAT_187L keyword"""

    keyword = "MAT"
    subkeyword = "187L"
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
                        "unused",
                        float,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        float,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "emod",
                        float,
                        40,
                        10,
                        kwargs.get("emod")
                    ),
                    Field(
                        "nue",
                        float,
                        50,
                        10,
                        kwargs.get("nue")
                    ),
                    Field(
                        "lcemod",
                        int,
                        60,
                        10,
                        kwargs.get("lcemod")
                    ),
                    Field(
                        "beta",
                        float,
                        70,
                        10,
                        kwargs.get("beta")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcid_t",
                        int,
                        0,
                        10,
                        kwargs.get("lcid_t")
                    ),
                    Field(
                        "lcid_c",
                        int,
                        10,
                        10,
                        kwargs.get("lcid_c", 0)
                    ),
                    Field(
                        "ctflg",
                        int,
                        20,
                        10,
                        kwargs.get("ctflg", 0)
                    ),
                    Field(
                        "rateop",
                        int,
                        30,
                        10,
                        kwargs.get("rateop", 0)
                    ),
                    Field(
                        "nuep",
                        float,
                        40,
                        10,
                        kwargs.get("nuep")
                    ),
                    Field(
                        "lcid-p",
                        int,
                        50,
                        10,
                        kwargs.get("lcid-p", 0)
                    ),
                    Field(
                        "rfiltf",
                        float,
                        60,
                        10,
                        kwargs.get("rfiltf", 0.95)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat187L.option_specs[0],
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
    def emod(self) -> typing.Optional[float]:
        """Get or set the Young's modulus
        """ # nopep8
        return self._cards[0].get_value("emod")

    @emod.setter
    def emod(self, value: float) -> None:
        self._cards[0].set_value("emod", value)

    @property
    def nue(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio
        """ # nopep8
        return self._cards[0].get_value("nue")

    @nue.setter
    def nue(self, value: float) -> None:
        self._cards[0].set_value("nue", value)

    @property
    def lcemod(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining Young’s modulus as function of effective strain rate.. LCEMOD ≠ 0 activates viscoelasticity, see remark 3. The parameters BETA and RFILTF have to be defined too
        """ # nopep8
        return self._cards[0].get_value("lcemod")

    @lcemod.setter
    def lcemod(self, value: int) -> None:
        self._cards[0].set_value("lcemod", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Decay constant in viscoelastic law. See remark 3. BETA has the unit[1/time].
        If LCEMOD > >0 is used, a non-zero value for BETA is mandatory
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[0].set_value("beta", value)

    @property
    def lcid_t(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID giving the yield stress as a function of plastic strain.
        These curves should be obtained from quasi-static and (optionally) dynamic uniaxial tensile tests.
        This input is mandatory.  If LCID-T is a table ID, the table values are effective strain rates
        , and a curve of yield stress versus plastic strain must be given for each of those strain rates.
        If the first value in the table is negative, LS-DYNA assumes that all the table values represent the natural logarithm of effective strain rate.
        When the highest effective strain rate is several orders of magnitude greater than the lowest strain rate,
        it is recommended that the natural log of strain rate be input in the table.
        """ # nopep8
        return self._cards[1].get_value("lcid_t")

    @lcid_t.setter
    def lcid_t(self, value: int) -> None:
        self._cards[1].set_value("lcid_t", value)

    @property
    def lcid_c(self) -> int:
        """Get or set the Optional load curve (or table) ID giving the yield stress as a function of plastic strain (and strain rate).
        This curve (or table) should be obtained from uniaxial compression tests.
        If LCID-C is defined as a curve and LCID-T given as a table, then the rate dependence from the tension table is adopted in compression as well
        """ # nopep8
        return self._cards[1].get_value("lcid_c")

    @lcid_c.setter
    def lcid_c(self, value: int) -> None:
        self._cards[1].set_value("lcid_c", value)

    @property
    def ctflg(self) -> int:
        """Get or set the Curve treatment flag (for LCID-T, LCID-C, and LCID-
        EQ.0:	Rediscretized curves are used(default).We recommend usingIt is recommended to use this option together with an appropriate value of LCINT for accurate resolution of the curves(see * DEFINE_CURVE and *CONTROL_SOLUTION).
        EQ.1 : Original curve values from the input are used.
        """ # nopep8
        return self._cards[1].get_value("ctflg")

    @ctflg.setter
    def ctflg(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ctflg must be one of {0,1}""")
        self._cards[1].set_value("ctflg", value)

    @property
    def rateop(self) -> int:
        """Get or set the Calculation of effective strain rate option:
        EQ.0:	Original method for calculating the effective strain rate
        EQ.2 : Improved method for calculating the effective strain rate.This method gives a slightly closer match to* MAT_SAMP - 1 and is thus recommended
        """ # nopep8
        return self._cards[1].get_value("rateop")

    @rateop.setter
    def rateop(self, value: int) -> None:
        if value not in [0, 2]:
            raise Exception("""rateop must be one of {0,2}""")
        self._cards[1].set_value("rateop", value)

    @property
    def nuep(self) -> typing.Optional[float]:
        """Get or set the Plastic Poisson’s ratio: an estimated ratio of transversal to longitudinal plastic rate of deformation under uniaxial loading should be given.
        """ # nopep8
        return self._cards[1].get_value("nuep")

    @nuep.setter
    def nuep(self, value: float) -> None:
        self._cards[1].set_value("nuep", value)

    @property
    def lcid_p(self) -> int:
        """Get or set the Load curve ID giving the plastic Poisson's ratio as a function of an equivalent plastic strain measure during uniaxial tensile and uniaxial compressive testing.The plastic strain measure on the abscissa is negative for compression and positive for tension.
        It is important to cover both tension and compression.  If LCID-P is given, NUEP is ignored
        """ # nopep8
        return self._cards[1].get_value("lcid-p")

    @lcid_p.setter
    def lcid_p(self, value: int) -> None:
        self._cards[1].set_value("lcid-p", value)

    @property
    def rfiltf(self) -> float:
        """Get or set the Smoothing factor on the effective strain rate.
        """ # nopep8
        return self._cards[1].get_value("rfiltf")

    @rfiltf.setter
    def rfiltf(self, value: float) -> None:
        self._cards[1].set_value("rfiltf", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

