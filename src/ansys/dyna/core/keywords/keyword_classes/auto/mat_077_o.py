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
from ansys.dyna.core.lib.duplicate_card import DuplicateCard
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class Mat077O(KeywordBase):
    """DYNA MAT_077_O keyword"""

    keyword = "MAT"
    subkeyword = "077_O"
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
                        "pr",
                        float,
                        20,
                        10,
                        kwargs.get("pr")
                    ),
                    Field(
                        "n",
                        int,
                        30,
                        10,
                        kwargs.get("n", 0)
                    ),
                    Field(
                        "nv",
                        int,
                        40,
                        10,
                        kwargs.get("nv", 6)
                    ),
                    Field(
                        "g",
                        float,
                        50,
                        10,
                        kwargs.get("g")
                    ),
                    Field(
                        "sigf",
                        float,
                        60,
                        10,
                        kwargs.get("sigf")
                    ),
                    Field(
                        "ref",
                        float,
                        70,
                        10,
                        kwargs.get("ref", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "tbhys",
                        float,
                        0,
                        10,
                        kwargs.get("tbhys")
                    ),
                ],
                lambda: self.pr and self.pr < 0,
            ),
            Card(
                [
                    Field(
                        "sgl",
                        float,
                        0,
                        10,
                        kwargs.get("sgl")
                    ),
                    Field(
                        "sw",
                        float,
                        10,
                        10,
                        kwargs.get("sw")
                    ),
                    Field(
                        "st",
                        float,
                        20,
                        10,
                        kwargs.get("st")
                    ),
                    Field(
                        "lcid1",
                        int,
                        30,
                        10,
                        kwargs.get("lcid1")
                    ),
                    Field(
                        "data",
                        float,
                        40,
                        10,
                        kwargs.get("data", 1.0)
                    ),
                    Field(
                        "lcid2",
                        int,
                        50,
                        10,
                        kwargs.get("lcid2")
                    ),
                    Field(
                        "bstart",
                        float,
                        60,
                        10,
                        kwargs.get("bstart")
                    ),
                    Field(
                        "tramp",
                        float,
                        70,
                        10,
                        kwargs.get("tramp")
                    ),
                ],
                lambda: self.n > 0,
            ),
            Card(
                [
                    Field(
                        "mu1",
                        float,
                        0,
                        10,
                        kwargs.get("mu1")
                    ),
                    Field(
                        "mu2",
                        float,
                        10,
                        10,
                        kwargs.get("mu2")
                    ),
                    Field(
                        "mu3",
                        float,
                        20,
                        10,
                        kwargs.get("mu3")
                    ),
                    Field(
                        "mu4",
                        float,
                        30,
                        10,
                        kwargs.get("mu4")
                    ),
                    Field(
                        "mu5",
                        float,
                        40,
                        10,
                        kwargs.get("mu5")
                    ),
                    Field(
                        "mu6",
                        float,
                        50,
                        10,
                        kwargs.get("mu6")
                    ),
                    Field(
                        "mu7",
                        float,
                        60,
                        10,
                        kwargs.get("mu7")
                    ),
                    Field(
                        "mu8",
                        float,
                        70,
                        10,
                        kwargs.get("mu8")
                    ),
                ],
                lambda: self.n == 0 or self.n == -1,
            ),
            Card(
                [
                    Field(
                        "alpha1",
                        float,
                        0,
                        10,
                        kwargs.get("alpha1")
                    ),
                    Field(
                        "alpha2",
                        float,
                        10,
                        10,
                        kwargs.get("alpha2")
                    ),
                    Field(
                        "alpha3",
                        float,
                        20,
                        10,
                        kwargs.get("alpha3")
                    ),
                    Field(
                        "alpha4",
                        float,
                        30,
                        10,
                        kwargs.get("alpha4")
                    ),
                    Field(
                        "alpha5",
                        float,
                        40,
                        10,
                        kwargs.get("alpha5")
                    ),
                    Field(
                        "alpha6",
                        float,
                        50,
                        10,
                        kwargs.get("alpha6")
                    ),
                    Field(
                        "alpha7",
                        float,
                        60,
                        10,
                        kwargs.get("alpha7")
                    ),
                    Field(
                        "alpha8",
                        float,
                        70,
                        10,
                        kwargs.get("alpha8")
                    ),
                ],
                lambda: self.n == 0 or self.n == -1,
            ),
            DuplicateCard(
                [
                    Field("gi", float, 0, 10),
                    Field("betai", float, 10, 10),
                    Field("vflag", int, 20, 10),
                ],
                None,
                data = kwargs.get("constants")),
            OptionCardSet(
                option_spec = Mat077O.option_specs[0],
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
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio ( => 0.49 is recommended, smaller values may not work and should not be used).
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def n(self) -> int:
        """Get or set the Order of fit to the Ogden model, (currently <9, 2 generally works okay).  The constants generated during the fit are printed in the output file and can be directly input in future runs, thereby, saving the cost of performing the nonlinear fit.
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: int) -> None:
        if value not in [-1, 0, 1, 2, 3, 4, 5, 6, 7, 8]:
            raise Exception("""n must be one of {-1,0,1,2,3,4,5,6,7,8}""")
        self._cards[0].set_value("n", value)

    @property
    def nv(self) -> int:
        """Get or set the Number of terms in fit. Currently, the maximum number is set to 6. Values less than 6, possibly 3-5 are recommended, since each term used adds significantly to the cost. Caution should be exercised when taking the results from the fit. Preferably, all generated coefficients should be positive. Negative values may lead to unstable results. Once a satisfactory fit has been achieved it is recommended that the coefficients which are written into the output file be input in future runs.
        Default is set to 6.
        """ # nopep8
        return self._cards[0].get_value("nv")

    @nv.setter
    def nv(self, value: int) -> None:
        self._cards[0].set_value("nv", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the Shear modulus for frequency independent damping. Frequency independent damping is based of a spring and slider in series. The critical stress for the slider mechanism is SIGF defined below. For the best results, the value of G should be 250-1000 times greater than SIGF.
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        self._cards[0].set_value("g", value)

    @property
    def sigf(self) -> typing.Optional[float]:
        """Get or set the Limit stress for frequency independent, frictional, damping.
        """ # nopep8
        return self._cards[0].get_value("sigf")

    @sigf.setter
    def sigf(self, value: float) -> None:
        self._cards[0].set_value("sigf", value)

    @property
    def ref(self) -> float:
        """Get or set the Use reference geometry to initialize the stress tensor. The reference
        geometry is defined by the keyword: *INITIAL_FOAM_REFERENCE_GEOMETRY (see there for more details).
        EQ.0.0: off
        EQ.1.0: on.
        """ # nopep8
        return self._cards[0].get_value("ref")

    @ref.setter
    def ref(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""ref must be one of {0.0,1.0}""")
        self._cards[0].set_value("ref", value)

    @property
    def tbhys(self) -> typing.Optional[float]:
        """Get or set the Table ID for hysteresis.
        """ # nopep8
        return self._cards[1].get_value("tbhys")

    @tbhys.setter
    def tbhys(self, value: float) -> None:
        self._cards[1].set_value("tbhys", value)

    @property
    def sgl(self) -> typing.Optional[float]:
        """Get or set the Specimen gauge length.
        """ # nopep8
        return self._cards[2].get_value("sgl")

    @sgl.setter
    def sgl(self, value: float) -> None:
        self._cards[2].set_value("sgl", value)

    @property
    def sw(self) -> typing.Optional[float]:
        """Get or set the Specimen width.
        """ # nopep8
        return self._cards[2].get_value("sw")

    @sw.setter
    def sw(self, value: float) -> None:
        self._cards[2].set_value("sw", value)

    @property
    def st(self) -> typing.Optional[float]:
        """Get or set the Specimen thickness.
        """ # nopep8
        return self._cards[2].get_value("st")

    @st.setter
    def st(self, value: float) -> None:
        self._cards[2].set_value("st", value)

    @property
    def lcid1(self) -> typing.Optional[int]:
        """Get or set the Load curve ID giving the force versus actual change in the gauge length.
        """ # nopep8
        return self._cards[2].get_value("lcid1")

    @lcid1.setter
    def lcid1(self, value: int) -> None:
        self._cards[2].set_value("lcid1", value)

    @property
    def data(self) -> float:
        """Get or set the Type of experimental data:
        EQ.1.0: uniaxial data (default),
        EQ.2.0: biaxial data.
        EQ.3.0: pure shear data
        """ # nopep8
        return self._cards[2].get_value("data")

    @data.setter
    def data(self, value: float) -> None:
        if value not in [1.0, 2.0, 3.0]:
            raise Exception("""data must be one of {1.0,2.0,3.0}""")
        self._cards[2].set_value("data", value)

    @property
    def lcid2(self) -> typing.Optional[int]:
        """Get or set the Load curve ID of relaxation curve If constants beta-i are determined via a least squares fit.
        This model ignores the constant stress.
        """ # nopep8
        return self._cards[2].get_value("lcid2")

    @lcid2.setter
    def lcid2(self, value: int) -> None:
        self._cards[2].set_value("lcid2", value)

    @property
    def bstart(self) -> typing.Optional[float]:
        """Get or set the In the fit, beta-1 is set to zero, beta-2 is set to BSTART, beta-3 is 10 times beta-2, beta-4 is 100 times greater than beta-3, and so on. If zero, BSTART is determined by an iterative trial and error scheme.
        """ # nopep8
        return self._cards[2].get_value("bstart")

    @bstart.setter
    def bstart(self, value: float) -> None:
        self._cards[2].set_value("bstart", value)

    @property
    def tramp(self) -> typing.Optional[float]:
        """Get or set the Optional ramp time for loading. If N=0, the constants MUi and ALPHAi have to be defined:
        """ # nopep8
        return self._cards[2].get_value("tramp")

    @tramp.setter
    def tramp(self, value: float) -> None:
        self._cards[2].set_value("tramp", value)

    @property
    def mu1(self) -> typing.Optional[float]:
        """Get or set the mu-1, first shear modulus.
        """ # nopep8
        return self._cards[3].get_value("mu1")

    @mu1.setter
    def mu1(self, value: float) -> None:
        self._cards[3].set_value("mu1", value)

    @property
    def mu2(self) -> typing.Optional[float]:
        """Get or set the mu-2, second shear modulus.
        """ # nopep8
        return self._cards[3].get_value("mu2")

    @mu2.setter
    def mu2(self, value: float) -> None:
        self._cards[3].set_value("mu2", value)

    @property
    def mu3(self) -> typing.Optional[float]:
        """Get or set the mu-3, third shear modulus.
        """ # nopep8
        return self._cards[3].get_value("mu3")

    @mu3.setter
    def mu3(self, value: float) -> None:
        self._cards[3].set_value("mu3", value)

    @property
    def mu4(self) -> typing.Optional[float]:
        """Get or set the mu-4, fourth shear modulus.
        """ # nopep8
        return self._cards[3].get_value("mu4")

    @mu4.setter
    def mu4(self, value: float) -> None:
        self._cards[3].set_value("mu4", value)

    @property
    def mu5(self) -> typing.Optional[float]:
        """Get or set the mu-5, fifth shear modulus.
        """ # nopep8
        return self._cards[3].get_value("mu5")

    @mu5.setter
    def mu5(self, value: float) -> None:
        self._cards[3].set_value("mu5", value)

    @property
    def mu6(self) -> typing.Optional[float]:
        """Get or set the mu-6, sixth shear modulus.
        """ # nopep8
        return self._cards[3].get_value("mu6")

    @mu6.setter
    def mu6(self, value: float) -> None:
        self._cards[3].set_value("mu6", value)

    @property
    def mu7(self) -> typing.Optional[float]:
        """Get or set the mu-7, seventh shear modulus.
        """ # nopep8
        return self._cards[3].get_value("mu7")

    @mu7.setter
    def mu7(self, value: float) -> None:
        self._cards[3].set_value("mu7", value)

    @property
    def mu8(self) -> typing.Optional[float]:
        """Get or set the mu-8, eighth shear modulus.
        """ # nopep8
        return self._cards[3].get_value("mu8")

    @mu8.setter
    def mu8(self, value: float) -> None:
        self._cards[3].set_value("mu8", value)

    @property
    def alpha1(self) -> typing.Optional[float]:
        """Get or set the alpha-1, first exponent.
        """ # nopep8
        return self._cards[4].get_value("alpha1")

    @alpha1.setter
    def alpha1(self, value: float) -> None:
        self._cards[4].set_value("alpha1", value)

    @property
    def alpha2(self) -> typing.Optional[float]:
        """Get or set the alpha-2, second exponent.
        """ # nopep8
        return self._cards[4].get_value("alpha2")

    @alpha2.setter
    def alpha2(self, value: float) -> None:
        self._cards[4].set_value("alpha2", value)

    @property
    def alpha3(self) -> typing.Optional[float]:
        """Get or set the alpha-3, third exponent.
        """ # nopep8
        return self._cards[4].get_value("alpha3")

    @alpha3.setter
    def alpha3(self, value: float) -> None:
        self._cards[4].set_value("alpha3", value)

    @property
    def alpha4(self) -> typing.Optional[float]:
        """Get or set the alpha-4, fourth exponent.
        """ # nopep8
        return self._cards[4].get_value("alpha4")

    @alpha4.setter
    def alpha4(self, value: float) -> None:
        self._cards[4].set_value("alpha4", value)

    @property
    def alpha5(self) -> typing.Optional[float]:
        """Get or set the alpha-5, fifth exponent.
        """ # nopep8
        return self._cards[4].get_value("alpha5")

    @alpha5.setter
    def alpha5(self, value: float) -> None:
        self._cards[4].set_value("alpha5", value)

    @property
    def alpha6(self) -> typing.Optional[float]:
        """Get or set the alpha-6, sixth exponent.
        """ # nopep8
        return self._cards[4].get_value("alpha6")

    @alpha6.setter
    def alpha6(self, value: float) -> None:
        self._cards[4].set_value("alpha6", value)

    @property
    def alpha7(self) -> typing.Optional[float]:
        """Get or set the alpha-7, seventh exponent.
        """ # nopep8
        return self._cards[4].get_value("alpha7")

    @alpha7.setter
    def alpha7(self, value: float) -> None:
        self._cards[4].set_value("alpha7", value)

    @property
    def alpha8(self) -> typing.Optional[float]:
        """Get or set the alpha-8, eighth exponent.
        """ # nopep8
        return self._cards[4].get_value("alpha8")

    @alpha8.setter
    def alpha8(self, value: float) -> None:
        self._cards[4].set_value("alpha8", value)

    @property
    def constants(self):
        '''Gets the table of constants'''
        return self._cards[5].table

    @constants.setter
    def constants(self, df):
        '''sets constants from the dataframe df'''
        self._cards[5].table = df

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[6].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[6].cards[0].set_value("title", value)


class MatOgdenRubber(Mat077O):
    subkeyword = "OGDEN_RUBBER"
