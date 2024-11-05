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

class Mat077H(KeywordBase):
    """DYNA MAT_077_H keyword"""

    keyword = "MAT"
    subkeyword = "077_H"
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
                        kwargs.get("nv")
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
                        kwargs.get("data")
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
                        "c10",
                        float,
                        0,
                        10,
                        kwargs.get("c10")
                    ),
                    Field(
                        "c01",
                        float,
                        10,
                        10,
                        kwargs.get("c01")
                    ),
                    Field(
                        "c11",
                        float,
                        20,
                        10,
                        kwargs.get("c11")
                    ),
                    Field(
                        "c20",
                        float,
                        30,
                        10,
                        kwargs.get("c20")
                    ),
                    Field(
                        "c02",
                        float,
                        40,
                        10,
                        kwargs.get("c02")
                    ),
                    Field(
                        "c30",
                        float,
                        50,
                        10,
                        kwargs.get("c30")
                    ),
                    Field(
                        "therml",
                        float,
                        60,
                        10,
                        kwargs.get("therml")
                    ),
                ],
                lambda: self.n == 0,
            ),
            DuplicateCard(
                [
                    Field("gi", float, 0, 10),
                    Field("betai", float, 10, 10),
                    Field("gj", float, 20, 10),
                    Field("sigfj", float, 30, 10),
                    Field("vflag", int, 40, 10),
                ],
                None,
                data = kwargs.get("constants")),
            OptionCardSet(
                option_spec = Mat077H.option_specs[0],
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
        """Get or set the Poisson's ratio (> .49 is recommended, smaller values may not work
        and should not be used). If this is set to a negative number, then the
        absolute value is used and an extra card is read for Mullins effect.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def n(self) -> int:
        """Get or set the Number of constants to solve for:,
        EQ.1: Solve for C10 and C01,
        EQ.2: Solve for C10, C01, C11, C20, and C02,
        EQ.3: Solve for C10, C01, C11, C20, C02, and C30
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""n must be one of {0,1,2,3}""")
        self._cards[0].set_value("n", value)

    @property
    def nv(self) -> typing.Optional[int]:
        """Get or set the Number of Prony series terms in fit. The default is 6. Currently, the maximum number is set to 6. Values less than 6, possibly 3-5 are recommended, since each term used adds significantly to the cost. Caution should be exercised when taking the results from the fit. Preferably, all generated coefficients should be positive. Negative values may lead to unstable results. Once a satisfactory fit has been achieved it is recommended that the coefficients which are written into the output file be input in future runs.
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
        geometry is defined by the keyword:*INITIAL_FOAM_REFERENCE_GEOMETRY (see there for more details).
        EQ.0.0: off,
        EQ.1.0: on..
        """ # nopep8
        return self._cards[0].get_value("ref")

    @ref.setter
    def ref(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""ref must be one of {0.0,1.0}""")
        self._cards[0].set_value("ref", value)

    @property
    def tbhys(self) -> typing.Optional[float]:
        """Get or set the Table ID for hysteresis, see Remarks.
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
    def data(self) -> typing.Optional[float]:
        """Get or set the Type of experimental data.
        EQ.0.0: uniaxial data (Only option for this model)
        """ # nopep8
        return self._cards[2].get_value("data")

    @data.setter
    def data(self, value: float) -> None:
        self._cards[2].set_value("data", value)

    @property
    def lcid2(self) -> typing.Optional[int]:
        """Get or set the Load curve ID of relaxation curve.
        If constants βi are determined via a least squares fit. This relaxation curve is shown in Figure 20.25. This model ignores the constant stress.
        """ # nopep8
        return self._cards[2].get_value("lcid2")

    @lcid2.setter
    def lcid2(self, value: int) -> None:
        self._cards[2].set_value("lcid2", value)

    @property
    def bstart(self) -> typing.Optional[float]:
        """Get or set the In the fit, β1 is set to zero, β2 is set to BSTART, β3 is 10 times β2, β4 is 100 times grater than β3, and so on. If zero, BSTART is determined by an iterative trial and error scheme.
        """ # nopep8
        return self._cards[2].get_value("bstart")

    @bstart.setter
    def bstart(self, value: float) -> None:
        self._cards[2].set_value("bstart", value)

    @property
    def tramp(self) -> typing.Optional[float]:
        """Get or set the Optional ramp time for loading.
        """ # nopep8
        return self._cards[2].get_value("tramp")

    @tramp.setter
    def tramp(self, value: float) -> None:
        self._cards[2].set_value("tramp", value)

    @property
    def c10(self) -> typing.Optional[float]:
        """Get or set the C10
        """ # nopep8
        return self._cards[3].get_value("c10")

    @c10.setter
    def c10(self, value: float) -> None:
        self._cards[3].set_value("c10", value)

    @property
    def c01(self) -> typing.Optional[float]:
        """Get or set the C01
        """ # nopep8
        return self._cards[3].get_value("c01")

    @c01.setter
    def c01(self, value: float) -> None:
        self._cards[3].set_value("c01", value)

    @property
    def c11(self) -> typing.Optional[float]:
        """Get or set the C11
        """ # nopep8
        return self._cards[3].get_value("c11")

    @c11.setter
    def c11(self, value: float) -> None:
        self._cards[3].set_value("c11", value)

    @property
    def c20(self) -> typing.Optional[float]:
        """Get or set the C20
        """ # nopep8
        return self._cards[3].get_value("c20")

    @c20.setter
    def c20(self, value: float) -> None:
        self._cards[3].set_value("c20", value)

    @property
    def c02(self) -> typing.Optional[float]:
        """Get or set the C02
        """ # nopep8
        return self._cards[3].get_value("c02")

    @c02.setter
    def c02(self, value: float) -> None:
        self._cards[3].set_value("c02", value)

    @property
    def c30(self) -> typing.Optional[float]:
        """Get or set the C30
        """ # nopep8
        return self._cards[3].get_value("c30")

    @c30.setter
    def c30(self, value: float) -> None:
        self._cards[3].set_value("c30", value)

    @property
    def therml(self) -> typing.Optional[float]:
        """Get or set the Flag for the thermal option. If THERML>0.0, then G, SIGF, C10 and C01 specify curve IDs giving the values as functions of temperature, otherwise they specify the constants.
        """ # nopep8
        return self._cards[3].get_value("therml")

    @therml.setter
    def therml(self, value: float) -> None:
        self._cards[3].set_value("therml", value)

    @property
    def constants(self):
        '''Gets the table of constants'''
        return self._cards[4].table

    @constants.setter
    def constants(self, df):
        '''sets constants from the dataframe df'''
        self._cards[4].table = df

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[5].cards[0].set_value("title", value)


class MatHyperelasticRubber(Mat077H):
    subkeyword = "HYPERELASTIC_RUBBER"
