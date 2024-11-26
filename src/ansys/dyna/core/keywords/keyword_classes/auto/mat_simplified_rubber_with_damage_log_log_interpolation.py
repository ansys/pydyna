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

class MatSimplifiedRubberWithDamageLogLogInterpolation(KeywordBase):
    """DYNA MAT_SIMPLIFIED_RUBBER_WITH_DAMAGE_LOG_LOG_INTERPOLATION keyword"""

    keyword = "MAT"
    subkeyword = "SIMPLIFIED_RUBBER_WITH_DAMAGE_LOG_LOG_INTERPOLATION"
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
                        "k",
                        float,
                        20,
                        10,
                        kwargs.get("k")
                    ),
                    Field(
                        "mu",
                        float,
                        30,
                        10,
                        kwargs.get("mu")
                    ),
                    Field(
                        "g",
                        float,
                        40,
                        10,
                        kwargs.get("g")
                    ),
                    Field(
                        "sigf",
                        float,
                        50,
                        10,
                        kwargs.get("sigf")
                    ),
                ],
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
                        "lc/tbid",
                        float,
                        30,
                        10,
                        kwargs.get("lc/tbid")
                    ),
                    Field(
                        "tension",
                        float,
                        40,
                        10,
                        kwargs.get("tension", -1.0)
                    ),
                    Field(
                        "rtype",
                        float,
                        50,
                        10,
                        kwargs.get("rtype", 0.0)
                    ),
                    Field(
                        "avgopt",
                        float,
                        60,
                        10,
                        kwargs.get("avgopt", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcunld",
                        int,
                        0,
                        10,
                        kwargs.get("lcunld")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatSimplifiedRubberWithDamageLogLogInterpolation.option_specs[0],
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
        """Get or set the Material identification. A unique number has to be chosen.
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
    def k(self) -> typing.Optional[float]:
        """Get or set the Linear bulk modulus.
        """ # nopep8
        return self._cards[0].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        self._cards[0].set_value("k", value)

    @property
    def mu(self) -> typing.Optional[float]:
        """Get or set the Damping coefficient.
        """ # nopep8
        return self._cards[0].get_value("mu")

    @mu.setter
    def mu(self, value: float) -> None:
        self._cards[0].set_value("mu", value)

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
    def sgl(self) -> typing.Optional[float]:
        """Get or set the Specimen gauge length.
        """ # nopep8
        return self._cards[1].get_value("sgl")

    @sgl.setter
    def sgl(self, value: float) -> None:
        self._cards[1].set_value("sgl", value)

    @property
    def sw(self) -> typing.Optional[float]:
        """Get or set the Specimen width.
        """ # nopep8
        return self._cards[1].get_value("sw")

    @sw.setter
    def sw(self, value: float) -> None:
        self._cards[1].set_value("sw", value)

    @property
    def st(self) -> typing.Optional[float]:
        """Get or set the Specimen thickness.
        """ # nopep8
        return self._cards[1].get_value("st")

    @st.setter
    def st(self, value: float) -> None:
        self._cards[1].set_value("st", value)

    @property
    def lc_tbid(self) -> typing.Optional[float]:
        """Get or set the Load curve or table ID, see *DEFINE_TABLE, defining the force versus actual change in the gauge length. If the table definition is used a family of curves are defined for discrete strain rates. The load curves should cover the complete range of expected loading, i.e., the smallest stretch ratio to the largest.
        """ # nopep8
        return self._cards[1].get_value("lc/tbid")

    @lc_tbid.setter
    def lc_tbid(self, value: float) -> None:
        self._cards[1].set_value("lc/tbid", value)

    @property
    def tension(self) -> float:
        """Get or set the Parameter that controls how the rate effects are treated. Applicable to the table definition.
        EQ.-1.-: rate effects are considered for loading either in tension or compression, but not for unloading,
        EQ.0.0: rate effects are considered for compressive loading only,
        EQ.1.0: rate effects are treated identically in tension and compression.
        """ # nopep8
        return self._cards[1].get_value("tension")

    @tension.setter
    def tension(self, value: float) -> None:
        if value not in [-1.0, 0.0, 1.0]:
            raise Exception("""tension must be one of {-1.0,0.0,1.0}""")
        self._cards[1].set_value("tension", value)

    @property
    def rtype(self) -> float:
        """Get or set the Strain rate type if a table is defined:
        EQ.0.0: true strain rate,
        EQ.1.0: engineering strain rate.
        """ # nopep8
        return self._cards[1].get_value("rtype")

    @rtype.setter
    def rtype(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""rtype must be one of {0.0,1.0}""")
        self._cards[1].set_value("rtype", value)

    @property
    def avgopt(self) -> float:
        """Get or set the Averaging option determine strain rate to reduce numerical noise.
        EQ.0.0: simple average of twelve time steps,
        EQ.1.0: running 12 point average.
        """ # nopep8
        return self._cards[1].get_value("avgopt")

    @avgopt.setter
    def avgopt(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""avgopt must be one of {0.0,1.0}""")
        self._cards[1].set_value("avgopt", value)

    @property
    def lcunld(self) -> typing.Optional[int]:
        """Get or set the Load curve, see *DEFINE_CURVE, defining the force versus actual change in the gauge length during unloading. The unload curve should cover exactly the same range as LCLD and its end points should have identical values, i.e., the combination of LCLD and LCUNLD describes a complete cycle of loading and unloading.
        """ # nopep8
        return self._cards[2].get_value("lcunld")

    @lcunld.setter
    def lcunld(self, value: int) -> None:
        self._cards[2].set_value("lcunld", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

