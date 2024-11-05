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

class MatTailoredProperties(KeywordBase):
    """DYNA MAT_TAILORED_PROPERTIES keyword"""

    keyword = "MAT"
    subkeyword = "TAILORED_PROPERTIES"
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
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        50,
                        10,
                        kwargs.get("unused")
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
                        "unused",
                        int,
                        0,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "lcss",
                        int,
                        20,
                        10,
                        kwargs.get("lcss", 0)
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "vp",
                        float,
                        40,
                        10,
                        kwargs.get("vp", 0.0)
                    ),
                    Field(
                        "hisvn",
                        int,
                        50,
                        10,
                        kwargs.get("hisvn", 0)
                    ),
                    Field(
                        "phase",
                        float,
                        60,
                        10,
                        kwargs.get("phase")
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
            OptionCardSet(
                option_spec = MatTailoredProperties.option_specs[0],
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
    def fail(self) -> float:
        """Get or set the Failure flag.
        LT.0.0: User defined failure subroutine, matusr_24 in dyn21.F, is
        called to determine failure
        EQ.0.0: Failure is not considered. This option is recommended if failure is not of interest since many calculations will be saved.
        GT.0.0: Effective plastic strain to failure. When the plastic strain	reaches this value, the element is deleted from the calculation
        """ # nopep8
        return self._cards[0].get_value("fail")

    @fail.setter
    def fail(self, value: float) -> None:
        self._cards[0].set_value("fail", value)

    @property
    def tdel(self) -> typing.Optional[float]:
        """Get or set the Minimum time step size for automatic element deletion
        """ # nopep8
        return self._cards[0].get_value("tdel")

    @tdel.setter
    def tdel(self, value: float) -> None:
        self._cards[0].set_value("tdel", value)

    @property
    def lcss(self) -> int:
        """Get or set the Load curve ID or Table ID (see full description of MAT_024). Load
        curve for stress vs. plastic strain. 2-D table for stress vs. plastic strain
        as a function of strain rates. 3-D table for stress vs. plastic strain as a
        function of strain rates as a function of history variable values (see HISVN).
        """ # nopep8
        return self._cards[1].get_value("lcss")

    @lcss.setter
    def lcss(self, value: int) -> None:
        self._cards[1].set_value("lcss", value)

    @property
    def vp(self) -> float:
        """Get or set the Formulation for rate effects:
        EQ.0.0: Scale yield stress (default),
        EQ.1.0: Viscoplastic formulation.
        """ # nopep8
        return self._cards[1].get_value("vp")

    @vp.setter
    def vp(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""vp must be one of {0.0,1.0}""")
        self._cards[1].set_value("vp", value)

    @property
    def hisvn(self) -> int:
        """Get or set the Location of history variable in the history array of *INITIAL_STRESS_SHELL that is used to evaluate the 3-D table LCSS
        """ # nopep8
        return self._cards[1].get_value("hisvn")

    @hisvn.setter
    def hisvn(self, value: int) -> None:
        self._cards[1].set_value("hisvn", value)

    @property
    def phase(self) -> typing.Optional[float]:
        """Get or set the Constant value to evaluate the 3-D table LCSS. Only used if HISVN = 0
        """ # nopep8
        return self._cards[1].get_value("phase")

    @phase.setter
    def phase(self, value: float) -> None:
        self._cards[1].set_value("phase", value)

    @property
    def eps1(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values (optional). At least 2 points should be defined. The first point must be zero corresponding to the initial yield stress.
        """ # nopep8
        return self._cards[2].get_value("eps1")

    @eps1.setter
    def eps1(self, value: float) -> None:
        self._cards[2].set_value("eps1", value)

    @property
    def eps2(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values (optional). At least 2 points should be defined. The first point must be zero corresponding to the initial yield stress.
        """ # nopep8
        return self._cards[2].get_value("eps2")

    @eps2.setter
    def eps2(self, value: float) -> None:
        self._cards[2].set_value("eps2", value)

    @property
    def eps3(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values (optional). At least 2 points should be defined. The first point must be zero corresponding to the initial yield stress.
        """ # nopep8
        return self._cards[2].get_value("eps3")

    @eps3.setter
    def eps3(self, value: float) -> None:
        self._cards[2].set_value("eps3", value)

    @property
    def eps4(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values (optional). At least 2 points should be defined. The first point must be zero corresponding to the initial yield stress.
        """ # nopep8
        return self._cards[2].get_value("eps4")

    @eps4.setter
    def eps4(self, value: float) -> None:
        self._cards[2].set_value("eps4", value)

    @property
    def eps5(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values (optional). At least 2 points should be defined. The first point must be zero corresponding to the initial yield stress
        """ # nopep8
        return self._cards[2].get_value("eps5")

    @eps5.setter
    def eps5(self, value: float) -> None:
        self._cards[2].set_value("eps5", value)

    @property
    def eps6(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values (optional). At least 2 points should be defined. The first point must be zero corresponding to the initial yield stress
        """ # nopep8
        return self._cards[2].get_value("eps6")

    @eps6.setter
    def eps6(self, value: float) -> None:
        self._cards[2].set_value("eps6", value)

    @property
    def eps7(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values (optional). At least 2 points should be defined. The first point must be zero corresponding to the initial yield stress
        """ # nopep8
        return self._cards[2].get_value("eps7")

    @eps7.setter
    def eps7(self, value: float) -> None:
        self._cards[2].set_value("eps7", value)

    @property
    def eps8(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values (optional). At least 2 points should be defined. The first point must be zero corresponding to the initial yield stress
        """ # nopep8
        return self._cards[2].get_value("eps8")

    @eps8.setter
    def eps8(self, value: float) -> None:
        self._cards[2].set_value("eps8", value)

    @property
    def es1(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress values to EPS1 - EPS8.
        """ # nopep8
        return self._cards[3].get_value("es1")

    @es1.setter
    def es1(self, value: float) -> None:
        self._cards[3].set_value("es1", value)

    @property
    def es2(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress values to EPS1 - EPS8.
        """ # nopep8
        return self._cards[3].get_value("es2")

    @es2.setter
    def es2(self, value: float) -> None:
        self._cards[3].set_value("es2", value)

    @property
    def es3(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress values to EPS1 - EPS8.
        """ # nopep8
        return self._cards[3].get_value("es3")

    @es3.setter
    def es3(self, value: float) -> None:
        self._cards[3].set_value("es3", value)

    @property
    def es4(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress values to EPS1 - EPS8.
        """ # nopep8
        return self._cards[3].get_value("es4")

    @es4.setter
    def es4(self, value: float) -> None:
        self._cards[3].set_value("es4", value)

    @property
    def es5(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress values to EPS1 - EPS8
        """ # nopep8
        return self._cards[3].get_value("es5")

    @es5.setter
    def es5(self, value: float) -> None:
        self._cards[3].set_value("es5", value)

    @property
    def es6(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress values to EPS1 - EPS8
        """ # nopep8
        return self._cards[3].get_value("es6")

    @es6.setter
    def es6(self, value: float) -> None:
        self._cards[3].set_value("es6", value)

    @property
    def es7(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress values to EPS1 - EPS8
        """ # nopep8
        return self._cards[3].get_value("es7")

    @es7.setter
    def es7(self, value: float) -> None:
        self._cards[3].set_value("es7", value)

    @property
    def es8(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress values to EPS1 - EPS8
        """ # nopep8
        return self._cards[3].get_value("es8")

    @es8.setter
    def es8(self, value: float) -> None:
        self._cards[3].set_value("es8", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[4].cards[0].set_value("title", value)

