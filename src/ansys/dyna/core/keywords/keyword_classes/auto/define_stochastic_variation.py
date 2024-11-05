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

class DefineStochasticVariation(KeywordBase):
    """DYNA DEFINE_STOCHASTIC_VARIATION keyword"""

    keyword = "DEFINE"
    subkeyword = "STOCHASTIC_VARIATION"
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
                        "id_sv",
                        int,
                        0,
                        10,
                        kwargs.get("id_sv")
                    ),
                    Field(
                        "pid",
                        int,
                        10,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "pid_typ",
                        int,
                        20,
                        10,
                        kwargs.get("pid_typ", 0)
                    ),
                    Field(
                        "icor",
                        int,
                        30,
                        10,
                        kwargs.get("icor", 0)
                    ),
                    Field(
                        "var_s",
                        int,
                        40,
                        10,
                        kwargs.get("var_s", 0)
                    ),
                    Field(
                        "var_f",
                        int,
                        50,
                        10,
                        kwargs.get("var_f", 0)
                    ),
                    Field(
                        "irng",
                        int,
                        60,
                        10,
                        kwargs.get("irng", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "r1",
                        float,
                        0,
                        10,
                        kwargs.get("r1")
                    ),
                    Field(
                        "r2",
                        float,
                        10,
                        10,
                        kwargs.get("r2")
                    ),
                    Field(
                        "r3",
                        float,
                        20,
                        10,
                        kwargs.get("r3")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcid",
                        int,
                        0,
                        10,
                        kwargs.get("lcid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "r1",
                        float,
                        0,
                        10,
                        kwargs.get("r1")
                    ),
                    Field(
                        "r2",
                        float,
                        10,
                        10,
                        kwargs.get("r2")
                    ),
                    Field(
                        "r3",
                        float,
                        20,
                        10,
                        kwargs.get("r3")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcid",
                        int,
                        0,
                        10,
                        kwargs.get("lcid")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineStochasticVariation.option_specs[0],
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
    def id_sv(self) -> typing.Optional[int]:
        """Get or set the Stochastic variation ID. A unique ID number must be used
        """ # nopep8
        return self._cards[0].get_value("id_sv")

    @id_sv.setter
    def id_sv(self, value: int) -> None:
        self._cards[0].set_value("id_sv", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the *PART ID or *SET_PART ID
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def pid_typ(self) -> int:
        """Get or set the Flag for PID type. If PID and PID_TYP are both 0, then the
        properties defined here apply to all shell and solid parts using
        materials with the STOCHASTIC option.
        EQ.0: PID is a *PART ID.
        EQ.1: PID is a *SET_PART ID
        """ # nopep8
        return self._cards[0].get_value("pid_typ")

    @pid_typ.setter
    def pid_typ(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""pid_typ must be one of {0,1}""")
        self._cards[0].set_value("pid_typ", value)

    @property
    def icor(self) -> int:
        """Get or set the Correlation between the yield stress and failure strain scaling.
        EQ.0: Perfect correlation.
        EQ.1: No correlation. The yield stress and failure strain are independently scaled.
        """ # nopep8
        return self._cards[0].get_value("icor")

    @icor.setter
    def icor(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""icor must be one of {0,1}""")
        self._cards[0].set_value("icor", value)

    @property
    def var_s(self) -> int:
        """Get or set the Variation type for scaling the yield stress.
        EQ.0: The scale factor is 1.0 everywhere.
        EQ.1: The scale factor is random number in the uniform random distribution in the interval defined by R1 and R2.
        EQ.2: The scale factor is a random number obeying the Gaussian distribution defined by R1, R2, and R3.
        EQ.3: The scale factor is defined by the probability distribution function defined by curve LCID.
        EQ.4: The scale factor is defined by the cumulative distribution function defined by curve LCID.
        """ # nopep8
        return self._cards[0].get_value("var_s")

    @var_s.setter
    def var_s(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4]:
            raise Exception("""var_s must be one of {0,1,2,3,4}""")
        self._cards[0].set_value("var_s", value)

    @property
    def var_f(self) -> int:
        """Get or set the Variation type for scaling failure strain.
        EQ.0: The scale factor is 1.0 everywhere.
        EQ.1: The scale factor is random number in the uniform random distribution in the interval defined by R1 and R2.
        EQ.2: The scale factor is a random number obeying the Gaussian distribution defined by R1, R2, and R3.
        EQ.3: The scale factor is defined by the probability distribution function defined by curve LCID.
        EQ.4: The scale factor is defined by the cumulative distribution function defined by curve LCID.
        """ # nopep8
        return self._cards[0].get_value("var_f")

    @var_f.setter
    def var_f(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4]:
            raise Exception("""var_f must be one of {0,1,2,3,4}""")
        self._cards[0].set_value("var_f", value)

    @property
    def irng(self) -> int:
        """Get or set the Flag for random number generation.
        EQ.0:	Use deterministic(pseudo - ) random number generator.The same input always leads to the same distribution.
        EQ.1 : Use non - deterministic(true) random number generator.With the same input, a different distribution is achieved in each run
        """ # nopep8
        return self._cards[0].get_value("irng")

    @irng.setter
    def irng(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""irng must be one of {0,1}""")
        self._cards[0].set_value("irng", value)

    @property
    def r1(self) -> typing.Optional[float]:
        """Get or set the Real values to define the stochastic distribution
        """ # nopep8
        return self._cards[1].get_value("r1")

    @r1.setter
    def r1(self, value: float) -> None:
        self._cards[1].set_value("r1", value)

    @property
    def r2(self) -> typing.Optional[float]:
        """Get or set the Real values to define the stochastic distribution
        """ # nopep8
        return self._cards[1].get_value("r2")

    @r2.setter
    def r2(self, value: float) -> None:
        self._cards[1].set_value("r2", value)

    @property
    def r3(self) -> typing.Optional[float]:
        """Get or set the Real values to define the stochastic distribution
        """ # nopep8
        return self._cards[1].get_value("r3")

    @r3.setter
    def r3(self, value: float) -> None:
        self._cards[1].set_value("r3", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Curve ID defining the stochastic distribution
        """ # nopep8
        return self._cards[2].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[2].set_value("lcid", value)

    @property
    def r1(self) -> typing.Optional[float]:
        """Get or set the Real values to define the stochastic distribution
        """ # nopep8
        return self._cards[3].get_value("r1")

    @r1.setter
    def r1(self, value: float) -> None:
        self._cards[3].set_value("r1", value)

    @property
    def r2(self) -> typing.Optional[float]:
        """Get or set the Real values to define the stochastic distribution
        """ # nopep8
        return self._cards[3].get_value("r2")

    @r2.setter
    def r2(self, value: float) -> None:
        self._cards[3].set_value("r2", value)

    @property
    def r3(self) -> typing.Optional[float]:
        """Get or set the Real values to define the stochastic distribution
        """ # nopep8
        return self._cards[3].get_value("r3")

    @r3.setter
    def r3(self, value: float) -> None:
        self._cards[3].set_value("r3", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Curve ID defining the stochastic distribution
        """ # nopep8
        return self._cards[4].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[4].set_value("lcid", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[5].cards[0].set_value("title", value)

