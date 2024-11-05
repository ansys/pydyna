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
from ansys.dyna.core.lib.keyword_base import KeywordBase

class EosJwlAfterburn(KeywordBase):
    """DYNA EOS_JWL_AFTERBURN keyword"""

    keyword = "EOS"
    subkeyword = "JWL_AFTERBURN"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "eosid",
                        int,
                        0,
                        10,
                        kwargs.get("eosid")
                    ),
                    Field(
                        "a",
                        float,
                        10,
                        10,
                        kwargs.get("a")
                    ),
                    Field(
                        "b",
                        float,
                        20,
                        10,
                        kwargs.get("b")
                    ),
                    Field(
                        "r1",
                        float,
                        30,
                        10,
                        kwargs.get("r1")
                    ),
                    Field(
                        "r2",
                        float,
                        40,
                        10,
                        kwargs.get("r2")
                    ),
                    Field(
                        "omeg",
                        float,
                        50,
                        10,
                        kwargs.get("omeg")
                    ),
                    Field(
                        "e0",
                        float,
                        60,
                        10,
                        kwargs.get("e0")
                    ),
                    Field(
                        "vo",
                        float,
                        70,
                        10,
                        kwargs.get("vo")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "opt",
                        float,
                        0,
                        10,
                        kwargs.get("opt", 0.0)
                    ),
                    Field(
                        "qt",
                        float,
                        10,
                        10,
                        kwargs.get("qt")
                    ),
                    Field(
                        "t1",
                        float,
                        20,
                        10,
                        kwargs.get("t1")
                    ),
                    Field(
                        "t2",
                        float,
                        30,
                        10,
                        kwargs.get("t2")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "opt",
                        float,
                        0,
                        10,
                        kwargs.get("opt", 0.0)
                    ),
                    Field(
                        "q0",
                        float,
                        10,
                        10,
                        kwargs.get("q0")
                    ),
                    Field(
                        "qa",
                        float,
                        20,
                        10,
                        kwargs.get("qa")
                    ),
                    Field(
                        "qm",
                        float,
                        30,
                        10,
                        kwargs.get("qm", 0.5)
                    ),
                    Field(
                        "qn",
                        float,
                        40,
                        10,
                        kwargs.get("qn", 0.17)
                    ),
                    Field(
                        "conm",
                        float,
                        50,
                        10,
                        kwargs.get("conm", 1.)
                    ),
                    Field(
                        "conl",
                        float,
                        60,
                        10,
                        kwargs.get("conl", 1.)
                    ),
                    Field(
                        "cont",
                        float,
                        70,
                        10,
                        kwargs.get("cont", 1.)
                    ),
                ],
            ),
        ]

    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state ID.
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        self._cards[0].set_value("eosid", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Equation of state coefficient, A.
        """ # nopep8
        return self._cards[0].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        self._cards[0].set_value("a", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Equation of state coefficient, B.
        """ # nopep8
        return self._cards[0].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        self._cards[0].set_value("b", value)

    @property
    def r1(self) -> typing.Optional[float]:
        """Get or set the Equation of state coefficient, R1.
        """ # nopep8
        return self._cards[0].get_value("r1")

    @r1.setter
    def r1(self, value: float) -> None:
        self._cards[0].set_value("r1", value)

    @property
    def r2(self) -> typing.Optional[float]:
        """Get or set the Equation of state coefficient, R2.
        """ # nopep8
        return self._cards[0].get_value("r2")

    @r2.setter
    def r2(self, value: float) -> None:
        self._cards[0].set_value("r2", value)

    @property
    def omeg(self) -> typing.Optional[float]:
        """Get or set the Equation of state coefficient, w.
        """ # nopep8
        return self._cards[0].get_value("omeg")

    @omeg.setter
    def omeg(self, value: float) -> None:
        self._cards[0].set_value("omeg", value)

    @property
    def e0(self) -> typing.Optional[float]:
        """Get or set the Detonation energy per unit volume and initial value for E. See equation in Remarks.
        """ # nopep8
        return self._cards[0].get_value("e0")

    @e0.setter
    def e0(self, value: float) -> None:
        self._cards[0].set_value("e0", value)

    @property
    def vo(self) -> typing.Optional[float]:
        """Get or set the Initial realtive volume.
        """ # nopep8
        return self._cards[0].get_value("vo")

    @vo.setter
    def vo(self, value: float) -> None:
        self._cards[0].set_value("vo", value)

    @property
    def opt(self) -> float:
        """Get or set the Afterburn option EQ.0.0: No afterburn energy (Standard EOS_JWL)
        EQ.1.0: Constant rate of afterburn energy added between times T1 and T2
        EQ.2.0: Linearly-increasing rate of afterburn energy added between times T1 and T2
        EQ.3.0: Miller's extension for afterburn energy.
        """ # nopep8
        return self._cards[1].get_value("opt")

    @opt.setter
    def opt(self, value: float) -> None:
        if value not in [0.0, 1.0, 2.0, 3.0]:
            raise Exception("""opt must be one of {0.0,1.0,2.0,3.0}""")
        self._cards[1].set_value("opt", value)

    @property
    def qt(self) -> typing.Optional[float]:
        """Get or set the Afterburn energy per unit volume for simple afterburn (OPT=1,2).
        """ # nopep8
        return self._cards[1].get_value("qt")

    @qt.setter
    def qt(self, value: float) -> None:
        self._cards[1].set_value("qt", value)

    @property
    def t1(self) -> typing.Optional[float]:
        """Get or set the Start time of energy addition for simple afterburn.
        """ # nopep8
        return self._cards[1].get_value("t1")

    @t1.setter
    def t1(self, value: float) -> None:
        self._cards[1].set_value("t1", value)

    @property
    def t2(self) -> typing.Optional[float]:
        """Get or set the End time of energy addition for simple afterburn.
        """ # nopep8
        return self._cards[1].get_value("t2")

    @t2.setter
    def t2(self, value: float) -> None:
        self._cards[1].set_value("t2", value)

    @property
    def opt(self) -> float:
        """Get or set the Afterburn option EQ.0.0: No afterburn energy (Standard EOS_JWL)
        EQ.1.0: Constant rate of afterburn energy added between times T1 and T2
        EQ.2.0: Linearly-increasing rate of afterburn energy added between times T1 and T2
        EQ.3.0: Miller's extension for afterburn energy.
        """ # nopep8
        return self._cards[2].get_value("opt")

    @opt.setter
    def opt(self, value: float) -> None:
        if value not in [0.0, 1.0, 2.0, 3.0]:
            raise Exception("""opt must be one of {0.0,1.0,2.0,3.0}""")
        self._cards[2].set_value("opt", value)

    @property
    def q0(self) -> typing.Optional[float]:
        """Get or set the Afterburn energy per unit volume for Miller's extension (OPT=3).
        """ # nopep8
        return self._cards[2].get_value("q0")

    @q0.setter
    def q0(self, value: float) -> None:
        self._cards[2].set_value("q0", value)

    @property
    def qa(self) -> typing.Optional[float]:
        """Get or set the Energy release constant a for Miller's extension.
        """ # nopep8
        return self._cards[2].get_value("qa")

    @qa.setter
    def qa(self, value: float) -> None:
        self._cards[2].set_value("qa", value)

    @property
    def qm(self) -> float:
        """Get or set the Energy release exponent m for Miller's extension.
        """ # nopep8
        return self._cards[2].get_value("qm")

    @qm.setter
    def qm(self, value: float) -> None:
        self._cards[2].set_value("qm", value)

    @property
    def qn(self) -> float:
        """Get or set the Pressure exponent n for Miller's extension.
        """ # nopep8
        return self._cards[2].get_value("qn")

    @qn.setter
    def qn(self, value: float) -> None:
        self._cards[2].set_value("qn", value)

    @property
    def conm(self) -> float:
        """Get or set the GT.0.0: Mass conversion factor from model units to calibration units for Miller's extension
        LT.0.0: Use predefined factors to convert model units to published
        calibration units of g, cm, μs. Choices for model units are:
        EQ.-1.0: g, mm, ms
        EQ.-2.0: g, cm, ms
        EQ.-3.0: kg, m, s
        EQ.-4.0: kg, mm, ms
        EQ.-5.0: metric ton, mm, s
        EQ.-6.0: lbf-s2/in, in, s
        EQ.-7.0: slug, ft, s.
        """ # nopep8
        return self._cards[2].get_value("conm")

    @conm.setter
    def conm(self, value: float) -> None:
        self._cards[2].set_value("conm", value)

    @property
    def conl(self) -> float:
        """Get or set the CONM.GT.0.0: Length conversion factor from model units to calibration units for Miller's extension CONM.
        LT.0.0: Ignored.
        """ # nopep8
        return self._cards[2].get_value("conl")

    @conl.setter
    def conl(self, value: float) -> None:
        self._cards[2].set_value("conl", value)

    @property
    def cont(self) -> float:
        """Get or set the CONM.GT.0.0: Time conversion factor from model units to calibration units for Miller's extension CONM.
        LT.0.0: Ignored.
        """ # nopep8
        return self._cards[2].get_value("cont")

    @cont.setter
    def cont(self, value: float) -> None:
        self._cards[2].set_value("cont", value)

