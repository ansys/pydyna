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

class MatAddFatigue(KeywordBase):
    """DYNA MAT_ADD_FATIGUE keyword"""

    keyword = "MAT"
    subkeyword = "ADD_FATIGUE"
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
                        "lcid",
                        int,
                        10,
                        10,
                        kwargs.get("lcid", -1)
                    ),
                    Field(
                        "ltype",
                        int,
                        20,
                        10,
                        kwargs.get("ltype", 0)
                    ),
                    Field(
                        "a",
                        float,
                        30,
                        10,
                        kwargs.get("a")
                    ),
                    Field(
                        "b",
                        float,
                        40,
                        10,
                        kwargs.get("b")
                    ),
                    Field(
                        "sthres",
                        float,
                        50,
                        10,
                        kwargs.get("sthres")
                    ),
                    Field(
                        "snlimt",
                        int,
                        60,
                        10,
                        kwargs.get("snlimt", 0)
                    ),
                    Field(
                        "sntype",
                        int,
                        70,
                        10,
                        kwargs.get("sntype", 0)
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
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "ai",
                        float,
                        30,
                        10,
                        kwargs.get("ai")
                    ),
                    Field(
                        "bi",
                        float,
                        40,
                        10,
                        kwargs.get("bi")
                    ),
                    Field(
                        "sthresi",
                        float,
                        50,
                        10,
                        kwargs.get("sthresi")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatAddFatigue.option_specs[0],
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
        """Get or set the Material identification for which the fatigue property applies.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def lcid(self) -> int:
        """Get or set the S-N fatigue curve ID.
        GT.0: S-N fatigue curve ID.
        EQ.-1: S-N fatigue curve uses equation N*S**b=a.
        EQ.-2: S-N fatigue curve uses equation log(S)=a-b*log(N).
        EQ.-3: S-N fatigue curve uses equation S=a*N**b
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def ltype(self) -> int:
        """Get or set the Type of S-N curve.
        EQ.0: Semi-log interpolation (default).
        EQ.1: Log-Log interpolation.
        EQ.2: Linear-Linear interpolation.
        """ # nopep8
        return self._cards[0].get_value("ltype")

    @ltype.setter
    def ltype(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""ltype must be one of {0,1,2}""")
        self._cards[0].set_value("ltype", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Material parameter a in S-N fatigue equation.
        """ # nopep8
        return self._cards[0].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        self._cards[0].set_value("a", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Material parameter a in S-N fatigue equation.
        """ # nopep8
        return self._cards[0].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        self._cards[0].set_value("b", value)

    @property
    def sthres(self) -> typing.Optional[float]:
        """Get or set the Fatigue threshold if the S-N curve is defined by equation (LCID<0).
        """ # nopep8
        return self._cards[0].get_value("sthres")

    @sthres.setter
    def sthres(self, value: float) -> None:
        self._cards[0].set_value("sthres", value)

    @property
    def snlimt(self) -> int:
        """Get or set the If LCID > 0
        Flag setting algorithm used when stress is lower than the lowest stress on S-N curve.
        EQ.0: use the life at the last point on S-N curve.
        EQ.1: extrapolation from the last two points on S-N curve.
        EQ.2: infinity.
        If LCID < 0
        Flag setting algorithm used when stress is lower than STHRES.
        EQ.0: use the life at STHRES.
        EQ.1: Ignored. only applicable for LCID > 0.
        EQ.2: infinity.
        """ # nopep8
        return self._cards[0].get_value("snlimt")

    @snlimt.setter
    def snlimt(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""snlimt must be one of {0,1,2}""")
        self._cards[0].set_value("snlimt", value)

    @property
    def sntype(self) -> int:
        """Get or set the Stress type of S-N curve.
        EQ.0: stress range (default)
        EQ.1: stress amplitude.
        """ # nopep8
        return self._cards[0].get_value("sntype")

    @sntype.setter
    def sntype(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""sntype must be one of {0,1}""")
        self._cards[0].set_value("sntype", value)

    @property
    def ai(self) -> typing.Optional[float]:
        """Get or set the Material parameter a in S-N fatigue equation for the i-th segment.
        """ # nopep8
        return self._cards[1].get_value("ai")

    @ai.setter
    def ai(self, value: float) -> None:
        self._cards[1].set_value("ai", value)

    @property
    def bi(self) -> typing.Optional[float]:
        """Get or set the Material parameter b in S-N fatigue equation for the i-th segment.
        """ # nopep8
        return self._cards[1].get_value("bi")

    @bi.setter
    def bi(self, value: float) -> None:
        self._cards[1].set_value("bi", value)

    @property
    def sthresi(self) -> typing.Optional[float]:
        """Get or set the Fatigue threshold stress for the i-th segment.
        """ # nopep8
        return self._cards[1].get_value("sthresi")

    @sthresi.setter
    def sthresi(self, value: float) -> None:
        self._cards[1].set_value("sthresi", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

