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

class Mat102T(KeywordBase):
    """DYNA MAT_102_T keyword"""

    keyword = "MAT"
    subkeyword = "102_T"
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
                        "alpha",
                        float,
                        20,
                        10,
                        kwargs.get("alpha")
                    ),
                    Field(
                        "n",
                        float,
                        30,
                        10,
                        kwargs.get("n")
                    ),
                    Field(
                        "a",
                        float,
                        40,
                        10,
                        kwargs.get("a")
                    ),
                    Field(
                        "q",
                        float,
                        50,
                        10,
                        kwargs.get("q")
                    ),
                    Field(
                        "g",
                        float,
                        60,
                        10,
                        kwargs.get("g")
                    ),
                    Field(
                        "epso",
                        float,
                        70,
                        10,
                        kwargs.get("epso")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lce",
                        float,
                        0,
                        10,
                        kwargs.get("lce")
                    ),
                    Field(
                        "lcpr",
                        float,
                        10,
                        10,
                        kwargs.get("lcpr")
                    ),
                    Field(
                        "lccte",
                        float,
                        20,
                        10,
                        kwargs.get("lccte")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat102T.option_specs[0],
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
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Not to be confused with coefficient of thermal expansion.
        """ # nopep8
        return self._cards[0].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        self._cards[0].set_value("alpha", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the See Remarks for detail.
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        self._cards[0].set_value("n", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the See Remarks for detail.
        """ # nopep8
        return self._cards[0].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        self._cards[0].set_value("a", value)

    @property
    def q(self) -> typing.Optional[float]:
        """Get or set the See Remarks for detail.
        """ # nopep8
        return self._cards[0].get_value("q")

    @q.setter
    def q(self, value: float) -> None:
        self._cards[0].set_value("q", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the See Remarks for detail
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        self._cards[0].set_value("g", value)

    @property
    def epso(self) -> typing.Optional[float]:
        """Get or set the Minimum strain rate considered in calculating Z
        """ # nopep8
        return self._cards[0].get_value("epso")

    @epso.setter
    def epso(self, value: float) -> None:
        self._cards[0].set_value("epso", value)

    @property
    def lce(self) -> typing.Optional[float]:
        """Get or set the ID of curve defining Young’s Modulus as a function of temperature
        """ # nopep8
        return self._cards[1].get_value("lce")

    @lce.setter
    def lce(self, value: float) -> None:
        self._cards[1].set_value("lce", value)

    @property
    def lcpr(self) -> typing.Optional[float]:
        """Get or set the ID of curve defining Poisson’s ratio as a function of temperature
        """ # nopep8
        return self._cards[1].get_value("lcpr")

    @lcpr.setter
    def lcpr(self, value: float) -> None:
        self._cards[1].set_value("lcpr", value)

    @property
    def lccte(self) -> typing.Optional[float]:
        """Get or set the ID of curve defining coefficient of thermal expansion as a function of temperature
        """ # nopep8
        return self._cards[1].get_value("lccte")

    @lccte.setter
    def lccte(self, value: float) -> None:
        self._cards[1].set_value("lccte", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

