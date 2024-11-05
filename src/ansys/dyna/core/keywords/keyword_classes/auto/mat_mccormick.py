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

class MatMccormick(KeywordBase):
    """DYNA MAT_MCCORMICK keyword"""

    keyword = "MAT"
    subkeyword = "MCCORMICK"
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
                        "sigy",
                        float,
                        40,
                        10,
                        kwargs.get("sigy")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "q1",
                        float,
                        0,
                        10,
                        kwargs.get("q1")
                    ),
                    Field(
                        "c1",
                        float,
                        10,
                        10,
                        kwargs.get("c1")
                    ),
                    Field(
                        "q2",
                        float,
                        20,
                        10,
                        kwargs.get("q2")
                    ),
                    Field(
                        "c2",
                        float,
                        30,
                        10,
                        kwargs.get("c2")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "s",
                        float,
                        0,
                        10,
                        kwargs.get("s")
                    ),
                    Field(
                        "h",
                        float,
                        10,
                        10,
                        kwargs.get("h")
                    ),
                    Field(
                        "omega",
                        float,
                        20,
                        10,
                        kwargs.get("omega")
                    ),
                    Field(
                        "td",
                        float,
                        30,
                        10,
                        kwargs.get("td")
                    ),
                    Field(
                        "alpha",
                        float,
                        40,
                        10,
                        kwargs.get("alpha")
                    ),
                    Field(
                        "eps0",
                        float,
                        50,
                        10,
                        kwargs.get("eps0")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatMccormick.option_specs[0],
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
        """Get or set the Material identification.  A unique number or label must be specified.
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
    def sigy(self) -> typing.Optional[float]:
        """Get or set the Initial yield stress.
        """ # nopep8
        return self._cards[0].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        self._cards[0].set_value("sigy", value)

    @property
    def q1(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter, Q1.
        """ # nopep8
        return self._cards[1].get_value("q1")

    @q1.setter
    def q1(self, value: float) -> None:
        self._cards[1].set_value("q1", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter, C1.
        """ # nopep8
        return self._cards[1].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        self._cards[1].set_value("c1", value)

    @property
    def q2(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter, Q2.
        """ # nopep8
        return self._cards[1].get_value("q2")

    @q2.setter
    def q2(self, value: float) -> None:
        self._cards[1].set_value("q2", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter, C2.
        """ # nopep8
        return self._cards[1].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        self._cards[1].set_value("c2", value)

    @property
    def s(self) -> typing.Optional[float]:
        """Get or set the Dynamic strain aging parameter.
        """ # nopep8
        return self._cards[2].get_value("s")

    @s.setter
    def s(self, value: float) -> None:
        self._cards[2].set_value("s", value)

    @property
    def h(self) -> typing.Optional[float]:
        """Get or set the Dynamic strain aging parameter.
        """ # nopep8
        return self._cards[2].get_value("h")

    @h.setter
    def h(self, value: float) -> None:
        self._cards[2].set_value("h", value)

    @property
    def omega(self) -> typing.Optional[float]:
        """Get or set the Dynamic strain aging parameter.
        """ # nopep8
        return self._cards[2].get_value("omega")

    @omega.setter
    def omega(self, value: float) -> None:
        self._cards[2].set_value("omega", value)

    @property
    def td(self) -> typing.Optional[float]:
        """Get or set the Dynamic strain aging parameter.
        """ # nopep8
        return self._cards[2].get_value("td")

    @td.setter
    def td(self, value: float) -> None:
        self._cards[2].set_value("td", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Dynamic strain aging parameter.
        """ # nopep8
        return self._cards[2].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        self._cards[2].set_value("alpha", value)

    @property
    def eps0(self) -> typing.Optional[float]:
        """Get or set the Reference strain rate.
        """ # nopep8
        return self._cards[2].get_value("eps0")

    @eps0.setter
    def eps0(self, value: float) -> None:
        self._cards[2].set_value("eps0", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

