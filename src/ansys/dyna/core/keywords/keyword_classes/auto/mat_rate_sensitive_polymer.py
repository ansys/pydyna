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

class MatRateSensitivePolymer(KeywordBase):
    """DYNA MAT_RATE_SENSITIVE_POLYMER keyword"""

    keyword = "MAT"
    subkeyword = "RATE_SENSITIVE_POLYMER"
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
                        "do",
                        float,
                        40,
                        10,
                        kwargs.get("do")
                    ),
                    Field(
                        "n",
                        float,
                        50,
                        10,
                        kwargs.get("n")
                    ),
                    Field(
                        "zo",
                        float,
                        60,
                        10,
                        kwargs.get("zo")
                    ),
                    Field(
                        "q",
                        float,
                        70,
                        10,
                        kwargs.get("q")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "omega",
                        float,
                        0,
                        10,
                        kwargs.get("omega")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatRateSensitivePolymer.option_specs[0],
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
    def e(self) -> typing.Optional[float]:
        """Get or set the Elastic modulus.
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
    def do(self) -> typing.Optional[float]:
        """Get or set the Refrence strain rate (=1000*max strain rate used in the test).
        """ # nopep8
        return self._cards[0].get_value("do")

    @do.setter
    def do(self, value: float) -> None:
        self._cards[0].set_value("do", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Exponent (see inelastic strain rate equation in user's manual).
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        self._cards[0].set_value("n", value)

    @property
    def zo(self) -> typing.Optional[float]:
        """Get or set the Initial hardness of material.
        """ # nopep8
        return self._cards[0].get_value("zo")

    @zo.setter
    def zo(self, value: float) -> None:
        self._cards[0].set_value("zo", value)

    @property
    def q(self) -> typing.Optional[float]:
        """Get or set the (see equations in user's manual).
        """ # nopep8
        return self._cards[0].get_value("q")

    @q.setter
    def q(self, value: float) -> None:
        self._cards[0].set_value("q", value)

    @property
    def omega(self) -> typing.Optional[float]:
        """Get or set the Maximum internal stress.
        """ # nopep8
        return self._cards[1].get_value("omega")

    @omega.setter
    def omega(self, value: float) -> None:
        self._cards[1].set_value("omega", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

