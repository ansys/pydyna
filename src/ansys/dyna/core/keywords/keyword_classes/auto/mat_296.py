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

class Mat296(KeywordBase):
    """DYNA MAT_296 keyword"""

    keyword = "MAT"
    subkeyword = "296"
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
                        "ym",
                        float,
                        20,
                        10,
                        kwargs.get("ym")
                    ),
                    Field(
                        "pr",
                        float,
                        30,
                        10,
                        kwargs.get("pr")
                    ),
                    Field(
                        "alpha",
                        float,
                        40,
                        10,
                        kwargs.get("alpha")
                    ),
                    Field(
                        "a1",
                        float,
                        50,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "ratioqr",
                        float,
                        60,
                        10,
                        kwargs.get("ratioqr")
                    ),
                    Field(
                        "xi",
                        float,
                        70,
                        10,
                        kwargs.get("xi")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "m",
                        float,
                        0,
                        10,
                        kwargs.get("m")
                    ),
                    Field(
                        "s0",
                        float,
                        10,
                        10,
                        kwargs.get("s0")
                    ),
                    Field(
                        "h0",
                        float,
                        20,
                        10,
                        kwargs.get("h0")
                    ),
                    Field(
                        "a2",
                        float,
                        30,
                        10,
                        kwargs.get("a2")
                    ),
                    Field(
                        "sbar",
                        float,
                        40,
                        10,
                        kwargs.get("sbar")
                    ),
                    Field(
                        "n",
                        float,
                        50,
                        10,
                        kwargs.get("n")
                    ),
                    Field(
                        "lcym",
                        int,
                        60,
                        10,
                        kwargs.get("lcym")
                    ),
                    Field(
                        "tref",
                        float,
                        70,
                        10,
                        kwargs.get("tref")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat296.option_specs[0],
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
    def ym(self) -> typing.Optional[float]:
        """Get or set the Young's Modulus.
        """ # nopep8
        return self._cards[0].get_value("ym")

    @ym.setter
    def ym(self, value: float) -> None:
        self._cards[0].set_value("ym", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Coefficient of thermal expansion.
        """ # nopep8
        return self._cards[0].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        self._cards[0].set_value("alpha", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Pre-exponential factor.
        """ # nopep8
        return self._cards[0].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[0].set_value("a1", value)

    @property
    def ratioqr(self) -> typing.Optional[float]:
        """Get or set the Ratio of the activation energy, to the universal gas constant.
        """ # nopep8
        return self._cards[0].get_value("ratioqr")

    @ratioqr.setter
    def ratioqr(self, value: float) -> None:
        self._cards[0].set_value("ratioqr", value)

    @property
    def xi(self) -> typing.Optional[float]:
        """Get or set the Multiplier of stress.
        """ # nopep8
        return self._cards[0].get_value("xi")

    @xi.setter
    def xi(self, value: float) -> None:
        self._cards[0].set_value("xi", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Strain rate sensitivity.
        """ # nopep8
        return self._cards[1].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        self._cards[1].set_value("m", value)

    @property
    def s0(self) -> typing.Optional[float]:
        """Get or set the Initial value of deformation resistance.
        """ # nopep8
        return self._cards[1].get_value("s0")

    @s0.setter
    def s0(self, value: float) -> None:
        self._cards[1].set_value("s0", value)

    @property
    def h0(self) -> typing.Optional[float]:
        """Get or set the Hardening/softening constant.
        """ # nopep8
        return self._cards[1].get_value("h0")

    @h0.setter
    def h0(self, value: float) -> None:
        self._cards[1].set_value("h0", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Strain rate sensitivity of hardening or softening.
        """ # nopep8
        return self._cards[1].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[1].set_value("a2", value)

    @property
    def sbar(self) -> typing.Optional[float]:
        """Get or set the Coefficient of deformation resistance saturation value.
        """ # nopep8
        return self._cards[1].get_value("sbar")

    @sbar.setter
    def sbar(self, value: float) -> None:
        self._cards[1].set_value("sbar", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Strain rate sensitivity of deformation resistance saturation value.
        """ # nopep8
        return self._cards[1].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        self._cards[1].set_value("n", value)

    @property
    def lcym(self) -> typing.Optional[int]:
        """Get or set the Load curve ID when Young's Modulus is temperature dependent.
        """ # nopep8
        return self._cards[1].get_value("lcym")

    @lcym.setter
    def lcym(self, value: int) -> None:
        self._cards[1].set_value("lcym", value)

    @property
    def tref(self) -> typing.Optional[float]:
        """Get or set the Reference temperature, Tref.
        """ # nopep8
        return self._cards[1].get_value("tref")

    @tref.setter
    def tref(self, value: float) -> None:
        self._cards[1].set_value("tref", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

