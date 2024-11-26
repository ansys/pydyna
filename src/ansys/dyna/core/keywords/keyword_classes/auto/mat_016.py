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

class Mat016(KeywordBase):
    """DYNA MAT_016 keyword"""

    keyword = "MAT"
    subkeyword = "016"
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
                        "g",
                        float,
                        20,
                        10,
                        kwargs.get("g")
                    ),
                    Field(
                        "pr",
                        float,
                        30,
                        10,
                        kwargs.get("pr")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sigf",
                        float,
                        0,
                        10,
                        kwargs.get("sigf")
                    ),
                    Field(
                        "a0",
                        float,
                        10,
                        10,
                        kwargs.get("a0")
                    ),
                    Field(
                        "a1",
                        float,
                        20,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "a2",
                        float,
                        30,
                        10,
                        kwargs.get("a2")
                    ),
                    Field(
                        "a0f",
                        float,
                        40,
                        10,
                        kwargs.get("a0f")
                    ),
                    Field(
                        "a1f",
                        float,
                        50,
                        10,
                        kwargs.get("a1f")
                    ),
                    Field(
                        "b1",
                        float,
                        60,
                        10,
                        kwargs.get("b1")
                    ),
                    Field(
                        "per",
                        float,
                        70,
                        10,
                        kwargs.get("per")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "er",
                        float,
                        0,
                        10,
                        kwargs.get("er")
                    ),
                    Field(
                        "prr",
                        float,
                        10,
                        10,
                        kwargs.get("prr")
                    ),
                    Field(
                        "sigy",
                        float,
                        20,
                        10,
                        kwargs.get("sigy")
                    ),
                    Field(
                        "etan",
                        float,
                        30,
                        10,
                        kwargs.get("etan")
                    ),
                    Field(
                        "lcp",
                        int,
                        40,
                        10,
                        kwargs.get("lcp", 0)
                    ),
                    Field(
                        "lcr",
                        int,
                        50,
                        10,
                        kwargs.get("lcr", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "x1",
                        float,
                        0,
                        10,
                        kwargs.get("x1")
                    ),
                    Field(
                        "x2",
                        float,
                        10,
                        10,
                        kwargs.get("x2")
                    ),
                    Field(
                        "x3",
                        float,
                        20,
                        10,
                        kwargs.get("x3")
                    ),
                    Field(
                        "x4",
                        float,
                        30,
                        10,
                        kwargs.get("x4")
                    ),
                    Field(
                        "x5",
                        float,
                        40,
                        10,
                        kwargs.get("x5")
                    ),
                    Field(
                        "x6",
                        float,
                        50,
                        10,
                        kwargs.get("x6")
                    ),
                    Field(
                        "x7",
                        float,
                        60,
                        10,
                        kwargs.get("x7")
                    ),
                    Field(
                        "x8",
                        float,
                        70,
                        10,
                        kwargs.get("x8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "x9",
                        float,
                        0,
                        10,
                        kwargs.get("x9")
                    ),
                    Field(
                        "x10",
                        float,
                        10,
                        10,
                        kwargs.get("x10")
                    ),
                    Field(
                        "x11",
                        float,
                        20,
                        10,
                        kwargs.get("x11")
                    ),
                    Field(
                        "x12",
                        float,
                        30,
                        10,
                        kwargs.get("x12")
                    ),
                    Field(
                        "x13",
                        float,
                        40,
                        10,
                        kwargs.get("x13")
                    ),
                    Field(
                        "x14",
                        float,
                        50,
                        10,
                        kwargs.get("x14")
                    ),
                    Field(
                        "x15",
                        float,
                        60,
                        10,
                        kwargs.get("x15")
                    ),
                    Field(
                        "x16",
                        float,
                        70,
                        10,
                        kwargs.get("x16")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ys1",
                        float,
                        0,
                        10,
                        kwargs.get("ys1")
                    ),
                    Field(
                        "ys2",
                        float,
                        10,
                        10,
                        kwargs.get("ys2")
                    ),
                    Field(
                        "ys3",
                        float,
                        20,
                        10,
                        kwargs.get("ys3")
                    ),
                    Field(
                        "ys4",
                        float,
                        30,
                        10,
                        kwargs.get("ys4")
                    ),
                    Field(
                        "ys5",
                        float,
                        40,
                        10,
                        kwargs.get("ys5")
                    ),
                    Field(
                        "ys6",
                        float,
                        50,
                        10,
                        kwargs.get("ys6")
                    ),
                    Field(
                        "ys7",
                        float,
                        60,
                        10,
                        kwargs.get("ys7")
                    ),
                    Field(
                        "ys8",
                        float,
                        70,
                        10,
                        kwargs.get("ys8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ys9",
                        float,
                        0,
                        10,
                        kwargs.get("ys9")
                    ),
                    Field(
                        "ys10",
                        float,
                        10,
                        10,
                        kwargs.get("ys10")
                    ),
                    Field(
                        "ys11",
                        float,
                        20,
                        10,
                        kwargs.get("ys11")
                    ),
                    Field(
                        "ys12",
                        float,
                        30,
                        10,
                        kwargs.get("ys12")
                    ),
                    Field(
                        "ys13",
                        float,
                        40,
                        10,
                        kwargs.get("ys13")
                    ),
                    Field(
                        "ys14",
                        float,
                        50,
                        10,
                        kwargs.get("ys14")
                    ),
                    Field(
                        "ys15",
                        float,
                        60,
                        10,
                        kwargs.get("ys15")
                    ),
                    Field(
                        "ys16",
                        float,
                        70,
                        10,
                        kwargs.get("ys16")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat016.option_specs[0],
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
    def g(self) -> typing.Optional[float]:
        """Get or set the Shear modulus.
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        self._cards[0].set_value("g", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def sigf(self) -> typing.Optional[float]:
        """Get or set the Tensile cutoff (maximum principal stress for failure).
        """ # nopep8
        return self._cards[1].get_value("sigf")

    @sigf.setter
    def sigf(self, value: float) -> None:
        self._cards[1].set_value("sigf", value)

    @property
    def a0(self) -> typing.Optional[float]:
        """Get or set the Cohesion.
        """ # nopep8
        return self._cards[1].get_value("a0")

    @a0.setter
    def a0(self, value: float) -> None:
        self._cards[1].set_value("a0", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Pressure hardening coefficient.
        """ # nopep8
        return self._cards[1].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[1].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Pressure hardening coefficient.
        """ # nopep8
        return self._cards[1].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[1].set_value("a2", value)

    @property
    def a0f(self) -> typing.Optional[float]:
        """Get or set the Cohesion for failed material.
        """ # nopep8
        return self._cards[1].get_value("a0f")

    @a0f.setter
    def a0f(self, value: float) -> None:
        self._cards[1].set_value("a0f", value)

    @property
    def a1f(self) -> typing.Optional[float]:
        """Get or set the Pressure hardening coefficient for failed material.
        """ # nopep8
        return self._cards[1].get_value("a1f")

    @a1f.setter
    def a1f(self, value: float) -> None:
        self._cards[1].set_value("a1f", value)

    @property
    def b1(self) -> typing.Optional[float]:
        """Get or set the Damage scaling factor.
        """ # nopep8
        return self._cards[1].get_value("b1")

    @b1.setter
    def b1(self, value: float) -> None:
        self._cards[1].set_value("b1", value)

    @property
    def per(self) -> typing.Optional[float]:
        """Get or set the Percent reinforcement.
        """ # nopep8
        return self._cards[1].get_value("per")

    @per.setter
    def per(self, value: float) -> None:
        self._cards[1].set_value("per", value)

    @property
    def er(self) -> typing.Optional[float]:
        """Get or set the Elastic modulus for reinforcement.
        """ # nopep8
        return self._cards[2].get_value("er")

    @er.setter
    def er(self, value: float) -> None:
        self._cards[2].set_value("er", value)

    @property
    def prr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio for reinforcement.
        """ # nopep8
        return self._cards[2].get_value("prr")

    @prr.setter
    def prr(self, value: float) -> None:
        self._cards[2].set_value("prr", value)

    @property
    def sigy(self) -> typing.Optional[float]:
        """Get or set the Initial yield stress.
        """ # nopep8
        return self._cards[2].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        self._cards[2].set_value("sigy", value)

    @property
    def etan(self) -> typing.Optional[float]:
        """Get or set the Tangent modulus/plastic hardening modulus.
        """ # nopep8
        return self._cards[2].get_value("etan")

    @etan.setter
    def etan(self, value: float) -> None:
        self._cards[2].set_value("etan", value)

    @property
    def lcp(self) -> int:
        """Get or set the Load curve ID giving rate sensitivity for principal material, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[2].get_value("lcp")

    @lcp.setter
    def lcp(self, value: int) -> None:
        self._cards[2].set_value("lcp", value)

    @property
    def lcr(self) -> int:
        """Get or set the Load curve ID giving rate sensitivity for reinforcement, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[2].get_value("lcr")

    @lcr.setter
    def lcr(self, value: int) -> None:
        self._cards[2].set_value("lcr", value)

    @property
    def x1(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[3].get_value("x1")

    @x1.setter
    def x1(self, value: float) -> None:
        self._cards[3].set_value("x1", value)

    @property
    def x2(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[3].get_value("x2")

    @x2.setter
    def x2(self, value: float) -> None:
        self._cards[3].set_value("x2", value)

    @property
    def x3(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[3].get_value("x3")

    @x3.setter
    def x3(self, value: float) -> None:
        self._cards[3].set_value("x3", value)

    @property
    def x4(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[3].get_value("x4")

    @x4.setter
    def x4(self, value: float) -> None:
        self._cards[3].set_value("x4", value)

    @property
    def x5(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[3].get_value("x5")

    @x5.setter
    def x5(self, value: float) -> None:
        self._cards[3].set_value("x5", value)

    @property
    def x6(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[3].get_value("x6")

    @x6.setter
    def x6(self, value: float) -> None:
        self._cards[3].set_value("x6", value)

    @property
    def x7(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[3].get_value("x7")

    @x7.setter
    def x7(self, value: float) -> None:
        self._cards[3].set_value("x7", value)

    @property
    def x8(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[3].get_value("x8")

    @x8.setter
    def x8(self, value: float) -> None:
        self._cards[3].set_value("x8", value)

    @property
    def x9(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[4].get_value("x9")

    @x9.setter
    def x9(self, value: float) -> None:
        self._cards[4].set_value("x9", value)

    @property
    def x10(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[4].get_value("x10")

    @x10.setter
    def x10(self, value: float) -> None:
        self._cards[4].set_value("x10", value)

    @property
    def x11(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[4].get_value("x11")

    @x11.setter
    def x11(self, value: float) -> None:
        self._cards[4].set_value("x11", value)

    @property
    def x12(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[4].get_value("x12")

    @x12.setter
    def x12(self, value: float) -> None:
        self._cards[4].set_value("x12", value)

    @property
    def x13(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[4].get_value("x13")

    @x13.setter
    def x13(self, value: float) -> None:
        self._cards[4].set_value("x13", value)

    @property
    def x14(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[4].get_value("x14")

    @x14.setter
    def x14(self, value: float) -> None:
        self._cards[4].set_value("x14", value)

    @property
    def x15(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[4].get_value("x15")

    @x15.setter
    def x15(self, value: float) -> None:
        self._cards[4].set_value("x15", value)

    @property
    def x16(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[4].get_value("x16")

    @x16.setter
    def x16(self, value: float) -> None:
        self._cards[4].set_value("x16", value)

    @property
    def ys1(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[5].get_value("ys1")

    @ys1.setter
    def ys1(self, value: float) -> None:
        self._cards[5].set_value("ys1", value)

    @property
    def ys2(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[5].get_value("ys2")

    @ys2.setter
    def ys2(self, value: float) -> None:
        self._cards[5].set_value("ys2", value)

    @property
    def ys3(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[5].get_value("ys3")

    @ys3.setter
    def ys3(self, value: float) -> None:
        self._cards[5].set_value("ys3", value)

    @property
    def ys4(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[5].get_value("ys4")

    @ys4.setter
    def ys4(self, value: float) -> None:
        self._cards[5].set_value("ys4", value)

    @property
    def ys5(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[5].get_value("ys5")

    @ys5.setter
    def ys5(self, value: float) -> None:
        self._cards[5].set_value("ys5", value)

    @property
    def ys6(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[5].get_value("ys6")

    @ys6.setter
    def ys6(self, value: float) -> None:
        self._cards[5].set_value("ys6", value)

    @property
    def ys7(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[5].get_value("ys7")

    @ys7.setter
    def ys7(self, value: float) -> None:
        self._cards[5].set_value("ys7", value)

    @property
    def ys8(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[5].get_value("ys8")

    @ys8.setter
    def ys8(self, value: float) -> None:
        self._cards[5].set_value("ys8", value)

    @property
    def ys9(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[6].get_value("ys9")

    @ys9.setter
    def ys9(self, value: float) -> None:
        self._cards[6].set_value("ys9", value)

    @property
    def ys10(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[6].get_value("ys10")

    @ys10.setter
    def ys10(self, value: float) -> None:
        self._cards[6].set_value("ys10", value)

    @property
    def ys11(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[6].get_value("ys11")

    @ys11.setter
    def ys11(self, value: float) -> None:
        self._cards[6].set_value("ys11", value)

    @property
    def ys12(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[6].get_value("ys12")

    @ys12.setter
    def ys12(self, value: float) -> None:
        self._cards[6].set_value("ys12", value)

    @property
    def ys13(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[6].get_value("ys13")

    @ys13.setter
    def ys13(self, value: float) -> None:
        self._cards[6].set_value("ys13", value)

    @property
    def ys14(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[6].get_value("ys14")

    @ys14.setter
    def ys14(self, value: float) -> None:
        self._cards[6].set_value("ys14", value)

    @property
    def ys15(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[6].get_value("ys15")

    @ys15.setter
    def ys15(self, value: float) -> None:
        self._cards[6].set_value("ys15", value)

    @property
    def ys16(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[6].get_value("ys16")

    @ys16.setter
    def ys16(self, value: float) -> None:
        self._cards[6].set_value("ys16", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[7].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[7].cards[0].set_value("title", value)

