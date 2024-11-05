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

class MatSccOnRcc(KeywordBase):
    """DYNA MAT_SCC_ON_RCC keyword"""

    keyword = "MAT"
    subkeyword = "SCC_ON_RCC"
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
                        "e0",
                        float,
                        20,
                        10,
                        kwargs.get("e0")
                    ),
                    Field(
                        "e1",
                        float,
                        30,
                        10,
                        kwargs.get("e1")
                    ),
                    Field(
                        "e2",
                        float,
                        40,
                        10,
                        kwargs.get("e2")
                    ),
                    Field(
                        "e3",
                        float,
                        50,
                        10,
                        kwargs.get("e3")
                    ),
                    Field(
                        "e4",
                        float,
                        60,
                        10,
                        kwargs.get("e4")
                    ),
                    Field(
                        "e5",
                        float,
                        70,
                        10,
                        kwargs.get("e5")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "pr",
                        float,
                        0,
                        10,
                        kwargs.get("pr")
                    ),
                    Field(
                        "g",
                        float,
                        10,
                        10,
                        kwargs.get("g")
                    ),
                    Field(
                        "g_scl",
                        float,
                        20,
                        10,
                        kwargs.get("g_scl", 1.0)
                    ),
                    Field(
                        "tsl",
                        float,
                        30,
                        10,
                        kwargs.get("tsl")
                    ),
                    Field(
                        "eps_tan",
                        float,
                        40,
                        10,
                        kwargs.get("eps_tan")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatSccOnRcc.option_specs[0],
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
    def e0(self) -> typing.Optional[float]:
        """Get or set the Ea, Young's modulus in a-direction.
        """ # nopep8
        return self._cards[0].get_value("e0")

    @e0.setter
    def e0(self, value: float) -> None:
        self._cards[0].set_value("e0", value)

    @property
    def e1(self) -> typing.Optional[float]:
        """Get or set the Eb, Young's modulus in b-direction.
        """ # nopep8
        return self._cards[0].get_value("e1")

    @e1.setter
    def e1(self, value: float) -> None:
        self._cards[0].set_value("e1", value)

    @property
    def e2(self) -> typing.Optional[float]:
        """Get or set the Eb, Young's modulus in c-direction.
        """ # nopep8
        return self._cards[0].get_value("e2")

    @e2.setter
    def e2(self, value: float) -> None:
        self._cards[0].set_value("e2", value)

    @property
    def e3(self) -> typing.Optional[float]:
        """Get or set the Vba, Poisson's ratio, ba.
        """ # nopep8
        return self._cards[0].get_value("e3")

    @e3.setter
    def e3(self, value: float) -> None:
        self._cards[0].set_value("e3", value)

    @property
    def e4(self) -> typing.Optional[float]:
        """Get or set the Vca, Poisson's ratio, ca.
        """ # nopep8
        return self._cards[0].get_value("e4")

    @e4.setter
    def e4(self, value: float) -> None:
        self._cards[0].set_value("e4", value)

    @property
    def e5(self) -> typing.Optional[float]:
        """Get or set the E5, Young's modulus of the yarn in transverse-direction.
        """ # nopep8
        return self._cards[0].get_value("e5")

    @e5.setter
    def e5(self, value: float) -> None:
        self._cards[0].set_value("e5", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[1].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[1].set_value("pr", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the Shear modulus.
        """ # nopep8
        return self._cards[1].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        self._cards[1].set_value("g", value)

    @property
    def g_scl(self) -> float:
        """Get or set the Shear modulus multiplier (default=1.0).
        """ # nopep8
        return self._cards[1].get_value("g_scl")

    @g_scl.setter
    def g_scl(self, value: float) -> None:
        self._cards[1].set_value("g_scl", value)

    @property
    def tsl(self) -> typing.Optional[float]:
        """Get or set the Tensile limit stress.
        """ # nopep8
        return self._cards[1].get_value("tsl")

    @tsl.setter
    def tsl(self, value: float) -> None:
        self._cards[1].set_value("tsl", value)

    @property
    def eps_tan(self) -> typing.Optional[float]:
        """Get or set the Strain at which E=tangent to the polynomial curve.
        """ # nopep8
        return self._cards[1].get_value("eps_tan")

    @eps_tan.setter
    def eps_tan(self, value: float) -> None:
        self._cards[1].set_value("eps_tan", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

