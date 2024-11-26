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

class MatAddFatigueEn(KeywordBase):
    """DYNA MAT_ADD_FATIGUE_EN keyword"""

    keyword = "MAT"
    subkeyword = "ADD_FATIGUE_EN"
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
                        "kp",
                        float,
                        10,
                        10,
                        kwargs.get("kp")
                    ),
                    Field(
                        "np",
                        float,
                        20,
                        10,
                        kwargs.get("np")
                    ),
                    Field(
                        "sigmaf",
                        float,
                        30,
                        10,
                        kwargs.get("sigmaf")
                    ),
                    Field(
                        "epsp",
                        float,
                        40,
                        10,
                        kwargs.get("epsp")
                    ),
                    Field(
                        "bp",
                        float,
                        50,
                        10,
                        kwargs.get("bp")
                    ),
                    Field(
                        "cp",
                        float,
                        60,
                        10,
                        kwargs.get("cp")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "e",
                        int,
                        0,
                        10,
                        kwargs.get("e")
                    ),
                    Field(
                        "pr",
                        float,
                        10,
                        10,
                        kwargs.get("pr")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatAddFatigueEn.option_specs[0],
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
    def kp(self) -> typing.Optional[float]:
        """Get or set the K^', the cyclic strength coefficient
        """ # nopep8
        return self._cards[0].get_value("kp")

    @kp.setter
    def kp(self, value: float) -> None:
        self._cards[0].set_value("kp", value)

    @property
    def np(self) -> typing.Optional[float]:
        """Get or set the N^', the cyclic strain hardening exponent
        """ # nopep8
        return self._cards[0].get_value("np")

    @np.setter
    def np(self, value: float) -> None:
        self._cards[0].set_value("np", value)

    @property
    def sigmaf(self) -> typing.Optional[float]:
        """Get or set the σ_f^', the fatigue strength coefficient
        """ # nopep8
        return self._cards[0].get_value("sigmaf")

    @sigmaf.setter
    def sigmaf(self, value: float) -> None:
        self._cards[0].set_value("sigmaf", value)

    @property
    def epsp(self) -> typing.Optional[float]:
        """Get or set the ε_f^', the fatigue ductility coefficient
        """ # nopep8
        return self._cards[0].get_value("epsp")

    @epsp.setter
    def epsp(self, value: float) -> None:
        self._cards[0].set_value("epsp", value)

    @property
    def bp(self) -> typing.Optional[float]:
        """Get or set the b^', the fatigue strength exponent (Basquin’s exponent)
        """ # nopep8
        return self._cards[0].get_value("bp")

    @bp.setter
    def bp(self, value: float) -> None:
        self._cards[0].set_value("bp", value)

    @property
    def cp(self) -> typing.Optional[float]:
        """Get or set the c^', the fatigue ductility exponent (Coffin-Manson exponent)
        """ # nopep8
        return self._cards[0].get_value("cp")

    @cp.setter
    def cp(self, value: float) -> None:
        self._cards[0].set_value("cp", value)

    @property
    def e(self) -> typing.Optional[int]:
        """Get or set the Young’s modulus
        """ # nopep8
        return self._cards[1].get_value("e")

    @e.setter
    def e(self, value: int) -> None:
        self._cards[1].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson’s ratio.
        """ # nopep8
        return self._cards[1].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[1].set_value("pr", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

