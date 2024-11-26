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

class Eos001(KeywordBase):
    """DYNA EOS_001 keyword"""

    keyword = "EOS"
    subkeyword = "001"

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
                        "c0",
                        float,
                        10,
                        10,
                        kwargs.get("c0", 0.0)
                    ),
                    Field(
                        "c1",
                        float,
                        20,
                        10,
                        kwargs.get("c1", 0.0)
                    ),
                    Field(
                        "c2",
                        float,
                        30,
                        10,
                        kwargs.get("c2", 0.0)
                    ),
                    Field(
                        "c3",
                        float,
                        40,
                        10,
                        kwargs.get("c3", 0.0)
                    ),
                    Field(
                        "c4",
                        float,
                        50,
                        10,
                        kwargs.get("c4", 0.0)
                    ),
                    Field(
                        "c5",
                        float,
                        60,
                        10,
                        kwargs.get("c5", 0.0)
                    ),
                    Field(
                        "c6",
                        float,
                        70,
                        10,
                        kwargs.get("c6", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "e0",
                        float,
                        0,
                        10,
                        kwargs.get("e0")
                    ),
                    Field(
                        "v0",
                        float,
                        10,
                        10,
                        kwargs.get("v0")
                    ),
                ],
            ),
        ]

    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state ID. A unique number or label must be specified (see *PART).
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        self._cards[0].set_value("eosid", value)

    @property
    def c0(self) -> float:
        """Get or set the The 0th polynomial equation coefficient
        """ # nopep8
        return self._cards[0].get_value("c0")

    @c0.setter
    def c0(self, value: float) -> None:
        self._cards[0].set_value("c0", value)

    @property
    def c1(self) -> float:
        """Get or set the The 1st polynomial equation coefficient (when used by itself, this is the elastic bulk modulus, meaning it cannot be used for deformation that is beyond the elastic regime).
        """ # nopep8
        return self._cards[0].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        self._cards[0].set_value("c1", value)

    @property
    def c2(self) -> float:
        """Get or set the The 2st polynomial equation coefficient
        """ # nopep8
        return self._cards[0].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        self._cards[0].set_value("c2", value)

    @property
    def c3(self) -> float:
        """Get or set the The 3rd polynomial equation coefficient
        """ # nopep8
        return self._cards[0].get_value("c3")

    @c3.setter
    def c3(self, value: float) -> None:
        self._cards[0].set_value("c3", value)

    @property
    def c4(self) -> float:
        """Get or set the The 4th polynomial equation coefficient
        """ # nopep8
        return self._cards[0].get_value("c4")

    @c4.setter
    def c4(self, value: float) -> None:
        self._cards[0].set_value("c4", value)

    @property
    def c5(self) -> float:
        """Get or set the The 5th polynomial equation coefficient
        """ # nopep8
        return self._cards[0].get_value("c5")

    @c5.setter
    def c5(self, value: float) -> None:
        self._cards[0].set_value("c5", value)

    @property
    def c6(self) -> float:
        """Get or set the The 6th polynomial equation coefficient
        """ # nopep8
        return self._cards[0].get_value("c6")

    @c6.setter
    def c6(self, value: float) -> None:
        self._cards[0].set_value("c6", value)

    @property
    def e0(self) -> typing.Optional[float]:
        """Get or set the Initial internal energy per unit reference volume (see the beginning of the *EOS section)
        """ # nopep8
        return self._cards[1].get_value("e0")

    @e0.setter
    def e0(self, value: float) -> None:
        self._cards[1].set_value("e0", value)

    @property
    def v0(self) -> typing.Optional[float]:
        """Get or set the Initial relative volume (see the beginning of the *EOS section).
        """ # nopep8
        return self._cards[1].get_value("v0")

    @v0.setter
    def v0(self, value: float) -> None:
        self._cards[1].set_value("v0", value)

