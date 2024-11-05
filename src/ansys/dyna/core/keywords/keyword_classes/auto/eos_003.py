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

class Eos003(KeywordBase):
    """DYNA EOS_003 keyword"""

    keyword = "EOS"
    subkeyword = "003"

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
                        "a1",
                        float,
                        10,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "a2",
                        float,
                        20,
                        10,
                        kwargs.get("a2")
                    ),
                    Field(
                        "a3",
                        float,
                        30,
                        10,
                        kwargs.get("a3")
                    ),
                    Field(
                        "b1",
                        float,
                        40,
                        10,
                        kwargs.get("b1")
                    ),
                    Field(
                        "b2",
                        float,
                        50,
                        10,
                        kwargs.get("b2")
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
        ]

    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state label
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        self._cards[0].set_value("eosid", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Constants in the equation of state
        """ # nopep8
        return self._cards[0].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[0].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Constants in the equation of state
        """ # nopep8
        return self._cards[0].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[0].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Constants in the equation of state
        """ # nopep8
        return self._cards[0].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[0].set_value("a3", value)

    @property
    def b1(self) -> typing.Optional[float]:
        """Get or set the Constants in the equation of state
        """ # nopep8
        return self._cards[0].get_value("b1")

    @b1.setter
    def b1(self, value: float) -> None:
        self._cards[0].set_value("b1", value)

    @property
    def b2(self) -> typing.Optional[float]:
        """Get or set the Constants in the equation of state
        """ # nopep8
        return self._cards[0].get_value("b2")

    @b2.setter
    def b2(self, value: float) -> None:
        self._cards[0].set_value("b2", value)

    @property
    def e0(self) -> typing.Optional[float]:
        """Get or set the Initial internal energy.
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

