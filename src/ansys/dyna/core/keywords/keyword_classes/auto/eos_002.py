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

class Eos002(KeywordBase):
    """DYNA EOS_002 keyword"""

    keyword = "EOS"
    subkeyword = "002"

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
                        "a",
                        float,
                        10,
                        10,
                        kwargs.get("a")
                    ),
                    Field(
                        "b",
                        float,
                        20,
                        10,
                        kwargs.get("b")
                    ),
                    Field(
                        "r1",
                        float,
                        30,
                        10,
                        kwargs.get("r1")
                    ),
                    Field(
                        "r2",
                        float,
                        40,
                        10,
                        kwargs.get("r2")
                    ),
                    Field(
                        "omeg",
                        float,
                        50,
                        10,
                        kwargs.get("omeg")
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
        """Get or set the Equation of state ID.
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        self._cards[0].set_value("eosid", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Equation of state coefficient, A.
        """ # nopep8
        return self._cards[0].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        self._cards[0].set_value("a", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Equation of state coefficient, B.
        """ # nopep8
        return self._cards[0].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        self._cards[0].set_value("b", value)

    @property
    def r1(self) -> typing.Optional[float]:
        """Get or set the Equation of state coefficient, R1.
        """ # nopep8
        return self._cards[0].get_value("r1")

    @r1.setter
    def r1(self, value: float) -> None:
        self._cards[0].set_value("r1", value)

    @property
    def r2(self) -> typing.Optional[float]:
        """Get or set the Equation of state coefficient, R2.
        """ # nopep8
        return self._cards[0].get_value("r2")

    @r2.setter
    def r2(self, value: float) -> None:
        self._cards[0].set_value("r2", value)

    @property
    def omeg(self) -> typing.Optional[float]:
        """Get or set the Equation of state coefficient, w.
        """ # nopep8
        return self._cards[0].get_value("omeg")

    @omeg.setter
    def omeg(self, value: float) -> None:
        self._cards[0].set_value("omeg", value)

    @property
    def e0(self) -> typing.Optional[float]:
        """Get or set the Detonation energy per unit volume and initial value for E. See equation in Remarks.
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

