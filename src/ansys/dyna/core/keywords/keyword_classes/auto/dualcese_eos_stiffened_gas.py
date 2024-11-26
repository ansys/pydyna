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

class DualceseEosStiffenedGas(KeywordBase):
    """DYNA DUALCESE_EOS_STIFFENED_GAS keyword"""

    keyword = "DUALCESE"
    subkeyword = "EOS_STIFFENED_GAS"

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
                        "ga",
                        float,
                        10,
                        10,
                        kwargs.get("ga")
                    ),
                    Field(
                        "gb",
                        float,
                        20,
                        10,
                        kwargs.get("gb")
                    ),
                ],
            ),
        ]

    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Wquation of state ID for this dual CESE solver EOS
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        self._cards[0].set_value("eosid", value)

    @property
    def ga(self) -> typing.Optional[float]:
        """Get or set the Adiabatic exponent, Must be>1.0.
        """ # nopep8
        return self._cards[0].get_value("ga")

    @ga.setter
    def ga(self, value: float) -> None:
        self._cards[0].set_value("ga", value)

    @property
    def gb(self) -> typing.Optional[float]:
        """Get or set the Reference pressure, Must be>+0.0
        """ # nopep8
        return self._cards[0].get_value("gb")

    @gb.setter
    def gb(self, value: float) -> None:
        self._cards[0].set_value("gb", value)

