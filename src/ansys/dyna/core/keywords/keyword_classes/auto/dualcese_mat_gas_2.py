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

class DualceseMatGas2(KeywordBase):
    """DYNA DUALCESE_MAT_GAS_2 keyword"""

    keyword = "DUALCESE"
    subkeyword = "MAT_GAS_2"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
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
                        "mu0",
                        float,
                        10,
                        10,
                        kwargs.get("mu0")
                    ),
                    Field(
                        "smu",
                        float,
                        20,
                        10,
                        kwargs.get("smu")
                    ),
                    Field(
                        "k0",
                        float,
                        30,
                        10,
                        kwargs.get("k0")
                    ),
                    Field(
                        "sk",
                        float,
                        40,
                        10,
                        kwargs.get("sk")
                    ),
                    Field(
                        "t0",
                        float,
                        50,
                        10,
                        kwargs.get("t0")
                    ),
                ],
            ),
        ]

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material ID
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def mu0(self) -> typing.Optional[float]:
        """Get or set the Two coefficients appearing
        """ # nopep8
        return self._cards[0].get_value("mu0")

    @mu0.setter
    def mu0(self, value: float) -> None:
        self._cards[0].set_value("mu0", value)

    @property
    def smu(self) -> typing.Optional[float]:
        """Get or set the Two coefficients appearing
        """ # nopep8
        return self._cards[0].get_value("smu")

    @smu.setter
    def smu(self, value: float) -> None:
        self._cards[0].set_value("smu", value)

    @property
    def k0(self) -> typing.Optional[float]:
        """Get or set the Two coefficients appearing
        """ # nopep8
        return self._cards[0].get_value("k0")

    @k0.setter
    def k0(self, value: float) -> None:
        self._cards[0].set_value("k0", value)

    @property
    def sk(self) -> typing.Optional[float]:
        """Get or set the Two coefficients appearing
        """ # nopep8
        return self._cards[0].get_value("sk")

    @sk.setter
    def sk(self, value: float) -> None:
        self._cards[0].set_value("sk", value)

    @property
    def t0(self) -> typing.Optional[float]:
        """Get or set the Reference temperature
        """ # nopep8
        return self._cards[0].get_value("t0")

    @t0.setter
    def t0(self, value: float) -> None:
        self._cards[0].set_value("t0", value)

