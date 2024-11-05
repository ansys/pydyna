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

class Eos012(KeywordBase):
    """DYNA EOS_012 keyword"""

    keyword = "EOS"
    subkeyword = "012"

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
                        "cv0",
                        float,
                        10,
                        10,
                        kwargs.get("cv0")
                    ),
                    Field(
                        "cp0",
                        float,
                        20,
                        10,
                        kwargs.get("cp0")
                    ),
                    Field(
                        "cl",
                        float,
                        30,
                        10,
                        kwargs.get("cl")
                    ),
                    Field(
                        "cq",
                        float,
                        40,
                        10,
                        kwargs.get("cq")
                    ),
                    Field(
                        "t0",
                        float,
                        50,
                        10,
                        kwargs.get("t0")
                    ),
                    Field(
                        "v0",
                        float,
                        60,
                        10,
                        kwargs.get("v0")
                    ),
                    Field(
                        "vc0",
                        float,
                        70,
                        10,
                        kwargs.get("vc0")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "adiab",
                        float,
                        0,
                        10,
                        kwargs.get("adiab")
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
    def cv0(self) -> typing.Optional[float]:
        """Get or set the Specific heat constant for definition of Cv
        """ # nopep8
        return self._cards[0].get_value("cv0")

    @cv0.setter
    def cv0(self, value: float) -> None:
        self._cards[0].set_value("cv0", value)

    @property
    def cp0(self) -> typing.Optional[float]:
        """Get or set the Specific heat constant for definition of Cp
        """ # nopep8
        return self._cards[0].get_value("cp0")

    @cp0.setter
    def cp0(self, value: float) -> None:
        self._cards[0].set_value("cp0", value)

    @property
    def cl(self) -> typing.Optional[float]:
        """Get or set the Specific heat constant for linear Cv and Cp
        """ # nopep8
        return self._cards[0].get_value("cl")

    @cl.setter
    def cl(self, value: float) -> None:
        self._cards[0].set_value("cl", value)

    @property
    def cq(self) -> typing.Optional[float]:
        """Get or set the Specific heat constant for quadratic Cv nd Cp
        """ # nopep8
        return self._cards[0].get_value("cq")

    @cq.setter
    def cq(self, value: float) -> None:
        self._cards[0].set_value("cq", value)

    @property
    def t0(self) -> typing.Optional[float]:
        """Get or set the Initial temperature
        """ # nopep8
        return self._cards[0].get_value("t0")

    @t0.setter
    def t0(self, value: float) -> None:
        self._cards[0].set_value("t0", value)

    @property
    def v0(self) -> typing.Optional[float]:
        """Get or set the Initial relative volume
        """ # nopep8
        return self._cards[0].get_value("v0")

    @v0.setter
    def v0(self, value: float) -> None:
        self._cards[0].set_value("v0", value)

    @property
    def vc0(self) -> typing.Optional[float]:
        """Get or set the Van der Waals covolume
        """ # nopep8
        return self._cards[0].get_value("vc0")

    @vc0.setter
    def vc0(self, value: float) -> None:
        self._cards[0].set_value("vc0", value)

    @property
    def adiab(self) -> typing.Optional[float]:
        """Get or set the Adiabatic flag:
        EQ.0.0: off
        EQ.1.0: on; ideal gas follows adiabatic law
        """ # nopep8
        return self._cards[1].get_value("adiab")

    @adiab.setter
    def adiab(self, value: float) -> None:
        self._cards[1].set_value("adiab", value)

