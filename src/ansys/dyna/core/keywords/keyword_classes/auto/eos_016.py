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

class Eos016(KeywordBase):
    """DYNA EOS_016 keyword"""

    keyword = "EOS"
    subkeyword = "016"

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
                        "gamma",
                        float,
                        10,
                        10,
                        kwargs.get("gamma")
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
                        "a3",
                        float,
                        40,
                        10,
                        kwargs.get("a3")
                    ),
                    Field(
                        "pel",
                        float,
                        50,
                        10,
                        kwargs.get("pel")
                    ),
                    Field(
                        "pco",
                        float,
                        60,
                        10,
                        kwargs.get("pco")
                    ),
                    Field(
                        "n",
                        float,
                        70,
                        10,
                        kwargs.get("n")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "alpha0",
                        float,
                        0,
                        10,
                        kwargs.get("alpha0")
                    ),
                    Field(
                        "e0",
                        float,
                        10,
                        10,
                        kwargs.get("e0")
                    ),
                    Field(
                        "v0",
                        float,
                        20,
                        10,
                        kwargs.get("v0")
                    ),
                ],
            ),
        ]

    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state ID, a unique number or label must be specified.
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        self._cards[0].set_value("eosid", value)

    @property
    def gamma(self) -> typing.Optional[float]:
        """Get or set the Gruneisen gamma.
        """ # nopep8
        return self._cards[0].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        self._cards[0].set_value("gamma", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Hugoniot polynomial coefficient
        """ # nopep8
        return self._cards[0].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[0].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Hugoniot polynomial coefficient.
        """ # nopep8
        return self._cards[0].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[0].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Hugoniot polynomial coefficient.
        """ # nopep8
        return self._cards[0].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[0].set_value("a3", value)

    @property
    def pel(self) -> typing.Optional[float]:
        """Get or set the Crush pressure.
        """ # nopep8
        return self._cards[0].get_value("pel")

    @pel.setter
    def pel(self, value: float) -> None:
        self._cards[0].set_value("pel", value)

    @property
    def pco(self) -> typing.Optional[float]:
        """Get or set the Compaction pressure.
        """ # nopep8
        return self._cards[0].get_value("pco")

    @pco.setter
    def pco(self, value: float) -> None:
        self._cards[0].set_value("pco", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Porosity exponent.
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        self._cards[0].set_value("n", value)

    @property
    def alpha0(self) -> typing.Optional[float]:
        """Get or set the Initial porosity.
        """ # nopep8
        return self._cards[1].get_value("alpha0")

    @alpha0.setter
    def alpha0(self, value: float) -> None:
        self._cards[1].set_value("alpha0", value)

    @property
    def e0(self) -> typing.Optional[float]:
        """Get or set the Initial internal energy.
        """ # nopep8
        return self._cards[1].get_value("e0")

    @e0.setter
    def e0(self, value: float) -> None:
        self._cards[1].set_value("e0", value)

    @property
    def v0(self) -> typing.Optional[float]:
        """Get or set the Initial relative volume.
        """ # nopep8
        return self._cards[1].get_value("v0")

    @v0.setter
    def v0(self, value: float) -> None:
        self._cards[1].set_value("v0", value)

