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

class EmEosMeadon(KeywordBase):
    """DYNA EM_EOS_MEADON keyword"""

    keyword = "EM"
    subkeyword = "EOS_MEADON"

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
                        "c1",
                        float,
                        10,
                        10,
                        kwargs.get("c1")
                    ),
                    Field(
                        "c2",
                        float,
                        20,
                        10,
                        kwargs.get("c2")
                    ),
                    Field(
                        "c3",
                        float,
                        30,
                        10,
                        kwargs.get("c3")
                    ),
                    Field(
                        "temuni",
                        int,
                        40,
                        10,
                        kwargs.get("temuni", 1)
                    ),
                    Field(
                        "v0",
                        float,
                        50,
                        10,
                        kwargs.get("v0")
                    ),
                    Field(
                        "gamma",
                        float,
                        60,
                        10,
                        kwargs.get("gamma")
                    ),
                    Field(
                        "expon",
                        int,
                        70,
                        10,
                        kwargs.get("expon")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lgtunit",
                        float,
                        0,
                        10,
                        kwargs.get("lgtunit")
                    ),
                    Field(
                        "timunit",
                        float,
                        10,
                        10,
                        kwargs.get("timunit")
                    ),
                    Field(
                        "adjust",
                        int,
                        20,
                        10,
                        kwargs.get("adjust", 0)
                    ),
                ],
            ),
        ]

    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Id of the EM_EOS.
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        self._cards[0].set_value("eosid", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the C1 constant (BUS).
        """ # nopep8
        return self._cards[0].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        self._cards[0].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the C2 constant (no units).
        """ # nopep8
        return self._cards[0].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        self._cards[0].set_value("c2", value)

    @property
    def c3(self) -> typing.Optional[float]:
        """Get or set the C3 constant (no units).
        """ # nopep8
        return self._cards[0].get_value("c3")

    @c3.setter
    def c3(self, value: float) -> None:
        self._cards[0].set_value("c3", value)

    @property
    def temuni(self) -> int:
        """Get or set the Temperature units
        =1: temperature in Celsius
        =2: temperature in Kelvins
        .
        """ # nopep8
        return self._cards[0].get_value("temuni")

    @temuni.setter
    def temuni(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""temuni must be one of {1,2}""")
        self._cards[0].set_value("temuni", value)

    @property
    def v0(self) -> typing.Optional[float]:
        """Get or set the reference specific volume V0 (UUS).
        """ # nopep8
        return self._cards[0].get_value("v0")

    @v0.setter
    def v0(self, value: float) -> None:
        self._cards[0].set_value("v0", value)

    @property
    def gamma(self) -> typing.Optional[float]:
        """Get or set the Reference Gruneisen value r0.(no units).
        """ # nopep8
        return self._cards[0].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        self._cards[0].set_value("gamma", value)

    @property
    def expon(self) -> typing.Optional[int]:
        """Get or set the exponent in equations (2).
        """ # nopep8
        return self._cards[0].get_value("expon")

    @expon.setter
    def expon(self, value: int) -> None:
        self._cards[0].set_value("expon", value)

    @property
    def lgtunit(self) -> typing.Optional[float]:
        """Get or set the Length units for UUS (relative to meter, i.e. =1.e-3 if UUS in mm).
        """ # nopep8
        return self._cards[1].get_value("lgtunit")

    @lgtunit.setter
    def lgtunit(self, value: float) -> None:
        self._cards[1].set_value("lgtunit", value)

    @property
    def timunit(self) -> typing.Optional[float]:
        """Get or set the Time units for UUS (relative to seconds).
        """ # nopep8
        return self._cards[1].get_value("timunit")

    @timunit.setter
    def timunit(self, value: float) -> None:
        self._cards[1].set_value("timunit", value)

    @property
    def adjust(self) -> int:
        """Get or set the ADJUST:
        = 0 (default): the conductivity is given by the Burgess formula.
        = 1: The conductivity is adjusted so that it is equal to the conductivity defined in *EM_MAT card   at room temperature
        .
        """ # nopep8
        return self._cards[1].get_value("adjust")

    @adjust.setter
    def adjust(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""adjust must be one of {0,1}""")
        self._cards[1].set_value("adjust", value)

