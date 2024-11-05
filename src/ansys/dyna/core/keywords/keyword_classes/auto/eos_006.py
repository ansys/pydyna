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

class Eos006(KeywordBase):
    """DYNA EOS_006 keyword"""

    keyword = "EOS"
    subkeyword = "006"

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
                        kwargs.get("c0")
                    ),
                    Field(
                        "c1",
                        float,
                        20,
                        10,
                        kwargs.get("c1")
                    ),
                    Field(
                        "c2",
                        float,
                        30,
                        10,
                        kwargs.get("c2")
                    ),
                    Field(
                        "c3",
                        float,
                        40,
                        10,
                        kwargs.get("c3")
                    ),
                    Field(
                        "c4",
                        float,
                        50,
                        10,
                        kwargs.get("c4")
                    ),
                    Field(
                        "c5",
                        float,
                        60,
                        10,
                        kwargs.get("c5")
                    ),
                    Field(
                        "c6",
                        float,
                        70,
                        10,
                        kwargs.get("c6")
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
                    Field(
                        "lcid",
                        int,
                        20,
                        10,
                        kwargs.get("lcid")
                    ),
                ],
            ),
        ]

    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state label.
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        self._cards[0].set_value("eosid", value)

    @property
    def c0(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("c0")

    @c0.setter
    def c0(self, value: float) -> None:
        self._cards[0].set_value("c0", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        self._cards[0].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        self._cards[0].set_value("c2", value)

    @property
    def c3(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("c3")

    @c3.setter
    def c3(self, value: float) -> None:
        self._cards[0].set_value("c3", value)

    @property
    def c4(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("c4")

    @c4.setter
    def c4(self, value: float) -> None:
        self._cards[0].set_value("c4", value)

    @property
    def c5(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("c5")

    @c5.setter
    def c5(self, value: float) -> None:
        self._cards[0].set_value("c5", value)

    @property
    def c6(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("c6")

    @c6.setter
    def c6(self, value: float) -> None:
        self._cards[0].set_value("c6", value)

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

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID, which can be the ID of *DEFINE_‌CURVE,
        *DEFINE_‌CURVE_‌FUNCTION or *DEFINE_‌FUNCTION, defining the energy deposition rate.
        If an energy leak rate is intended, do not specify a negative ordinate in LCID,
        rather, use the constant(s) in the equation of state, e.g., set C4 to a negative value.
        If *DEFINE_‌FUNCTION is used, the input of the defined function is time.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[1].set_value("lcid", value)

