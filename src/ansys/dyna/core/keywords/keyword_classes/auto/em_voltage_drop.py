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

class EmVoltageDrop(KeywordBase):
    """DYNA EM_VOLTAGE_DROP keyword"""

    keyword = "EM"
    subkeyword = "VOLTAGE_DROP"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "vdid",
                        int,
                        0,
                        10,
                        kwargs.get("vdid")
                    ),
                    Field(
                        "vdtype",
                        int,
                        10,
                        10,
                        kwargs.get("vdtype")
                    ),
                    Field(
                        "ssid1",
                        int,
                        20,
                        10,
                        kwargs.get("ssid1")
                    ),
                    Field(
                        "ssid2 ",
                        int,
                        30,
                        10,
                        kwargs.get("ssid2 ")
                    ),
                    Field(
                        "volt ",
                        float,
                        40,
                        10,
                        kwargs.get("volt ")
                    ),
                ],
            ),
        ]

    @property
    def vdid(self) -> typing.Optional[int]:
        """Get or set the Voltage Drop ID
        """ # nopep8
        return self._cards[0].get_value("vdid")

    @vdid.setter
    def vdid(self, value: int) -> None:
        self._cards[0].set_value("vdid", value)

    @property
    def vdtype(self) -> typing.Optional[int]:
        """Get or set the Voltage Drop Type:EQ.1:	Voltage drop between the two corresponding nodes of the two segment sets SSID1 and SSID2.
        """ # nopep8
        return self._cards[0].get_value("vdtype")

    @vdtype.setter
    def vdtype(self, value: int) -> None:
        self._cards[0].set_value("vdtype", value)

    @property
    def ssid1(self) -> typing.Optional[int]:
        """Get or set the Segment Set ID 1
        """ # nopep8
        return self._cards[0].get_value("ssid1")

    @ssid1.setter
    def ssid1(self, value: int) -> None:
        self._cards[0].set_value("ssid1", value)

    @property
    def ssid2_(self) -> typing.Optional[int]:
        """Get or set the Segment Set ID 2
        """ # nopep8
        return self._cards[0].get_value("ssid2 ")

    @ssid2_.setter
    def ssid2_(self, value: int) -> None:
        self._cards[0].set_value("ssid2 ", value)

    @property
    def volt_(self) -> typing.Optional[float]:
        """Get or set the Value of the voltage drop
        """ # nopep8
        return self._cards[0].get_value("volt ")

    @volt_.setter
    def volt_(self, value: float) -> None:
        self._cards[0].set_value("volt ", value)

