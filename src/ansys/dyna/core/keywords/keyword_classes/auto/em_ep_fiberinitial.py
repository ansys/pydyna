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

class EmEpFiberinitial(KeywordBase):
    """DYNA EM_EP_FIBERINITIAL keyword"""

    keyword = "EM"
    subkeyword = "EP_FIBERINITIAL"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "id",
                        int,
                        0,
                        10,
                        kwargs.get("id")
                    ),
                    Field(
                        "partid",
                        int,
                        10,
                        10,
                        kwargs.get("partid")
                    ),
                    Field(
                        "stype",
                        int,
                        20,
                        10,
                        kwargs.get("stype", 1)
                    ),
                    Field(
                        "ssid1",
                        int,
                        30,
                        10,
                        kwargs.get("ssid1")
                    ),
                    Field(
                        "ssid0",
                        int,
                        40,
                        10,
                        kwargs.get("ssid0")
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the ID of the Laplace system to solve (define new id with each new line)
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def partid(self) -> typing.Optional[int]:
        """Get or set the Part id on which the system is solved
        """ # nopep8
        return self._cards[0].get_value("partid")

    @partid.setter
    def partid(self, value: int) -> None:
        self._cards[0].set_value("partid", value)

    @property
    def stype(self) -> int:
        """Get or set the Segment type:
        EQ.1:	node set
        EQ.2 : segment set
        """ # nopep8
        return self._cards[0].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""stype must be one of {1,2}""")
        self._cards[0].set_value("stype", value)

    @property
    def ssid1(self) -> typing.Optional[int]:
        """Get or set the Set on which a potential of value 1 is prescribed
        """ # nopep8
        return self._cards[0].get_value("ssid1")

    @ssid1.setter
    def ssid1(self, value: int) -> None:
        self._cards[0].set_value("ssid1", value)

    @property
    def ssid0(self) -> typing.Optional[int]:
        """Get or set the Set on which a potential of value 0 is prescribed
        """ # nopep8
        return self._cards[0].get_value("ssid0")

    @ssid0.setter
    def ssid0(self, value: int) -> None:
        self._cards[0].set_value("ssid0", value)

