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

class EmCircuitRogo(KeywordBase):
    """DYNA EM_CIRCUIT_ROGO keyword"""

    keyword = "EM"
    subkeyword = "CIRCUIT_ROGO"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "rogoid",
                        int,
                        0,
                        10,
                        kwargs.get("rogoid")
                    ),
                    Field(
                        "setid",
                        int,
                        10,
                        10,
                        kwargs.get("setid")
                    ),
                    Field(
                        "settype",
                        int,
                        20,
                        10,
                        kwargs.get("settype", 1)
                    ),
                    Field(
                        "curtyp",
                        int,
                        30,
                        10,
                        kwargs.get("curtyp", 1)
                    ),
                ],
            ),
        ]

    @property
    def rogoid(self) -> typing.Optional[int]:
        """Get or set the Rogowsky coil ID.
        """ # nopep8
        return self._cards[0].get_value("rogoid")

    @rogoid.setter
    def rogoid(self, value: int) -> None:
        self._cards[0].set_value("rogoid", value)

    @property
    def setid(self) -> typing.Optional[int]:
        """Get or set the Segment or node set ID.
        """ # nopep8
        return self._cards[0].get_value("setid")

    @setid.setter
    def setid(self, value: int) -> None:
        self._cards[0].set_value("setid", value)

    @property
    def settype(self) -> int:
        """Get or set the Type of set:
        EQ.1: Segment set
        EQ.2: Node set (not available yet)

        """ # nopep8
        return self._cards[0].get_value("settype")

    @settype.setter
    def settype(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""settype must be one of {1,2}""")
        self._cards[0].set_value("settype", value)

    @property
    def curtyp(self) -> int:
        """Get or set the Type of current measured:
        EQ.1: Volume current
        EQ.2: Surface current (not available yet_
        EQ.3: Magnetic field flow (B field times Area)
        """ # nopep8
        return self._cards[0].get_value("curtyp")

    @curtyp.setter
    def curtyp(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""curtyp must be one of {1,2,3}""")
        self._cards[0].set_value("curtyp", value)

