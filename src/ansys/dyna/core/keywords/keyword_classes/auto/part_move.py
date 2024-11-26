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

class PartMove(KeywordBase):
    """DYNA PART_MOVE keyword"""

    keyword = "PART"
    subkeyword = "MOVE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        8,
                        kwargs.get("pid")
                    ),
                    Field(
                        "xmov",
                        float,
                        8,
                        16,
                        kwargs.get("xmov")
                    ),
                    Field(
                        "ymov",
                        float,
                        24,
                        16,
                        kwargs.get("ymov")
                    ),
                    Field(
                        "zmov",
                        float,
                        40,
                        16,
                        kwargs.get("zmov")
                    ),
                    Field(
                        "cid",
                        int,
                        56,
                        8,
                        kwargs.get("cid", 0)
                    ),
                    Field(
                        "ifset",
                        int,
                        64,
                        8,
                        kwargs.get("ifset", 0)
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part id or Part Set id
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def xmov(self) -> typing.Optional[float]:
        """Get or set the Move shell part set, in the x-direction by the incremental distance, XMOV.
        """ # nopep8
        return self._cards[0].get_value("xmov")

    @xmov.setter
    def xmov(self, value: float) -> None:
        self._cards[0].set_value("xmov", value)

    @property
    def ymov(self) -> typing.Optional[float]:
        """Get or set the Move shell part set, in the y-direction by the incremental distance, YMOV.
        """ # nopep8
        return self._cards[0].get_value("ymov")

    @ymov.setter
    def ymov(self, value: float) -> None:
        self._cards[0].set_value("ymov", value)

    @property
    def zmov(self) -> typing.Optional[float]:
        """Get or set the Move shell part set, in the z-direction by the incremental distance, ZMOV.
        """ # nopep8
        return self._cards[0].get_value("zmov")

    @zmov.setter
    def zmov(self, value: float) -> None:
        self._cards[0].set_value("zmov", value)

    @property
    def cid(self) -> int:
        """Get or set the Coordinate system ID to define incremental displacement in local coordinate system.  All displacements, XMOV, YMOV, and ZMOV, are with respect to CID.
        EQ.0: global
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[0].set_value("cid", value)

    @property
    def ifset(self) -> int:
        """Get or set the part move flag: 0: part id; 1: part set id
        """ # nopep8
        return self._cards[0].get_value("ifset")

    @ifset.setter
    def ifset(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ifset must be one of {0,1}""")
        self._cards[0].set_value("ifset", value)

