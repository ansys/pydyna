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

class LoadThermalConstant(KeywordBase):
    """DYNA LOAD_THERMAL_CONSTANT keyword"""

    keyword = "LOAD"
    subkeyword = "THERMAL_CONSTANT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nsid",
                        int,
                        0,
                        10,
                        kwargs.get("nsid")
                    ),
                    Field(
                        "nsidex",
                        int,
                        10,
                        10,
                        kwargs.get("nsidex", 0)
                    ),
                    Field(
                        "boxid",
                        int,
                        20,
                        10,
                        kwargs.get("boxid", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "t",
                        float,
                        0,
                        10,
                        kwargs.get("t", 0.0)
                    ),
                    Field(
                        "te",
                        float,
                        10,
                        10,
                        kwargs.get("te", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID containing nodes for initial temperature, see *SET_NODE:
        EQ.0: all nodes are included.
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        self._cards[0].set_value("nsid", value)

    @property
    def nsidex(self) -> int:
        """Get or set the Node set ID containing nodes that are exempted from the imposed temperature (optional).
        """ # nopep8
        return self._cards[0].get_value("nsidex")

    @nsidex.setter
    def nsidex(self, value: int) -> None:
        self._cards[0].set_value("nsidex", value)

    @property
    def boxid(self) -> int:
        """Get or set the All nodes in box which belong to NSID are initialized. Others are excluded (optional).
        """ # nopep8
        return self._cards[0].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        self._cards[0].set_value("boxid", value)

    @property
    def t(self) -> float:
        """Get or set the Temperature.
        """ # nopep8
        return self._cards[1].get_value("t")

    @t.setter
    def t(self, value: float) -> None:
        self._cards[1].set_value("t", value)

    @property
    def te(self) -> float:
        """Get or set the Temperature of exempted nodes (optional).
        """ # nopep8
        return self._cards[1].get_value("te")

    @te.setter
    def te(self, value: float) -> None:
        self._cards[1].set_value("te", value)

