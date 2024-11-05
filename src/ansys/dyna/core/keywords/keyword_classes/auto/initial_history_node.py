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

class InitialHistoryNode(KeywordBase):
    """DYNA INITIAL_HISTORY_NODE keyword"""

    keyword = "INITIAL"
    subkeyword = "HISTORY_NODE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nid",
                        int,
                        0,
                        10,
                        kwargs.get("nid")
                    ),
                    Field(
                        "nhisv",
                        int,
                        10,
                        10,
                        kwargs.get("nhisv")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "hindex",
                        int,
                        0,
                        10,
                        kwargs.get("hindex")
                    ),
                    Field(
                        "val",
                        float,
                        10,
                        10,
                        kwargs.get("val", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node id.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[0].set_value("nid", value)

    @property
    def nhisv(self) -> typing.Optional[int]:
        """Get or set the Number of history variables to be initialized
        """ # nopep8
        return self._cards[0].get_value("nhisv")

    @nhisv.setter
    def nhisv(self, value: int) -> None:
        self._cards[0].set_value("nhisv", value)

    @property
    def hindex(self) -> typing.Optional[int]:
        """Get or set the Define the index in the history variable vector
        """ # nopep8
        return self._cards[1].get_value("hindex")

    @hindex.setter
    def hindex(self, value: int) -> None:
        self._cards[1].set_value("hindex", value)

    @property
    def val(self) -> float:
        """Get or set the Define the value of the history variable
        """ # nopep8
        return self._cards[1].get_value("val")

    @val.setter
    def val(self, value: float) -> None:
        self._cards[1].set_value("val", value)

