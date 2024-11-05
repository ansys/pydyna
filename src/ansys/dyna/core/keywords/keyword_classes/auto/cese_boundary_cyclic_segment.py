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

class CeseBoundaryCyclicSegment(KeywordBase):
    """DYNA CESE_BOUNDARY_CYCLIC_SEGMENT keyword"""

    keyword = "CESE"
    subkeyword = "BOUNDARY_CYCLIC_SEGMENT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nd1",
                        int,
                        0,
                        10,
                        kwargs.get("nd1")
                    ),
                    Field(
                        "nd2",
                        int,
                        10,
                        10,
                        kwargs.get("nd2")
                    ),
                    Field(
                        "nd3",
                        int,
                        20,
                        10,
                        kwargs.get("nd3")
                    ),
                    Field(
                        "nd4",
                        int,
                        30,
                        10,
                        kwargs.get("nd4")
                    ),
                    Field(
                        "np1",
                        int,
                        40,
                        10,
                        kwargs.get("np1")
                    ),
                    Field(
                        "np2",
                        int,
                        50,
                        10,
                        kwargs.get("np2")
                    ),
                    Field(
                        "np3",
                        int,
                        60,
                        10,
                        kwargs.get("np3")
                    ),
                    Field(
                        "np4",
                        int,
                        70,
                        10,
                        kwargs.get("np4")
                    ),
                ],
            ),
        ]

    @property
    def nd1(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining segment.
        """ # nopep8
        return self._cards[0].get_value("nd1")

    @nd1.setter
    def nd1(self, value: int) -> None:
        self._cards[0].set_value("nd1", value)

    @property
    def nd2(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining segment.
        """ # nopep8
        return self._cards[0].get_value("nd2")

    @nd2.setter
    def nd2(self, value: int) -> None:
        self._cards[0].set_value("nd2", value)

    @property
    def nd3(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining segment.
        """ # nopep8
        return self._cards[0].get_value("nd3")

    @nd3.setter
    def nd3(self, value: int) -> None:
        self._cards[0].set_value("nd3", value)

    @property
    def nd4(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining segment.
        """ # nopep8
        return self._cards[0].get_value("nd4")

    @nd4.setter
    def nd4(self, value: int) -> None:
        self._cards[0].set_value("nd4", value)

    @property
    def np1(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining segment.
        """ # nopep8
        return self._cards[0].get_value("np1")

    @np1.setter
    def np1(self, value: int) -> None:
        self._cards[0].set_value("np1", value)

    @property
    def np2(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining segment.
        """ # nopep8
        return self._cards[0].get_value("np2")

    @np2.setter
    def np2(self, value: int) -> None:
        self._cards[0].set_value("np2", value)

    @property
    def np3(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining segment.
        """ # nopep8
        return self._cards[0].get_value("np3")

    @np3.setter
    def np3(self, value: int) -> None:
        self._cards[0].set_value("np3", value)

    @property
    def np4(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining segment.
        """ # nopep8
        return self._cards[0].get_value("np4")

    @np4.setter
    def np4(self, value: int) -> None:
        self._cards[0].set_value("np4", value)

