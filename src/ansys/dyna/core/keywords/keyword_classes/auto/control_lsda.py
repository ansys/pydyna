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

class ControlLsda(KeywordBase):
    """DYNA CONTROL_LSDA keyword"""

    keyword = "CONTROL"
    subkeyword = "LSDA"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "npexcl",
                        int,
                        0,
                        10,
                        kwargs.get("npexcl")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "p1",
                        int,
                        0,
                        10,
                        kwargs.get("p1")
                    ),
                    Field(
                        "p2",
                        int,
                        10,
                        10,
                        kwargs.get("p2")
                    ),
                    Field(
                        "p3",
                        int,
                        20,
                        10,
                        kwargs.get("p3")
                    ),
                    Field(
                        "p4",
                        int,
                        30,
                        10,
                        kwargs.get("p4")
                    ),
                    Field(
                        "p5",
                        int,
                        40,
                        10,
                        kwargs.get("p5")
                    ),
                    Field(
                        "p6",
                        int,
                        50,
                        10,
                        kwargs.get("p6")
                    ),
                    Field(
                        "p7",
                        int,
                        60,
                        10,
                        kwargs.get("p7")
                    ),
                    Field(
                        "p8",
                        int,
                        70,
                        10,
                        kwargs.get("p8")
                    ),
                ],
            ),
        ]

    @property
    def npexcl(self) -> typing.Optional[int]:
        """Get or set the Number of parts to exclude from the dynain.lsda files.
        """ # nopep8
        return self._cards[0].get_value("npexcl")

    @npexcl.setter
    def npexcl(self, value: int) -> None:
        self._cards[0].set_value("npexcl", value)

    @property
    def p1(self) -> typing.Optional[int]:
        """Get or set the Define parts to exclude from dynain.lsda when included from a keyword input deck.
        """ # nopep8
        return self._cards[1].get_value("p1")

    @p1.setter
    def p1(self, value: int) -> None:
        self._cards[1].set_value("p1", value)

    @property
    def p2(self) -> typing.Optional[int]:
        """Get or set the Define parts to exclude from dynain.lsda when included from a keyword input deck.
        """ # nopep8
        return self._cards[1].get_value("p2")

    @p2.setter
    def p2(self, value: int) -> None:
        self._cards[1].set_value("p2", value)

    @property
    def p3(self) -> typing.Optional[int]:
        """Get or set the Define parts to exclude from dynain.lsda when included from a keyword input deck.
        """ # nopep8
        return self._cards[1].get_value("p3")

    @p3.setter
    def p3(self, value: int) -> None:
        self._cards[1].set_value("p3", value)

    @property
    def p4(self) -> typing.Optional[int]:
        """Get or set the Define parts to exclude from dynain.lsda when included from a keyword input deck.
        """ # nopep8
        return self._cards[1].get_value("p4")

    @p4.setter
    def p4(self, value: int) -> None:
        self._cards[1].set_value("p4", value)

    @property
    def p5(self) -> typing.Optional[int]:
        """Get or set the Define parts to exclude from dynain.lsda when included from a keyword input deck.
        """ # nopep8
        return self._cards[1].get_value("p5")

    @p5.setter
    def p5(self, value: int) -> None:
        self._cards[1].set_value("p5", value)

    @property
    def p6(self) -> typing.Optional[int]:
        """Get or set the Define parts to exclude from dynain.lsda when included from a keyword input deck.
        """ # nopep8
        return self._cards[1].get_value("p6")

    @p6.setter
    def p6(self, value: int) -> None:
        self._cards[1].set_value("p6", value)

    @property
    def p7(self) -> typing.Optional[int]:
        """Get or set the Define parts to exclude from dynain.lsda when included from a keyword input deck.
        """ # nopep8
        return self._cards[1].get_value("p7")

    @p7.setter
    def p7(self, value: int) -> None:
        self._cards[1].set_value("p7", value)

    @property
    def p8(self) -> typing.Optional[int]:
        """Get or set the Define parts to exclude from dynain.lsda when included from a keyword input deck.
        """ # nopep8
        return self._cards[1].get_value("p8")

    @p8.setter
    def p8(self, value: int) -> None:
        self._cards[1].set_value("p8", value)

