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

class ConstrainedImmersedInSpg(KeywordBase):
    """DYNA CONSTRAINED_IMMERSED_IN_SPG keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "IMMERSED_IN_SPG"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "spgpid",
                        int,
                        0,
                        10,
                        kwargs.get("spgpid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ipid1",
                        int,
                        0,
                        10,
                        kwargs.get("ipid1")
                    ),
                    Field(
                        "ipid2",
                        int,
                        10,
                        10,
                        kwargs.get("ipid2")
                    ),
                    Field(
                        "ipid3",
                        int,
                        20,
                        10,
                        kwargs.get("ipid3")
                    ),
                    Field(
                        "ipid4",
                        int,
                        30,
                        10,
                        kwargs.get("ipid4")
                    ),
                    Field(
                        "ipid5",
                        int,
                        40,
                        10,
                        kwargs.get("ipid5")
                    ),
                    Field(
                        "ipid6",
                        int,
                        50,
                        10,
                        kwargs.get("ipid6")
                    ),
                    Field(
                        "ipid7",
                        int,
                        60,
                        10,
                        kwargs.get("ipid7")
                    ),
                    Field(
                        "ipid8",
                        int,
                        70,
                        10,
                        kwargs.get("ipid8")
                    ),
                ],
            ),
        ]

    @property
    def spgpid(self) -> typing.Optional[int]:
        """Get or set the Part ID of SPG solids where FEM beams are immersed into.
        """ # nopep8
        return self._cards[0].get_value("spgpid")

    @spgpid.setter
    def spgpid(self, value: int) -> None:
        self._cards[0].set_value("spgpid", value)

    @property
    def ipid1(self) -> typing.Optional[int]:
        """Get or set the Part IDs of FEM beams.
        """ # nopep8
        return self._cards[1].get_value("ipid1")

    @ipid1.setter
    def ipid1(self, value: int) -> None:
        self._cards[1].set_value("ipid1", value)

    @property
    def ipid2(self) -> typing.Optional[int]:
        """Get or set the Part IDs of FEM beams.
        """ # nopep8
        return self._cards[1].get_value("ipid2")

    @ipid2.setter
    def ipid2(self, value: int) -> None:
        self._cards[1].set_value("ipid2", value)

    @property
    def ipid3(self) -> typing.Optional[int]:
        """Get or set the Part IDs of FEM beams.
        """ # nopep8
        return self._cards[1].get_value("ipid3")

    @ipid3.setter
    def ipid3(self, value: int) -> None:
        self._cards[1].set_value("ipid3", value)

    @property
    def ipid4(self) -> typing.Optional[int]:
        """Get or set the Part IDs of FEM beams.
        """ # nopep8
        return self._cards[1].get_value("ipid4")

    @ipid4.setter
    def ipid4(self, value: int) -> None:
        self._cards[1].set_value("ipid4", value)

    @property
    def ipid5(self) -> typing.Optional[int]:
        """Get or set the Part IDs of FEM beams.
        """ # nopep8
        return self._cards[1].get_value("ipid5")

    @ipid5.setter
    def ipid5(self, value: int) -> None:
        self._cards[1].set_value("ipid5", value)

    @property
    def ipid6(self) -> typing.Optional[int]:
        """Get or set the Part IDs of FEM beams.
        """ # nopep8
        return self._cards[1].get_value("ipid6")

    @ipid6.setter
    def ipid6(self, value: int) -> None:
        self._cards[1].set_value("ipid6", value)

    @property
    def ipid7(self) -> typing.Optional[int]:
        """Get or set the Part IDs of FEM beams.
        """ # nopep8
        return self._cards[1].get_value("ipid7")

    @ipid7.setter
    def ipid7(self, value: int) -> None:
        self._cards[1].set_value("ipid7", value)

    @property
    def ipid8(self) -> typing.Optional[int]:
        """Get or set the Part IDs of FEM beams.
        """ # nopep8
        return self._cards[1].get_value("ipid8")

    @ipid8.setter
    def ipid8(self, value: int) -> None:
        self._cards[1].set_value("ipid8", value)

