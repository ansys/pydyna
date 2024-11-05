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

class AleReferenceSystemNode(KeywordBase):
    """DYNA ALE_REFERENCE_SYSTEM_NODE keyword"""

    keyword = "ALE"
    subkeyword = "REFERENCE_SYSTEM_NODE"

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
                ],
            ),
            Card(
                [
                    Field(
                        "nid1",
                        int,
                        0,
                        10,
                        kwargs.get("nid1")
                    ),
                    Field(
                        "nid2",
                        int,
                        10,
                        10,
                        kwargs.get("nid2")
                    ),
                    Field(
                        "nid3",
                        int,
                        20,
                        10,
                        kwargs.get("nid3")
                    ),
                    Field(
                        "nid4",
                        int,
                        30,
                        10,
                        kwargs.get("nid4")
                    ),
                    Field(
                        "nid5",
                        int,
                        40,
                        10,
                        kwargs.get("nid5")
                    ),
                    Field(
                        "nid6",
                        int,
                        50,
                        10,
                        kwargs.get("nid6")
                    ),
                    Field(
                        "nid7",
                        int,
                        60,
                        10,
                        kwargs.get("nid7")
                    ),
                    Field(
                        "nid8",
                        int,
                        70,
                        10,
                        kwargs.get("nid8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nid9",
                        int,
                        0,
                        10,
                        kwargs.get("nid9")
                    ),
                    Field(
                        "nid10",
                        int,
                        10,
                        10,
                        kwargs.get("nid10")
                    ),
                    Field(
                        "nid11",
                        int,
                        20,
                        10,
                        kwargs.get("nid11")
                    ),
                    Field(
                        "nid12",
                        int,
                        30,
                        10,
                        kwargs.get("nid12")
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Node group ID for PRTYPE 3 or 7, see *ALE_REFERENCE_SYSTEM_GROUP.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def nid1(self) -> typing.Optional[int]:
        """Get or set the Node one of user specified nodes.
        """ # nopep8
        return self._cards[1].get_value("nid1")

    @nid1.setter
    def nid1(self, value: int) -> None:
        self._cards[1].set_value("nid1", value)

    @property
    def nid2(self) -> typing.Optional[int]:
        """Get or set the Node two of user specified nodes.
        """ # nopep8
        return self._cards[1].get_value("nid2")

    @nid2.setter
    def nid2(self, value: int) -> None:
        self._cards[1].set_value("nid2", value)

    @property
    def nid3(self) -> typing.Optional[int]:
        """Get or set the Node three of user specified nodes.
        """ # nopep8
        return self._cards[1].get_value("nid3")

    @nid3.setter
    def nid3(self, value: int) -> None:
        self._cards[1].set_value("nid3", value)

    @property
    def nid4(self) -> typing.Optional[int]:
        """Get or set the Node four of user specified nodes.
        """ # nopep8
        return self._cards[1].get_value("nid4")

    @nid4.setter
    def nid4(self, value: int) -> None:
        self._cards[1].set_value("nid4", value)

    @property
    def nid5(self) -> typing.Optional[int]:
        """Get or set the Node five of user specified nodes.
        """ # nopep8
        return self._cards[1].get_value("nid5")

    @nid5.setter
    def nid5(self, value: int) -> None:
        self._cards[1].set_value("nid5", value)

    @property
    def nid6(self) -> typing.Optional[int]:
        """Get or set the Node six of user specified nodes.
        """ # nopep8
        return self._cards[1].get_value("nid6")

    @nid6.setter
    def nid6(self, value: int) -> None:
        self._cards[1].set_value("nid6", value)

    @property
    def nid7(self) -> typing.Optional[int]:
        """Get or set the Node seven of user specified nodes.
        """ # nopep8
        return self._cards[1].get_value("nid7")

    @nid7.setter
    def nid7(self, value: int) -> None:
        self._cards[1].set_value("nid7", value)

    @property
    def nid8(self) -> typing.Optional[int]:
        """Get or set the Node eight of user specified nodes.
        """ # nopep8
        return self._cards[1].get_value("nid8")

    @nid8.setter
    def nid8(self, value: int) -> None:
        self._cards[1].set_value("nid8", value)

    @property
    def nid9(self) -> typing.Optional[int]:
        """Get or set the Node nine of user specified nodes.
        """ # nopep8
        return self._cards[2].get_value("nid9")

    @nid9.setter
    def nid9(self, value: int) -> None:
        self._cards[2].set_value("nid9", value)

    @property
    def nid10(self) -> typing.Optional[int]:
        """Get or set the Node ten of user specified nodes.
        """ # nopep8
        return self._cards[2].get_value("nid10")

    @nid10.setter
    def nid10(self, value: int) -> None:
        self._cards[2].set_value("nid10", value)

    @property
    def nid11(self) -> typing.Optional[int]:
        """Get or set the Node eleven of user specified nodes.
        """ # nopep8
        return self._cards[2].get_value("nid11")

    @nid11.setter
    def nid11(self, value: int) -> None:
        self._cards[2].set_value("nid11", value)

    @property
    def nid12(self) -> typing.Optional[int]:
        """Get or set the Node twelve of user specified nodes.
        """ # nopep8
        return self._cards[2].get_value("nid12")

    @nid12.setter
    def nid12(self, value: int) -> None:
        self._cards[2].set_value("nid12", value)

