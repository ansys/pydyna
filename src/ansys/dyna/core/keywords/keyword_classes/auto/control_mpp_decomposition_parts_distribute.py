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

class ControlMppDecompositionPartsDistribute(KeywordBase):
    """DYNA CONTROL_MPP_DECOMPOSITION_PARTS_DISTRIBUTE keyword"""

    keyword = "CONTROL"
    subkeyword = "MPP_DECOMPOSITION_PARTS_DISTRIBUTE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "id1",
                        int,
                        0,
                        10,
                        kwargs.get("id1")
                    ),
                    Field(
                        "id2",
                        int,
                        10,
                        10,
                        kwargs.get("id2")
                    ),
                    Field(
                        "id3",
                        int,
                        20,
                        10,
                        kwargs.get("id3")
                    ),
                    Field(
                        "id4",
                        int,
                        30,
                        10,
                        kwargs.get("id4")
                    ),
                    Field(
                        "id5",
                        int,
                        40,
                        10,
                        kwargs.get("id5")
                    ),
                    Field(
                        "id6",
                        int,
                        50,
                        10,
                        kwargs.get("id6")
                    ),
                    Field(
                        "id7",
                        int,
                        60,
                        10,
                        kwargs.get("id7")
                    ),
                    Field(
                        "id8",
                        int,
                        70,
                        10,
                        kwargs.get("id8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "id9",
                        int,
                        0,
                        10,
                        kwargs.get("id9")
                    ),
                    Field(
                        "id10",
                        int,
                        10,
                        10,
                        kwargs.get("id10")
                    ),
                    Field(
                        "id11",
                        int,
                        20,
                        10,
                        kwargs.get("id11")
                    ),
                    Field(
                        "id12",
                        int,
                        30,
                        10,
                        kwargs.get("id12")
                    ),
                    Field(
                        "id13",
                        int,
                        40,
                        10,
                        kwargs.get("id13")
                    ),
                    Field(
                        "id14",
                        int,
                        50,
                        10,
                        kwargs.get("id14")
                    ),
                    Field(
                        "id15",
                        int,
                        60,
                        10,
                        kwargs.get("id15")
                    ),
                    Field(
                        "id16",
                        int,
                        70,
                        10,
                        kwargs.get("id16")
                    ),
                ],
            ),
        ]

    @property
    def id1(self) -> typing.Optional[int]:
        """Get or set the Part ID to be distributed. If ID1,ID2,.. < 0, abs(ID1) will be used as part set ID.  All parts defined in this card will be treated as a single region to be decomposed
        """ # nopep8
        return self._cards[0].get_value("id1")

    @id1.setter
    def id1(self, value: int) -> None:
        self._cards[0].set_value("id1", value)

    @property
    def id2(self) -> typing.Optional[int]:
        """Get or set the Part ID to be distributed. If ID1,ID2,.. < 0, abs(ID1) will be used as part set ID.  All parts defined in this card will be treated as a single region to be decomposed
        """ # nopep8
        return self._cards[0].get_value("id2")

    @id2.setter
    def id2(self, value: int) -> None:
        self._cards[0].set_value("id2", value)

    @property
    def id3(self) -> typing.Optional[int]:
        """Get or set the Part ID to be distributed. If ID1,ID2,.. < 0, abs(ID1) will be used as part set ID.  All parts defined in this card will be treated as a single region to be decomposed
        """ # nopep8
        return self._cards[0].get_value("id3")

    @id3.setter
    def id3(self, value: int) -> None:
        self._cards[0].set_value("id3", value)

    @property
    def id4(self) -> typing.Optional[int]:
        """Get or set the Part ID to be distributed. If ID1,ID2,.. < 0, abs(ID1) will be used as part set ID.  All parts defined in this card will be treated as a single region to be decomposed
        """ # nopep8
        return self._cards[0].get_value("id4")

    @id4.setter
    def id4(self, value: int) -> None:
        self._cards[0].set_value("id4", value)

    @property
    def id5(self) -> typing.Optional[int]:
        """Get or set the Part ID to be distributed. If ID1,ID2,.. < 0, abs(ID1) will be used as part set ID.  All parts defined in this card will be treated as a single region to be decomposed
        """ # nopep8
        return self._cards[0].get_value("id5")

    @id5.setter
    def id5(self, value: int) -> None:
        self._cards[0].set_value("id5", value)

    @property
    def id6(self) -> typing.Optional[int]:
        """Get or set the Part ID to be distributed. If ID1,ID2,.. < 0, abs(ID1) will be used as part set ID.  All parts defined in this card will be treated as a single region to be decomposed
        """ # nopep8
        return self._cards[0].get_value("id6")

    @id6.setter
    def id6(self, value: int) -> None:
        self._cards[0].set_value("id6", value)

    @property
    def id7(self) -> typing.Optional[int]:
        """Get or set the Part ID to be distributed. If ID1,ID2,.. < 0, abs(ID1) will be used as part set ID.  All parts defined in this card will be treated as a single region to be decomposed
        """ # nopep8
        return self._cards[0].get_value("id7")

    @id7.setter
    def id7(self, value: int) -> None:
        self._cards[0].set_value("id7", value)

    @property
    def id8(self) -> typing.Optional[int]:
        """Get or set the Part ID to be distributed. If ID1,ID2,.. < 0, abs(ID1) will be used as part set ID.  All parts defined in this card will be treated as a single region to be decomposed
        """ # nopep8
        return self._cards[0].get_value("id8")

    @id8.setter
    def id8(self, value: int) -> None:
        self._cards[0].set_value("id8", value)

    @property
    def id9(self) -> typing.Optional[int]:
        """Get or set the Part ID to be distributed. If ID1,ID2,.. < 0, abs(ID1) will be used as part set ID.  All parts defined in this card will be treated as a single region to be decomposed
        """ # nopep8
        return self._cards[1].get_value("id9")

    @id9.setter
    def id9(self, value: int) -> None:
        self._cards[1].set_value("id9", value)

    @property
    def id10(self) -> typing.Optional[int]:
        """Get or set the Part ID to be distributed. If ID1,ID2,.. < 0, abs(ID1) will be used as part set ID.  All parts defined in this card will be treated as a single region to be decomposed
        """ # nopep8
        return self._cards[1].get_value("id10")

    @id10.setter
    def id10(self, value: int) -> None:
        self._cards[1].set_value("id10", value)

    @property
    def id11(self) -> typing.Optional[int]:
        """Get or set the Part ID to be distributed. If ID1,ID2,.. < 0, abs(ID1) will be used as part set ID.  All parts defined in this card will be treated as a single region to be decomposed
        """ # nopep8
        return self._cards[1].get_value("id11")

    @id11.setter
    def id11(self, value: int) -> None:
        self._cards[1].set_value("id11", value)

    @property
    def id12(self) -> typing.Optional[int]:
        """Get or set the Part ID to be distributed. If ID1,ID2,.. < 0, abs(ID1) will be used as part set ID.  All parts defined in this card will be treated as a single region to be decomposed
        """ # nopep8
        return self._cards[1].get_value("id12")

    @id12.setter
    def id12(self, value: int) -> None:
        self._cards[1].set_value("id12", value)

    @property
    def id13(self) -> typing.Optional[int]:
        """Get or set the Part ID to be distributed. If ID1,ID2,.. < 0, abs(ID1) will be used as part set ID.  All parts defined in this card will be treated as a single region to be decomposed
        """ # nopep8
        return self._cards[1].get_value("id13")

    @id13.setter
    def id13(self, value: int) -> None:
        self._cards[1].set_value("id13", value)

    @property
    def id14(self) -> typing.Optional[int]:
        """Get or set the Part ID to be distributed. If ID1,ID2,.. < 0, abs(ID1) will be used as part set ID.  All parts defined in this card will be treated as a single region to be decomposed
        """ # nopep8
        return self._cards[1].get_value("id14")

    @id14.setter
    def id14(self, value: int) -> None:
        self._cards[1].set_value("id14", value)

    @property
    def id15(self) -> typing.Optional[int]:
        """Get or set the Part ID to be distributed. If ID1,ID2,.. < 0, abs(ID1) will be used as part set ID.  All parts defined in this card will be treated as a single region to be decomposed
        """ # nopep8
        return self._cards[1].get_value("id15")

    @id15.setter
    def id15(self, value: int) -> None:
        self._cards[1].set_value("id15", value)

    @property
    def id16(self) -> typing.Optional[int]:
        """Get or set the Part ID to be distributed. If ID1,ID2,.. < 0, abs(ID1) will be used as part set ID.  All parts defined in this card will be treated as a single region to be decomposed
        """ # nopep8
        return self._cards[1].get_value("id16")

    @id16.setter
    def id16(self, value: int) -> None:
        self._cards[1].set_value("id16", value)

