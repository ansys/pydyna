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
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DefineHexSpotweldAssembly(KeywordBase):
    """DYNA DEFINE_HEX_SPOTWELD_ASSEMBLY keyword"""

    keyword = "DEFINE"
    subkeyword = "HEX_SPOTWELD_ASSEMBLY"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "id_sw",
                        int,
                        0,
                        10,
                        kwargs.get("id_sw")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "eid1",
                        int,
                        0,
                        10,
                        kwargs.get("eid1")
                    ),
                    Field(
                        "eid2",
                        int,
                        10,
                        10,
                        kwargs.get("eid2")
                    ),
                    Field(
                        "eid3",
                        int,
                        20,
                        10,
                        kwargs.get("eid3")
                    ),
                    Field(
                        "eid4",
                        int,
                        30,
                        10,
                        kwargs.get("eid4")
                    ),
                    Field(
                        "eid5",
                        int,
                        40,
                        10,
                        kwargs.get("eid5")
                    ),
                    Field(
                        "eid6",
                        int,
                        50,
                        10,
                        kwargs.get("eid6")
                    ),
                    Field(
                        "eid7",
                        int,
                        60,
                        10,
                        kwargs.get("eid7")
                    ),
                    Field(
                        "eid8",
                        int,
                        70,
                        10,
                        kwargs.get("eid8")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineHexSpotweldAssembly.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]

    @property
    def id_sw(self) -> typing.Optional[int]:
        """Get or set the spot weld ID. A uniquie ID number must be used.
        """ # nopep8
        return self._cards[0].get_value("id_sw")

    @id_sw.setter
    def id_sw(self, value: int) -> None:
        self._cards[0].set_value("id_sw", value)

    @property
    def eid1(self) -> typing.Optional[int]:
        """Get or set the Element ID n for up to 16 solid hexahedral elements
        """ # nopep8
        return self._cards[1].get_value("eid1")

    @eid1.setter
    def eid1(self, value: int) -> None:
        self._cards[1].set_value("eid1", value)

    @property
    def eid2(self) -> typing.Optional[int]:
        """Get or set the Element ID n for up to 16 solid hexahedral elements.
        """ # nopep8
        return self._cards[1].get_value("eid2")

    @eid2.setter
    def eid2(self, value: int) -> None:
        self._cards[1].set_value("eid2", value)

    @property
    def eid3(self) -> typing.Optional[int]:
        """Get or set the Element ID n for up to 16 solid hexahedral elements
        """ # nopep8
        return self._cards[1].get_value("eid3")

    @eid3.setter
    def eid3(self, value: int) -> None:
        self._cards[1].set_value("eid3", value)

    @property
    def eid4(self) -> typing.Optional[int]:
        """Get or set the Element ID n for up to 16 solid hexahedral elements.
        """ # nopep8
        return self._cards[1].get_value("eid4")

    @eid4.setter
    def eid4(self, value: int) -> None:
        self._cards[1].set_value("eid4", value)

    @property
    def eid5(self) -> typing.Optional[int]:
        """Get or set the Element ID n for up to 16 solid hexahedral elements
        """ # nopep8
        return self._cards[1].get_value("eid5")

    @eid5.setter
    def eid5(self, value: int) -> None:
        self._cards[1].set_value("eid5", value)

    @property
    def eid6(self) -> typing.Optional[int]:
        """Get or set the Element ID n for up to 16 solid hexahedral elements
        """ # nopep8
        return self._cards[1].get_value("eid6")

    @eid6.setter
    def eid6(self, value: int) -> None:
        self._cards[1].set_value("eid6", value)

    @property
    def eid7(self) -> typing.Optional[int]:
        """Get or set the Element ID n for up to 16 solid hexahedral elements
        """ # nopep8
        return self._cards[1].get_value("eid7")

    @eid7.setter
    def eid7(self, value: int) -> None:
        self._cards[1].set_value("eid7", value)

    @property
    def eid8(self) -> typing.Optional[int]:
        """Get or set the Element ID n for up to 16 solid hexahedral elements
        """ # nopep8
        return self._cards[1].get_value("eid8")

    @eid8.setter
    def eid8(self, value: int) -> None:
        self._cards[1].set_value("eid8", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

