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

class DefineFilter(KeywordBase):
    """DYNA DEFINE_FILTER keyword"""

    keyword = "DEFINE"
    subkeyword = "FILTER"
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
                        "id",
                        int,
                        0,
                        10,
                        kwargs.get("id", 0)
                    ),
                    Field(
                        "title",
                        str,
                        10,
                        70,
                        kwargs.get("title")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "type",
                        str,
                        0,
                        10,
                        kwargs.get("type")
                    ),
                    Field(
                        "data1",
                        str,
                        10,
                        10,
                        kwargs.get("data1")
                    ),
                    Field(
                        "data2",
                        str,
                        20,
                        10,
                        kwargs.get("data2")
                    ),
                    Field(
                        "data3",
                        str,
                        30,
                        10,
                        kwargs.get("data3")
                    ),
                    Field(
                        "data4",
                        str,
                        40,
                        10,
                        kwargs.get("data4")
                    ),
                    Field(
                        "data5",
                        str,
                        50,
                        10,
                        kwargs.get("data5")
                    ),
                    Field(
                        "data6",
                        str,
                        60,
                        10,
                        kwargs.get("data6")
                    ),
                    Field(
                        "data7",
                        str,
                        70,
                        10,
                        kwargs.get("data7")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineFilter.option_specs[0],
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
    def id(self) -> int:
        """Get or set the ID of a GEOMETRY defining high explosive particle domain.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Geometry type
        EQ.1: box
        EQ.2: sphere
        EQ.3: cylinder
        EQ.4: ellipsoid
        EQ.5: hemisphere (see Remark 1).
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[0].set_value("title", value)

    @property
    def type(self) -> typing.Optional[str]:
        """Get or set the One of the 3 currently defined filter types: DISCRETE, CONTINUOUS, or CHAIN.
        """ # nopep8
        return self._cards[1].get_value("type")

    @type.setter
    def type(self, value: str) -> None:
        self._cards[1].set_value("type", value)

    @property
    def data1(self) -> typing.Optional[str]:
        """Get or set the Filter type specific data, which determines what the filter does.
        """ # nopep8
        return self._cards[1].get_value("data1")

    @data1.setter
    def data1(self, value: str) -> None:
        self._cards[1].set_value("data1", value)

    @property
    def data2(self) -> typing.Optional[str]:
        """Get or set the Filter type specific data, which determines what the filter does.
        """ # nopep8
        return self._cards[1].get_value("data2")

    @data2.setter
    def data2(self, value: str) -> None:
        self._cards[1].set_value("data2", value)

    @property
    def data3(self) -> typing.Optional[str]:
        """Get or set the Filter type specific data, which determines what the filter does.
        """ # nopep8
        return self._cards[1].get_value("data3")

    @data3.setter
    def data3(self, value: str) -> None:
        self._cards[1].set_value("data3", value)

    @property
    def data4(self) -> typing.Optional[str]:
        """Get or set the Filter type specific data, which determines what the filter does.
        """ # nopep8
        return self._cards[1].get_value("data4")

    @data4.setter
    def data4(self, value: str) -> None:
        self._cards[1].set_value("data4", value)

    @property
    def data5(self) -> typing.Optional[str]:
        """Get or set the Filter type specific data, which determines what the filter does.
        """ # nopep8
        return self._cards[1].get_value("data5")

    @data5.setter
    def data5(self, value: str) -> None:
        self._cards[1].set_value("data5", value)

    @property
    def data6(self) -> typing.Optional[str]:
        """Get or set the Filter type specific data, which determines what the filter does.
        """ # nopep8
        return self._cards[1].get_value("data6")

    @data6.setter
    def data6(self, value: str) -> None:
        self._cards[1].set_value("data6", value)

    @property
    def data7(self) -> typing.Optional[str]:
        """Get or set the Filter type specific data, which determines what the filter does.
        """ # nopep8
        return self._cards[1].get_value("data7")

    @data7.setter
    def data7(self, value: str) -> None:
        self._cards[1].set_value("data7", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

