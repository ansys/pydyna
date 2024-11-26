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

class SetSegmentIntersect(KeywordBase):
    """DYNA SET_SEGMENT_INTERSECT keyword"""

    keyword = "SET"
    subkeyword = "SEGMENT_INTERSECT"
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
                        "sid",
                        int,
                        0,
                        10,
                        kwargs.get("sid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ssid1",
                        int,
                        0,
                        10,
                        kwargs.get("ssid1")
                    ),
                    Field(
                        "ssid2",
                        int,
                        10,
                        10,
                        kwargs.get("ssid2")
                    ),
                    Field(
                        "ssid3",
                        int,
                        20,
                        10,
                        kwargs.get("ssid3")
                    ),
                    Field(
                        "ssid4",
                        int,
                        30,
                        10,
                        kwargs.get("ssid4")
                    ),
                    Field(
                        "ssid5",
                        int,
                        40,
                        10,
                        kwargs.get("ssid5")
                    ),
                    Field(
                        "ssid6",
                        int,
                        50,
                        10,
                        kwargs.get("ssid6")
                    ),
                    Field(
                        "ssid7",
                        int,
                        60,
                        10,
                        kwargs.get("ssid7")
                    ),
                    Field(
                        "ssid8",
                        int,
                        70,
                        10,
                        kwargs.get("ssid8")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = SetSegmentIntersect.option_specs[0],
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
    def sid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID. All segment sets should have a unique set ID.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[0].set_value("sid", value)

    @property
    def ssid1(self) -> typing.Optional[int]:
        """Get or set the The nth shell set ID.
        """ # nopep8
        return self._cards[1].get_value("ssid1")

    @ssid1.setter
    def ssid1(self, value: int) -> None:
        self._cards[1].set_value("ssid1", value)

    @property
    def ssid2(self) -> typing.Optional[int]:
        """Get or set the The nth shell set ID.
        """ # nopep8
        return self._cards[1].get_value("ssid2")

    @ssid2.setter
    def ssid2(self, value: int) -> None:
        self._cards[1].set_value("ssid2", value)

    @property
    def ssid3(self) -> typing.Optional[int]:
        """Get or set the The nth shell set ID.
        """ # nopep8
        return self._cards[1].get_value("ssid3")

    @ssid3.setter
    def ssid3(self, value: int) -> None:
        self._cards[1].set_value("ssid3", value)

    @property
    def ssid4(self) -> typing.Optional[int]:
        """Get or set the The nth shell set ID.
        """ # nopep8
        return self._cards[1].get_value("ssid4")

    @ssid4.setter
    def ssid4(self, value: int) -> None:
        self._cards[1].set_value("ssid4", value)

    @property
    def ssid5(self) -> typing.Optional[int]:
        """Get or set the The nth shell set ID.
        """ # nopep8
        return self._cards[1].get_value("ssid5")

    @ssid5.setter
    def ssid5(self, value: int) -> None:
        self._cards[1].set_value("ssid5", value)

    @property
    def ssid6(self) -> typing.Optional[int]:
        """Get or set the The nth shell set ID.
        """ # nopep8
        return self._cards[1].get_value("ssid6")

    @ssid6.setter
    def ssid6(self, value: int) -> None:
        self._cards[1].set_value("ssid6", value)

    @property
    def ssid7(self) -> typing.Optional[int]:
        """Get or set the The nth shell set ID.
        """ # nopep8
        return self._cards[1].get_value("ssid7")

    @ssid7.setter
    def ssid7(self, value: int) -> None:
        self._cards[1].set_value("ssid7", value)

    @property
    def ssid8(self) -> typing.Optional[int]:
        """Get or set the The nth shell set ID.
        """ # nopep8
        return self._cards[1].get_value("ssid8")

    @ssid8.setter
    def ssid8(self, value: int) -> None:
        self._cards[1].set_value("ssid8", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

