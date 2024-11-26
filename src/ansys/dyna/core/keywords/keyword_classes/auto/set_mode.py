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

class SetMode(KeywordBase):
    """DYNA SET_MODE keyword"""

    keyword = "SET"
    subkeyword = "MODE"
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
                        "mid1",
                        int,
                        0,
                        10,
                        kwargs.get("mid1")
                    ),
                    Field(
                        "mid2",
                        int,
                        10,
                        10,
                        kwargs.get("mid2")
                    ),
                    Field(
                        "mid3",
                        int,
                        20,
                        10,
                        kwargs.get("mid3")
                    ),
                    Field(
                        "mid4",
                        int,
                        30,
                        10,
                        kwargs.get("mid4")
                    ),
                    Field(
                        "mid5",
                        int,
                        40,
                        10,
                        kwargs.get("mid5")
                    ),
                    Field(
                        "mid6",
                        int,
                        50,
                        10,
                        kwargs.get("mid6")
                    ),
                    Field(
                        "mid7",
                        int,
                        60,
                        10,
                        kwargs.get("mid7")
                    ),
                    Field(
                        "mid8",
                        int,
                        70,
                        10,
                        kwargs.get("mid8")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = SetMode.option_specs[0],
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
        """Get or set the Set identification. All mode sets should have a unique set ID.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[0].set_value("sid", value)

    @property
    def mid1(self) -> typing.Optional[int]:
        """Get or set the Mode ID.
        """ # nopep8
        return self._cards[1].get_value("mid1")

    @mid1.setter
    def mid1(self, value: int) -> None:
        self._cards[1].set_value("mid1", value)

    @property
    def mid2(self) -> typing.Optional[int]:
        """Get or set the Mode ID.
        """ # nopep8
        return self._cards[1].get_value("mid2")

    @mid2.setter
    def mid2(self, value: int) -> None:
        self._cards[1].set_value("mid2", value)

    @property
    def mid3(self) -> typing.Optional[int]:
        """Get or set the Mode ID.
        """ # nopep8
        return self._cards[1].get_value("mid3")

    @mid3.setter
    def mid3(self, value: int) -> None:
        self._cards[1].set_value("mid3", value)

    @property
    def mid4(self) -> typing.Optional[int]:
        """Get or set the Mode ID.
        """ # nopep8
        return self._cards[1].get_value("mid4")

    @mid4.setter
    def mid4(self, value: int) -> None:
        self._cards[1].set_value("mid4", value)

    @property
    def mid5(self) -> typing.Optional[int]:
        """Get or set the Mode ID.
        """ # nopep8
        return self._cards[1].get_value("mid5")

    @mid5.setter
    def mid5(self, value: int) -> None:
        self._cards[1].set_value("mid5", value)

    @property
    def mid6(self) -> typing.Optional[int]:
        """Get or set the Mode ID.
        """ # nopep8
        return self._cards[1].get_value("mid6")

    @mid6.setter
    def mid6(self, value: int) -> None:
        self._cards[1].set_value("mid6", value)

    @property
    def mid7(self) -> typing.Optional[int]:
        """Get or set the Mode ID.
        """ # nopep8
        return self._cards[1].get_value("mid7")

    @mid7.setter
    def mid7(self, value: int) -> None:
        self._cards[1].set_value("mid7", value)

    @property
    def mid8(self) -> typing.Optional[int]:
        """Get or set the Mode ID.
        """ # nopep8
        return self._cards[1].get_value("mid8")

    @mid8.setter
    def mid8(self, value: int) -> None:
        self._cards[1].set_value("mid8", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

