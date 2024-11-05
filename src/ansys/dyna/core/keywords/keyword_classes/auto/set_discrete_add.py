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

class SetDiscreteAdd(KeywordBase):
    """DYNA SET_DISCRETE_ADD keyword"""

    keyword = "SET"
    subkeyword = "DISCRETE_ADD"
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
                        "dsid1",
                        int,
                        0,
                        10,
                        kwargs.get("dsid1")
                    ),
                    Field(
                        "dsid2",
                        int,
                        10,
                        10,
                        kwargs.get("dsid2")
                    ),
                    Field(
                        "dsid3",
                        int,
                        20,
                        10,
                        kwargs.get("dsid3")
                    ),
                    Field(
                        "dsid4",
                        int,
                        30,
                        10,
                        kwargs.get("dsid4")
                    ),
                    Field(
                        "dsid5",
                        int,
                        40,
                        10,
                        kwargs.get("dsid5")
                    ),
                    Field(
                        "dsid6",
                        int,
                        50,
                        10,
                        kwargs.get("dsid6")
                    ),
                    Field(
                        "dsid7",
                        int,
                        60,
                        10,
                        kwargs.get("dsid7")
                    ),
                    Field(
                        "dsid8",
                        int,
                        70,
                        10,
                        kwargs.get("dsid8")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = SetDiscreteAdd.option_specs[0],
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
        """Get or set the Set ID of new discrete set. All discrete sets should have a unique set ID.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[0].set_value("sid", value)

    @property
    def dsid1(self) -> typing.Optional[int]:
        """Get or set the First discrete set ID.
        """ # nopep8
        return self._cards[1].get_value("dsid1")

    @dsid1.setter
    def dsid1(self, value: int) -> None:
        self._cards[1].set_value("dsid1", value)

    @property
    def dsid2(self) -> typing.Optional[int]:
        """Get or set the Second discrete set ID.
        """ # nopep8
        return self._cards[1].get_value("dsid2")

    @dsid2.setter
    def dsid2(self, value: int) -> None:
        self._cards[1].set_value("dsid2", value)

    @property
    def dsid3(self) -> typing.Optional[int]:
        """Get or set the Third discrete set ID.
        """ # nopep8
        return self._cards[1].get_value("dsid3")

    @dsid3.setter
    def dsid3(self, value: int) -> None:
        self._cards[1].set_value("dsid3", value)

    @property
    def dsid4(self) -> typing.Optional[int]:
        """Get or set the Fourth discrete set ID.
        """ # nopep8
        return self._cards[1].get_value("dsid4")

    @dsid4.setter
    def dsid4(self, value: int) -> None:
        self._cards[1].set_value("dsid4", value)

    @property
    def dsid5(self) -> typing.Optional[int]:
        """Get or set the Fifth discrete set ID.
        """ # nopep8
        return self._cards[1].get_value("dsid5")

    @dsid5.setter
    def dsid5(self, value: int) -> None:
        self._cards[1].set_value("dsid5", value)

    @property
    def dsid6(self) -> typing.Optional[int]:
        """Get or set the Sixth discrete set ID.
        """ # nopep8
        return self._cards[1].get_value("dsid6")

    @dsid6.setter
    def dsid6(self, value: int) -> None:
        self._cards[1].set_value("dsid6", value)

    @property
    def dsid7(self) -> typing.Optional[int]:
        """Get or set the Seventh discrete set ID.
        """ # nopep8
        return self._cards[1].get_value("dsid7")

    @dsid7.setter
    def dsid7(self, value: int) -> None:
        self._cards[1].set_value("dsid7", value)

    @property
    def dsid8(self) -> typing.Optional[int]:
        """Get or set the Eighth discrete set ID.
        """ # nopep8
        return self._cards[1].get_value("dsid8")

    @dsid8.setter
    def dsid8(self, value: int) -> None:
        self._cards[1].set_value("dsid8", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

