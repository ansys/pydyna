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

class SetPartTree(KeywordBase):
    """DYNA SET_PART_TREE keyword"""

    keyword = "SET"
    subkeyword = "PART_TREE"
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
                        "brid",
                        int,
                        0,
                        10,
                        kwargs.get("brid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "heading",
                        str,
                        0,
                        80,
                        kwargs.get("heading")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "compi",
                        int,
                        0,
                        10,
                        kwargs.get("compi")
                    ),
                    Field(
                        "compi",
                        int,
                        10,
                        10,
                        kwargs.get("compi")
                    ),
                    Field(
                        "compi",
                        int,
                        20,
                        10,
                        kwargs.get("compi")
                    ),
                    Field(
                        "compi",
                        int,
                        30,
                        10,
                        kwargs.get("compi")
                    ),
                    Field(
                        "compi",
                        int,
                        40,
                        10,
                        kwargs.get("compi")
                    ),
                    Field(
                        "compi",
                        int,
                        50,
                        10,
                        kwargs.get("compi")
                    ),
                    Field(
                        "compi",
                        int,
                        60,
                        10,
                        kwargs.get("compi")
                    ),
                    Field(
                        "compi",
                        int,
                        70,
                        10,
                        kwargs.get("compi")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = SetPartTree.option_specs[0],
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
    def brid(self) -> typing.Optional[int]:
        """Get or set the Branch identification. A unique number must be specified..
        """ # nopep8
        return self._cards[0].get_value("brid")

    @brid.setter
    def brid(self, value: int) -> None:
        self._cards[0].set_value("brid", value)

    @property
    def heading(self) -> typing.Optional[str]:
        """Get or set the Heading for the branch.
        """ # nopep8
        return self._cards[1].get_value("heading")

    @heading.setter
    def heading(self, value: str) -> None:
        self._cards[1].set_value("heading", value)

    @property
    def compi(self) -> typing.Optional[int]:
        """Get or set the Components of branch BRID:
        GT.0: ID of a sub-branch
        LT.0: ID of a part.
        """ # nopep8
        return self._cards[2].get_value("compi")

    @compi.setter
    def compi(self, value: int) -> None:
        self._cards[2].set_value("compi", value)

    @property
    def compi(self) -> typing.Optional[int]:
        """Get or set the Components of branch BRID:
        GT.0: ID of a sub-branch
        LT.0: ID of a part.
        """ # nopep8
        return self._cards[2].get_value("compi")

    @compi.setter
    def compi(self, value: int) -> None:
        self._cards[2].set_value("compi", value)

    @property
    def compi(self) -> typing.Optional[int]:
        """Get or set the Components of branch BRID:
        GT.0: ID of a sub-branch
        LT.0: ID of a part.
        """ # nopep8
        return self._cards[2].get_value("compi")

    @compi.setter
    def compi(self, value: int) -> None:
        self._cards[2].set_value("compi", value)

    @property
    def compi(self) -> typing.Optional[int]:
        """Get or set the Components of branch BRID:
        GT.0: ID of a sub-branch
        LT.0: ID of a part.
        """ # nopep8
        return self._cards[2].get_value("compi")

    @compi.setter
    def compi(self, value: int) -> None:
        self._cards[2].set_value("compi", value)

    @property
    def compi(self) -> typing.Optional[int]:
        """Get or set the Components of branch BRID:
        GT.0: ID of a sub-branch
        LT.0: ID of a part.
        """ # nopep8
        return self._cards[2].get_value("compi")

    @compi.setter
    def compi(self, value: int) -> None:
        self._cards[2].set_value("compi", value)

    @property
    def compi(self) -> typing.Optional[int]:
        """Get or set the Components of branch BRID:
        GT.0: ID of a sub-branch
        LT.0: ID of a part.
        """ # nopep8
        return self._cards[2].get_value("compi")

    @compi.setter
    def compi(self, value: int) -> None:
        self._cards[2].set_value("compi", value)

    @property
    def compi(self) -> typing.Optional[int]:
        """Get or set the Components of branch BRID:
        GT.0: ID of a sub-branch
        LT.0: ID of a part.
        """ # nopep8
        return self._cards[2].get_value("compi")

    @compi.setter
    def compi(self, value: int) -> None:
        self._cards[2].set_value("compi", value)

    @property
    def compi(self) -> typing.Optional[int]:
        """Get or set the Components of branch BRID:
        GT.0: ID of a sub-branch
        LT.0: ID of a part.
        """ # nopep8
        return self._cards[2].get_value("compi")

    @compi.setter
    def compi(self, value: int) -> None:
        self._cards[2].set_value("compi", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

