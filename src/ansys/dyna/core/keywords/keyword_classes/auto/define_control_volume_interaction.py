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

class DefineControlVolumeInteraction(KeywordBase):
    """DYNA DEFINE_CONTROL_VOLUME_INTERACTION keyword"""

    keyword = "DEFINE"
    subkeyword = "CONTROL_VOLUME_INTERACTION"
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
                        kwargs.get("id")
                    ),
                    Field(
                        "cvid1",
                        int,
                        10,
                        10,
                        kwargs.get("cvid1")
                    ),
                    Field(
                        "cvid2",
                        int,
                        20,
                        10,
                        kwargs.get("cvid2")
                    ),
                    Field(
                        "lcid ",
                        int,
                        30,
                        10,
                        kwargs.get("lcid ")
                    ),
                    Field(
                        "area ",
                        float,
                        40,
                        10,
                        kwargs.get("area ")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineControlVolumeInteraction.option_specs[0],
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
    def id(self) -> typing.Optional[int]:
        """Get or set the Fluid cavity interaction ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def cvid1(self) -> typing.Optional[int]:
        """Get or set the First control volume ID
        """ # nopep8
        return self._cards[0].get_value("cvid1")

    @cvid1.setter
    def cvid1(self, value: int) -> None:
        self._cards[0].set_value("cvid1", value)

    @property
    def cvid2(self) -> typing.Optional[int]:
        """Get or set the Second control volume ID
        """ # nopep8
        return self._cards[0].get_value("cvid2")

    @cvid2.setter
    def cvid2(self, value: int) -> None:
        self._cards[0].set_value("cvid2", value)

    @property
    def lcid_(self) -> typing.Optional[int]:
        """Get or set the Load curve id (*DEFINE_CURVE_FUNCTION). Tables, see *DEFINE_TABLE, and load curves may not share common IDs.
        LS-DYNA allows load curves IDs and table IDs to be used interchangeably.
        A unique number has to be defined.
        """ # nopep8
        return self._cards[0].get_value("lcid ")

    @lcid_.setter
    def lcid_(self, value: int) -> None:
        self._cards[0].set_value("lcid ", value)

    @property
    def area_(self) -> typing.Optional[float]:
        """Get or set the This is a constant area for the case when a flow area definition is not defined
        """ # nopep8
        return self._cards[0].get_value("area ")

    @area_.setter
    def area_(self, value: float) -> None:
        self._cards[0].set_value("area ", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

