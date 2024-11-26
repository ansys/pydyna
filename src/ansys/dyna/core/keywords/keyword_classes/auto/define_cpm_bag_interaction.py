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

class DefineCpmBagInteraction(KeywordBase):
    """DYNA DEFINE_CPM_BAG_INTERACTION keyword"""

    keyword = "DEFINE"
    subkeyword = "CPM_BAG_INTERACTION"
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
                        "bagid1",
                        int,
                        0,
                        10,
                        kwargs.get("bagid1")
                    ),
                    Field(
                        "bagid2",
                        int,
                        10,
                        10,
                        kwargs.get("bagid2")
                    ),
                    Field(
                        "nspec",
                        int,
                        30,
                        10,
                        kwargs.get("nspec")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineCpmBagInteraction.option_specs[0],
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
    def bagid1(self) -> typing.Optional[int]:
        """Get or set the Airbag ID of source CPM particle bag
        """ # nopep8
        return self._cards[0].get_value("bagid1")

    @bagid1.setter
    def bagid1(self, value: int) -> None:
        self._cards[0].set_value("bagid1", value)

    @property
    def bagid2(self) -> typing.Optional[int]:
        """Get or set the Airbag ID of sink CV bag switched from CPM bag
        """ # nopep8
        return self._cards[0].get_value("bagid2")

    @bagid2.setter
    def bagid2(self, value: int) -> None:
        self._cards[0].set_value("bagid2", value)

    @property
    def nspec(self) -> typing.Optional[int]:
        """Get or set the The location of the 1st gas component from the CPM bag to be filled in the CV bag.
        """ # nopep8
        return self._cards[0].get_value("nspec")

    @nspec.setter
    def nspec(self, value: int) -> None:
        self._cards[0].set_value("nspec", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

