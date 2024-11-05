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
from ansys.dyna.core.lib.duplicate_card import DuplicateCard
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DefineTransformation(KeywordBase):
    """DYNA DEFINE_TRANSFORMATION keyword"""

    keyword = "DEFINE"
    subkeyword = "TRANSFORMATION"
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
                        "tranid",
                        int,
                        0,
                        10,
                        kwargs.get("tranid")
                    ),
                ],
            ),
            DuplicateCard(
                [
                    Field("option", str, 0, 10),
                    Field("a1", float, 10, 10),
                    Field("a2", float, 20, 10),
                    Field("a3", float, 30, 10),
                    Field("a4", float, 40, 10),
                    Field("a5", float, 50, 10),
                    Field("a6", float, 60, 10),
                    Field("a7", float, 70, 10),
                ],
                None,
                data = kwargs.get("transforms")),
            OptionCardSet(
                option_spec = DefineTransformation.option_specs[0],
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
    def tranid(self) -> typing.Optional[int]:
        """Get or set the Transform ID.
        """ # nopep8
        return self._cards[0].get_value("tranid")

    @tranid.setter
    def tranid(self, value: int) -> None:
        self._cards[0].set_value("tranid", value)

    @property
    def transforms(self):
        '''Gets the table of transforms'''
        return self._cards[1].table

    @transforms.setter
    def transforms(self, df):
        '''sets transforms from the dataframe df'''
        self._cards[1].table = df

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

