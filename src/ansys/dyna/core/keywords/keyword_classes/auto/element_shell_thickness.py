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
from ansys.dyna.core.lib.duplicate_card_group import DuplicateCardGroup
from ansys.dyna.core.lib.keyword_base import KeywordBase

class ElementShellThickness(KeywordBase):
    """DYNA ELEMENT_SHELL_THICKNESS keyword"""

    keyword = "ELEMENT"
    subkeyword = "SHELL_THICKNESS"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            DuplicateCardGroup(
                [
                    Card(
                            [
                                Field(
                                    "eid",
                                    int,
                                    0,
                                    8,
                                ),
                                Field(
                                    "pid",
                                    int,
                                    8,
                                    8,
                                ),
                                Field(
                                    "n1",
                                    int,
                                    16,
                                    8,
                                ),
                                Field(
                                    "n2",
                                    int,
                                    24,
                                    8,
                                ),
                                Field(
                                    "n3",
                                    int,
                                    32,
                                    8,
                                ),
                                Field(
                                    "n4",
                                    int,
                                    40,
                                    8,
                                ),
                                Field(
                                    "n5",
                                    int,
                                    48,
                                    8,
                                ),
                                Field(
                                    "n6",
                                    int,
                                    56,
                                    8,
                                ),
                                Field(
                                    "n7",
                                    int,
                                    64,
                                    8,
                                ),
                                Field(
                                    "n8",
                                    int,
                                    72,
                                    8,
                                ),
                            ],
                    ),
                    Card(
                            [
                                Field(
                                    "thic1",
                                    float,
                                    0,
                                    16,
                                ),
                                Field(
                                    "thic2",
                                    float,
                                    16,
                                    16,
                                ),
                                Field(
                                    "thic3",
                                    float,
                                    32,
                                    16,
                                ),
                                Field(
                                    "thic4",
                                    float,
                                    48,
                                    16,
                                ),
                                Field(
                                    "beta",
                                    float,
                                    64,
                                    16,
                                ),
                            ],
                    ),
                    Card(
                            [
                                Field(
                                    "thic5",
                                    float,
                                    0,
                                    16,
                                ),
                                Field(
                                    "thic6",
                                    float,
                                    16,
                                    16,
                                ),
                                Field(
                                    "thic7",
                                    float,
                                    32,
                                    16,
                                ),
                                Field(
                                    "thic8",
                                    float,
                                    48,
                                    16,
                                ),
                            ],
                            lambda: self.elements['n5'].any(),
                    ),
                ],
                None,
                data = kwargs.get("elements")),
        ]

    @property
    def elements(self):
        '''Gets the full table of elements'''
        return self._cards[0].table

    @elements.setter
    def elements(self, df):
        '''sets elements from the dataframe df'''
        self._cards[0].table = df

