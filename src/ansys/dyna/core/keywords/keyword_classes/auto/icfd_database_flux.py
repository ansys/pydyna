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
from ansys.dyna.core.lib.keyword_base import KeywordBase

class IcfdDatabaseFlux(KeywordBase):
    """DYNA ICFD_DATABASE_FLUX keyword"""

    keyword = "ICFD"
    subkeyword = "DATABASE_FLUX"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            DuplicateCard(
                [
                    Field("pid", int, 0, 10),
                    Field("dtout", float, 10, 10),
                ],
                None,
                data = kwargs.get("boundaries")),
        ]

    @property
    def boundaries(self):
        '''Gets the table of boundaries'''
        return self._cards[0].table

    @boundaries.setter
    def boundaries(self, df):
        '''sets boundaries from the dataframe df'''
        self._cards[0].table = df
