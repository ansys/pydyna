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

class BoundarySpcNode(KeywordBase):
    """DYNA BOUNDARY_SPC_NODE keyword"""

    keyword = "BOUNDARY"
    subkeyword = "SPC_NODE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            DuplicateCard(
                [
                    Field("nid", int, 0, 10),
                    Field("cid", int, 10, 10),
                    Field("dofx", int, 20, 10),
                    Field("dofy", int, 30, 10),
                    Field("dofz", int, 40, 10),
                    Field("dofrx", int, 50, 10),
                    Field("dofry", int, 60, 10),
                    Field("dofrz", int, 70, 10),
                ],
                None,
                data = kwargs.get("nodes")),
        ]

    @property
    def nodes(self):
        '''Gets the table of nodes'''
        return self._cards[0].table

    @nodes.setter
    def nodes(self, df):
        '''sets nodes from the dataframe df'''
        self._cards[0].table = df

