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

class MeshSurfaceElement(KeywordBase):
    """DYNA MESH_SURFACE_ELEMENT keyword"""

    keyword = "MESH"
    subkeyword = "SURFACE_ELEMENT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            DuplicateCard(
                [
                    Field("eid", int, 0, 8),
                    Field("pid", int, 8, 8),
                    Field("n1", int, 16, 8),
                    Field("n2", int, 24, 8),
                    Field("n3", int, 32, 8),
                    Field("n4", int, 40, 8),
                ],
                None,
                data = kwargs.get("elements")),
        ]

    @property
    def elements(self):
        '''Gets the table of elements'''
        return self._cards[0].table

    @elements.setter
    def elements(self, df):
        '''sets elements from the dataframe df'''
        self._cards[0].table = df

