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

"""Module providing the IcfdBoundaryFsi class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.keyword_base import KeywordBase

class IcfdBoundaryFsi(KeywordBase):
    """DYNA ICFD_BOUNDARY_FSI keyword"""

    keyword = "ICFD"
    subkeyword = "BOUNDARY_FSI"

    def __init__(self, **kwargs):
        """Initialize the IcfdBoundaryFsi class."""
        super().__init__(**kwargs)
        self._cards = [
            TableCard(
                [
                    Field("pid", int, 0, 10, None),
                ],
                None,
                name="boundaries",
                **kwargs,
            ),
        ]

    @property
    def boundaries(self):
        """Get the table of boundaries."""
        return self._cards[0].table

    @boundaries.setter
    def boundaries(self, df):
        """Set boundaries from the dataframe df"""
        self._cards[0].table = df

