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

"""Module providing the ConstrainedAdaptivity class."""
import typing
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.keyword_base import KeywordBase

class ConstrainedAdaptivity(KeywordBase):
    """DYNA CONSTRAINED_ADAPTIVITY keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "ADAPTIVITY"

    def __init__(self, **kwargs):
        """Initialize the ConstrainedAdaptivity class."""
        super().__init__(**kwargs)
        self._cards = [
            TableCard(
                [
                    Field("dnid", int, 0, 10, None),
                    Field("nid1", int, 10, 10, None),
                    Field("nid2", int, 20, 10, None),
                ],
                None,
                name="constrains",
                **kwargs,
            ),        ]
    @property
    def constrains(self) -> pd.DataFrame:
        """Get the table of constrains."""
        return self._cards[0].table

    @constrains.setter
    def constrains(self, df: pd.DataFrame):
        """Set constrains from the dataframe df"""
        self._cards[0].table = df

