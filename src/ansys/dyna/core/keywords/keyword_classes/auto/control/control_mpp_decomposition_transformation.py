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

"""Module providing the ControlMppDecompositionTransformation class."""
import typing
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card_group import TableCardGroup
from ansys.dyna.core.lib.keyword_base import KeywordBase

class ControlMppDecompositionTransformation(KeywordBase):
    """DYNA CONTROL_MPP_DECOMPOSITION_TRANSFORMATION keyword"""

    keyword = "CONTROL"
    subkeyword = "MPP_DECOMPOSITION_TRANSFORMATION"

    def __init__(self, **kwargs):
        """Initialize the ControlMppDecompositionTransformation class."""
        super().__init__(**kwargs)
        self._cards = [
            TableCardGroup(
                [
                    Card(
                            [
                                Field(
                                    "type",
                                    str,
                                    0,
                                    10,
                                ),
                                Field(
                                    "v1",
                                    float,
                                    10,
                                    10,
                                ),
                                Field(
                                    "v2",
                                    float,
                                    20,
                                    10,
                                ),
                                Field(
                                    "v3",
                                    float,
                                    30,
                                    10,
                                ),
                                Field(
                                    "v4",
                                    float,
                                    40,
                                    10,
                                ),
                                Field(
                                    "v5",
                                    float,
                                    50,
                                    10,
                                ),
                                Field(
                                    "v6",
                                    float,
                                    60,
                                    10,
                                ),
                            ],
                            _internal=True,
                    ),
                    Card(
                            [
                                Field(
                                    "v7",
                                    float,
                                    0,
                                    10,
                                ),
                                Field(
                                    "v8",
                                    float,
                                    10,
                                    10,
                                ),
                                Field(
                                    "v9",
                                    float,
                                    20,
                                    10,
                                ),
                            ],
                            lambda: not self.transformation.empty and self.transformation['type'].iloc[-1] in ['VEC3', 'C2R', 'S2R', 'MAT'],
                            _internal=True,
                    ),
                ],
                None,
                None,
                "transformation",
                **kwargs,
            ),        ]
    @property
    def transformation(self) -> pd.DataFrame:
        """Gets the full table of transformation."""
        return self._cards[0].table

    @transformation.setter
    def transformation(self, df: pd.DataFrame):
        """sets transformation from the dataframe df."""
        self._cards[0].table = df

