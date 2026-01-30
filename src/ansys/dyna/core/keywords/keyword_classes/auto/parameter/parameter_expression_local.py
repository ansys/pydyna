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

"""Module providing the ParameterExpressionLocal class."""
import typing
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.keyword_base import KeywordBase

class ParameterExpressionLocal(KeywordBase):
    """DYNA PARAMETER_EXPRESSION_LOCAL keyword"""

    keyword = "PARAMETER"
    subkeyword = "EXPRESSION_LOCAL"

    def __init__(self, **kwargs):
        """Initialize the ParameterExpressionLocal class."""
        super().__init__(**kwargs)
        self._cards = [
            TableCard(
                [
                    Field("prmr", str, 0, 10, None),
                    Field("expression", str, 10, 70, None),
                ],
                None,
                name="parameters",
                **kwargs,
            ),        ]
    @property
    def parameters(self) -> pd.DataFrame:
        """Get the table of parameters."""
        return self._cards[0].table

    @parameters.setter
    def parameters(self, df: pd.DataFrame):
        """Set parameters from the dataframe df"""
        self._cards[0].table = df

