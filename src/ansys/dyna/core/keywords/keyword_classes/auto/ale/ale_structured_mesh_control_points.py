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

"""Module providing the AleStructuredMeshControlPoints class."""
import typing
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ALESTRUCTUREDMESHCONTROLPOINTS_CARD0 = (
    FieldSchema("cpid", int, 0, 10, 0),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("icase", int, 20, 10, 0),
    FieldSchema("sfo", float, 30, 10, 1.0),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("offo", float, 50, 10, 0.0),
)

class AleStructuredMeshControlPoints(KeywordBase):
    """DYNA ALE_STRUCTURED_MESH_CONTROL_POINTS keyword"""

    keyword = "ALE"
    subkeyword = "STRUCTURED_MESH_CONTROL_POINTS"

    def __init__(self, **kwargs):
        """Initialize the AleStructuredMeshControlPoints class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ALESTRUCTUREDMESHCONTROLPOINTS_CARD0,
                **kwargs,
            ),            TableCard(
                [
                    Field("n", int, 0, 20, 0),
                    Field("x", float, 20, 20, None),
                    Field("ratio", float, 40, 20, 0.0),
                ],
                None,
                name="control_points",
                **kwargs,
            ),        ]
    @property
    def cpid(self) -> int:
        """Get or set the Control Points ID. A unique number must be specified. This ID is to be
        referred in the three fields marked up CPIDX, CPIDY, CPIDZ in *ALE_	STRUCTURED_MESH.
        """ # nopep8
        return self._cards[0].get_value("cpid")

    @cpid.setter
    def cpid(self, value: int) -> None:
        """Set the cpid property."""
        self._cards[0].set_value("cpid", value)

    @property
    def icase(self) -> int:
        """Get or set the A flag to trigger special logic for a more user-friendly input format for progressive mesh spacing. Please see examples sections below on ICASE usage.
        """ # nopep8
        return self._cards[0].get_value("icase")

    @icase.setter
    def icase(self, value: int) -> None:
        """Set the icase property."""
        self._cards[0].set_value("icase", value)

    @property
    def sfo(self) -> float:
        """Get or set the Scale factor for ordinate value. This is useful for simple modifications.	EQ.0.0: default set to 1.0.
        """ # nopep8
        return self._cards[0].get_value("sfo")

    @sfo.setter
    def sfo(self, value: float) -> None:
        """Set the sfo property."""
        self._cards[0].set_value("sfo", value)

    @property
    def offo(self) -> float:
        """Get or set the Offset for ordinate values. See Remark 1.
        """ # nopep8
        return self._cards[0].get_value("offo")

    @offo.setter
    def offo(self, value: float) -> None:
        """Set the offo property."""
        self._cards[0].set_value("offo", value)

    @property
    def control_points(self) -> pd.DataFrame:
        """Get the table of control_points."""
        return self._cards[1].table

    @control_points.setter
    def control_points(self, df: pd.DataFrame):
        """Set control_points from the dataframe df"""
        self._cards[1].table = df

