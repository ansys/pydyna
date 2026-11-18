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
from ansys.dyna.core.lib.row_condition import RowCondition, is_missing
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
                    (
                        FieldSchema("type", str, 0, 10, "RX"),
                        FieldSchema("v1", float, 10, 10, 0.0),
                        FieldSchema("v2", float, 20, 10, 0.0),
                        FieldSchema("v3", float, 30, 10, 0.0),
                        FieldSchema("v4", float, 40, 10, 0.0),
                        FieldSchema("v5", float, 50, 10, 0.0),
                        FieldSchema("v6", float, 60, 10, 0.0),
                    ),
                    (
                        FieldSchema("v7", float, 0, 10, 0.0),
                        FieldSchema("v8", float, 10, 10, 0.0),
                        FieldSchema("v9", float, 20, 10, 0.0),
                    ),
                ],
                None,
                None,
                "transformation",
                row_conditions=[
                    None,
                    RowCondition(
                        ("type",),
                        lambda type: not is_missing(type) and type in ("VEC3", "C2R", "S2R", "MAT"),
                        lambda table: table["type"].isin(["VEC3", "C2R", "S2R", "MAT"]).to_numpy(dtype=bool),
                    ),
                ],
                **kwargs,
            ),
        ]
    @property
    def transformation(self) -> pd.DataFrame:
        """Gets the full table of transformation."""
        return self._cards[0].table

    @transformation.setter
    def transformation(self, df: pd.DataFrame):
        """sets transformation from the dataframe df."""
        self._cards[0].table = df

    @property
    def type(self) -> pd.Series:
        """Get or set the Which transformation to apply. The possible values are:VEC3,C2R,S2R,MAT.
        """ # nopep8
        return self._cards[0].get_column("type")

    @type.setter
    def type(self, value: typing.Union[str, typing.Sequence[str], pd.Series]) -> None:
        """Set the type column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("type", value)

    @property
    def v1(self) -> pd.Series:
        """Get or set the Parameters to the transformation.
        """ # nopep8
        return self._cards[0].get_column("v1")

    @v1.setter
    def v1(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the v1 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("v1", value)

    @property
    def v2(self) -> pd.Series:
        """Get or set the Parameters to the transformation.
        """ # nopep8
        return self._cards[0].get_column("v2")

    @v2.setter
    def v2(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the v2 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("v2", value)

    @property
    def v3(self) -> pd.Series:
        """Get or set the Parameters to the transformation.
        """ # nopep8
        return self._cards[0].get_column("v3")

    @v3.setter
    def v3(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the v3 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("v3", value)

    @property
    def v4(self) -> pd.Series:
        """Get or set the Parameters to the transformation.
        """ # nopep8
        return self._cards[0].get_column("v4")

    @v4.setter
    def v4(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the v4 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("v4", value)

    @property
    def v5(self) -> pd.Series:
        """Get or set the Parameters to the transformation.
        """ # nopep8
        return self._cards[0].get_column("v5")

    @v5.setter
    def v5(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the v5 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("v5", value)

    @property
    def v6(self) -> pd.Series:
        """Get or set the Parameters to the transformation.
        """ # nopep8
        return self._cards[0].get_column("v6")

    @v6.setter
    def v6(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the v6 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("v6", value)

    @property
    def v7(self) -> pd.Series:
        """Get or set the Parameters to the transformation.
        """ # nopep8
        return self._cards[0].get_column("v7")

    @v7.setter
    def v7(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the v7 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("v7", value)

    @property
    def v8(self) -> pd.Series:
        """Get or set the Parameters to the transformation.
        """ # nopep8
        return self._cards[0].get_column("v8")

    @v8.setter
    def v8(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the v8 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("v8", value)

    @property
    def v9(self) -> pd.Series:
        """Get or set the Parameters to the transformation.
        """ # nopep8
        return self._cards[0].get_column("v9")

    @v9.setter
    def v9(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the v9 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("v9", value)

