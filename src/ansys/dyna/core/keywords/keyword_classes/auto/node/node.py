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

"""Module providing the Node class."""
import typing
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.keyword_base import KeywordBase

class Node(KeywordBase):
    """DYNA NODE keyword"""

    keyword = "NODE"
    subkeyword = "NODE"

    def __init__(self, **kwargs):
        """Initialize the Node class."""
        super().__init__(**kwargs)
        self._cards = [
            TableCard(
                [
                    Field("nid", int, 0, 8, None),
                    Field("x", float, 8, 16, 0.0),
                    Field("y", float, 24, 16, 0.0),
                    Field("z", float, 40, 16, 0.0),
                    Field("tc", int, 56, 8, 0),
                    Field("rc", int, 64, 8, 0),
                ],
                None,
                name="nodes",
                **kwargs,
            ),
        ]
    @property
    def nodes(self) -> pd.DataFrame:
        """Get the table of nodes."""
        return self._cards[0].table

    @nodes.setter
    def nodes(self, df: pd.DataFrame):
        """Set nodes from the dataframe df"""
        self._cards[0].table = df

    @property
    def nid(self) -> pd.Series:
        """Get or set the Node ID.
        """ # nopep8
        return self._cards[0].get_column("nid")

    @nid.setter
    def nid(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the nid column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("nid", value)

    @property
    def x(self) -> pd.Series:
        """Get or set the x-coordinate.
        """ # nopep8
        return self._cards[0].get_column("x")

    @x.setter
    def x(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the x column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("x", value)

    @property
    def y(self) -> pd.Series:
        """Get or set the y-coordinate.
        """ # nopep8
        return self._cards[0].get_column("y")

    @y.setter
    def y(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the y column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("y", value)

    @property
    def z(self) -> pd.Series:
        """Get or set the z-coordinate.
        """ # nopep8
        return self._cards[0].get_column("z")

    @z.setter
    def z(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the z column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("z", value)

    @property
    def tc(self) -> pd.Series:
        """Get or set the Translational constraint:
        EQ.0: no constraints,
        EQ.1: constrained x displacement,
        EQ.2: constrained y displacement,
        EQ.3: constrained z displacement,
        EQ.4: constrained x and y displacements,
        EQ.5: constrained y and z displacements,
        EQ.6: constrained z and x displacements,
        EQ.7: constrained x, y, and z displacements.
        """ # nopep8
        return self._cards[0].get_column("tc")

    @tc.setter
    def tc(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the tc column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("tc", value)

    @property
    def rc(self) -> pd.Series:
        """Get or set the Rotational constraint:
        EQ.0: no constraints,
        EQ.1: constrained x rotation,
        EQ.2: constrained y rotation,
        EQ.3: constrained z rotation,
        EQ.4: constrained x and y rotations,
        EQ.5: constrained y and z rotations,
        EQ.6: constrained z and x rotations,
        EQ.7: constrained x, y, and z rotations.
        """ # nopep8
        return self._cards[0].get_column("rc")

    @rc.setter
    def rc(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the rc column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("rc", value)

