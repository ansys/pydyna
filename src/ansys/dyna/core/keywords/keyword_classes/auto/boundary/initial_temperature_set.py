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

"""Module providing the InitialTemperatureSet class."""
import typing
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

class InitialTemperatureSet(KeywordBase):
    """DYNA INITIAL_TEMPERATURE_SET keyword"""

    keyword = "INITIAL"
    subkeyword = "TEMPERATURE_SET"
    _link_fields = {
        "nsid": LinkType.SET_NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the InitialTemperatureSet class."""
        super().__init__(**kwargs)
        self._cards = [
            TableCard(
                [
                    Field("nsid", int, 0, 10, None),
                    Field("temp", float, 10, 10, 0.0),
                    Field("loc", int, 20, 10, 0),
                ],
                None,
                name="sets",
                **kwargs,
            ),
        ]
    @property
    def sets(self) -> pd.DataFrame:
        """Get the table of sets."""
        return self._cards[0].table

    @sets.setter
    def sets(self, df: pd.DataFrame):
        """Set sets from the dataframe df"""
        self._cards[0].table = df

    @property
    def nsid(self) -> pd.Series:
        """Get or set the Nodal set ID, see also *SET_NODE.
        EQ.0: all nodes are included.
        """ # nopep8
        return self._cards[0].get_column("nsid")

    @nsid.setter
    def nsid(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the nsid column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("nsid", value)

    @property
    def temp(self) -> pd.Series:
        """Get or set the Temperature at node set.
        """ # nopep8
        return self._cards[0].get_column("temp")

    @temp.setter
    def temp(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the temp column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("temp", value)

    @property
    def loc(self) -> pd.Series:
        """Get or set the Application of surface for thermal shell elements, see parameter, TSHELL, in the *CONTROL_SHELL input.
        EQ.-1: lower surface of thermal shell element.
        EQ. 0 middle surface of thermal shell element.
        EQ. 1: upper surface of thermal shell element.
        """ # nopep8
        return self._cards[0].get_column("loc")

    @loc.setter
    def loc(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the loc column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("loc", value)

    @property
    def nsid_links(self) -> typing.Dict[typing.Any, KeywordBase]:
        """Get the keywords linked by the nsid column, keyed by nsid value."""
        return self._get_column_links(self.nsid, lambda value: self._get_set_link("NODE", value))

