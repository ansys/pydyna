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

"""Module providing the ElementShellMcid class."""
import typing
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card_group import TableCardGroup
from ansys.dyna.core.lib.row_condition import RowCondition, is_missing
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

class ElementShellMcid(KeywordBase):
    """DYNA ELEMENT_SHELL_MCID keyword"""

    keyword = "ELEMENT"
    subkeyword = "SHELL_MCID"
    _link_fields = {
        "n1": LinkType.NODE,
        "n2": LinkType.NODE,
        "n3": LinkType.NODE,
        "n4": LinkType.NODE,
        "n5": LinkType.NODE,
        "n6": LinkType.NODE,
        "n7": LinkType.NODE,
        "n8": LinkType.NODE,
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ElementShellMcid class."""
        super().__init__(**kwargs)
        self._cards = [
            TableCardGroup(
                [
                    (
                        FieldSchema("eid", int, 0, 8, None),
                        FieldSchema("pid", int, 8, 8, None),
                        FieldSchema("n1", int, 16, 8, None),
                        FieldSchema("n2", int, 24, 8, None),
                        FieldSchema("n3", int, 32, 8, None),
                        FieldSchema("n4", int, 40, 8, None),
                        FieldSchema("n5", int, 48, 8, None),
                        FieldSchema("n6", int, 56, 8, None),
                        FieldSchema("n7", int, 64, 8, None),
                        FieldSchema("n8", int, 72, 8, None),
                    ),
                    (
                        FieldSchema("thic1", float, 0, 16, 0.0),
                        FieldSchema("thic2", float, 16, 16, 0.0),
                        FieldSchema("thic3", float, 32, 16, 0.0),
                        FieldSchema("thic4", float, 48, 16, 0.0),
                        FieldSchema("mcid", int, 64, 16, 0),
                    ),
                    (
                        FieldSchema("thic5", float, 0, 16, 0.0),
                        FieldSchema("thic6", float, 16, 16, 0.0),
                        FieldSchema("thic7", float, 32, 16, 0.0),
                        FieldSchema("thic8", float, 48, 16, 0.0),
                    ),
                ],
                None,
                None,
                "elements",
                row_conditions=[
                    None,
                    None,
                    RowCondition(
                        ("n5",),
                        lambda n5: not is_missing(n5) and n5 != 0,
                        lambda table: (table["n5"].notna() & table["n5"].ne(0)).to_numpy(dtype=bool),
                    ),
                ],
                **kwargs,
            ),
        ]
    @property
    def elements(self) -> pd.DataFrame:
        """Gets the full table of elements."""
        return self._cards[0].table

    @elements.setter
    def elements(self, df: pd.DataFrame):
        """sets elements from the dataframe df."""
        self._cards[0].table = df

    @property
    def eid(self) -> pd.Series:
        """Get or set the Element ID. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_column("eid")

    @eid.setter
    def eid(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the eid column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("eid", value)

    @property
    def pid(self) -> pd.Series:
        """Get or set the Part ID, see *PART.
        """ # nopep8
        return self._cards[0].get_column("pid")

    @pid.setter
    def pid(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the pid column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("pid", value)

    @property
    def n1(self) -> pd.Series:
        """Get or set the Nodal point 1.
        """ # nopep8
        return self._cards[0].get_column("n1")

    @n1.setter
    def n1(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the n1 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("n1", value)

    @property
    def n2(self) -> pd.Series:
        """Get or set the Nodal point 2.
        """ # nopep8
        return self._cards[0].get_column("n2")

    @n2.setter
    def n2(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the n2 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("n2", value)

    @property
    def n3(self) -> pd.Series:
        """Get or set the Nodal point 3.
        """ # nopep8
        return self._cards[0].get_column("n3")

    @n3.setter
    def n3(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the n3 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("n3", value)

    @property
    def n4(self) -> pd.Series:
        """Get or set the Nodal point 4.
        """ # nopep8
        return self._cards[0].get_column("n4")

    @n4.setter
    def n4(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the n4 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("n4", value)

    @property
    def n5(self) -> pd.Series:
        """Get or set the Mid side nodal point 5.
        """ # nopep8
        return self._cards[0].get_column("n5")

    @n5.setter
    def n5(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the n5 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("n5", value)

    @property
    def n6(self) -> pd.Series:
        """Get or set the Mid side nodal point 6.
        """ # nopep8
        return self._cards[0].get_column("n6")

    @n6.setter
    def n6(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the n6 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("n6", value)

    @property
    def n7(self) -> pd.Series:
        """Get or set the Mid side nodal point 7.
        """ # nopep8
        return self._cards[0].get_column("n7")

    @n7.setter
    def n7(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the n7 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("n7", value)

    @property
    def n8(self) -> pd.Series:
        """Get or set the Mid side nodal point 8.
        """ # nopep8
        return self._cards[0].get_column("n8")

    @n8.setter
    def n8(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the n8 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("n8", value)

    @property
    def thic1(self) -> pd.Series:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_column("thic1")

    @thic1.setter
    def thic1(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the thic1 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("thic1", value)

    @property
    def thic2(self) -> pd.Series:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_column("thic2")

    @thic2.setter
    def thic2(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the thic2 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("thic2", value)

    @property
    def thic3(self) -> pd.Series:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_column("thic3")

    @thic3.setter
    def thic3(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the thic3 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("thic3", value)

    @property
    def thic4(self) -> pd.Series:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_column("thic4")

    @thic4.setter
    def thic4(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the thic4 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("thic4", value)

    @property
    def mcid(self) -> pd.Series:
        """Get or set the Material coordinate system ID. The angle BETA ia determined by the projection of the X axis of the local system onto the shell element.
        """ # nopep8
        return self._cards[0].get_column("mcid")

    @mcid.setter
    def mcid(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the mcid column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("mcid", value)

    @property
    def thic5(self) -> pd.Series:
        """Get or set the Shell thickness at node 5.
        """ # nopep8
        return self._cards[0].get_column("thic5")

    @thic5.setter
    def thic5(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the thic5 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("thic5", value)

    @property
    def thic6(self) -> pd.Series:
        """Get or set the Shell thickness at node 6.
        """ # nopep8
        return self._cards[0].get_column("thic6")

    @thic6.setter
    def thic6(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the thic6 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("thic6", value)

    @property
    def thic7(self) -> pd.Series:
        """Get or set the Shell thickness at node 7.
        """ # nopep8
        return self._cards[0].get_column("thic7")

    @thic7.setter
    def thic7(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the thic7 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("thic7", value)

    @property
    def thic8(self) -> pd.Series:
        """Get or set the Shell thickness at node 8.
        """ # nopep8
        return self._cards[0].get_column("thic8")

    @thic8.setter
    def thic8(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the thic8 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("thic8", value)

