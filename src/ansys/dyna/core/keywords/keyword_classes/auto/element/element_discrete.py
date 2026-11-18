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

"""Module providing the ElementDiscrete class."""
import typing
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

class ElementDiscrete(KeywordBase):
    """DYNA ELEMENT_DISCRETE keyword"""

    keyword = "ELEMENT"
    subkeyword = "DISCRETE"
    _link_fields = {
        "n1": LinkType.NODE,
        "n2": LinkType.NODE,
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ElementDiscrete class."""
        super().__init__(**kwargs)
        self._cards = [
            TableCard(
                [
                    Field("eid", int, 0, 8, None),
                    Field("pid", int, 8, 8, None),
                    Field("n1", int, 16, 8, None),
                    Field("n2", int, 24, 8, None),
                    Field("vid", int, 32, 8, 0),
                    Field("s", float, 40, 16, 1.0),
                    Field("pf", int, 56, 8, 0),
                    Field("offset", float, 64, 16, 0.0),
                ],
                None,
                name="elements",
                **kwargs,
            ),
        ]
    @property
    def elements(self) -> pd.DataFrame:
        """Get the table of elements."""
        return self._cards[0].table

    @elements.setter
    def elements(self, df: pd.DataFrame):
        """Set elements from the dataframe df"""
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
        """Get or set the Nodal point 2. If zero, the spring/damper connects node N1 to ground.
        """ # nopep8
        return self._cards[0].get_column("n2")

    @n2.setter
    def n2(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the n2 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("n2", value)

    @property
    def vid(self) -> pd.Series:
        """Get or set the Orientation option: The orientation option should be used cautiously since forces, which are generated as the nodal points displace, are not orthogonal to rigid body rotation unless the nodes are coincident.. The type 6, 3D beam element, is recommended when orientation is required with the absolute value of the parameter SCOOR set to 2 or 3, since this option avoids rotational constraints.
        EQ.0: the spring/damper acts along the axis from node N1 to N2,
        NE.0: the spring/damper acts along the axis defined by the orientation vector, VID defined in the *DEFINE_SD_ORIENTATION section.
        """ # nopep8
        return self._cards[0].get_column("vid")

    @vid.setter
    def vid(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the vid column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("vid", value)

    @property
    def s(self) -> pd.Series:
        """Get or set the Scale factor on forces.
        """ # nopep8
        return self._cards[0].get_column("s")

    @s.setter
    def s(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the s column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("s", value)

    @property
    def pf(self) -> pd.Series:
        """Get or set the Print flag:
        EQ.0: forces are printed in DEFORC file, see *DATABASE_OPTION,
        EQ.1: forces are not printed in DEFORC file.
        """ # nopep8
        return self._cards[0].get_column("pf")

    @pf.setter
    def pf(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the pf column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("pf", value)

    @property
    def offset(self) -> pd.Series:
        """Get or set the Initial offset. The initial offset is a displacement or rotation at time zero.
        """ # nopep8
        return self._cards[0].get_column("offset")

    @offset.setter
    def offset(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the offset column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("offset", value)

    @property
    def n1_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all NODE keywords for n1, keyed by n1 value."""
        return self._get_links_from_table("NODE", "nid", "elements", "n1", "")

    def get_n1_link(self, n1: int) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n1."""
        return self._get_link_by_attr("NODE", "nid", n1, "")

    @property
    def n2_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all NODE keywords for n2, keyed by n2 value."""
        return self._get_links_from_table("NODE", "nid", "elements", "n2", "")

    def get_n2_link(self, n2: int) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n2."""
        return self._get_link_by_attr("NODE", "nid", n2, "")

    @property
    def pid_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all PART keywords for pid, keyed by pid value."""
        return self._get_links_from_table("PART", "pid", "elements", "pid", "parts")

    def get_pid_link(self, pid: int) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", pid, "parts")

