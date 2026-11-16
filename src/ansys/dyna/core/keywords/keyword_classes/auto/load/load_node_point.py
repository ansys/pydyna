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

"""Module providing the LoadNodePoint class."""
import typing
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

class LoadNodePoint(KeywordBase):
    """DYNA LOAD_NODE_POINT keyword"""

    keyword = "LOAD"
    subkeyword = "NODE_POINT"
    _link_fields = {
        "nid": LinkType.NODE,
        "m1": LinkType.NODE,
        "m2": LinkType.NODE,
        "m3": LinkType.NODE,
        "cid": LinkType.DEFINE_COORDINATE_SYSTEM,
    }

    def __init__(self, **kwargs):
        """Initialize the LoadNodePoint class."""
        super().__init__(**kwargs)
        self._cards = [
            TableCard(
                [
                    Field("nid", int, 0, 10, None),
                    Field("dof", int, 10, 10, 0),
                    Field("lcid", int, 20, 10, None),
                    Field("sf", float, 30, 10, 1.0),
                    Field("cid", int, 40, 10, 0),
                    Field("m1", int, 50, 10, 0),
                    Field("m2", int, 60, 10, 0),
                    Field("m3", int, 70, 10, 0),
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
        """Get or set the Node ID, see also *NODE.
        """ # nopep8
        return self._cards[0].get_column("nid")

    @nid.setter
    def nid(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the nid column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("nid", value)

    @property
    def dof(self) -> pd.Series:
        """Get or set the Applicable degrees-of-freedom:
        EQ.1:	x - direction of load action.For a cylindrical or spherical system, this is the direction of the local radial degree of freedom r.
        EQ.2 : y - direction of load action.For a cylindrical system, this is the direction of the local circumferential degree of freedom ?, and for a spherical system, it is the direction of the local latitude degree of freedom ?.
        EQ.3 : z - direction of load action.For a cylindrical system, this is the direction of the local z - translational degree of freedom, and for a spherical system, it is the direction of the local longitude degree of freedom ?.
        EQ.4 : Follower force(see Remark 2),
        EQ.5 : Moment about the x - axis(see Remark 4).For a cylindrical or spherical system, this is the direction of the local radial degree of freedom r.
        EQ.6 : Moment about the y - axis axis(see Remark 4).For a cylindrical system, this is the direction of the local circumferential degree of freedom ?, and for a spherical system, it is the direction of the local latitude degree of freedom ?.
        EQ.7 : Moment about the z - axis axis(see Remark 4).For a cylindrical system, this is the direction of the local z - translational degree of freedom, and for a spherical system, it is the direction of the local longitude degree of freedom ?.
        EQ.8 : Follower moment(see Remarks 2 and 4).
        """ # nopep8
        return self._cards[0].get_column("dof")

    @dof.setter
    def dof(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the dof column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("dof", value)

    @property
    def lcid(self) -> pd.Series:
        """Get or set the Load curve ID(see *DEFINE_CURVE) or function ID(see *DEFINE_FUNCTION).
        """ # nopep8
        return self._cards[0].get_column("lcid")

    @lcid.setter
    def lcid(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the lcid column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("lcid", value)

    @property
    def sf(self) -> pd.Series:
        """Get or set the Load curve scale factor.
        """ # nopep8
        return self._cards[0].get_column("sf")

    @sf.setter
    def sf(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the sf column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("sf", value)

    @property
    def cid(self) -> pd.Series:
        """Get or set the Coordinate system ID (optional).
        """ # nopep8
        return self._cards[0].get_column("cid")

    @cid.setter
    def cid(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the cid column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("cid", value)

    @property
    def m1(self) -> pd.Series:
        """Get or set the Node 1 ID. Only necessary if DOF EQ.4 or EQ.8.
        """ # nopep8
        return self._cards[0].get_column("m1")

    @m1.setter
    def m1(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the m1 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("m1", value)

    @property
    def m2(self) -> pd.Series:
        """Get or set the Node 2 ID. Only necessary if DOF EQ.4 or EQ.8.
        """ # nopep8
        return self._cards[0].get_column("m2")

    @m2.setter
    def m2(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the m2 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("m2", value)

    @property
    def m3(self) -> pd.Series:
        """Get or set the Node 3 ID. Only necessary if DOF EQ.4 or EQ.8.
        """ # nopep8
        return self._cards[0].get_column("m3")

    @m3.setter
    def m3(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the m3 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("m3", value)

    @property
    def nid_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all NODE keywords for nid, keyed by nid value."""
        return self._get_links_from_table("NODE", "nid", "nodes", "nid", "")

    def get_nid_link(self, nid: int) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid."""
        return self._get_link_by_attr("NODE", "nid", nid, "")

    @property
    def m1_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all NODE keywords for m1, keyed by m1 value."""
        return self._get_links_from_table("NODE", "nid", "nodes", "m1", "")

    def get_m1_link(self, m1: int) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given m1."""
        return self._get_link_by_attr("NODE", "nid", m1, "")

    @property
    def m2_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all NODE keywords for m2, keyed by m2 value."""
        return self._get_links_from_table("NODE", "nid", "nodes", "m2", "")

    def get_m2_link(self, m2: int) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given m2."""
        return self._get_link_by_attr("NODE", "nid", m2, "")

    @property
    def m3_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all NODE keywords for m3, keyed by m3 value."""
        return self._get_links_from_table("NODE", "nid", "nodes", "m3", "")

    def get_m3_link(self, m3: int) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given m3."""
        return self._get_link_by_attr("NODE", "nid", m3, "")

    @property
    def cid_links(self) -> typing.Dict[typing.Any, KeywordBase]:
        """Get the keywords linked by the cid column, keyed by cid value."""
        return self._get_column_links(self.cid, lambda value: self._find_keyword("DEFINE", "COORDINATE_SYSTEM", "cid", value))

