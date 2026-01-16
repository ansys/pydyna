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
            ),        ]
    @property
    def nodes(self) -> pd.DataFrame:
        """Get the table of nodes."""
        return self._cards[0].table

    @nodes.setter
    def nodes(self, df: pd.DataFrame):
        """Set nodes from the dataframe df"""
        self._cards[0].table = df

    @property
    def nid_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all NODE keywords for nid, keyed by nid value."""
        return self._get_links_from_table("NODE", "nid", "nodes", "nid", "parts")

    def get_nid_link(self, nid: int) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid."""
        return self._get_link_by_attr("NODE", "nid", nid, "parts")

    @property
    def m1_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all NODE keywords for m1, keyed by m1 value."""
        return self._get_links_from_table("NODE", "nid", "nodes", "m1", "parts")

    def get_m1_link(self, m1: int) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given m1."""
        return self._get_link_by_attr("NODE", "nid", m1, "parts")

    @property
    def m2_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all NODE keywords for m2, keyed by m2 value."""
        return self._get_links_from_table("NODE", "nid", "nodes", "m2", "parts")

    def get_m2_link(self, m2: int) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given m2."""
        return self._get_link_by_attr("NODE", "nid", m2, "parts")

    @property
    def m3_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all NODE keywords for m3, keyed by m3 value."""
        return self._get_links_from_table("NODE", "nid", "nodes", "m3", "parts")

    def get_m3_link(self, m3: int) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given m3."""
        return self._get_link_by_attr("NODE", "nid", m3, "parts")

    @property
    def cid_link(self) -> DefineCoordinateSystem:
        """Get the DefineCoordinateSystem object for cid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "COORDINATE_SYSTEM"):
            if kwd.cid == self.cid:
                return kwd
        return None

    @cid_link.setter
    def cid_link(self, value: DefineCoordinateSystem) -> None:
        """Set the DefineCoordinateSystem object for cid."""
        self.cid = value.cid

