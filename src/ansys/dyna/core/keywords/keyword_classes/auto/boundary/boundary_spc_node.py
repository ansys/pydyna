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

"""Module providing the BoundarySpcNode class."""
import typing
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_BOUNDARYSPCNODE_OPTION0_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("heading", str, 10, 70, None),
)

class BoundarySpcNode(KeywordBase):
    """DYNA BOUNDARY_SPC_NODE keyword"""

    keyword = "BOUNDARY"
    subkeyword = "SPC_NODE"
    option_specs = [
        OptionSpec("ID", -2, 1),
    ]
    _link_fields = {
        "nid": LinkType.NODE,
        "cid": LinkType.DEFINE_COORDINATE_SYSTEM,
    }

    def __init__(self, **kwargs):
        """Initialize the BoundarySpcNode class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            TableCard(
                [
                    Field("nid", int, 0, 10, None),
                    Field("cid", int, 10, 10, 0),
                    Field("dofx", int, 20, 10, 0),
                    Field("dofy", int, 30, 10, 0),
                    Field("dofz", int, 40, 10, 0),
                    Field("dofrx", int, 50, 10, 0),
                    Field("dofry", int, 60, 10, 0),
                    Field("dofrz", int, 70, 10, 0),
                ],
                None,
                name="nodes",
                **kwargs,
            ),            OptionCardSet(
                option_spec = BoundarySpcNode.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _BOUNDARYSPCNODE_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
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
    def id(self) -> typing.Optional[int]:
        """Get or set the ID keyword option
        """ # nopep8
        return self._cards[1].cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[1].cards[0].set_value("id", value)

        if value:
            self.activate_option("ID")

    @property
    def heading(self) -> typing.Optional[str]:
        """Get or set the Descriptor. We suggest using unique descriptions.
        """ # nopep8
        return self._cards[1].cards[0].get_value("heading")

    @heading.setter
    def heading(self, value: str) -> None:
        """Set the heading property."""
        self._cards[1].cards[0].set_value("heading", value)

        if value:
            self.activate_option("HEADING")

    @property
    def nid_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all NODE keywords for nid, keyed by nid value."""
        return self._get_links_from_table("NODE", "nid", "nodes", "nid", "")

    def get_nid_link(self, nid: int) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid."""
        return self._get_link_by_attr("NODE", "nid", nid, "")

    @property
    def cid_link(self) -> typing.Optional[DefineCoordinateSystem]:
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

