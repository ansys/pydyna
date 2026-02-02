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

"""Module providing the ConstrainedNodalRigidBody class."""
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

_CONSTRAINEDNODALRIGIDBODY_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class ConstrainedNodalRigidBody(KeywordBase):
    """DYNA CONSTRAINED_NODAL_RIGID_BODY keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "NODAL_RIGID_BODY"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "pnode": LinkType.NODE,
        "cid": LinkType.DEFINE_COORDINATE_SYSTEM,
        "nsid": LinkType.SET_NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the ConstrainedNodalRigidBody class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            TableCard(
                [
                    Field("pid", int, 0, 10, None),
                    Field("cid", int, 10, 10, None),
                    Field("nsid", int, 20, 10, None),
                    Field("pnode", int, 30, 10, 0),
                    Field("iprt", int, 40, 10, 0),
                    Field("drflag", int, 50, 10, 0),
                    Field("rrflag", int, 60, 10, 0),
                ],
                None,
                name="constrained_nodal_rigid_bodies",
                **kwargs,
            ),            OptionCardSet(
                option_spec = ConstrainedNodalRigidBody.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _CONSTRAINEDNODALRIGIDBODY_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def constrained_nodal_rigid_bodies(self) -> pd.DataFrame:
        """Get the table of constrained_nodal_rigid_bodies."""
        return self._cards[0].table

    @constrained_nodal_rigid_bodies.setter
    def constrained_nodal_rigid_bodies(self, df: pd.DataFrame):
        """Set constrained_nodal_rigid_bodies from the dataframe df"""
        self._cards[0].table = df

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[1].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def pnode_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all NODE keywords for pnode, keyed by pnode value."""
        return self._get_links_from_table("NODE", "nid", "constrained_nodal_rigid_bodies", "pnode", "parts")

    def get_pnode_link(self, pnode: int) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given pnode."""
        return self._get_link_by_attr("NODE", "nid", pnode, "parts")

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

    @property
    def nsid_link(self) -> KeywordBase:
        """Get the SET_NODE_* keyword for nsid."""
        return self._get_set_link("NODE", self.nsid)

    @nsid_link.setter
    def nsid_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for nsid."""
        self.nsid = value.sid

