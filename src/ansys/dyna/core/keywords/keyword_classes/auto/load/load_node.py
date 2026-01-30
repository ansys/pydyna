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

"""Module providing the LoadNode class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_LOADNODE_CARD0 = (
    FieldSchema("nid", int, 0, 10, None),
    FieldSchema("dof", int, 10, 10, 0),
    FieldSchema("lcid", int, 20, 10, None),
    FieldSchema("sf", float, 30, 10, 1.0),
    FieldSchema("cid", int, 40, 10, 0),
    FieldSchema("m1", int, 50, 10, 0),
    FieldSchema("m2", int, 60, 10, 0),
    FieldSchema("m3", int, 70, 10, 0),
)

class LoadNode(KeywordBase):
    """DYNA LOAD_NODE keyword"""

    keyword = "LOAD"
    subkeyword = "NODE"
    _link_fields = {
        "nid": LinkType.NODE,
        "m1": LinkType.NODE,
        "m2": LinkType.NODE,
        "m3": LinkType.NODE,
        "cid": LinkType.DEFINE_COORDINATE_SYSTEM,
    }

    def __init__(self, **kwargs):
        """Initialize the LoadNode class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADNODE_CARD0,
                **kwargs,
            ),        ]
    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node ID, see also *NODE.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[0].set_value("nid", value)

    @property
    def dof(self) -> int:
        """Get or set the Applicable degrees-of-freedom:
        EQ.0: Not valid, please use any of the other available options,
        EQ.1: x-direction of load action (default),
        EQ.2: y-direction of load action,
        EQ.3: z-direction of load action,
        EQ.4: follower force,
        EQ.5: moment about the x-axis,
        EQ.6: moment about the y-axis,
        EQ.7: moment about the z-axis,
        EQ.8: follower moment.
        """ # nopep8
        return self._cards[0].get_value("dof")

    @dof.setter
    def dof(self, value: int) -> None:
        """Set the dof property."""
        if value not in [0, 1, 2, 3, 4, 5, 6, 7, 8, None]:
            raise Exception("""dof must be `None` or one of {0,1,2,3,4,5,6,7,8}.""")
        self._cards[0].set_value("dof", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID(see *DEFINE_CURVE) or function ID(see *DEFINE_FUNCTION).
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def sf(self) -> float:
        """Get or set the Load curve scale factor.
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[0].set_value("sf", value)

    @property
    def cid(self) -> int:
        """Get or set the Coordinate system ID (optional).
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def m1(self) -> int:
        """Get or set the Node 1 ID. Only necessary if DOF EQ.4 or EQ.8.
        """ # nopep8
        return self._cards[0].get_value("m1")

    @m1.setter
    def m1(self, value: int) -> None:
        """Set the m1 property."""
        self._cards[0].set_value("m1", value)

    @property
    def m2(self) -> int:
        """Get or set the Node 2 ID. Only necessary if DOF EQ.4 or EQ.8.
        """ # nopep8
        return self._cards[0].get_value("m2")

    @m2.setter
    def m2(self, value: int) -> None:
        """Set the m2 property."""
        self._cards[0].set_value("m2", value)

    @property
    def m3(self) -> int:
        """Get or set the Node 3 ID. Only necessary if DOF EQ.4 or EQ.8.
        """ # nopep8
        return self._cards[0].get_value("m3")

    @m3.setter
    def m3(self, value: int) -> None:
        """Set the m3 property."""
        self._cards[0].set_value("m3", value)

    @property
    def nid_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given nid."""
        return self._get_link_by_attr("NODE", "nid", self.nid, "parts")

    @property
    def m1_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given m1."""
        return self._get_link_by_attr("NODE", "nid", self.m1, "parts")

    @property
    def m2_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given m2."""
        return self._get_link_by_attr("NODE", "nid", self.m2, "parts")

    @property
    def m3_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given m3."""
        return self._get_link_by_attr("NODE", "nid", self.m3, "parts")

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

