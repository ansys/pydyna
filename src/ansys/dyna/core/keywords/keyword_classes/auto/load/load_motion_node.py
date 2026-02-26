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

"""Module providing the LoadMotionNode class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_LOADMOTIONNODE_CARD0 = (
    FieldSchema("node1", int, 0, 10, None),
    FieldSchema("dof1", int, 10, 10, 0),
    FieldSchema("lcid", int, 20, 10, None),
    FieldSchema("sf", float, 30, 10, 1.0),
    FieldSchema("cid1", int, 40, 10, 0),
    FieldSchema("node2", int, 50, 10, 0),
    FieldSchema("dof2", int, 60, 10, 0),
    FieldSchema("cid2", int, 70, 10, 0),
)

class LoadMotionNode(KeywordBase):
    """DYNA LOAD_MOTION_NODE keyword"""

    keyword = "LOAD"
    subkeyword = "MOTION_NODE"
    _link_fields = {
        "node1": LinkType.NODE,
        "node2": LinkType.NODE,
        "cid1": LinkType.DEFINE_COORDINATE_SYSTEM,
        "cid2": LinkType.DEFINE_COORDINATE_SYSTEM,
    }

    def __init__(self, **kwargs):
        """Initialize the LoadMotionNode class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADMOTIONNODE_CARD0,
                **kwargs,
            ),        ]
    @property
    def node1(self) -> typing.Optional[int]:
        """Get or set the Node ID for the concentrated force
        """ # nopep8
        return self._cards[0].get_value("node1")

    @node1.setter
    def node1(self, value: int) -> None:
        """Set the node1 property."""
        self._cards[0].set_value("node1", value)

    @property
    def dof1(self) -> int:
        """Get or set the Applicable degrees-of-freedom:
        EQ.0: Not valid, please use any of the other available options,
        EQ.1:  x-direction of load action,
        EQ.2:  y-direction of load action,
        EQ.3:  z-direction of load action,
        EQ.4:  moment about the x-axis,
        EQ.5:  moment about the y-axis,
        EQ.6:  moment about the z-axis.
        """ # nopep8
        return self._cards[0].get_value("dof1")

    @dof1.setter
    def dof1(self, value: int) -> None:
        """Set the dof1 property."""
        if value not in [0, 1, 2, 3, 4, 5, 6, None]:
            raise Exception("""dof1 must be `None` or one of {0,1,2,3,4,5,6}.""")
        self._cards[0].set_value("dof1", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID, see *DEFINE_CURVE. The  applied force is a function of the applicable degree-of-freedom of NODE2
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
    def cid1(self) -> int:
        """Get or set the Coordinate system ID (optional), see remark 1 on next page.
        """ # nopep8
        return self._cards[0].get_value("cid1")

    @cid1.setter
    def cid1(self, value: int) -> None:
        """Set the cid1 property."""
        self._cards[0].set_value("cid1", value)

    @property
    def node2(self) -> int:
        """Get or set the Node ID for calculating the force.
        """ # nopep8
        return self._cards[0].get_value("node2")

    @node2.setter
    def node2(self, value: int) -> None:
        """Set the node2 property."""
        self._cards[0].set_value("node2", value)

    @property
    def dof2(self) -> int:
        """Get or set the Applicable degrees-of-freedom:
        EQ. 1:  x-coordinate
        EQ. 2:  y-coordinate,
        EQ. 3:  z-coordinate,
        EQ. 4:  x-translational displacement,
        EQ. 5:  y-translational displacement,
        EQ. 6:  z-translational displacement,
        EQ. 7:  rotational displacement about the x-axis,
        EQ. 8:  rotational displacement about the y-axis,
        EQ. 9:  rotational displacement about the z-axis.
        EQ.10:  x-translational velocity,
        EQ.11:  y-translational velocity,
        EQ.12:  z-translational velocity,
        EQ.13:  rotational velocity about the x-axis,
        EQ.14:  rotational velocity about the y-axis,
        EQ.15:  rotational velocity about the z-axis.
        """ # nopep8
        return self._cards[0].get_value("dof2")

    @dof2.setter
    def dof2(self, value: int) -> None:
        """Set the dof2 property."""
        if value not in [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, None]:
            raise Exception("""dof2 must be `None` or one of {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}.""")
        self._cards[0].set_value("dof2", value)

    @property
    def cid2(self) -> int:
        """Get or set the Coordinate system ID (optional), see remark 1.
        """ # nopep8
        return self._cards[0].get_value("cid2")

    @cid2.setter
    def cid2(self, value: int) -> None:
        """Set the cid2 property."""
        self._cards[0].set_value("cid2", value)

    @property
    def node1_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given node1."""
        return self._get_link_by_attr("NODE", "nid", self.node1, "parts")

    @property
    def node2_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given node2."""
        return self._get_link_by_attr("NODE", "nid", self.node2, "parts")

    @property
    def cid1_link(self) -> typing.Optional[DefineCoordinateSystem]:
        """Get the DefineCoordinateSystem object for cid1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "COORDINATE_SYSTEM"):
            if kwd.cid == self.cid1:
                return kwd
        return None

    @cid1_link.setter
    def cid1_link(self, value: DefineCoordinateSystem) -> None:
        """Set the DefineCoordinateSystem object for cid1."""
        self.cid1 = value.cid

    @property
    def cid2_link(self) -> typing.Optional[DefineCoordinateSystem]:
        """Get the DefineCoordinateSystem object for cid2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "COORDINATE_SYSTEM"):
            if kwd.cid == self.cid2:
                return kwd
        return None

    @cid2_link.setter
    def cid2_link(self, value: DefineCoordinateSystem) -> None:
        """Set the DefineCoordinateSystem object for cid2."""
        self.cid2 = value.cid

