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

"""Module providing the RigidwallPlanarMovingForcesId class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_box import DefineBox

_RIGIDWALLPLANARMOVINGFORCESID_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("title", str, 10, 70, None),
)

_RIGIDWALLPLANARMOVINGFORCESID_CARD1 = (
    FieldSchema("nsid", int, 0, 10, None),
    FieldSchema("nsidex", int, 10, 10, 0),
    FieldSchema("boxid", int, 20, 10, 0),
    FieldSchema("offset", float, 30, 10, 0.0),
    FieldSchema("birth", float, 40, 10, 0.0),
    FieldSchema("death", float, 50, 10, 1e+20),
    FieldSchema("rwksf", float, 60, 10, 1.0),
)

_RIGIDWALLPLANARMOVINGFORCESID_CARD2 = (
    FieldSchema("xt", float, 0, 10, 0.0),
    FieldSchema("yt", float, 10, 10, 0.0),
    FieldSchema("zt", float, 20, 10, 0.0),
    FieldSchema("xh", float, 30, 10, 0.0),
    FieldSchema("yh", float, 40, 10, 0.0),
    FieldSchema("zh", float, 50, 10, 0.0),
    FieldSchema("fric", float, 60, 10, 0.0),
    FieldSchema("wvel", float, 70, 10, 0.0),
)

_RIGIDWALLPLANARMOVINGFORCESID_CARD3 = (
    FieldSchema("mass", float, 0, 10, None),
    FieldSchema("v0", float, 10, 10, 0.0),
)

_RIGIDWALLPLANARMOVINGFORCESID_CARD4 = (
    FieldSchema("soft", int, 0, 10, 0),
    FieldSchema("ssid", int, 10, 10, 0),
    FieldSchema("n1", int, 20, 10, 0),
    FieldSchema("n2", int, 30, 10, 0),
    FieldSchema("n3", int, 40, 10, 0),
    FieldSchema("n4", int, 50, 10, 0),
)

class RigidwallPlanarMovingForcesId(KeywordBase):
    """DYNA RIGIDWALL_PLANAR_MOVING_FORCES_ID keyword"""

    keyword = "RIGIDWALL"
    subkeyword = "PLANAR_MOVING_FORCES_ID"
    _link_fields = {
        "n1": LinkType.NODE,
        "n2": LinkType.NODE,
        "n3": LinkType.NODE,
        "n4": LinkType.NODE,
        "boxid": LinkType.DEFINE_BOX,
        "nsid": LinkType.SET_NODE,
        "nsidex": LinkType.SET_NODE,
        "ssid": LinkType.SET_SEGMENT,
    }

    def __init__(self, **kwargs):
        """Initialize the RigidwallPlanarMovingForcesId class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _RIGIDWALLPLANARMOVINGFORCESID_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _RIGIDWALLPLANARMOVINGFORCESID_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _RIGIDWALLPLANARMOVINGFORCESID_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _RIGIDWALLPLANARMOVINGFORCESID_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _RIGIDWALLPLANARMOVINGFORCESID_CARD4,
                **kwargs,
            ),        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Optional Rigidwall ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Ridigwall id descriptor. It is suggested that unique descriptions be used.
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[0].set_value("title", value)

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID containing tracked nodes, see *SET_NODE_OPTION.
        EQ.0: All nodes are tracked for interacting with the rigid wall.
        """ # nopep8
        return self._cards[1].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        """Set the nsid property."""
        self._cards[1].set_value("nsid", value)

    @property
    def nsidex(self) -> int:
        """Get or set the Node set ID containing nodes that exempted as tracked nodes, see *SET_NODE_OPTION.
        """ # nopep8
        return self._cards[1].get_value("nsidex")

    @nsidex.setter
    def nsidex(self, value: int) -> None:
        """Set the nsidex property."""
        self._cards[1].set_value("nsidex", value)

    @property
    def boxid(self) -> int:
        """Get or set the All nodes in box are included as tracked nodes for the interacting with ther rigid wall, see *DEFINE_BOX. If options NSID or NSIDEX are active then only the subset of nodes activated by these options are checked to see if they are within the box.
        """ # nopep8
        return self._cards[1].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        """Set the boxid property."""
        self._cards[1].set_value("boxid", value)

    @property
    def offset(self) -> float:
        """Get or set the All nodes within a normal offset distance, OFFSET, to the rigid wall are included as tracked nodes for the rigid wall. If options NSID, NSIDEX, or BOXID are active then only the subset of nodes activated by these options are checked to see if they are within the offset distance.
        """ # nopep8
        return self._cards[1].get_value("offset")

    @offset.setter
    def offset(self, value: float) -> None:
        """Set the offset property."""
        self._cards[1].set_value("offset", value)

    @property
    def birth(self) -> float:
        """Get or set the Birth time of rigid wall.  The time values of the load curves that control the motion of the wall are offset by the birth time.
        """ # nopep8
        return self._cards[1].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        """Set the birth property."""
        self._cards[1].set_value("birth", value)

    @property
    def death(self) -> float:
        """Get or set the Death time of rigid wall.  At this time the wall is deleted from the calculation
        """ # nopep8
        return self._cards[1].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        """Set the death property."""
        self._cards[1].set_value("death", value)

    @property
    def rwksf(self) -> float:
        """Get or set the Stiffness scaling factor. If RWKSF is also specified in *CONTROL_ CONTACT, the stiffness is scaled by the product of the two values.
        """ # nopep8
        return self._cards[1].get_value("rwksf")

    @rwksf.setter
    def rwksf(self, value: float) -> None:
        """Set the rwksf property."""
        self._cards[1].set_value("rwksf", value)

    @property
    def xt(self) -> float:
        """Get or set the x-coordinate of tail of any outward drawn normal vector, n, originating on wall (tail) and terminating in space (head).
        """ # nopep8
        return self._cards[2].get_value("xt")

    @xt.setter
    def xt(self, value: float) -> None:
        """Set the xt property."""
        self._cards[2].set_value("xt", value)

    @property
    def yt(self) -> float:
        """Get or set the y-coordinate of tail of normal vector n.
        """ # nopep8
        return self._cards[2].get_value("yt")

    @yt.setter
    def yt(self, value: float) -> None:
        """Set the yt property."""
        self._cards[2].set_value("yt", value)

    @property
    def zt(self) -> float:
        """Get or set the z-coordinate of tail of normal vector n
        """ # nopep8
        return self._cards[2].get_value("zt")

    @zt.setter
    def zt(self, value: float) -> None:
        """Set the zt property."""
        self._cards[2].set_value("zt", value)

    @property
    def xh(self) -> float:
        """Get or set the x-coordinate of head of normal vector n
        """ # nopep8
        return self._cards[2].get_value("xh")

    @xh.setter
    def xh(self, value: float) -> None:
        """Set the xh property."""
        self._cards[2].set_value("xh", value)

    @property
    def yh(self) -> float:
        """Get or set the y-coordinate of head of normal vector n.
        """ # nopep8
        return self._cards[2].get_value("yh")

    @yh.setter
    def yh(self, value: float) -> None:
        """Set the yh property."""
        self._cards[2].set_value("yh", value)

    @property
    def zh(self) -> float:
        """Get or set the z-coordinate of head of normal vector n.
        """ # nopep8
        return self._cards[2].get_value("zh")

    @zh.setter
    def zh(self, value: float) -> None:
        """Set the zh property."""
        self._cards[2].set_value("zh", value)

    @property
    def fric(self) -> float:
        """Get or set the Interface friction:
        EQ.0.0: frictionless sliding after contact,
        EQ.1.0: no sliding after contact, 0.0 < FRIC < 1.0: Coulomb friction coefficient,
        EQ.2.0: node is welded after contact with frictionless sliding. Welding occurs if and only if the normal value of the impact velocity exceeds the critical value specified by WVEL,
        EQ.3.0: node is welded after contact with no sliding. Welding occurs if and only if the normal value of the impact velocity exceeds the critical value specified by WVEL.
        """ # nopep8
        return self._cards[2].get_value("fric")

    @fric.setter
    def fric(self, value: float) -> None:
        """Set the fric property."""
        self._cards[2].set_value("fric", value)

    @property
    def wvel(self) -> float:
        """Get or set the Critical normal velocity at which nodes weld to wall (FRIC = 2.0 or 3.0).
        """ # nopep8
        return self._cards[2].get_value("wvel")

    @wvel.setter
    def wvel(self, value: float) -> None:
        """Set the wvel property."""
        self._cards[2].set_value("wvel", value)

    @property
    def mass(self) -> typing.Optional[float]:
        """Get or set the Total mass of stonewall.
        """ # nopep8
        return self._cards[3].get_value("mass")

    @mass.setter
    def mass(self, value: float) -> None:
        """Set the mass property."""
        self._cards[3].set_value("mass", value)

    @property
    def v0(self) -> float:
        """Get or set the Initial velocity of stonewall in direction of defining vector, n.
        """ # nopep8
        return self._cards[3].get_value("v0")

    @v0.setter
    def v0(self, value: float) -> None:
        """Set the v0 property."""
        self._cards[3].set_value("v0", value)

    @property
    def soft(self) -> int:
        """Get or set the Number of cycles to zero relative velocity to reduce force spike
        """ # nopep8
        return self._cards[4].get_value("soft")

    @soft.setter
    def soft(self, value: int) -> None:
        """Set the soft property."""
        self._cards[4].set_value("soft", value)

    @property
    def ssid(self) -> int:
        """Get or set the Segment set ID for defining areas for force output, see *SET_SEGMENT.
        """ # nopep8
        return self._cards[4].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[4].set_value("ssid", value)

    @property
    def n1(self) -> int:
        """Get or set the Optional nodal point for visualization in LS-DYNA database.
        """ # nopep8
        return self._cards[4].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        """Set the n1 property."""
        self._cards[4].set_value("n1", value)

    @property
    def n2(self) -> int:
        """Get or set the Optional nodal point for visualization.
        """ # nopep8
        return self._cards[4].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        """Set the n2 property."""
        self._cards[4].set_value("n2", value)

    @property
    def n3(self) -> int:
        """Get or set the Optional nodal point for visualization.
        """ # nopep8
        return self._cards[4].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        """Set the n3 property."""
        self._cards[4].set_value("n3", value)

    @property
    def n4(self) -> int:
        """Get or set the Optional nodal point for visualization.
        """ # nopep8
        return self._cards[4].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        """Set the n4 property."""
        self._cards[4].set_value("n4", value)

    @property
    def n1_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n1."""
        return self._get_link_by_attr("NODE", "nid", self.n1, "parts")

    @property
    def n2_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n2."""
        return self._get_link_by_attr("NODE", "nid", self.n2, "parts")

    @property
    def n3_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n3."""
        return self._get_link_by_attr("NODE", "nid", self.n3, "parts")

    @property
    def n4_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n4."""
        return self._get_link_by_attr("NODE", "nid", self.n4, "parts")

    @property
    def boxid_link(self) -> DefineBox:
        """Get the DefineBox object for boxid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "BOX"):
            if kwd.boxid == self.boxid:
                return kwd
        return None

    @boxid_link.setter
    def boxid_link(self, value: DefineBox) -> None:
        """Set the DefineBox object for boxid."""
        self.boxid = value.boxid

    @property
    def nsid_link(self) -> KeywordBase:
        """Get the SET_NODE_* keyword for nsid."""
        return self._get_set_link("NODE", self.nsid)

    @nsid_link.setter
    def nsid_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for nsid."""
        self.nsid = value.sid

    @property
    def nsidex_link(self) -> KeywordBase:
        """Get the SET_NODE_* keyword for nsidex."""
        return self._get_set_link("NODE", self.nsidex)

    @nsidex_link.setter
    def nsidex_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for nsidex."""
        self.nsidex = value.sid

    @property
    def ssid_link(self) -> KeywordBase:
        """Get the SET_SEGMENT_* keyword for ssid."""
        return self._get_set_link("SEGMENT", self.ssid)

    @ssid_link.setter
    def ssid_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for ssid."""
        self.ssid = value.sid

