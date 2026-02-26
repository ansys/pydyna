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

"""Module providing the RigidwallGeometricFlatDisplay class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_box import DefineBox

_RIGIDWALLGEOMETRICFLATDISPLAY_CARD0 = (
    FieldSchema("nsid", int, 0, 10, None),
    FieldSchema("nsidex", int, 10, 10, 0),
    FieldSchema("boxid", int, 20, 10, 0),
    FieldSchema("birth", float, 30, 10, 0.0),
    FieldSchema("death", float, 40, 10, 1e+20),
)

_RIGIDWALLGEOMETRICFLATDISPLAY_CARD1 = (
    FieldSchema("xt", float, 0, 10, 0.0),
    FieldSchema("yt", float, 10, 10, 0.0),
    FieldSchema("zt", float, 20, 10, 0.0),
    FieldSchema("xh", float, 30, 10, 0.0),
    FieldSchema("yh", float, 40, 10, 0.0),
    FieldSchema("zh", float, 50, 10, 0.0),
    FieldSchema("fric", float, 60, 10, 0.0),
)

_RIGIDWALLGEOMETRICFLATDISPLAY_CARD2 = (
    FieldSchema("xhev", float, 0, 10, 0.0),
    FieldSchema("yhev", float, 10, 10, 0.0),
    FieldSchema("zhev", float, 20, 10, 0.0),
    FieldSchema("lenl", float, 30, 10, 0.0),
    FieldSchema("lenm", float, 40, 10, 0.0),
)

_RIGIDWALLGEOMETRICFLATDISPLAY_CARD3 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, 1e-09),
    FieldSchema("e", float, 20, 10, 0.0001),
    FieldSchema("pr", float, 30, 10, 0.3),
)

_RIGIDWALLGEOMETRICFLATDISPLAY_OPTION0_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("title", str, 10, 70, None),
)

class RigidwallGeometricFlatDisplay(KeywordBase):
    """DYNA RIGIDWALL_GEOMETRIC_FLAT_DISPLAY keyword"""

    keyword = "RIGIDWALL"
    subkeyword = "GEOMETRIC_FLAT_DISPLAY"
    option_specs = [
        OptionSpec("ID", -2, 1),
    ]
    _link_fields = {
        "boxid": LinkType.DEFINE_BOX,
        "nsid": LinkType.SET_NODE,
        "nsidex": LinkType.SET_NODE,
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the RigidwallGeometricFlatDisplay class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _RIGIDWALLGEOMETRICFLATDISPLAY_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _RIGIDWALLGEOMETRICFLATDISPLAY_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _RIGIDWALLGEOMETRICFLATDISPLAY_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _RIGIDWALLGEOMETRICFLATDISPLAY_CARD3,
                **kwargs,
            ),            OptionCardSet(
                option_spec = RigidwallGeometricFlatDisplay.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _RIGIDWALLGEOMETRICFLATDISPLAY_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID containing tracked nodes, see *SET_NODE_OPTION.
        EQ.0: all nodes are tracked with respects to the rigid wall.
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        """Set the nsid property."""
        self._cards[0].set_value("nsid", value)

    @property
    def nsidex(self) -> int:
        """Get or set the Node set ID containing nodes that exempted as tracked nodes, see *SET_NODE_OPTION.
        """ # nopep8
        return self._cards[0].get_value("nsidex")

    @nsidex.setter
    def nsidex(self, value: int) -> None:
        """Set the nsidex property."""
        self._cards[0].set_value("nsidex", value)

    @property
    def boxid(self) -> int:
        """Get or set the If defined, only nodes in box are included as tracked nodes for the rigid wall.
        """ # nopep8
        return self._cards[0].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        """Set the boxid property."""
        self._cards[0].set_value("boxid", value)

    @property
    def birth(self) -> float:
        """Get or set the Birth time of rigid wall.  The time values of the load curves that control the motion of the wall are offset by the birth time.
        """ # nopep8
        return self._cards[0].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        """Set the birth property."""
        self._cards[0].set_value("birth", value)

    @property
    def death(self) -> float:
        """Get or set the Death time of rigid wall.  At this time the wall is deleted from the calculation
        """ # nopep8
        return self._cards[0].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        """Set the death property."""
        self._cards[0].set_value("death", value)

    @property
    def xt(self) -> float:
        """Get or set the x-coordinate of tail of any outward drawn normal vector, n, originating on wall (tail) and terminating in space (head).
        """ # nopep8
        return self._cards[1].get_value("xt")

    @xt.setter
    def xt(self, value: float) -> None:
        """Set the xt property."""
        self._cards[1].set_value("xt", value)

    @property
    def yt(self) -> float:
        """Get or set the y-coordinate of tail of normal vector n.
        """ # nopep8
        return self._cards[1].get_value("yt")

    @yt.setter
    def yt(self, value: float) -> None:
        """Set the yt property."""
        self._cards[1].set_value("yt", value)

    @property
    def zt(self) -> float:
        """Get or set the z-coordinate of tail of normal vector n.
        """ # nopep8
        return self._cards[1].get_value("zt")

    @zt.setter
    def zt(self, value: float) -> None:
        """Set the zt property."""
        self._cards[1].set_value("zt", value)

    @property
    def xh(self) -> float:
        """Get or set the x-coordinate of head of normal vector n.
        """ # nopep8
        return self._cards[1].get_value("xh")

    @xh.setter
    def xh(self, value: float) -> None:
        """Set the xh property."""
        self._cards[1].set_value("xh", value)

    @property
    def yh(self) -> float:
        """Get or set the y-coordinate of head of normal vector n.
        """ # nopep8
        return self._cards[1].get_value("yh")

    @yh.setter
    def yh(self, value: float) -> None:
        """Set the yh property."""
        self._cards[1].set_value("yh", value)

    @property
    def zh(self) -> float:
        """Get or set the z-coordinate of head of normal vector n.
        """ # nopep8
        return self._cards[1].get_value("zh")

    @zh.setter
    def zh(self, value: float) -> None:
        """Set the zh property."""
        self._cards[1].set_value("zh", value)

    @property
    def fric(self) -> float:
        """Get or set the Coulomb friction coefficient, except as noted below:
        EQ.0.0: Frictionless sliding when in contact,
        EQ.1.0: No sliding when in contact
        """ # nopep8
        return self._cards[1].get_value("fric")

    @fric.setter
    def fric(self, value: float) -> None:
        """Set the fric property."""
        self._cards[1].set_value("fric", value)

    @property
    def xhev(self) -> float:
        """Get or set the x-coordinate of head of edge vector l.
        """ # nopep8
        return self._cards[2].get_value("xhev")

    @xhev.setter
    def xhev(self, value: float) -> None:
        """Set the xhev property."""
        self._cards[2].set_value("xhev", value)

    @property
    def yhev(self) -> float:
        """Get or set the y-coordinate of head of edge vector l.
        """ # nopep8
        return self._cards[2].get_value("yhev")

    @yhev.setter
    def yhev(self, value: float) -> None:
        """Set the yhev property."""
        self._cards[2].set_value("yhev", value)

    @property
    def zhev(self) -> float:
        """Get or set the z-coordinate of head of edge vector l.
        """ # nopep8
        return self._cards[2].get_value("zhev")

    @zhev.setter
    def zhev(self, value: float) -> None:
        """Set the zhev property."""
        self._cards[2].set_value("zhev", value)

    @property
    def lenl(self) -> float:
        """Get or set the Length of l edge.
        EQ.0.0: defines an infinite size plane.
        """ # nopep8
        return self._cards[2].get_value("lenl")

    @lenl.setter
    def lenl(self, value: float) -> None:
        """Set the lenl property."""
        self._cards[2].set_value("lenl", value)

    @property
    def lenm(self) -> float:
        """Get or set the Length of m edge.
        EQ.0.0: defines an infinite size plane.
        """ # nopep8
        return self._cards[2].get_value("lenm")

    @lenm.setter
    def lenm(self, value: float) -> None:
        """Set the lenm property."""
        self._cards[2].set_value("lenm", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Unique part ID for moving geometric rigid wall.  If zero, a part ID will be set that is larger than the maximum of all user defined part IDs.
        """ # nopep8
        return self._cards[3].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[3].set_value("pid", value)

    @property
    def ro(self) -> float:
        """Get or set the Density of rigid wall.
        """ # nopep8
        return self._cards[3].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[3].set_value("ro", value)

    @property
    def e(self) -> float:
        """Get or set the Youngs modulus.
        """ # nopep8
        return self._cards[3].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[3].set_value("e", value)

    @property
    def pr(self) -> float:
        """Get or set the Poissons ratio.
        """ # nopep8
        return self._cards[3].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[3].set_value("pr", value)

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Optional Rigidwall ID.
        """ # nopep8
        return self._cards[4].cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[4].cards[0].set_value("id", value)

        if value:
            self.activate_option("ID")

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Rigidwall id descriptor. It is suggested that unique descriptions be used.
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[4].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def boxid_link(self) -> typing.Optional[DefineBox]:
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
    def nsid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for nsid."""
        return self._get_set_link("NODE", self.nsid)

    @nsid_link.setter
    def nsid_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for nsid."""
        self.nsid = value.sid

    @property
    def nsidex_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for nsidex."""
        return self._get_set_link("NODE", self.nsidex)

    @nsidex_link.setter
    def nsidex_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for nsidex."""
        self.nsidex = value.sid

    @property
    def pid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

