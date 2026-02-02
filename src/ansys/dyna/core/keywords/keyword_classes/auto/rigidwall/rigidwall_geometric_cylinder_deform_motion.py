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

"""Module providing the RigidwallGeometricCylinderDeformMotion class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_box import DefineBox

_RIGIDWALLGEOMETRICCYLINDERDEFORMMOTION_CARD0 = (
    FieldSchema("nsid", int, 0, 10, None),
    FieldSchema("nsidex", int, 10, 10, 0),
    FieldSchema("boxid", int, 20, 10, 0),
    FieldSchema("birth", float, 30, 10, 0.0),
    FieldSchema("death", float, 40, 10, 1e+20),
)

_RIGIDWALLGEOMETRICCYLINDERDEFORMMOTION_CARD1 = (
    FieldSchema("xt", float, 0, 10, 0.0),
    FieldSchema("yt", float, 10, 10, 0.0),
    FieldSchema("zt", float, 20, 10, 0.0),
    FieldSchema("xh", float, 30, 10, 0.0),
    FieldSchema("yh", float, 40, 10, 0.0),
    FieldSchema("zh", float, 50, 10, 0.0),
    FieldSchema("fric", float, 60, 10, 0.0),
)

_RIGIDWALLGEOMETRICCYLINDERDEFORMMOTION_CARD2 = (
    FieldSchema("radcyl", float, 0, 10, None),
    FieldSchema("lencyl", float, 10, 10, None),
    FieldSchema("nsegs", int, 20, 10, None),
)

_RIGIDWALLGEOMETRICCYLINDERDEFORMMOTION_CARD3 = (
    FieldSchema("vl", float, 0, 10, None),
    FieldSchema("height", float, 10, 10, None),
)

_RIGIDWALLGEOMETRICCYLINDERDEFORMMOTION_CARD4 = (
    FieldSchema("xp", float, 0, 10, None),
    FieldSchema("yp", float, 10, 10, None),
    FieldSchema("zp", float, 20, 10, None),
    FieldSchema("nl", int, 30, 10, None),
    FieldSchema("narc", int, 40, 10, None),
    FieldSchema("nr", int, 50, 10, None),
)

_RIGIDWALLGEOMETRICCYLINDERDEFORMMOTION_CARD5 = (
    FieldSchema("lcidr", int, 0, 10, None),
    FieldSchema("lcida", int, 10, 10, None),
    FieldSchema("lcidb", int, 20, 10, None),
    FieldSchema("lcidg", int, 30, 10, None),
)

_RIGIDWALLGEOMETRICCYLINDERDEFORMMOTION_CARD6 = (
    FieldSchema("lcid", int, 0, 10, None),
    FieldSchema("opt", int, 10, 10, 0),
    FieldSchema("vx", float, 20, 10, None),
    FieldSchema("vy", float, 30, 10, None),
    FieldSchema("vz", float, 40, 10, None),
)

_RIGIDWALLGEOMETRICCYLINDERDEFORMMOTION_OPTION0_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("title", str, 10, 70, None),
)

class RigidwallGeometricCylinderDeformMotion(KeywordBase):
    """DYNA RIGIDWALL_GEOMETRIC_CYLINDER_DEFORM_MOTION keyword"""

    keyword = "RIGIDWALL"
    subkeyword = "GEOMETRIC_CYLINDER_DEFORM_MOTION"
    option_specs = [
        OptionSpec("ID", -2, 1),
    ]
    _link_fields = {
        "lcidr": LinkType.DEFINE_CURVE,
        "lcida": LinkType.DEFINE_CURVE,
        "lcidb": LinkType.DEFINE_CURVE,
        "lcidg": LinkType.DEFINE_CURVE,
        "lcid": LinkType.DEFINE_CURVE,
        "boxid": LinkType.DEFINE_BOX,
        "nsid": LinkType.SET_NODE,
        "nsidex": LinkType.SET_NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the RigidwallGeometricCylinderDeformMotion class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _RIGIDWALLGEOMETRICCYLINDERDEFORMMOTION_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _RIGIDWALLGEOMETRICCYLINDERDEFORMMOTION_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _RIGIDWALLGEOMETRICCYLINDERDEFORMMOTION_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _RIGIDWALLGEOMETRICCYLINDERDEFORMMOTION_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _RIGIDWALLGEOMETRICCYLINDERDEFORMMOTION_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _RIGIDWALLGEOMETRICCYLINDERDEFORMMOTION_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _RIGIDWALLGEOMETRICCYLINDERDEFORMMOTION_CARD6,
                **kwargs,
            ),            OptionCardSet(
                option_spec = RigidwallGeometricCylinderDeformMotion.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _RIGIDWALLGEOMETRICCYLINDERDEFORMMOTION_OPTION0_CARD0,
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
    def radcyl(self) -> typing.Optional[float]:
        """Get or set the Radius of cylinder.
        """ # nopep8
        return self._cards[2].get_value("radcyl")

    @radcyl.setter
    def radcyl(self, value: float) -> None:
        """Set the radcyl property."""
        self._cards[2].set_value("radcyl", value)

    @property
    def lencyl(self) -> typing.Optional[float]:
        """Get or set the Length of cylinder. Only if a value larger than zero is specified is a finite length is assumed.
        """ # nopep8
        return self._cards[2].get_value("lencyl")

    @lencyl.setter
    def lencyl(self, value: float) -> None:
        """Set the lencyl property."""
        self._cards[2].set_value("lencyl", value)

    @property
    def nsegs(self) -> typing.Optional[int]:
        """Get or set the Number of subsections
        """ # nopep8
        return self._cards[2].get_value("nsegs")

    @nsegs.setter
    def nsegs(self, value: int) -> None:
        """Set the nsegs property."""
        self._cards[2].set_value("nsegs", value)

    @property
    def vl(self) -> typing.Optional[float]:
        """Get or set the Distance from the Cylinder base
        """ # nopep8
        return self._cards[3].get_value("vl")

    @vl.setter
    def vl(self, value: float) -> None:
        """Set the vl property."""
        self._cards[3].set_value("vl", value)

    @property
    def height(self) -> typing.Optional[float]:
        """Get or set the Section height
        """ # nopep8
        return self._cards[3].get_value("height")

    @height.setter
    def height(self, value: float) -> None:
        """Set the height property."""
        self._cards[3].set_value("height", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point in the local xz-plane, see Remark 1.
        """ # nopep8
        return self._cards[4].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[4].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point in the local xz-plane, see Remark 1.
        """ # nopep8
        return self._cards[4].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[4].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point in the local xz-plane, see Remark 1.
        """ # nopep8
        return self._cards[4].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[4].set_value("zp", value)

    @property
    def nl(self) -> typing.Optional[int]:
        """Get or set the Number of auto-generated elements in the longitudinal direction. If DISPLAY option is not used, NL will be ignored. See Remark 2.
        """ # nopep8
        return self._cards[4].get_value("nl")

    @nl.setter
    def nl(self, value: int) -> None:
        """Set the nl property."""
        self._cards[4].set_value("nl", value)

    @property
    def narc(self) -> typing.Optional[int]:
        """Get or set the Number of auto-generated elements in the circumferential direction. If DISPLAY option is not used, NARC will be ignored.
        """ # nopep8
        return self._cards[4].get_value("narc")

    @narc.setter
    def narc(self, value: int) -> None:
        """Set the narc property."""
        self._cards[4].set_value("narc", value)

    @property
    def nr(self) -> typing.Optional[int]:
        """Get or set the Number of auto-generated elements in the radius direction. If DISPLAY option is not used, NR will be ignored.
        """ # nopep8
        return self._cards[4].get_value("nr")

    @nr.setter
    def nr(self, value: int) -> None:
        """Set the nr property."""
        self._cards[4].set_value("nr", value)

    @property
    def lcidr(self) -> typing.Optional[int]:
        """Get or set the Curve ID to describe the change of the radius over time.
        """ # nopep8
        return self._cards[5].get_value("lcidr")

    @lcidr.setter
    def lcidr(self, value: int) -> None:
        """Set the lcidr property."""
        self._cards[5].set_value("lcidr", value)

    @property
    def lcida(self) -> typing.Optional[int]:
        """Get or set the Curve ID to describe the change of the rotation in radians about the local x-axis over time.
        """ # nopep8
        return self._cards[5].get_value("lcida")

    @lcida.setter
    def lcida(self, value: int) -> None:
        """Set the lcida property."""
        self._cards[5].set_value("lcida", value)

    @property
    def lcidb(self) -> typing.Optional[int]:
        """Get or set the Curve ID to describe the change of the bending curvature over time. Bending occurs
        in the local xz-plane with the center of the bending lying on the negative side of the local x-axis.
        """ # nopep8
        return self._cards[5].get_value("lcidb")

    @lcidb.setter
    def lcidb(self, value: int) -> None:
        """Set the lcidb property."""
        self._cards[5].set_value("lcidb", value)

    @property
    def lcidg(self) -> typing.Optional[int]:
        """Get or set the Curve ID to describe the change of the rotation in radians about the local z-axis over time.
        """ # nopep8
        return self._cards[5].get_value("lcidg")

    @lcidg.setter
    def lcidg(self, value: int) -> None:
        """Set the lcidg property."""
        self._cards[5].set_value("lcidg", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Stonewall motion curve number, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[6].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[6].set_value("lcid", value)

    @property
    def opt(self) -> int:
        """Get or set the Type of motion:
        EQ.0: velocity specified,
        EQ.1: displacement specified.
        """ # nopep8
        return self._cards[6].get_value("opt")

    @opt.setter
    def opt(self, value: int) -> None:
        """Set the opt property."""
        if value not in [0, 1, None]:
            raise Exception("""opt must be `None` or one of {0,1}.""")
        self._cards[6].set_value("opt", value)

    @property
    def vx(self) -> typing.Optional[float]:
        """Get or set the x-direction cosine of velocity/displacement vector.
        """ # nopep8
        return self._cards[6].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        """Set the vx property."""
        self._cards[6].set_value("vx", value)

    @property
    def vy(self) -> typing.Optional[float]:
        """Get or set the y-direction cosine of velocity/displacement vector.
        """ # nopep8
        return self._cards[6].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        """Set the vy property."""
        self._cards[6].set_value("vy", value)

    @property
    def vz(self) -> typing.Optional[float]:
        """Get or set the z-direction cosine of velocity/displacement vector.
        """ # nopep8
        return self._cards[6].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        """Set the vz property."""
        self._cards[6].set_value("vz", value)

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Optional Rigidwall ID.
        """ # nopep8
        return self._cards[7].cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[7].cards[0].set_value("id", value)

        if value:
            self.activate_option("ID")

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Rigidwall id descriptor. It is suggested that unique descriptions be used.
        """ # nopep8
        return self._cards[7].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[7].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def lcidr_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidr."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidr:
                return kwd
        return None

    @lcidr_link.setter
    def lcidr_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidr."""
        self.lcidr = value.lcid

    @property
    def lcida_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcida."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcida:
                return kwd
        return None

    @lcida_link.setter
    def lcida_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcida."""
        self.lcida = value.lcid

    @property
    def lcidb_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidb."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidb:
                return kwd
        return None

    @lcidb_link.setter
    def lcidb_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidb."""
        self.lcidb = value.lcid

    @property
    def lcidg_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidg."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidg:
                return kwd
        return None

    @lcidg_link.setter
    def lcidg_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidg."""
        self.lcidg = value.lcid

    @property
    def lcid_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid:
                return kwd
        return None

    @lcid_link.setter
    def lcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid."""
        self.lcid = value.lcid

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

