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

"""Module providing the BoundaryPrescribedMotionSetLine class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_vector import DefineVector

_BOUNDARYPRESCRIBEDMOTIONSETLINE_CARD0 = (
    FieldSchema("typeid", int, 0, 10, None),
    FieldSchema("dof", int, 10, 10, 0),
    FieldSchema("vad", int, 20, 10, 0),
    FieldSchema("lcid", int, 30, 10, None),
    FieldSchema("sf", float, 40, 10, 1.0),
    FieldSchema("vid", int, 50, 10, 0),
    FieldSchema("death", float, 60, 10, 1e+28),
    FieldSchema("birth", float, 70, 10, 0.0),
)

_BOUNDARYPRESCRIBEDMOTIONSETLINE_CARD1 = (
    FieldSchema("offset1", float, 0, 10, 0.0),
    FieldSchema("offset2", float, 10, 10, 0.0),
    FieldSchema("lrb", int, 20, 10, 0),
    FieldSchema("node1", int, 30, 10, 0),
    FieldSchema("node2", int, 40, 10, 0),
)

_BOUNDARYPRESCRIBEDMOTIONSETLINE_CARD2 = (
    FieldSchema("nbeg", int, 0, 10, None),
    FieldSchema("nend", int, 10, 10, None),
)

class BoundaryPrescribedMotionSetLine(KeywordBase):
    """DYNA BOUNDARY_PRESCRIBED_MOTION_SET_LINE keyword"""

    keyword = "BOUNDARY"
    subkeyword = "PRESCRIBED_MOTION_SET_LINE"
    _link_fields = {
        "node1": LinkType.NODE,
        "node2": LinkType.NODE,
        "nbeg": LinkType.NODE,
        "nend": LinkType.NODE,
        "vid": LinkType.DEFINE_VECTOR,
        "typeid": LinkType.SET_NODE,
        "lrb": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the BoundaryPrescribedMotionSetLine class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYPRESCRIBEDMOTIONSETLINE_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BOUNDARYPRESCRIBEDMOTIONSETLINE_CARD1,
                active_func=lambda: abs(self.dof) in [9, 10, 11] or self.vad==4,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BOUNDARYPRESCRIBEDMOTIONSETLINE_CARD2,
                **kwargs,
            ),
        ]
    @property
    def typeid(self) -> typing.Optional[int]:
        """Get or set the nodal set ID (SID in *SET_NODE)
        """ # nopep8
        return self._cards[0].get_value("typeid")

    @typeid.setter
    def typeid(self, value: int) -> None:
        """Set the typeid property."""
        self._cards[0].set_value("typeid", value)

    @property
    def dof(self) -> int:
        """Get or set the Applicable degrees of freedom:
        EQ.1: x - translational degree of freedom for Cartesian systems(local or global).For a cylindrical or spherical system, this is the local radial degree of freedom r.To specify a local coordinate system, set VID to the negative of the local coordinate system ID.See Remark 9 and Figure 0 - 1.
        EQ.2: y - translational degree of freedom for Cartesian systems(local or global).For a cylindrical system, this is the local circumferential degree of freedom ?, and for a spherical system, it is the local latitude degree of freedom ?.To specify a local coordinate system, set VID to the negative of the local coordinate system ID.See Remark 9 and Figure 0 - 1.
        EQ.3: z - translational degree of freedom for Cartesian systems(local or global).For a cylindrical system, this is the local z - translational degree of freedom, and for a spherical system, it is the local longitude degree of freedom ?.To specify a local coordinate system, set VID to the negative of the local coordinate system ID.See Remark 9 and Figure 0 - 1.
        EQ.4: Translational motion in the direction given by the VID.Movement on a plane normal to the vector is permitted.
        EQ. - 4: Translational motion in the direction given by the VID.Movement on a plane normal to the vector is not permitted. In explicit analyses, this option only applies to rigid bodies if | CMO |= 2 on * MAT_RIGID or *CONSTRAINED_NODAL_RIGID_BODY.
        EQ.5: x - rotational degree of freedom.For a cylindrical or spherical system, this is the local radial degree of freedom r.To specify a local coordinate system, set VID to the negative of the local coordinate system ID.See Remark 9 and Figure 0 - 1.
        EQ.6: y - rotational degree of freedom.For a cylindrical system, this is the local circumferential degree of freedom ?, and for a spherical system, it is the local latitude degree of freedom ?.To specify a local coordinate system, set VID to the negative of the local coordinate system ID.See Remark 9 and Figure 0 - 1.
        EQ.7: z - rotational degree of freedom.For a cylindrical system, this is the local z - translational degree of freedom, and for a spherical system, it is the local longitude degree of freedom ?.To specify a local coordinate system, set VID to the negative of the local coordinate system ID.See Remark 9 and Figure 0 - 1.
        EQ.8: Rotational motion about an axis, parallel to vector VID,  passing through the center of gravity of the node, node set, or rigid body(or passing through a specified point when | CMO |= 2 on * MAT_RIGID or *CONSTRAINED_NODAL_RIGID_BODY).Rotation about the normal axes, namely, axes in a plane normal to VID, is permitted.
        EQ. - 8: Rotational motion about an axis, parallel to vector VID,  passing through the center of gravity of the node, node set, or rigid body(or passing through a specified point when | CMO |= 2 on * MAT_RIGID or *CONSTRAINED_NODAL_RIGID_BODY).Rotation about the normal axes is not permitted.This option only applies to rigid bodies if | CMO |= 2.
        EQ.9: Rotation with axis parallel to the x - axis and passing through the yz - plane at y = OFFSET1 and z = OFFSET2. Radial motion is NOT permitted.This option is not applicable to rigid bodies.
        EQ. - 9: Rotation with axis parallel to the x - axis and passing through the yz - plane at y = OFFSET1 and z = OFFSET2. Radial motion is permitted.This option is not applicable to rigid bodies.
        EQ.10: Rotation with axis parallel to the y - axis and passing through the zx - plane at z = OFFSET1 and x = OFFSET2. Radial motion is NOT permitted.This option is not applicable to rigid bodies.
        EQ. - 10: Rotation with axis parallel to the y - axis and passing through the zx - plane at z = OFFSET1 and x = OFFSET2. Radial motion is permitted.This option is not applicable to rigid bodies.
        EQ.11: Rotation with axis parallel to the z - axis and passing through the xy - plane at x = OFFSET1 and y = OFFSET2. Radial motion is NOT permitted.This option is not applicable to rigid bodies.
        EQ. - 11: Rotation with axis parallel to the z - axis and passing through the xy - plane at x = OFFSET1 and y = OFFSET2. Radial motion is permitted.This option is not applicable to rigid bodies.
        EQ.12: Translational motion in the direction given by the normals to the segments.This option is available only when using the SET_SEGMENT keyword option.
        """ # nopep8
        return self._cards[0].get_value("dof")

    @dof.setter
    def dof(self, value: int) -> None:
        """Set the dof property."""
        if value not in [0, 1, 2, 3, 4, -4, 5, 6, 7, 8, -8, 9, -9, 10, -10, 11, -11, 12, None]:
            raise Exception("""dof must be `None` or one of {0,1,2,3,4,-4,5,6,7,8,-8,9,-9,10,-10,11,-11,12}.""")
        self._cards[0].set_value("dof", value)

    @property
    def vad(self) -> int:
        """Get or set the Velocity/Acceleration/Displacement flag:
        EQ.0: Velocity(rigid bodies and nodes)
        EQ.1: Acceleration(rigid bodies and nodes)
        EQ.2: Displacement(rigid bodies and nodes; see Remark 2)
        EQ.3: Velocity as a function of displacement.This option only applies to rigid bodies with | CMO | !=2 on * MAT_RIGID or *CONSTRAINED_NODAL_RIGID_BODY.See Remark 3.
        EQ.4: Relative displacement.This option only applies to rigid bodies with | CMO | !=2 on * MAT_RIGID or *CONSTRAINED_NODAL_RIGID_BODY.See Remark 4.
        """ # nopep8
        return self._cards[0].get_value("vad")

    @vad.setter
    def vad(self, value: int) -> None:
        """Set the vad property."""
        if value not in [0, 1, 2, 3, 4, None]:
            raise Exception("""vad must be `None` or one of {0,1,2,3,4}.""")
        self._cards[0].set_value("vad", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Curve ID or function ID to describe motion value as a function of time; see *DEFINE_CURVE, *DEFINE_CURVE_FUNCTION, or *DEFINE_FUNCTION. If LCID refers to *DEFINE_FUNCTION, the function has four arguments: time and x, y and z coordinates of the node or rigid body, such as f(t,x,y,z)=10.0t+max(x-100,0.). If VAD = 2, the function has one argument which is time, such as f(t)=10.0t (see Remark 2). See BIRTH below.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def sf(self) -> float:
        """Get or set the Load curve scale factor.The default is 1.0).
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[0].set_value("sf", value)

    @property
    def vid(self) -> int:
        """Get or set the Direction and update option for the constraint.
        GT.0: Vector ID for DOF values of 4 or 8; see *DEFINE_VECTOR.The direction of this vector is not updated with time.
        LT.0: -VID is the coordinate system ID(see *DEFINE_COORDINATE_NODES and *DEFINE_COORDINATE_SYSTEM) for DOF values 1 through 3 when using a local coordinate system.See Remark 9 and Figure 0 - 1.
        """ # nopep8
        return self._cards[0].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        """Set the vid property."""
        self._cards[0].set_value("vid", value)

    @property
    def death(self) -> float:
        """Get or set the Removal time for the imposed motion/constraint. For alternatives to DEATH and BIRTH for specifying the transitory application of prescribed motion, see the SET_BOX option, or *DEFINE_DEATH_TIMES, or *SENSOR_CONTROL.
        EQ.0.0: default set to 1028
        """ # nopep8
        return self._cards[0].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        """Set the death property."""
        self._cards[0].set_value("death", value)

    @property
    def birth(self) -> float:
        """Get or set the Activation time for the imposed motion/constraint. The prescribed motion begins acting at time = BIRTH but with the motion from the zero abscissa value of the curve or function (*DEFINE_FUNCTION). In other words, the abscissae are shifted by an amount BIRTH, such that it has the same effect as setting OFFA = BIRTH in *DEFINE_CURVE. Warning: BIRTH is ignored if the LCID is defined as a function, as in *DEFINE_CURVE_FUNCTION.
        """ # nopep8
        return self._cards[0].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        """Set the birth property."""
        self._cards[0].set_value("birth", value)

    @property
    def offset1(self) -> float:
        """Get or set the Offset for DOF types 9-11 (y, z, x direction).
        """ # nopep8
        return self._cards[1].get_value("offset1")

    @offset1.setter
    def offset1(self, value: float) -> None:
        """Set the offset1 property."""
        self._cards[1].set_value("offset1", value)

    @property
    def offset2(self) -> float:
        """Get or set the Offset for DOF types 9-11 (z, x, y direction.)
        """ # nopep8
        return self._cards[1].get_value("offset2")

    @offset2.setter
    def offset2(self, value: float) -> None:
        """Set the offset2 property."""
        self._cards[1].set_value("offset2", value)

    @property
    def lrb(self) -> int:
        """Get or set the lead rigid body for measuring the relative displacement.
        """ # nopep8
        return self._cards[1].get_value("lrb")

    @lrb.setter
    def lrb(self, value: int) -> None:
        """Set the lrb property."""
        self._cards[1].set_value("lrb", value)

    @property
    def node1(self) -> int:
        """Get or set the Optional orientation node, n1, for relative displacement.
        """ # nopep8
        return self._cards[1].get_value("node1")

    @node1.setter
    def node1(self, value: int) -> None:
        """Set the node1 property."""
        self._cards[1].set_value("node1", value)

    @property
    def node2(self) -> int:
        """Get or set the Optional orientation node, n2, for relative displacement.
        """ # nopep8
        return self._cards[1].get_value("node2")

    @node2.setter
    def node2(self, value: int) -> None:
        """Set the node2 property."""
        self._cards[1].set_value("node2", value)

    @property
    def nbeg(self) -> typing.Optional[int]:
        """Get or set the Node ID of a starting node.
        """ # nopep8
        return self._cards[2].get_value("nbeg")

    @nbeg.setter
    def nbeg(self, value: int) -> None:
        """Set the nbeg property."""
        self._cards[2].set_value("nbeg", value)

    @property
    def nend(self) -> typing.Optional[int]:
        """Get or set the Node ID of an ending node. All existing nodes and new nodes
        generated by h-adaptive mesh refinement along the straight line
        connecting NBEG and NEND will be included in the prescribed
        boundary motions.
        """ # nopep8
        return self._cards[2].get_value("nend")

    @nend.setter
    def nend(self, value: int) -> None:
        """Set the nend property."""
        self._cards[2].set_value("nend", value)

    @property
    def node1_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given node1."""
        return self._get_link_by_attr("NODE", "nid", self.node1, "parts")

    @property
    def node2_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given node2."""
        return self._get_link_by_attr("NODE", "nid", self.node2, "parts")

    @property
    def nbeg_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nbeg."""
        return self._get_link_by_attr("NODE", "nid", self.nbeg, "parts")

    @property
    def nend_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nend."""
        return self._get_link_by_attr("NODE", "nid", self.nend, "parts")

    @property
    def vid_link(self) -> typing.Optional[DefineVector]:
        """Get the DefineVector object for vid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.vid:
                return kwd
        return None

    @vid_link.setter
    def vid_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for vid."""
        self.vid = value.vid

    @property
    def typeid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for typeid."""
        return self._get_set_link("NODE", self.typeid)

    @typeid_link.setter
    def typeid_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for typeid."""
        self.typeid = value.sid

    @property
    def lrb_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given lrb."""
        return self._get_link_by_attr("PART", "pid", self.lrb, "parts")

