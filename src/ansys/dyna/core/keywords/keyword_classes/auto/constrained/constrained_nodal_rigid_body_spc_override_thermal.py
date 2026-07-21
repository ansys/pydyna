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

"""Module providing the ConstrainedNodalRigidBodySpcOverrideThermal class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_CONSTRAINEDNODALRIGIDBODYSPCOVERRIDETHERMAL_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("cid", int, 10, 10, None),
    FieldSchema("nsid", int, 20, 10, None),
    FieldSchema("pnode", int, 30, 10, 0),
    FieldSchema("iprt", int, 40, 10, 0),
    FieldSchema("drflag", int, 50, 10, 0),
    FieldSchema("rrflag", int, 60, 10, 0),
)

_CONSTRAINEDNODALRIGIDBODYSPCOVERRIDETHERMAL_CARD1 = (
    FieldSchema("cmo", float, 0, 10, 0.0),
    FieldSchema("con1", int, 10, 10, 0),
    FieldSchema("con2", int, 20, 10, 0),
    FieldSchema("spcnid", int, 30, 10, 0),
    FieldSchema("xspc", float, 40, 10, 0.0),
    FieldSchema("yspc", float, 50, 10, 0.0),
    FieldSchema("zspc", float, 60, 10, 0.0),
)

_CONSTRAINEDNODALRIGIDBODYSPCOVERRIDETHERMAL_CARD2 = (
    FieldSchema("icnt", int, 0, 10, 0),
    FieldSchema("ibag", int, 10, 10, 0),
    FieldSchema("ipsm", int, 20, 10, 0),
)

_CONSTRAINEDNODALRIGIDBODYSPCOVERRIDETHERMAL_CARD3 = (
    FieldSchema("idthrm", int, 0, 10, 1),
)

_CONSTRAINEDNODALRIGIDBODYSPCOVERRIDETHERMAL_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class ConstrainedNodalRigidBodySpcOverrideThermal(KeywordBase):
    """DYNA CONSTRAINED_NODAL_RIGID_BODY_SPC_OVERRIDE_THERMAL keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "NODAL_RIGID_BODY_SPC_OVERRIDE_THERMAL"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "pnode": LinkType.NODE,
        "cid": LinkType.DEFINE_COORDINATE_SYSTEM,
        "nsid": LinkType.SET_NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the ConstrainedNodalRigidBodySpcOverrideThermal class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDNODALRIGIDBODYSPCOVERRIDETHERMAL_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDNODALRIGIDBODYSPCOVERRIDETHERMAL_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDNODALRIGIDBODYSPCOVERRIDETHERMAL_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDNODALRIGIDBODYSPCOVERRIDETHERMAL_CARD3,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = ConstrainedNodalRigidBodySpcOverrideThermal._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _CONSTRAINEDNODALRIGIDBODYSPCOVERRIDETHERMAL_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of the nodal rigid body.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Coordinate system ID for output of data in local system, see *DEFINE_COORDINATE_OPTION. Only necessary if no local system is defined below.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID, see *SET_NODE. This nodal set defines the rigid body.If NSID=0, then NSID=PID, i.e., the node set ID and the part ID are assumed to be identical.
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        """Set the nsid property."""
        self._cards[0].set_value("nsid", value)

    @property
    def pnode(self) -> int:
        """Get or set the An optional node (a massless node is allowed) used for post-processing rigid body data. If PNODE is not located at the rigid bodys center of mass, LS-DYNA resets the initial coordinates of PNODE to the center of mass.  If CID is defined, LS-DYNA outputs the velocities and accelerations of PNODE in the local system to the d3plot and d3thdt files unless you specify PNODE as a negative number, in which case the global system is used.
        """ # nopep8
        return self._cards[0].get_value("pnode")

    @pnode.setter
    def pnode(self, value: int) -> None:
        """Set the pnode property."""
        self._cards[0].set_value("pnode", value)

    @property
    def iprt(self) -> int:
        """Get or set the Print flag.  For nodal rigid bodies, the following values apply:
        EQ.-2: Do not write data into rbdout. Output the forces and moments in bndout with respect to the nodal rigid body�s local coordinate system.
        EQ.-1: Write data into rbdout. Output the forces and moments in bndout with respect to the nodal rigid body�s local coordinate system.
        EQ.0: Defaults to the value of IPRTF in *CONTROL_OUTPUT for all nodal rigid bodies except two-noded rigid bodies.
        For two-noded rigid bodies, printing to rbdout is suppressed. This behavior exists to avoid excessively large rbdout files when the model
        contains many two-noded welds. Note that IPRTF does not change the coordinate system for bndout. Thus, for all nodal rigid bodies, the forces
        and moments in the bndout file are expressed in the global coordinate system.
        EQ.1: Write data into rbdout.  Output the forces and moments in bndout with respect to the global coordinate system.
        EQ.2: Do not write data into rbdout. Output the forces and moments in bndout with respect to the global coordinate system.
        """ # nopep8
        return self._cards[0].get_value("iprt")

    @iprt.setter
    def iprt(self, value: int) -> None:
        """Set the iprt property."""
        self._cards[0].set_value("iprt", value)

    @property
    def drflag(self) -> int:
        """Get or set the Displacement release flag for all nodes except the first node in the definition.
        EQ.-7: release x, y, and z displacement in global system,
        EQ.-6: release z and x displacement in global system,
        EQ.-5: release y and z displacement in global system,
        EQ.-4: release x and y displacement in global system,
        EQ.-3: release z displacement in global system,
        EQ.-2: release y displacement in global system,
        EQ.-1: release x displacement in global system,
        EQ. 0: off for rigid body behavior,
        EQ. 1: release x displacement in rigid body local system,
        EQ. 2: release y displacement in rigid body local system,
        EQ. 3: release z displacement in rigid body local system,
        EQ. 4: release x and y displacement in rigid body local system,
        EQ. 5: release y and z displacement in rigid body local system,
        EQ. 6: release z and x displacement in rigid body local system,
        EQ. 7: release x, y, and z displacement in rigid body local system
        """ # nopep8
        return self._cards[0].get_value("drflag")

    @drflag.setter
    def drflag(self, value: int) -> None:
        """Set the drflag property."""
        if value not in [0, -7, -6, -5, -4, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, None]:
            raise Exception("""drflag must be `None` or one of {0,-7,-6,-5,-4,-3,-2,-1,1,2,3,4,5,6,7}.""")
        self._cards[0].set_value("drflag", value)

    @property
    def rrflag(self) -> int:
        """Get or set the Rotation release flag for all nodes except the first node in the definition.
        EQ.-7: release x, y, and z rotations in global system,
        EQ.-6: release z and x rotations in global system,
        EQ.-5: release y and z rotations in global system,
        EQ.-4: release x and y rotations in global system,
        EQ.-3: release z rotation in global system,
        EQ.-2: release y rotation in global system,
        EQ.-1: release x rotation in global system,
        EQ. 0: off for rigid body behavior,
        EQ. 1: release x rotation in rigid body local system,
        EQ. 2: release y rotation in rigid body local system,
        EQ. 3: release z rotation in rigid body local system,
        EQ. 4: release x and y rotations in rigid body local system,
        EQ. 5: release y and z rotations in rigid body local system,
        EQ. 6: release z and x rotations in rigid body local system,
        EQ. 7: release x, y, and z rotations in rigid body local system,
        """ # nopep8
        return self._cards[0].get_value("rrflag")

    @rrflag.setter
    def rrflag(self, value: int) -> None:
        """Set the rrflag property."""
        if value not in [0, -7, -6, -5, -4, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, None]:
            raise Exception("""rrflag must be `None` or one of {0,-7,-6,-5,-4,-3,-2,-1,1,2,3,4,5,6,7}.""")
        self._cards[0].set_value("rrflag", value)

    @property
    def cmo(self) -> float:
        """Get or set the Constraint option, CMO (see Remark 4):
        EQ. + 2.0: Constraints applied in global directions at the coordinate given by XSPC, YSPCand ZSPC or the initial coordinates of node SPCNID.Unless prescribed motion is applied to the rigid body, the constraint coordinates are fixed in time.
        EQ. + 1.0: Constraints applied in global directions,
        EQ.0.0: No constraints,
        EQ. - 1.0: Constraints applied in local directions(SPC constraint).
        EQ. - 2.0: Constraints applied in local directions(SPC constraint) at the coordinate given by XSPC, YSPCand ZSPC or the initial coordinates of node SPCNID.Unless prescribed motion is applied to the rigid body, the constraint coordinates are fixed in time.
        """ # nopep8
        return self._cards[1].get_value("cmo")

    @cmo.setter
    def cmo(self, value: float) -> None:
        """Set the cmo property."""
        if value not in [0.0, -1.0, -2.0, 1.0, 2.0, None]:
            raise Exception("""cmo must be `None` or one of {0.0,-1.0,-2.0,1.0,2.0}.""")
        self._cards[1].set_value("cmo", value)

    @property
    def con1(self) -> int:
        """Get or set the First constraint parameter:
        If CMO>0, then specify global translational constraint:
        EQ.0: no constraints,
        EQ.1: constrained x displacement,
        EQ.2: constrained y displacement,
        EQ.3: constrained z displacement,
        EQ.4: constrained x and y displacements,
        EQ.5: constrained y and z displacements,
        EQ.6: constrained z and x displacements,
        EQ.7: constrained x, y, and z displacements.
        If CM0<0, then specify local coordinate system ID. See *DEFINE_ COORDINATE_OPTION: This coordinate system is fixed in time.
        """ # nopep8
        return self._cards[1].get_value("con1")

    @con1.setter
    def con1(self, value: int) -> None:
        """Set the con1 property."""
        self._cards[1].set_value("con1", value)

    @property
    def con2(self) -> int:
        """Get or set the If CMO>0, then specify global rotational constraint:
        EQ.0: no constraints,
        EQ.1: constrained x rotation,
        EQ.2: constrained y rotation,
        EQ.3: constrained z rotation,
        EQ.4: constrained x and y rotations,
        EQ.5: constrained y and z rotations,
        EQ.6: constrained z and x rotations,
        EQ.7: constrained x, y, and z rotations.
        If CM0<0, then specify local (SPC) constraint:
        EQ.000000 no constraint,
        EQ.100000 constrained x translation,
        EQ.010000 constrained y translation,
        EQ.001000 constrained z translation,
        EQ.000100 constrained x rotation,
        EQ.000010 constrained y rotation,
        EQ.000001 constrained z rotation.
        To specify a combination of local constraints, input the sum of the desired constraints
        """ # nopep8
        return self._cards[1].get_value("con2")

    @con2.setter
    def con2(self, value: int) -> None:
        """Set the con2 property."""
        self._cards[1].set_value("con2", value)

    @property
    def spcnid(self) -> int:
        """Get or set the For |CMO| = 2.0, the constraint coordinates (see below) are the (initial) coordinates of the node with this ID.
        """ # nopep8
        return self._cards[1].get_value("spcnid")

    @spcnid.setter
    def spcnid(self, value: int) -> None:
        """Set the spcnid property."""
        self._cards[1].set_value("spcnid", value)

    @property
    def xspc(self) -> float:
        """Get or set the Coordinate where the constraints act. Superseded by SPCNID.
        """ # nopep8
        return self._cards[1].get_value("xspc")

    @xspc.setter
    def xspc(self, value: float) -> None:
        """Set the xspc property."""
        self._cards[1].set_value("xspc", value)

    @property
    def yspc(self) -> float:
        """Get or set the Coordinate where the constraints act. Superseded by SPCNID.
        """ # nopep8
        return self._cards[1].get_value("yspc")

    @yspc.setter
    def yspc(self, value: float) -> None:
        """Set the yspc property."""
        self._cards[1].set_value("yspc", value)

    @property
    def zspc(self) -> float:
        """Get or set the Coordinate where the constraints act. Superseded by SPCNID.
        """ # nopep8
        return self._cards[1].get_value("zspc")

    @zspc.setter
    def zspc(self, value: float) -> None:
        """Set the zspc property."""
        self._cards[1].set_value("zspc", value)

    @property
    def icnt(self) -> int:
        """Get or set the Flag for contact synchronization:
        EQ.0: No synchronization,
        EQ.1: Since there exists no contact when both tracked and reference sides belong to the same rigid body,
        setting ICNT = 1 will turn off / on all contact definitions of which the tracked and reference sides belong to
        the same nodal rigid body PID when PID is turned on / off by *SENSOR_CONTROL.With ICNT=1, all related contact definitions need the optional ID, see *CONTACT.
        """ # nopep8
        return self._cards[2].get_value("icnt")

    @icnt.setter
    def icnt(self, value: int) -> None:
        """Set the icnt property."""
        if value not in [0, 1, None]:
            raise Exception("""icnt must be `None` or one of {0,1}.""")
        self._cards[2].set_value("icnt", value)

    @property
    def ibag(self) -> int:
        """Get or set the Flag for control volume airbag synchronization:
        EQ.0: No synchronization,
        EQ.1: Since airbag pressure will not change when all segments constituting the airbag belong to
        the same rigid body, setting IBAG = 1 will skip calculation of control volume airbags of
        which all the segments belong to the same nodal rigid body PID when PID is on.The airbag calculation will be resumed,
        with time offset to related airbag time - dependent curves, when *SENSOR_CONTROL turns off PID.The related airbag time-dependent curves will be offset to not include the time that PID is on.  With IBAG=1, all associated airbags need the optional ID, see *AIRBAG.
        """ # nopep8
        return self._cards[2].get_value("ibag")

    @ibag.setter
    def ibag(self, value: int) -> None:
        """Set the ibag property."""
        if value not in [0, 1, None]:
            raise Exception("""ibag must be `None` or one of {0,1}.""")
        self._cards[2].set_value("ibag", value)

    @property
    def ipsm(self) -> int:
        """Get or set the Flag for prescribed-motion synchronization:
        EQ.0: No synchronization,
        EQ.1:Prescribed boundary conditions (*BOUNDARY_PRESCRIBED_MOTION) for PID are turned off automatically when *SENSOR_CONTROL turns off PID.  A prescribed boundary condition not for PID and not for all nodes belonging to PID, that is, boundary conditions that only apply to some of the nodes belonging to PID, are turned off when PID is active to avoid boundary condition conflict.  Those boundary conditions are turned on when *SENSOR_CONTROL turns off PID. The related time-dependent curves will be offset to not include the time that PID is on.  With IPSM > 0, all related boundary prescribed motion cards need the optional ID; see *BOUNDARY_PRESCRIBED.
        EQ.2: Same as IPSM = 1, however, without time offset when those boundary conditions not for PID are turned on.
        """ # nopep8
        return self._cards[2].get_value("ipsm")

    @ipsm.setter
    def ipsm(self, value: int) -> None:
        """Set the ipsm property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""ipsm must be `None` or one of {0,1,2}.""")
        self._cards[2].set_value("ipsm", value)

    @property
    def idthrm(self) -> int:
        """Get or set the Flag for the thermal constraint option:
        EQ.1: Set the temperature of the first node defined in node set NSID as the average of the other nodal temperatures in the node set.
        LT.0: Set the temperature of node | IDTHRM | as the average of the nodal temperatures for the nodes in node set NSID.The average excludes node | IDTHERM | if it is a member of this node set.
        """ # nopep8
        return self._cards[3].get_value("idthrm")

    @idthrm.setter
    def idthrm(self, value: int) -> None:
        """Set the idthrm property."""
        self._cards[3].set_value("idthrm", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[4].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def pnode_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given pnode."""
        return self._get_link_by_attr("NODE", "nid", self.pnode, "parts")

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

    @property
    def nsid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for nsid."""
        return self._get_set_link("NODE", self.nsid)

    @nsid_link.setter
    def nsid_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for nsid."""
        self.nsid = value.sid

