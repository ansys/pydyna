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

"""Module providing the BoundaryTemperatureTrajectory class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_BOUNDARYTEMPERATURETRAJECTORY_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("pype", int, 10, 10, 1),
    FieldSchema("nsid1", int, 20, 10, None),
    FieldSchema("spd1", float, 30, 10, None),
    FieldSchema("nsid2", int, 40, 10, None),
    FieldSchema("spd2", float, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("relvel", int, 70, 10, 0),
)

_BOUNDARYTEMPERATURETRAJECTORY_CARD1 = (
    FieldSchema("iform", int, 0, 10, 1),
    FieldSchema("lcid", int, 10, 10, None),
    FieldSchema("tmult", float, 20, 10, None),
    FieldSchema("lcrot", int, 30, 10, None),
    FieldSchema("lcmov", int, 40, 10, None),
    FieldSchema("lclat", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

_BOUNDARYTEMPERATURETRAJECTORY_CARD2 = (
    FieldSchema("p1", float, 0, 10, None),
    FieldSchema("p2", float, 10, 10, None),
    FieldSchema("p3", float, 20, 10, None),
    FieldSchema("p4", float, 30, 10, None),
    FieldSchema("p5", float, 40, 10, None),
    FieldSchema("p6", float, 50, 10, None),
    FieldSchema("p7", float, 60, 10, None),
    FieldSchema("p8", float, 70, 10, None),
)

_BOUNDARYTEMPERATURETRAJECTORY_CARD3 = (
    FieldSchema("tx", float, 0, 10, None),
    FieldSchema("ty", float, 10, 10, None),
    FieldSchema("tz", float, 20, 10, None),
)

class BoundaryTemperatureTrajectory(KeywordBase):
    """DYNA BOUNDARY_TEMPERATURE_TRAJECTORY keyword"""

    keyword = "BOUNDARY"
    subkeyword = "TEMPERATURE_TRAJECTORY"
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
        "lcrot": LinkType.DEFINE_CURVE,
        "lcmov": LinkType.DEFINE_CURVE,
        "lclat": LinkType.DEFINE_CURVE,
        "nsid1": LinkType.SET_NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the BoundaryTemperatureTrajectory class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYTEMPERATURETRAJECTORY_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _BOUNDARYTEMPERATURETRAJECTORY_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _BOUNDARYTEMPERATURETRAJECTORY_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _BOUNDARYTEMPERATURETRAJECTORY_CARD3,
                **kwargs,
            ),        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID or part set ID to what the temperature boundary condition will be applied on.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def pype(self) -> int:
        """Get or set the PID type:
        EQ.1:	part ID.
        EQ.2: part set ID.
        """ # nopep8
        return self._cards[0].get_value("pype")

    @pype.setter
    def pype(self, value: int) -> None:
        """Set the pype property."""
        if value not in [1, 2, None]:
            raise Exception("""pype must be `None` or one of {1,2}.""")
        self._cards[0].set_value("pype", value)

    @property
    def nsid1(self) -> typing.Optional[int]:
        """Get or set the Node set defining the path of the moving volume.  The moving volume travels along the path at speed SPD1.
        The nodes are traversed according to their order in the node set.  See Remark 1.
        """ # nopep8
        return self._cards[0].get_value("nsid1")

    @nsid1.setter
    def nsid1(self, value: int) -> None:
        """Set the nsid1 property."""
        self._cards[0].set_value("nsid1", value)

    @property
    def spd1(self) -> typing.Optional[float]:
        """Get or set the Speed of the moving volume on the trajectory:
        GT.0.0:	Constant speed
        LT.0.0:	 is a load curve ID defining the speed as a function of time.
        """ # nopep8
        return self._cards[0].get_value("spd1")

    @spd1.setter
    def spd1(self, value: float) -> None:
        """Set the spd1 property."""
        self._cards[0].set_value("spd1", value)

    @property
    def nsid2(self) -> typing.Optional[int]:
        """Get or set the Node or segment set that specifies the orientation of the moving volume's center axis.
        GT.0:	NSID2 together with SPD2 define a curve in the same way that NSID1 and SPD1 define a curve.
        Orientation of the moving volume's center axis is defined as a vector pointing from the current position on NSID2 to the current position on NSID1.
        EQ.0:	The moving volume's center axis is oriented as  input on Card?4.
        LT.0:	 specifies a segment set.  The moving volume's center axis is aligned with normals to segments in this set.
        To ensure that the axis orientation can be unambiguously determined at each point of the nodal path,
        LS-DYNA requires that each pair of consecutive nodes in NSID1 must both be in at least one segment of.
        When the center of the moving volume is.
        on a node that is part of more than one segment in |NSID2|, the direction is determined by averaging the adjacent segment normals.
        """ # nopep8
        return self._cards[0].get_value("nsid2")

    @nsid2.setter
    def nsid2(self, value: int) -> None:
        """Set the nsid2 property."""
        self._cards[0].set_value("nsid2", value)

    @property
    def spd2(self) -> typing.Optional[float]:
        """Get or set the Speed of reference point in NSID2 (ignored unless NSID2 > 0)
        GT.0:	constant speed
        LT.0:	 |SPD2| is a load curve ID defining the speed as a function of time..
        """ # nopep8
        return self._cards[0].get_value("spd2")

    @spd2.setter
    def spd2(self, value: float) -> None:
        """Set the spd2 property."""
        self._cards[0].set_value("spd2", value)

    @property
    def relvel(self) -> int:
        """Get or set the Defines if SPD1 and SPD2 are relative or absolute speeds in thermo-mechanical coupled analysis.
        EQ.0:	absolute speeds
        EQ.1:	relative speeds with respect to underlying structures.
        """ # nopep8
        return self._cards[0].get_value("relvel")

    @relvel.setter
    def relvel(self, value: int) -> None:
        """Set the relvel property."""
        self._cards[0].set_value("relvel", value)

    @property
    def iform(self) -> int:
        """Get or set the Geometric description of the moving volume:
        EQ.1:	cylindrical volume
        EQ.2:	rectangular prism volume.
        """ # nopep8
        return self._cards[1].get_value("iform")

    @iform.setter
    def iform(self, value: int) -> None:
        """Set the iform property."""
        if value not in [1, 2, None]:
            raise Exception("""iform must be `None` or one of {1,2}.""")
        self._cards[1].set_value("iform", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for temperature as a function of time
        EQ.0:	temperature is a constant defined by the value TMULT.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[1].set_value("lcid", value)

    @property
    def tmult(self) -> typing.Optional[float]:
        """Get or set the Curve multiplier for temperature.
        """ # nopep8
        return self._cards[1].get_value("tmult")

    @tmult.setter
    def tmult(self, value: float) -> None:
        """Set the tmult property."""
        self._cards[1].set_value("tmult", value)

    @property
    def lcrot(self) -> typing.Optional[int]:
        """Get or set the Load curve defining the rotation angle (in degrees) of the moving volume around the trajectory as a function of time.  See Remark 2.
        """ # nopep8
        return self._cards[1].get_value("lcrot")

    @lcrot.setter
    def lcrot(self, value: int) -> None:
        """Set the lcrot property."""
        self._cards[1].set_value("lcrot", value)

    @property
    def lcmov(self) -> typing.Optional[int]:
        """Get or set the Load curve defining the offset of the moving volume along its center axis as a function of time.  See Remark 2.
        """ # nopep8
        return self._cards[1].get_value("lcmov")

    @lcmov.setter
    def lcmov(self, value: int) -> None:
        """Set the lcmov property."""
        self._cards[1].set_value("lcmov", value)

    @property
    def lclat(self) -> typing.Optional[int]:
        """Get or set the Load curve defining the lateral offset of the moving volume as a function of time.  See Remark 2.
        """ # nopep8
        return self._cards[1].get_value("lclat")

    @lclat.setter
    def lclat(self, value: int) -> None:
        """Set the lclat property."""
        self._cards[1].set_value("lclat", value)

    @property
    def p1(self) -> typing.Optional[float]:
        """Get or set the Parameters defining the moving volume's geometry.
        The meaning of each parameter depends on field IFORM.  See Remark 3 for details.
        """ # nopep8
        return self._cards[2].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        """Set the p1 property."""
        self._cards[2].set_value("p1", value)

    @property
    def p2(self) -> typing.Optional[float]:
        """Get or set the Parameters defining the moving volume's geometry.
        The meaning of each parameter depends on field IFORM.  See Remark 3 for details.
        """ # nopep8
        return self._cards[2].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        """Set the p2 property."""
        self._cards[2].set_value("p2", value)

    @property
    def p3(self) -> typing.Optional[float]:
        """Get or set the Parameters defining the moving volume's geometry.
        The meaning of each parameter depends on field IFORM.  See Remark 3 for details.
        """ # nopep8
        return self._cards[2].get_value("p3")

    @p3.setter
    def p3(self, value: float) -> None:
        """Set the p3 property."""
        self._cards[2].set_value("p3", value)

    @property
    def p4(self) -> typing.Optional[float]:
        """Get or set the Parameters defining the moving volume's geometry.
        The meaning of each parameter depends on field IFORM.  See Remark 3 for details.
        """ # nopep8
        return self._cards[2].get_value("p4")

    @p4.setter
    def p4(self, value: float) -> None:
        """Set the p4 property."""
        self._cards[2].set_value("p4", value)

    @property
    def p5(self) -> typing.Optional[float]:
        """Get or set the Parameters defining the moving volume's geometry.
        The meaning of each parameter depends on field IFORM.  See Remark 3 for details.
        """ # nopep8
        return self._cards[2].get_value("p5")

    @p5.setter
    def p5(self, value: float) -> None:
        """Set the p5 property."""
        self._cards[2].set_value("p5", value)

    @property
    def p6(self) -> typing.Optional[float]:
        """Get or set the Parameters defining the moving volume's geometry.
        The meaning of each parameter depends on field IFORM.  See Remark 3 for details.
        """ # nopep8
        return self._cards[2].get_value("p6")

    @p6.setter
    def p6(self, value: float) -> None:
        """Set the p6 property."""
        self._cards[2].set_value("p6", value)

    @property
    def p7(self) -> typing.Optional[float]:
        """Get or set the Parameters defining the moving volume's geometry.
        The meaning of each parameter depends on field IFORM.  See Remark 3 for details.
        """ # nopep8
        return self._cards[2].get_value("p7")

    @p7.setter
    def p7(self, value: float) -> None:
        """Set the p7 property."""
        self._cards[2].set_value("p7", value)

    @property
    def p8(self) -> typing.Optional[float]:
        """Get or set the Parameters defining the moving volume's geometry.
        The meaning of each parameter depends on field IFORM.  See Remark 3 for details.
        """ # nopep8
        return self._cards[2].get_value("p8")

    @p8.setter
    def p8(self, value: float) -> None:
        """Set the p8 property."""
        self._cards[2].set_value("p8", value)

    @property
    def tx(self) -> typing.Optional[float]:
        """Get or set the Orientation vector of the moving volume's center axis in global coordinates (NSID2 = 0 only).
        """ # nopep8
        return self._cards[3].get_value("tx")

    @tx.setter
    def tx(self, value: float) -> None:
        """Set the tx property."""
        self._cards[3].set_value("tx", value)

    @property
    def ty(self) -> typing.Optional[float]:
        """Get or set the Orientation vector of the moving volume's center axis in global coordinates (NSID2 = 0 only).
        """ # nopep8
        return self._cards[3].get_value("ty")

    @ty.setter
    def ty(self, value: float) -> None:
        """Set the ty property."""
        self._cards[3].set_value("ty", value)

    @property
    def tz(self) -> typing.Optional[float]:
        """Get or set the Orientation vector of the moving volume's center axis in global coordinates (NSID2 = 0 only).
        """ # nopep8
        return self._cards[3].get_value("tz")

    @tz.setter
    def tz(self, value: float) -> None:
        """Set the tz property."""
        self._cards[3].set_value("tz", value)

    @property
    def lcid_link(self) -> typing.Optional[DefineCurve]:
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
    def lcrot_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcrot."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcrot:
                return kwd
        return None

    @lcrot_link.setter
    def lcrot_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcrot."""
        self.lcrot = value.lcid

    @property
    def lcmov_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcmov."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcmov:
                return kwd
        return None

    @lcmov_link.setter
    def lcmov_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcmov."""
        self.lcmov = value.lcid

    @property
    def lclat_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lclat."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lclat:
                return kwd
        return None

    @lclat_link.setter
    def lclat_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lclat."""
        self.lclat = value.lcid

    @property
    def nsid1_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for nsid1."""
        return self._get_set_link("NODE", self.nsid1)

    @nsid1_link.setter
    def nsid1_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for nsid1."""
        self.nsid1 = value.sid

