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

"""Module providing the LoadHeatLaserAbsorption class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_LOADHEATLASERABSORPTION_CARD0 = (
    FieldSchema("nsid1", int, 0, 10, None),
    FieldSchema("spd1", int, 10, 10, None),
    FieldSchema("nsid2", int, 20, 10, None),
    FieldSchema("spd2", float, 30, 10, None),
    FieldSchema("relvel", int, 40, 10, 0),
    FieldSchema("tx", float, 50, 10, None),
    FieldSchema("ty", float, 60, 10, None),
    FieldSchema("tz", float, 70, 10, None),
)

_LOADHEATLASERABSORPTION_CARD1 = (
    FieldSchema("iform", int, 0, 10, None),
    FieldSchema("p1", str, 10, 10, None),
    FieldSchema("p2", str, 20, 10, None),
    FieldSchema("p3", str, 30, 10, None),
    FieldSchema("p4", str, 40, 10, None),
    FieldSchema("p5", str, 50, 10, None),
    FieldSchema("p6", str, 60, 10, None),
    FieldSchema("p7", str, 70, 10, None),
)

_LOADHEATLASERABSORPTION_CARD2 = (
    FieldSchema("nlay", int, 0, 10, None),
    FieldSchema("lcrot", int, 10, 10, None),
    FieldSchema("lclat", int, 20, 10, None),
    FieldSchema("lctim", int, 30, 10, None),
    FieldSchema("q", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

_LOADHEATLASERABSORPTION_CARD3 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("idtyp", int, 10, 10, 0),
    FieldSchema("absor", float, 20, 10, None),
    FieldSchema("lcabs", int, 30, 10, None),
)

class LoadHeatLaserAbsorption(KeywordBase):
    """DYNA LOAD_HEAT_LASER_ABSORPTION keyword"""

    keyword = "LOAD"
    subkeyword = "HEAT_LASER_ABSORPTION"
    _link_fields = {
        "lcrot": LinkType.DEFINE_CURVE,
        "lclat": LinkType.DEFINE_CURVE,
        "lctim": LinkType.DEFINE_CURVE,
        "lcabs": LinkType.DEFINE_CURVE,
        "nsid1": LinkType.SET_NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the LoadHeatLaserAbsorption class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADHEATLASERABSORPTION_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _LOADHEATLASERABSORPTION_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _LOADHEATLASERABSORPTION_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _LOADHEATLASERABSORPTION_CARD3,
                **kwargs,
            ),
        ]
    @property
    def nsid1(self) -> typing.Optional[int]:
        """Get or set the Node set defining the path of the laser heat source.  The source travels along the path at speed SPD1.  The nodes are traversed in the order of the node set.  See Remark 1.
        """ # nopep8
        return self._cards[0].get_value("nsid1")

    @nsid1.setter
    def nsid1(self, value: int) -> None:
        """Set the nsid1 property."""
        self._cards[0].set_value("nsid1", value)

    @property
    def spd1(self) -> typing.Optional[int]:
        """Get or set the Speed of the heat source on the trajectory
        GT.0.0:	Constant speed
        LT.0.0 : | SPD1 | is a load curve ID defining weld speed as a function of time.
        """ # nopep8
        return self._cards[0].get_value("spd1")

    @spd1.setter
    def spd1(self, value: int) -> None:
        """Set the spd1 property."""
        self._cards[0].set_value("spd1", value)

    @property
    def nsid2(self) -> typing.Optional[int]:
        """Get or set the Node or segment set containing information about the laser orientation:
        GT.0:	NSID2 together with SPD2 define a curve in the same way that NSID1 and SPD1 define a curve. The aiming direction is taken to be the vector pointing from the current position along NSID2(for example your hand holding the torch) to the current position on NSID1(the weld source).
        EQ.0 : The beam aiming direction is(TX ,TY ,TZ).
        """ # nopep8
        return self._cards[0].get_value("nsid2")

    @nsid2.setter
    def nsid2(self, value: int) -> None:
        """Set the nsid2 property."""
        self._cards[0].set_value("nsid2", value)

    @property
    def spd2(self) -> typing.Optional[float]:
        """Get or set the Speed of reference point in NSID2 (ignored unless NSID2 > 0)
        GT.0.0:	Constant speed
        LT.0.0 : | SPD2 | is a load curve ID defining weld speed as a function of time.
        """ # nopep8
        return self._cards[0].get_value("spd2")

    @spd2.setter
    def spd2(self, value: float) -> None:
        """Set the spd2 property."""
        self._cards[0].set_value("spd2", value)

    @property
    def relvel(self) -> int:
        """Get or set the Defines if SPD1 and SPD2 are relative or absolute speeds in coupled simulations
        EQ.0:	Absolute speeds
        EQ.1 : Relative speeds with respect to the underlying structure
        """ # nopep8
        return self._cards[0].get_value("relvel")

    @relvel.setter
    def relvel(self, value: int) -> None:
        """Set the relvel property."""
        if value not in [0, 1, None]:
            raise Exception("""relvel must be `None` or one of {0,1}.""")
        self._cards[0].set_value("relvel", value)

    @property
    def tx(self) -> typing.Optional[float]:
        """Get or set the Laser beam vector in global coordinates (NSID2 = 0 only)
        """ # nopep8
        return self._cards[0].get_value("tx")

    @tx.setter
    def tx(self, value: float) -> None:
        """Set the tx property."""
        self._cards[0].set_value("tx", value)

    @property
    def ty(self) -> typing.Optional[float]:
        """Get or set the Laser beam vector in global coordinates (NSID2 = 0 only)
        """ # nopep8
        return self._cards[0].get_value("ty")

    @ty.setter
    def ty(self, value: float) -> None:
        """Set the ty property."""
        self._cards[0].set_value("ty", value)

    @property
    def tz(self) -> typing.Optional[float]:
        """Get or set the Laser beam vector in global coordinates (NSID2 = 0 only)
        """ # nopep8
        return self._cards[0].get_value("tz")

    @tz.setter
    def tz(self, value: float) -> None:
        """Set the tz property."""
        self._cards[0].set_value("tz", value)

    @property
    def iform(self) -> typing.Optional[int]:
        """Get or set the Geometry description for energy rate density distribution (see Remark 3):
        EQ.1:	Double elliptic with constant density
        EQ.2 : Double elliptic with Gaussian distribution
        """ # nopep8
        return self._cards[1].get_value("iform")

    @iform.setter
    def iform(self, value: int) -> None:
        """Set the iform property."""
        self._cards[1].set_value("iform", value)

    @property
    def p1(self) -> typing.Optional[str]:
        """Get or set the Parameters defining flux geometry, depending on field IFORM.  See Remark 3 for details.
        """ # nopep8
        return self._cards[1].get_value("p1")

    @p1.setter
    def p1(self, value: str) -> None:
        """Set the p1 property."""
        self._cards[1].set_value("p1", value)

    @property
    def p2(self) -> typing.Optional[str]:
        """Get or set the Parameters defining flux geometry, depending on field IFORM.  See Remark 3 for details.
        """ # nopep8
        return self._cards[1].get_value("p2")

    @p2.setter
    def p2(self, value: str) -> None:
        """Set the p2 property."""
        self._cards[1].set_value("p2", value)

    @property
    def p3(self) -> typing.Optional[str]:
        """Get or set the Parameters defining flux geometry, depending on field IFORM.  See Remark 3 for details.
        """ # nopep8
        return self._cards[1].get_value("p3")

    @p3.setter
    def p3(self, value: str) -> None:
        """Set the p3 property."""
        self._cards[1].set_value("p3", value)

    @property
    def p4(self) -> typing.Optional[str]:
        """Get or set the Parameters defining flux geometry, depending on field IFORM.  See Remark 3 for details.
        """ # nopep8
        return self._cards[1].get_value("p4")

    @p4.setter
    def p4(self, value: str) -> None:
        """Set the p4 property."""
        self._cards[1].set_value("p4", value)

    @property
    def p5(self) -> typing.Optional[str]:
        """Get or set the Parameters defining flux geometry, depending on field IFORM.  See Remark 3 for details.
        """ # nopep8
        return self._cards[1].get_value("p5")

    @p5.setter
    def p5(self, value: str) -> None:
        """Set the p5 property."""
        self._cards[1].set_value("p5", value)

    @property
    def p6(self) -> typing.Optional[str]:
        """Get or set the Parameters defining flux geometry, depending on field IFORM.  See Remark 3 for details.
        """ # nopep8
        return self._cards[1].get_value("p6")

    @p6.setter
    def p6(self, value: str) -> None:
        """Set the p6 property."""
        self._cards[1].set_value("p6", value)

    @property
    def p7(self) -> typing.Optional[str]:
        """Get or set the Parameters defining flux geometry, depending on field IFORM.  See Remark 3 for details.
        """ # nopep8
        return self._cards[1].get_value("p7")

    @p7.setter
    def p7(self, value: str) -> None:
        """Set the p7 property."""
        self._cards[1].set_value("p7", value)

    @property
    def nlay(self) -> typing.Optional[int]:
        """Get or set the Number of material layers
        """ # nopep8
        return self._cards[2].get_value("nlay")

    @nlay.setter
    def nlay(self, value: int) -> None:
        """Set the nlay property."""
        self._cards[2].set_value("nlay", value)

    @property
    def lcrot(self) -> typing.Optional[int]:
        """Get or set the Load curve defining the rotation (angle in degrees) of the heat source around the trajectory as function of time.  See Remark 2.
        """ # nopep8
        return self._cards[2].get_value("lcrot")

    @lcrot.setter
    def lcrot(self, value: int) -> None:
        """Set the lcrot property."""
        self._cards[2].set_value("lcrot", value)

    @property
    def lclat(self) -> typing.Optional[int]:
        """Get or set the Load curve giving the lateral offset of the heat source as function of time.  See Remark 2.
        """ # nopep8
        return self._cards[2].get_value("lclat")

    @lclat.setter
    def lclat(self, value: int) -> None:
        """Set the lclat property."""
        self._cards[2].set_value("lclat", value)

    @property
    def lctim(self) -> typing.Optional[int]:
        """Get or set the Load curve ID giving a flux energy input rate multiplier q_1 (t) as a function of time, see Remark 4.
        EQ.0:	Use constant multiplier value q_1(t) = 1.0.
        """ # nopep8
        return self._cards[2].get_value("lctim")

    @lctim.setter
    def lctim(self, value: int) -> None:
        """Set the lctim property."""
        self._cards[2].set_value("lctim", value)

    @property
    def q(self) -> typing.Optional[int]:
        """Get or set the Base energy input rate Q_b [energy/time]
        """ # nopep8
        return self._cards[2].get_value("q")

    @q.setter
    def q(self, value: int) -> None:
        """Set the q property."""
        self._cards[2].set_value("q", value)

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Set or part ID defining the material layer
        """ # nopep8
        return self._cards[3].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[3].set_value("id", value)

    @property
    def idtyp(self) -> int:
        """Get or set the Type for ID
        EQ.0:	Part ID
        EQ.1 : Part set ID
        EQ.2 : Solid element set ID
        """ # nopep8
        return self._cards[3].get_value("idtyp")

    @idtyp.setter
    def idtyp(self, value: int) -> None:
        """Set the idtyp property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""idtyp must be `None` or one of {0,1,2}.""")
        self._cards[3].set_value("idtyp", value)

    @property
    def absor(self) -> typing.Optional[float]:
        """Get or set the Absorption factor
        """ # nopep8
        return self._cards[3].get_value("absor")

    @absor.setter
    def absor(self, value: float) -> None:
        """Set the absor property."""
        self._cards[3].set_value("absor", value)

    @property
    def lcabs(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining the absorption factor as function of temperature T
        """ # nopep8
        return self._cards[3].get_value("lcabs")

    @lcabs.setter
    def lcabs(self, value: int) -> None:
        """Set the lcabs property."""
        self._cards[3].set_value("lcabs", value)

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
    def lctim_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lctim."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lctim:
                return kwd
        return None

    @lctim_link.setter
    def lctim_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lctim."""
        self.lctim = value.lcid

    @property
    def lcabs_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcabs."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcabs:
                return kwd
        return None

    @lcabs_link.setter
    def lcabs_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcabs."""
        self.lcabs = value.lcid

    @property
    def nsid1_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for nsid1."""
        return self._get_set_link("NODE", self.nsid1)

    @nsid1_link.setter
    def nsid1_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for nsid1."""
        self.nsid1 = value.sid

