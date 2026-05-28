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

"""Module providing the AirbagParticle class."""
import typing
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.table_card_group import TableCardGroup
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_vector import DefineVector

_AIRBAGPARTICLE_CARD0 = (
    FieldSchema("sid1", int, 0, 10, None),
    FieldSchema("stype1", int, 10, 10, 0),
    FieldSchema("sid2", int, 20, 10, 0),
    FieldSchema("stype2", int, 30, 10, 0),
    FieldSchema("block", int, 40, 10, None),
    FieldSchema("npdata", int, 50, 10, 0),
    FieldSchema("fric", float, 60, 10, 0.0),
    FieldSchema("irdp", int, 70, 10, 0),
)

_AIRBAGPARTICLE_CARD1 = (
    FieldSchema("np", int, 0, 10, 200000),
    FieldSchema("unit", int, 10, 10, 0),
    FieldSchema("visflg", int, 20, 10, 1),
    FieldSchema("tatm", float, 30, 10, 293.0),
    FieldSchema("patm", float, 40, 10, 1.0),
    FieldSchema("nvent", int, 50, 10, 0),
    FieldSchema("tend", float, 60, 10, 10000000000.0),
    FieldSchema("tsw", float, 70, 10, 10000000000.0),
)

_AIRBAGPARTICLE_CARD2 = (
    FieldSchema("tstop", float, 1, 9, 100000000000.0),
    FieldSchema("tsmth", float, 10, 10, 1.0),
    FieldSchema("occup", float, 20, 10, 0.1),
    FieldSchema("rebl", int, 30, 10, 0),
    FieldSchema("sidsv", int, 40, 10, None),
    FieldSchema("psid1", int, 50, 10, None),
    FieldSchema("tsplit", float, 60, 10, None),
    FieldSchema("sffdc", float, 70, 10, 1.0),
)

_AIRBAGPARTICLE_CARD3 = (
    FieldSchema("sfiair4", float, 1, 9, 1.0),
    FieldSchema("idfric", int, 10, 10, 0),
)

_AIRBAGPARTICLE_CARD4 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("mass", float, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("time", float, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("length", float, 50, 10, None),
)

_AIRBAGPARTICLE_CARD5 = (
    FieldSchema("iair", int, 0, 10, 0),
    FieldSchema("ngas", int, 10, 10, None),
    FieldSchema("norif", int, 20, 10, None),
    FieldSchema("nid1", int, 30, 10, 0),
    FieldSchema("nid2", int, 40, 10, 0),
    FieldSchema("nid3", int, 50, 10, 0),
    FieldSchema("chm", int, 60, 10, 0),
    FieldSchema("cd_ext", float, 70, 10, 0.0),
)

_AIRBAGPARTICLE_CARD8 = (
    FieldSchema("pair", float, 0, 10, None),
    FieldSchema("tair", float, 10, 10, 0.0),
    FieldSchema("xmair", float, 20, 10, None),
    FieldSchema("aair", float, 30, 10, None),
    FieldSchema("bair", float, 40, 10, 0.0),
    FieldSchema("cair", float, 50, 10, 0.0),
    FieldSchema("npair", int, 60, 10, 0),
    FieldSchema("nprlx", str, 70, 10, "0"),
)

class AirbagParticle(KeywordBase):
    """DYNA AIRBAG_PARTICLE keyword"""

    keyword = "AIRBAG"
    subkeyword = "PARTICLE"
    _link_fields = {
        "nid1": LinkType.NODE,
        "nid2": LinkType.NODE,
        "nid3": LinkType.NODE,
        "nidi": LinkType.NODE,
        "hconv": LinkType.DEFINE_CURVE,
        "lctc23": LinkType.DEFINE_CURVE,
        "lcpc23": LinkType.DEFINE_CURVE,
        "lcmi": LinkType.DEFINE_CURVE,
        "lcti": LinkType.DEFINE_CURVE,
        "vdi": LinkType.DEFINE_VECTOR,
        "sidsv": LinkType.SET_PART,
        "psid1": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the AirbagParticle class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _AIRBAGPARTICLE_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _AIRBAGPARTICLE_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _AIRBAGPARTICLE_CARD2,
                active_func=lambda: self._cards[2].has_nondefault_values(),
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _AIRBAGPARTICLE_CARD3,
                active_func=lambda: self._cards[3].has_nondefault_values(),
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _AIRBAGPARTICLE_CARD4,
                active_func=lambda: self.unit == 3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _AIRBAGPARTICLE_CARD5,
                **kwargs,
            ),
            TableCardGroup(
                [
                    (
                        FieldSchema("sidup", int, 0, 10, None),
                        FieldSchema("styup", int, 10, 10, 0),
                        FieldSchema("pfrac", float, 20, 10, 0.0),
                        FieldSchema("linking", int, 30, 10, None),
                    ),
                    (
                        FieldSchema("sidh", int, 0, 10, None),
                        FieldSchema("stypeh", int, 10, 10, 0),
                        FieldSchema("hconv", float, 20, 10, None),
                        FieldSchema("pfric", float, 30, 10, 0.0),
                        FieldSchema("sdfblk", float, 40, 10, 1.0),
                        FieldSchema("kp", float, 50, 10, 0.0),
                        FieldSchema("inip", int, 60, 10, 0),
                        FieldSchema("cp", float, 70, 10, None),
                    ),
                ],
                lambda: self.npdata or 0,
                None,
                "parts_data",
                **kwargs,
            ),
            TableCard(
                [
                    Field("sid3", int, 0, 10, None),
                    Field("stype3", int, 10, 10, 0),
                    Field("c23", float, 20, 10, 1.0),
                    Field("lctc23", int, 30, 10, None),
                    Field("lcpc23", int, 40, 10, None),
                    Field("enh_v", int, 50, 10, 0),
                    Field("ppop", float, 60, 10, 0.0),
                ],
                lambda: self.nvent or 0,
                name="vents",
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _AIRBAGPARTICLE_CARD8,
                **kwargs,
            ),
            TableCard(
                [
                    Field("lcmi", int, 0, 10, None),
                    Field("lcti", int, 10, 10, None),
                    Field("xmi", float, 20, 10, None),
                    Field("ai", float, 30, 10, None),
                    Field("bi", float, 40, 10, 0.0),
                    Field("ci", float, 50, 10, 0.0),
                    Field("infgi", int, 60, 10, 1),
                ],
                lambda: self.ngas or 0,
                name="gas_components",
                **kwargs,
            ),
            TableCard(
                [
                    Field("nidi", int, 0, 10, None),
                    Field("ani", float, 10, 10, None),
                    Field("vdi", int, 20, 10, None),
                    Field("cai", float, 30, 10, 30.0),
                    Field("infoi", int, 40, 10, 1),
                    Field("imom", int, 50, 10, 0),
                    Field("iang", int, 60, 10, 0),
                    Field("chm_id", int, 70, 10, None),
                ],
                lambda: self.norif or 0,
                name="orifices",
                **kwargs,
            ),
        ]
    @property
    def sid1(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID defining the complete airbag.
        """ # nopep8
        return self._cards[0].get_value("sid1")

    @sid1.setter
    def sid1(self, value: int) -> None:
        """Set the sid1 property."""
        self._cards[0].set_value("sid1", value)

    @property
    def stype1(self) -> int:
        """Get or set the Set type:
        EQ.0: Part
        EQ.1: Part set.
        """ # nopep8
        return self._cards[0].get_value("stype1")

    @stype1.setter
    def stype1(self, value: int) -> None:
        """Set the stype1 property."""
        if value not in [0, 1, None]:
            raise Exception("""stype1 must be `None` or one of {0,1}.""")
        self._cards[0].set_value("stype1", value)

    @property
    def sid2(self) -> int:
        """Get or set the Part or part set ID defining internal parts of the airbag.
        """ # nopep8
        return self._cards[0].get_value("sid2")

    @sid2.setter
    def sid2(self, value: int) -> None:
        """Set the sid2 property."""
        self._cards[0].set_value("sid2", value)

    @property
    def stype2(self) -> int:
        """Get or set the Set type:
        EQ.0: Part
        EQ.1: Part set.
        EQ.2:	Number of parts to read (Not recommended for general use)
        """ # nopep8
        return self._cards[0].get_value("stype2")

    @stype2.setter
    def stype2(self, value: int) -> None:
        """Set the stype2 property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""stype2 must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("stype2", value)

    @property
    def block(self) -> typing.Optional[int]:
        """Get or set the Blocking.  Block must be set to a two-digit number "BLOCK"="M"x10+"N",
        The 10’s digit controls the treatment of particles that escape due to deleted elements (particles are always tracked and marked).
        M.EQ.0:	Active particle method which causes particles to be put back into the bag.
        M.EQ.1:	Particles are leaked through vents. See Remark 3.
        The 1’s digit controls the treatment of leakage.
        N.EQ.0:	Always consider porosity leakage without considering blockage due to contact.
        N.EQ.1:	Check if airbag node is in contact or not. If yes, 1/4 (quad) or 1/3 (tri) of the segment surface will not have porosity leakage due to contact.
        N.EQ.2:	Same as 1 but no blockage for external vents
        N.EQ.3:	Same as 1 but no blockage for internal vents
        N.EQ.4:	Same as 1 but no blockage for all vents.
        """ # nopep8
        return self._cards[0].get_value("block")

    @block.setter
    def block(self, value: int) -> None:
        """Set the block property."""
        self._cards[0].set_value("block", value)

    @property
    def npdata(self) -> int:
        """Get or set the Number of parts or part sets data.
        """ # nopep8
        return self._cards[0].get_value("npdata")

    @npdata.setter
    def npdata(self, value: int) -> None:
        """Set the npdata property."""
        self._cards[0].set_value("npdata", value)

    @property
    def fric(self) -> float:
        """Get or set the Friction factor F_r if -1.0 < FRIC ≤ 1.0.  Otherwise,
        LE.-1.0:	|"FRIC" | is the curve ID which defines F_r as a function of the part pressure.
        GT.1.0:	FRIC is the *DEFINE_FUNCTION ID that defines F_r.  See Remark 2
        """ # nopep8
        return self._cards[0].get_value("fric")

    @fric.setter
    def fric(self, value: float) -> None:
        """Set the fric property."""
        self._cards[0].set_value("fric", value)

    @property
    def irdp(self) -> int:
        """Get or set the Dynamically scaling of particle radius
        EQ.0: Off
        EQ.1: On
        """ # nopep8
        return self._cards[0].get_value("irdp")

    @irdp.setter
    def irdp(self, value: int) -> None:
        """Set the irdp property."""
        if value not in [0, 1, None]:
            raise Exception("""irdp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("irdp", value)

    @property
    def np(self) -> int:
        """Get or set the Number of particles (Default 200,000).
        """ # nopep8
        return self._cards[1].get_value("np")

    @np.setter
    def np(self, value: int) -> None:
        """Set the np property."""
        self._cards[1].set_value("np", value)

    @property
    def unit(self) -> int:
        """Get or set the Unit system
        EQ.0: kg-mm-ms-K
        EQ.1: SI-units
        EQ.2: tonne-mm-s-K.
        EQ.3:	User defined units (see Remark 11)
        """ # nopep8
        return self._cards[1].get_value("unit")

    @unit.setter
    def unit(self, value: int) -> None:
        """Set the unit property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""unit must be `None` or one of {0,1,2,3}.""")
        self._cards[1].set_value("unit", value)

    @property
    def visflg(self) -> int:
        """Get or set the Visible particles(only support CPM database, see remark 6)
        EQ.0: Default to 1
        EQ.1: Output particle's coordinates, velocities, mass, radius, spin energy,
        translational energy
        EQ.2: Output reduce data set with corrdinates only
        EQ.3: Supress CPM database.
        """ # nopep8
        return self._cards[1].get_value("visflg")

    @visflg.setter
    def visflg(self, value: int) -> None:
        """Set the visflg property."""
        if value not in [1, 0, 2, 3, None]:
            raise Exception("""visflg must be `None` or one of {1,0,2,3}.""")
        self._cards[1].set_value("visflg", value)

    @property
    def tatm(self) -> float:
        """Get or set the Atmospheric temperature (Default 293K).
        """ # nopep8
        return self._cards[1].get_value("tatm")

    @tatm.setter
    def tatm(self, value: float) -> None:
        """Set the tatm property."""
        self._cards[1].set_value("tatm", value)

    @property
    def patm(self) -> float:
        """Get or set the Atmospheric pressure (Default 1ATM).
        """ # nopep8
        return self._cards[1].get_value("patm")

    @patm.setter
    def patm(self, value: float) -> None:
        """Set the patm property."""
        self._cards[1].set_value("patm", value)

    @property
    def nvent(self) -> int:
        """Get or set the Number of vent hole parts or part sets.
        """ # nopep8
        return self._cards[1].get_value("nvent")

    @nvent.setter
    def nvent(self, value: int) -> None:
        """Set the nvent property."""
        self._cards[1].set_value("nvent", value)

    @property
    def tend(self) -> float:
        """Get or set the Time when all particles (NP) have entered bag (Default 1.0e10).
        """ # nopep8
        return self._cards[1].get_value("tend")

    @tend.setter
    def tend(self, value: float) -> None:
        """Set the tend property."""
        self._cards[1].set_value("tend", value)

    @property
    def tsw(self) -> float:
        """Get or set the Time for switch to control volume calculation (Default 1.0e10).
        """ # nopep8
        return self._cards[1].get_value("tsw")

    @tsw.setter
    def tsw(self, value: float) -> None:
        """Set the tsw property."""
        self._cards[1].set_value("tsw", value)

    @property
    def tstop(self) -> float:
        """Get or set the Time at which front tracking switches from IAIR = 4 to IAIR = 2.
        """ # nopep8
        return self._cards[2].get_value("tstop")

    @tstop.setter
    def tstop(self, value: float) -> None:
        """Set the tstop property."""
        self._cards[2].set_value("tstop", value)

    @property
    def tsmth(self) -> float:
        """Get or set the To avoid sudden jumps in the pressure signal during switching,
        the front tracking is tapered during a transition period.
        The default time of 1.0 millisecond will be applied if this value is set to zero
        """ # nopep8
        return self._cards[2].get_value("tsmth")

    @tsmth.setter
    def tsmth(self, value: float) -> None:
        """Set the tsmth property."""
        self._cards[2].set_value("tsmth", value)

    @property
    def occup(self) -> float:
        """Get or set the Particles occupy OCCUP percent of the airbag’s volume.  The default value of OCCUP is 10%.
        This field can be used to balance computational cost and signal quality.  OCCUP ranges from 0.001 to 0.1..
        """ # nopep8
        return self._cards[2].get_value("occup")

    @occup.setter
    def occup(self, value: float) -> None:
        """Set the occup property."""
        self._cards[2].set_value("occup", value)

    @property
    def rebl(self) -> int:
        """Get or set the If the option is ON, all energy stored from damping will be evenly distributed as vibrational energy to all particles.
        This improves the pressure calculation in certain applications.
        EQ.0:	Off (Default)
        EQ.1:	On.
        """ # nopep8
        return self._cards[2].get_value("rebl")

    @rebl.setter
    def rebl(self, value: int) -> None:
        """Set the rebl property."""
        if value not in [0, 1, None]:
            raise Exception("""rebl must be `None` or one of {0,1}.""")
        self._cards[2].set_value("rebl", value)

    @property
    def sidsv(self) -> typing.Optional[int]:
        """Get or set the Part set ID for internal shell part.  The volume formed by this internal shell part will be excluded from the bag volume.  These internal parts must have consistent orientation to get correct excluded volume.
        """ # nopep8
        return self._cards[2].get_value("sidsv")

    @sidsv.setter
    def sidsv(self, value: int) -> None:
        """Set the sidsv property."""
        self._cards[2].set_value("sidsv", value)

    @property
    def psid1(self) -> typing.Optional[int]:
        """Get or set the Part set ID for external parts which have normal pointed outward.  This option is usually used with airbag integrity check while there are two CPM bags connected with bag interaction.  Therefore, one of the bag can have the correct shell orientation but the share parts for the second bag will have wrong orientation.  This option will automatically flip the parts defined in this set in the second bag during integrity checking.
        """ # nopep8
        return self._cards[2].get_value("psid1")

    @psid1.setter
    def psid1(self, value: int) -> None:
        """Set the psid1 property."""
        self._cards[2].set_value("psid1", value)

    @property
    def tsplit(self) -> typing.Optional[float]:
        """Get or set the Start time to activate particle splitting algorithm. See Remark 15.
        """ # nopep8
        return self._cards[2].get_value("tsplit")

    @tsplit.setter
    def tsplit(self, value: float) -> None:
        """Set the tsplit property."""
        self._cards[2].set_value("tsplit", value)

    @property
    def sffdc(self) -> float:
        """Get or set the Scale factor for the force decay constant.  SFFDC has a range of . The default value is 1.0.  The value given here will replaced the values from *CONTROL_CPM
        """ # nopep8
        return self._cards[2].get_value("sffdc")

    @sffdc.setter
    def sffdc(self, value: float) -> None:
        """Set the sffdc property."""
        self._cards[2].set_value("sffdc", value)

    @property
    def sfiair4(self) -> float:
        """Get or set the Scale factor for the ratio of initial air particles to inflator gas particles for IAIR = 4.
        Smaller values weaken the effect of gas front tracking.
        """ # nopep8
        return self._cards[3].get_value("sfiair4")

    @sfiair4.setter
    def sfiair4(self, value: float) -> None:
        """Set the sfiair4 property."""
        self._cards[3].set_value("sfiair4", value)

    @property
    def idfric(self) -> int:
        """Get or set the Direction of P2F impact force:
        EQ.0:	No change(default)
        EQ.1 : The force is applied in the segment normal direction
        """ # nopep8
        return self._cards[3].get_value("idfric")

    @idfric.setter
    def idfric(self, value: int) -> None:
        """Set the idfric property."""
        if value not in [0, 1, None]:
            raise Exception("""idfric must be `None` or one of {0,1}.""")
        self._cards[3].set_value("idfric", value)

    @property
    def mass(self) -> typing.Optional[float]:
        """Get or set the Conversion factor from current unit to MKS unit.  For example, if the current unit is using kg-mm-ms, the input should be 1.0, 0.001, 0.001.
        """ # nopep8
        return self._cards[4].get_value("mass")

    @mass.setter
    def mass(self, value: float) -> None:
        """Set the mass property."""
        self._cards[4].set_value("mass", value)

    @property
    def time(self) -> typing.Optional[float]:
        """Get or set the Conversion factor from current unit to MKS unit.  For example, if the current unit is using kg-mm-ms, the input should be 1.0, 0.001, 0.001.
        """ # nopep8
        return self._cards[4].get_value("time")

    @time.setter
    def time(self, value: float) -> None:
        """Set the time property."""
        self._cards[4].set_value("time", value)

    @property
    def length(self) -> typing.Optional[float]:
        """Get or set the Conversion factor from current unit to MKS unit.  For example, if the current unit is using kg-mm-ms, the input should be 1.0, 0.001, 0.001.
        """ # nopep8
        return self._cards[4].get_value("length")

    @length.setter
    def length(self, value: float) -> None:
        """Set the length property."""
        self._cards[4].set_value("length", value)

    @property
    def iair(self) -> int:
        """Get or set the Initial gas inside bag considered:
        EQ.0:	No
        EQ.1:	Yes, using control volume method.
        EQ.-1:	Yes, using control volume method. In this case ambient air enters the bag when PATM is greater than bag pressure.
        EQ.2:	Yes, using the particle method.
        EQ.4:	Yes, using the particle method.  Initial air particles are used for the gas front tracking algorithm,
        but they do not apply forces when they collide with a segment.
        Instead, a uniform pressure is applied to the airbag based on the ratio of air and inflator particles.
        In this case NPRLX must be negative so that forces are not applied by the initial air.
        """ # nopep8
        return self._cards[5].get_value("iair")

    @iair.setter
    def iair(self, value: int) -> None:
        """Set the iair property."""
        self._cards[5].set_value("iair", value)

    @property
    def ngas(self) -> typing.Optional[int]:
        """Get or set the Number of gas components.
        """ # nopep8
        return self._cards[5].get_value("ngas")

    @ngas.setter
    def ngas(self, value: int) -> None:
        """Set the ngas property."""
        self._cards[5].set_value("ngas", value)

    @property
    def norif(self) -> typing.Optional[int]:
        """Get or set the Number of orifices.
        """ # nopep8
        return self._cards[5].get_value("norif")

    @norif.setter
    def norif(self, value: int) -> None:
        """Set the norif property."""
        self._cards[5].set_value("norif", value)

    @property
    def nid1(self) -> int:
        """Get or set the NID1-NID3, Three nodes defining a moving coordinate system for the direction of flow through the gas inlet nozzles (Default fixed system).
        """ # nopep8
        return self._cards[5].get_value("nid1")

    @nid1.setter
    def nid1(self, value: int) -> None:
        """Set the nid1 property."""
        self._cards[5].set_value("nid1", value)

    @property
    def nid2(self) -> int:
        """Get or set the NID1-NID3, Three nodes defining a moving coordinate system for the direction of flow through the gas inlet nozzles (Default fixed system).
        """ # nopep8
        return self._cards[5].get_value("nid2")

    @nid2.setter
    def nid2(self, value: int) -> None:
        """Set the nid2 property."""
        self._cards[5].set_value("nid2", value)

    @property
    def nid3(self) -> int:
        """Get or set the NID1-NID3, Three nodes defining a moving coordinate system for the direction of flow through the gas inlet nozzles (Default fixed system).
        """ # nopep8
        return self._cards[5].get_value("nid3")

    @nid3.setter
    def nid3(self, value: int) -> None:
        """Set the nid3 property."""
        self._cards[5].set_value("nid3", value)

    @property
    def chm(self) -> int:
        """Get or set the Chamber ID used in *DEFINE_CPM_CHAMBER.
        """ # nopep8
        return self._cards[5].get_value("chm")

    @chm.setter
    def chm(self, value: int) -> None:
        """Set the chm property."""
        self._cards[5].set_value("chm", value)

    @property
    def cd_ext(self) -> float:
        """Get or set the Drag coefficient for external air. If the value is not zero, the inertial effect
        from external air will be considered and forces will be applied in the normal
        direction on the exterior airbag surface.
        """ # nopep8
        return self._cards[5].get_value("cd_ext")

    @cd_ext.setter
    def cd_ext(self, value: float) -> None:
        """Set the cd_ext property."""
        self._cards[5].set_value("cd_ext", value)

    @property
    def parts_data(self) -> pd.DataFrame:
        """Gets the full table of parts_data."""
        return self._cards[6].table

    @parts_data.setter
    def parts_data(self, df: pd.DataFrame):
        """sets parts_data from the dataframe df."""
        self._cards[6].table = df

    @property
    def vents(self) -> pd.DataFrame:
        """Get the table of vents."""
        return self._cards[7].table

    @vents.setter
    def vents(self, df: pd.DataFrame):
        """Set vents from the dataframe df"""
        self._cards[7].table = df

    @property
    def pair(self) -> typing.Optional[float]:
        """Get or set the Initial pressure inside bag .
        """ # nopep8
        return self._cards[8].get_value("pair")

    @pair.setter
    def pair(self, value: float) -> None:
        """Set the pair property."""
        self._cards[8].set_value("pair", value)

    @property
    def tair(self) -> float:
        """Get or set the Initial temperature inside bag .
        """ # nopep8
        return self._cards[8].get_value("tair")

    @tair.setter
    def tair(self, value: float) -> None:
        """Set the tair property."""
        self._cards[8].set_value("tair", value)

    @property
    def xmair(self) -> typing.Optional[float]:
        """Get or set the Molar mass of gas initially inside bag.
        LT.0:	-XMAIR references the ID of a *DEFINE_CPM_GAS_PROPERTIES keyword that defines the gas thermodynamic properties.
        Note that AAIR, BAIR, and CAIR are ignored
        """ # nopep8
        return self._cards[8].get_value("xmair")

    @xmair.setter
    def xmair(self, value: float) -> None:
        """Set the xmair property."""
        self._cards[8].set_value("xmair", value)

    @property
    def aair(self) -> typing.Optional[float]:
        """Get or set the Constant, linear, and quadratic heat capacity parameters.
        """ # nopep8
        return self._cards[8].get_value("aair")

    @aair.setter
    def aair(self, value: float) -> None:
        """Set the aair property."""
        self._cards[8].set_value("aair", value)

    @property
    def bair(self) -> float:
        """Get or set the Constant, linear, and quadratic heat capacity parameters.
        """ # nopep8
        return self._cards[8].get_value("bair")

    @bair.setter
    def bair(self, value: float) -> None:
        """Set the bair property."""
        self._cards[8].set_value("bair", value)

    @property
    def cair(self) -> float:
        """Get or set the Constant, linear, and quadratic heat capacity parameters.
        """ # nopep8
        return self._cards[8].get_value("cair")

    @cair.setter
    def cair(self, value: float) -> None:
        """Set the cair property."""
        self._cards[8].set_value("cair", value)

    @property
    def npair(self) -> int:
        """Get or set the Number of particle for air.
        """ # nopep8
        return self._cards[8].get_value("npair")

    @npair.setter
    def npair(self, value: int) -> None:
        """Set the npair property."""
        self._cards[8].set_value("npair", value)

    @property
    def nprlx(self) -> str:
        """Get or set the Number of cycles to reach thermal equilibrium.  See Remark 6.
        LT.0:	If more than 50% of the collision to fabric is from initial air particles, the contact force will not apply to the fabric segment in order to keep its original shape.
        If the number contains “.”, “e” or “E”, NPRLX will treated as an end time rather than as a cycle count.
        """ # nopep8
        return self._cards[8].get_value("nprlx")

    @nprlx.setter
    def nprlx(self, value: str) -> None:
        """Set the nprlx property."""
        self._cards[8].set_value("nprlx", value)

    @property
    def gas_components(self) -> pd.DataFrame:
        """Get the table of gas_components."""
        return self._cards[9].table

    @gas_components.setter
    def gas_components(self, df: pd.DataFrame):
        """Set gas_components from the dataframe df"""
        self._cards[9].table = df

    @property
    def orifices(self) -> pd.DataFrame:
        """Get the table of orifices."""
        return self._cards[10].table

    @orifices.setter
    def orifices(self, df: pd.DataFrame):
        """Set orifices from the dataframe df"""
        self._cards[10].table = df

    @property
    def nid1_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid1."""
        return self._get_link_by_attr("NODE", "nid", self.nid1, "parts")

    @property
    def nid2_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid2."""
        return self._get_link_by_attr("NODE", "nid", self.nid2, "parts")

    @property
    def nid3_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid3."""
        return self._get_link_by_attr("NODE", "nid", self.nid3, "parts")

    @property
    def nidi_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all NODE keywords for nidi, keyed by nidi value."""
        return self._get_links_from_table("NODE", "nid", "orifices", "nidi", "")

    def get_nidi_link(self, nidi: int) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nidi."""
        return self._get_link_by_attr("NODE", "nid", nidi, "")

    @property
    def hconv_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for hconv."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.hconv:
                return kwd
        return None

    @hconv_link.setter
    def hconv_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for hconv."""
        self.hconv = value.lcid

    @property
    def lctc23_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lctc23."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lctc23:
                return kwd
        return None

    @lctc23_link.setter
    def lctc23_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lctc23."""
        self.lctc23 = value.lcid

    @property
    def lcpc23_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcpc23."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcpc23:
                return kwd
        return None

    @lcpc23_link.setter
    def lcpc23_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcpc23."""
        self.lcpc23 = value.lcid

    @property
    def lcmi_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcmi."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcmi:
                return kwd
        return None

    @lcmi_link.setter
    def lcmi_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcmi."""
        self.lcmi = value.lcid

    @property
    def lcti_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcti."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcti:
                return kwd
        return None

    @lcti_link.setter
    def lcti_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcti."""
        self.lcti = value.lcid

    @property
    def vdi_link(self) -> typing.Optional[DefineVector]:
        """Get the DefineVector object for vdi."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.vdi:
                return kwd
        return None

    @vdi_link.setter
    def vdi_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for vdi."""
        self.vdi = value.vid

    @property
    def sidsv_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for sidsv."""
        return self._get_set_link("PART", self.sidsv)

    @sidsv_link.setter
    def sidsv_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for sidsv."""
        self.sidsv = value.sid

    @property
    def psid1_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for psid1."""
        return self._get_set_link("PART", self.psid1)

    @psid1_link.setter
    def psid1_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psid1."""
        self.psid1 = value.sid

