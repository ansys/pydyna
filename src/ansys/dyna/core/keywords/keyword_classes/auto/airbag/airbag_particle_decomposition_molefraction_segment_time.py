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

"""Module providing the AirbagParticleDecompositionMolefractionSegmentTime class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_vector import DefineVector

_AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("title", str, 10, 70, None),
)

_AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD1 = (
    FieldSchema("birth", float, 0, 10, None),
    FieldSchema("death", float, 10, 10, None),
)

_AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD2 = (
    FieldSchema("sid1", int, 0, 10, None),
    FieldSchema("stype1", int, 10, 10, 0),
    FieldSchema("sid2", int, 20, 10, 0),
    FieldSchema("stype2", int, 30, 10, 0),
    FieldSchema("block", int, 40, 10, None),
    FieldSchema("npdata", int, 50, 10, 0),
    FieldSchema("fric", float, 60, 10, 0.0),
    FieldSchema("irdp", int, 70, 10, 0),
)

_AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD3 = (
    FieldSchema("segsid", int, 0, 10, None),
)

_AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD4 = (
    FieldSchema("np", int, 0, 10, 200000),
    FieldSchema("unit", int, 10, 10, 0),
    FieldSchema("visflg", int, 20, 10, 1),
    FieldSchema("tatm", float, 30, 10, 293.0),
    FieldSchema("patm", float, 40, 10, 1.0),
    FieldSchema("nvent", int, 50, 10, 0),
    FieldSchema("tend", float, 60, 10, 10000000000.0),
    FieldSchema("tsw", float, 70, 10, 10000000000.0),
)

_AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD5 = (
    FieldSchema("tstop", float, 1, 9, 100000000000.0),
    FieldSchema("tsmth", float, 10, 10, 1.0),
    FieldSchema("occup", float, 20, 10, 0.1),
    FieldSchema("rebl", int, 30, 10, 0),
    FieldSchema("sidsv", int, 40, 10, None),
    FieldSchema("psid1", int, 50, 10, None),
    FieldSchema("tsplit", float, 60, 10, None),
    FieldSchema("sffdc", float, 70, 10, 1.0),
)

_AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD6 = (
    FieldSchema("sfiair4", float, 1, 9, 1.0),
    FieldSchema("idfric", int, 10, 10, 0),
)

_AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD7 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("mass", float, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("time", float, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("length", float, 50, 10, None),
)

_AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD8 = (
    FieldSchema("iair", int, 0, 10, 0),
    FieldSchema("ngas", int, 10, 10, None),
    FieldSchema("norif", int, 20, 10, None),
    FieldSchema("nid1", int, 30, 10, 0),
    FieldSchema("nid2", int, 40, 10, 0),
    FieldSchema("nid3", int, 50, 10, 0),
    FieldSchema("chm", int, 60, 10, 0),
    FieldSchema("cd_ext", float, 70, 10, 0.0),
)

_AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD9 = (
    FieldSchema("sidup", int, 0, 10, None),
    FieldSchema("styup", int, 10, 10, 0),
    FieldSchema("pfrac", float, 20, 10, 0.0),
    FieldSchema("linking", int, 30, 10, None),
)

_AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD10 = (
    FieldSchema("sidh", int, 0, 10, None),
    FieldSchema("stypeh", int, 10, 10, 0),
    FieldSchema("hconv", float, 20, 10, None),
    FieldSchema("pfric", float, 30, 10, 0.0),
    FieldSchema("sdfblk", float, 40, 10, 1.0),
    FieldSchema("kp", float, 50, 10, 0.0),
    FieldSchema("inip", int, 60, 10, 0),
    FieldSchema("cp", float, 70, 10, None),
)

_AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD11 = (
    FieldSchema("sid3", int, 0, 10, None),
    FieldSchema("stype3", int, 10, 10, 0),
    FieldSchema("c23", float, 20, 10, 1.0),
    FieldSchema("lctc23", int, 30, 10, None),
    FieldSchema("lcpc23", int, 40, 10, None),
    FieldSchema("enh_v", int, 50, 10, 0),
    FieldSchema("ppop", float, 60, 10, 0.0),
)

_AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD12 = (
    FieldSchema("pair", float, 0, 10, None),
    FieldSchema("tair", float, 10, 10, 0.0),
    FieldSchema("xmair", float, 20, 10, None),
    FieldSchema("aair", float, 30, 10, None),
    FieldSchema("bair", float, 40, 10, 0.0),
    FieldSchema("cair", float, 50, 10, 0.0),
    FieldSchema("npair", int, 60, 10, 0),
    FieldSchema("nprlx", str, 70, 10, "0"),
)

_AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD13 = (
    FieldSchema("lcmass", int, 0, 10, None),
)

_AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD14 = (
    FieldSchema("lcmi", int, 0, 10, None),
    FieldSchema("lcti", int, 10, 10, None),
    FieldSchema("xmi", float, 20, 10, None),
    FieldSchema("ai", float, 30, 10, None),
    FieldSchema("bi", float, 40, 10, 0.0),
    FieldSchema("ci", float, 50, 10, 0.0),
    FieldSchema("infgi", int, 60, 10, 1),
)

_AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD15 = (
    FieldSchema("nidi", int, 0, 10, None),
    FieldSchema("ani", float, 10, 10, None),
    FieldSchema("vdi", int, 20, 10, None),
    FieldSchema("cai", float, 30, 10, 30.0),
    FieldSchema("infoi", int, 40, 10, 1),
    FieldSchema("imom", int, 50, 10, 0),
    FieldSchema("iang", int, 60, 10, 0),
    FieldSchema("chm_id", int, 70, 10, None),
)

class AirbagParticleDecompositionMolefractionSegmentTime(KeywordBase):
    """DYNA AIRBAG_PARTICLE_DECOMPOSITION_MOLEFRACTION_SEGMENT_TIME keyword"""

    keyword = "AIRBAG"
    subkeyword = "PARTICLE_DECOMPOSITION_MOLEFRACTION_SEGMENT_TIME"
    _link_fields = {
        "nid1": LinkType.NODE,
        "nid2": LinkType.NODE,
        "nid3": LinkType.NODE,
        "nidi": LinkType.NODE,
        "hconv": LinkType.DEFINE_CURVE,
        "lctc23": LinkType.DEFINE_CURVE,
        "lcpc23": LinkType.DEFINE_CURVE,
        "lcmass": LinkType.DEFINE_CURVE,
        "lcmi": LinkType.DEFINE_CURVE,
        "lcti": LinkType.DEFINE_CURVE,
        "vdi": LinkType.DEFINE_VECTOR,
    }

    def __init__(self, **kwargs):
        """Initialize the AirbagParticleDecompositionMolefractionSegmentTime class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD6,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD7,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD8,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD9,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD10,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD11,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD12,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD13,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD14,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGPARTICLEDECOMPOSITIONMOLEFRACTIONSEGMENTTIME_CARD15,
                **kwargs,
            ),        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Optional Airbag ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Airbag id descriptor. It is suggested that unique descriptions be used.
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[0].set_value("title", value)

    @property
    def birth(self) -> typing.Optional[float]:
        """Get or set the Scale factor for X direction use for MPP decomposition of particle domain.
        """ # nopep8
        return self._cards[1].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        """Set the birth property."""
        self._cards[1].set_value("birth", value)

    @property
    def death(self) -> typing.Optional[float]:
        """Get or set the Scale factor for Y direction use for MPP decomposition of particle domain.
        """ # nopep8
        return self._cards[1].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        """Set the death property."""
        self._cards[1].set_value("death", value)

    @property
    def sid1(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID defining the complete airbag.
        """ # nopep8
        return self._cards[2].get_value("sid1")

    @sid1.setter
    def sid1(self, value: int) -> None:
        """Set the sid1 property."""
        self._cards[2].set_value("sid1", value)

    @property
    def stype1(self) -> int:
        """Get or set the Set type:
        EQ.0: Part
        EQ.1: Part set.
        """ # nopep8
        return self._cards[2].get_value("stype1")

    @stype1.setter
    def stype1(self, value: int) -> None:
        """Set the stype1 property."""
        if value not in [0, 1, None]:
            raise Exception("""stype1 must be `None` or one of {0,1}.""")
        self._cards[2].set_value("stype1", value)

    @property
    def sid2(self) -> int:
        """Get or set the Part or part set ID defining internal parts of the airbag.
        """ # nopep8
        return self._cards[2].get_value("sid2")

    @sid2.setter
    def sid2(self, value: int) -> None:
        """Set the sid2 property."""
        self._cards[2].set_value("sid2", value)

    @property
    def stype2(self) -> int:
        """Get or set the Set type:
        EQ.0: Part
        EQ.1: Part set.
        EQ.2:	Number of parts to read (Not recommended for general use)
        """ # nopep8
        return self._cards[2].get_value("stype2")

    @stype2.setter
    def stype2(self, value: int) -> None:
        """Set the stype2 property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""stype2 must be `None` or one of {0,1,2}.""")
        self._cards[2].set_value("stype2", value)

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
        return self._cards[2].get_value("block")

    @block.setter
    def block(self, value: int) -> None:
        """Set the block property."""
        self._cards[2].set_value("block", value)

    @property
    def npdata(self) -> int:
        """Get or set the Number of parts or part sets data.
        """ # nopep8
        return self._cards[2].get_value("npdata")

    @npdata.setter
    def npdata(self, value: int) -> None:
        """Set the npdata property."""
        self._cards[2].set_value("npdata", value)

    @property
    def fric(self) -> float:
        """Get or set the Friction factor F_r if -1.0 < FRIC ≤ 1.0.  Otherwise,
        LE.-1.0:	|"FRIC" | is the curve ID which defines F_r as a function of the part pressure.
        GT.1.0:	FRIC is the *DEFINE_FUNCTION ID that defines F_r.  See Remark 2
        """ # nopep8
        return self._cards[2].get_value("fric")

    @fric.setter
    def fric(self, value: float) -> None:
        """Set the fric property."""
        self._cards[2].set_value("fric", value)

    @property
    def irdp(self) -> int:
        """Get or set the Dynamically scaling of particle radius
        EQ.0: Off
        EQ.1: On
        """ # nopep8
        return self._cards[2].get_value("irdp")

    @irdp.setter
    def irdp(self, value: int) -> None:
        """Set the irdp property."""
        if value not in [0, 1, None]:
            raise Exception("""irdp must be `None` or one of {0,1}.""")
        self._cards[2].set_value("irdp", value)

    @property
    def segsid(self) -> typing.Optional[int]:
        """Get or set the ID for a segment set.  The segments define the volume and should belong to the parts from SID1.
        """ # nopep8
        return self._cards[3].get_value("segsid")

    @segsid.setter
    def segsid(self, value: int) -> None:
        """Set the segsid property."""
        self._cards[3].set_value("segsid", value)

    @property
    def np(self) -> int:
        """Get or set the Number of particles (Default 200,000).
        """ # nopep8
        return self._cards[4].get_value("np")

    @np.setter
    def np(self, value: int) -> None:
        """Set the np property."""
        self._cards[4].set_value("np", value)

    @property
    def unit(self) -> int:
        """Get or set the Unit system
        EQ.0: kg-mm-ms-K
        EQ.1: SI-units
        EQ.2: tonne-mm-s-K.
        EQ.3:	User defined units (see Remark 11)
        """ # nopep8
        return self._cards[4].get_value("unit")

    @unit.setter
    def unit(self, value: int) -> None:
        """Set the unit property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""unit must be `None` or one of {0,1,2,3}.""")
        self._cards[4].set_value("unit", value)

    @property
    def visflg(self) -> int:
        """Get or set the Visible particles(only support CPM database, see remark 6)
        EQ.0: Default to 1
        EQ.1: Output particle's coordinates, velocities, mass, radius, spin energy,
        translational energy
        EQ.2: Output reduce data set with corrdinates only
        EQ.3: Supress CPM database.
        """ # nopep8
        return self._cards[4].get_value("visflg")

    @visflg.setter
    def visflg(self, value: int) -> None:
        """Set the visflg property."""
        if value not in [1, 0, 2, 3, None]:
            raise Exception("""visflg must be `None` or one of {1,0,2,3}.""")
        self._cards[4].set_value("visflg", value)

    @property
    def tatm(self) -> float:
        """Get or set the Atmospheric temperature (Default 293K).
        """ # nopep8
        return self._cards[4].get_value("tatm")

    @tatm.setter
    def tatm(self, value: float) -> None:
        """Set the tatm property."""
        self._cards[4].set_value("tatm", value)

    @property
    def patm(self) -> float:
        """Get or set the Atmospheric pressure (Default 1ATM).
        """ # nopep8
        return self._cards[4].get_value("patm")

    @patm.setter
    def patm(self, value: float) -> None:
        """Set the patm property."""
        self._cards[4].set_value("patm", value)

    @property
    def nvent(self) -> int:
        """Get or set the Number of vent hole parts or part sets.
        """ # nopep8
        return self._cards[4].get_value("nvent")

    @nvent.setter
    def nvent(self, value: int) -> None:
        """Set the nvent property."""
        self._cards[4].set_value("nvent", value)

    @property
    def tend(self) -> float:
        """Get or set the Time when all particles (NP) have entered bag (Default 1.0e10).
        """ # nopep8
        return self._cards[4].get_value("tend")

    @tend.setter
    def tend(self, value: float) -> None:
        """Set the tend property."""
        self._cards[4].set_value("tend", value)

    @property
    def tsw(self) -> float:
        """Get or set the Time for switch to control volume calculation (Default 1.0e10).
        """ # nopep8
        return self._cards[4].get_value("tsw")

    @tsw.setter
    def tsw(self, value: float) -> None:
        """Set the tsw property."""
        self._cards[4].set_value("tsw", value)

    @property
    def tstop(self) -> float:
        """Get or set the Time at which front tracking switches from IAIR = 4 to IAIR = 2.
        """ # nopep8
        return self._cards[5].get_value("tstop")

    @tstop.setter
    def tstop(self, value: float) -> None:
        """Set the tstop property."""
        self._cards[5].set_value("tstop", value)

    @property
    def tsmth(self) -> float:
        """Get or set the To avoid sudden jumps in the pressure signal during switching,
        the front tracking is tapered during a transition period.
        The default time of 1.0 millisecond will be applied if this value is set to zero
        """ # nopep8
        return self._cards[5].get_value("tsmth")

    @tsmth.setter
    def tsmth(self, value: float) -> None:
        """Set the tsmth property."""
        self._cards[5].set_value("tsmth", value)

    @property
    def occup(self) -> float:
        """Get or set the Particles occupy OCCUP percent of the airbag’s volume.  The default value of OCCUP is 10%.
        This field can be used to balance computational cost and signal quality.  OCCUP ranges from 0.001 to 0.1..
        """ # nopep8
        return self._cards[5].get_value("occup")

    @occup.setter
    def occup(self, value: float) -> None:
        """Set the occup property."""
        self._cards[5].set_value("occup", value)

    @property
    def rebl(self) -> int:
        """Get or set the If the option is ON, all energy stored from damping will be evenly distributed as vibrational energy to all particles.
        This improves the pressure calculation in certain applications.
        EQ.0:	Off (Default)
        EQ.1:	On.
        """ # nopep8
        return self._cards[5].get_value("rebl")

    @rebl.setter
    def rebl(self, value: int) -> None:
        """Set the rebl property."""
        if value not in [0, 1, None]:
            raise Exception("""rebl must be `None` or one of {0,1}.""")
        self._cards[5].set_value("rebl", value)

    @property
    def sidsv(self) -> typing.Optional[int]:
        """Get or set the Part set ID for internal shell part.  The volume formed by this internal shell part will be excluded from the bag volume.  These internal parts must have consistent orientation to get correct excluded volume.
        """ # nopep8
        return self._cards[5].get_value("sidsv")

    @sidsv.setter
    def sidsv(self, value: int) -> None:
        """Set the sidsv property."""
        self._cards[5].set_value("sidsv", value)

    @property
    def psid1(self) -> typing.Optional[int]:
        """Get or set the Part set ID for external parts which have normal pointed outward.  This option is usually used with airbag integrity check while there are two CPM bags connected with bag interaction.  Therefore, one of the bag can have the correct shell orientation but the share parts for the second bag will have wrong orientation.  This option will automatically flip the parts defined in this set in the second bag during integrity checking.
        """ # nopep8
        return self._cards[5].get_value("psid1")

    @psid1.setter
    def psid1(self, value: int) -> None:
        """Set the psid1 property."""
        self._cards[5].set_value("psid1", value)

    @property
    def tsplit(self) -> typing.Optional[float]:
        """Get or set the Start time to activate particle splitting algorithm. See Remark 15.
        """ # nopep8
        return self._cards[5].get_value("tsplit")

    @tsplit.setter
    def tsplit(self, value: float) -> None:
        """Set the tsplit property."""
        self._cards[5].set_value("tsplit", value)

    @property
    def sffdc(self) -> float:
        """Get or set the Scale factor for the force decay constant.  SFFDC has a range of . The default value is 1.0.  The value given here will replaced the values from *CONTROL_CPM
        """ # nopep8
        return self._cards[5].get_value("sffdc")

    @sffdc.setter
    def sffdc(self, value: float) -> None:
        """Set the sffdc property."""
        self._cards[5].set_value("sffdc", value)

    @property
    def sfiair4(self) -> float:
        """Get or set the Scale factor for the ratio of initial air particles to inflator gas particles for IAIR = 4.
        Smaller values weaken the effect of gas front tracking.
        """ # nopep8
        return self._cards[6].get_value("sfiair4")

    @sfiair4.setter
    def sfiair4(self, value: float) -> None:
        """Set the sfiair4 property."""
        self._cards[6].set_value("sfiair4", value)

    @property
    def idfric(self) -> int:
        """Get or set the Direction of P2F impact force:
        EQ.0:	No change(default)
        EQ.1 : The force is applied in the segment normal direction
        """ # nopep8
        return self._cards[6].get_value("idfric")

    @idfric.setter
    def idfric(self, value: int) -> None:
        """Set the idfric property."""
        if value not in [0, 1, None]:
            raise Exception("""idfric must be `None` or one of {0,1}.""")
        self._cards[6].set_value("idfric", value)

    @property
    def mass(self) -> typing.Optional[float]:
        """Get or set the Conversion factor from current unit to MKS unit.  For example, if the current unit is using kg-mm-ms, the input should be 1.0, 0.001, 0.001.
        """ # nopep8
        return self._cards[7].get_value("mass")

    @mass.setter
    def mass(self, value: float) -> None:
        """Set the mass property."""
        self._cards[7].set_value("mass", value)

    @property
    def time(self) -> typing.Optional[float]:
        """Get or set the Conversion factor from current unit to MKS unit.  For example, if the current unit is using kg-mm-ms, the input should be 1.0, 0.001, 0.001.
        """ # nopep8
        return self._cards[7].get_value("time")

    @time.setter
    def time(self, value: float) -> None:
        """Set the time property."""
        self._cards[7].set_value("time", value)

    @property
    def length(self) -> typing.Optional[float]:
        """Get or set the Conversion factor from current unit to MKS unit.  For example, if the current unit is using kg-mm-ms, the input should be 1.0, 0.001, 0.001.
        """ # nopep8
        return self._cards[7].get_value("length")

    @length.setter
    def length(self, value: float) -> None:
        """Set the length property."""
        self._cards[7].set_value("length", value)

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
        return self._cards[8].get_value("iair")

    @iair.setter
    def iair(self, value: int) -> None:
        """Set the iair property."""
        self._cards[8].set_value("iair", value)

    @property
    def ngas(self) -> typing.Optional[int]:
        """Get or set the Number of gas components.
        """ # nopep8
        return self._cards[8].get_value("ngas")

    @ngas.setter
    def ngas(self, value: int) -> None:
        """Set the ngas property."""
        self._cards[8].set_value("ngas", value)

    @property
    def norif(self) -> typing.Optional[int]:
        """Get or set the Number of orifices.
        """ # nopep8
        return self._cards[8].get_value("norif")

    @norif.setter
    def norif(self, value: int) -> None:
        """Set the norif property."""
        self._cards[8].set_value("norif", value)

    @property
    def nid1(self) -> int:
        """Get or set the NID1-NID3, Three nodes defining a moving coordinate system for the direction of flow through the gas inlet nozzles (Default fixed system).
        """ # nopep8
        return self._cards[8].get_value("nid1")

    @nid1.setter
    def nid1(self, value: int) -> None:
        """Set the nid1 property."""
        self._cards[8].set_value("nid1", value)

    @property
    def nid2(self) -> int:
        """Get or set the NID1-NID3, Three nodes defining a moving coordinate system for the direction of flow through the gas inlet nozzles (Default fixed system).
        """ # nopep8
        return self._cards[8].get_value("nid2")

    @nid2.setter
    def nid2(self, value: int) -> None:
        """Set the nid2 property."""
        self._cards[8].set_value("nid2", value)

    @property
    def nid3(self) -> int:
        """Get or set the NID1-NID3, Three nodes defining a moving coordinate system for the direction of flow through the gas inlet nozzles (Default fixed system).
        """ # nopep8
        return self._cards[8].get_value("nid3")

    @nid3.setter
    def nid3(self, value: int) -> None:
        """Set the nid3 property."""
        self._cards[8].set_value("nid3", value)

    @property
    def chm(self) -> int:
        """Get or set the Chamber ID used in *DEFINE_CPM_CHAMBER.
        """ # nopep8
        return self._cards[8].get_value("chm")

    @chm.setter
    def chm(self, value: int) -> None:
        """Set the chm property."""
        self._cards[8].set_value("chm", value)

    @property
    def cd_ext(self) -> float:
        """Get or set the Drag coefficient for external air. If the value is not zero, the inertial effect
        from external air will be considered and forces will be applied in the normal
        direction on the exterior airbag surface.
        """ # nopep8
        return self._cards[8].get_value("cd_ext")

    @cd_ext.setter
    def cd_ext(self, value: float) -> None:
        """Set the cd_ext property."""
        self._cards[8].set_value("cd_ext", value)

    @property
    def sidup(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID defining the internal parts that pressure will be applied to.
        This internal structure acts as a valve to control the external vent hole area.
        Pressure will be applied only after switch to UP (uniform pressure) using TSW.
        """ # nopep8
        return self._cards[9].get_value("sidup")

    @sidup.setter
    def sidup(self, value: int) -> None:
        """Set the sidup property."""
        self._cards[9].set_value("sidup", value)

    @property
    def styup(self) -> int:
        """Get or set the Set defining internal parts will be applied pressure
        Set type EQ.0: Part
        EQ.1: Part set.
        """ # nopep8
        return self._cards[9].get_value("styup")

    @styup.setter
    def styup(self, value: int) -> None:
        """Set the styup property."""
        if value not in [0, 1, None]:
            raise Exception("""styup must be `None` or one of {0,1}.""")
        self._cards[9].set_value("styup", value)

    @property
    def pfrac(self) -> float:
        """Get or set the Part or part set ID defining the internal parts that pressure will be applied to.
        This internal structure acts as a valve to control the external vent hole area.
        Pressure will be applied only after switch to UP (uniform pressure) using TSW.
        """ # nopep8
        return self._cards[9].get_value("pfrac")

    @pfrac.setter
    def pfrac(self, value: float) -> None:
        """Set the pfrac property."""
        self._cards[9].set_value("pfrac", value)

    @property
    def linking(self) -> typing.Optional[int]:
        """Get or set the Part ID of an internal part that is coupled to the external vent definition.
        The minimum area of this part or the vent hole will be used for actual venting area.
        """ # nopep8
        return self._cards[9].get_value("linking")

    @linking.setter
    def linking(self, value: int) -> None:
        """Set the linking property."""
        self._cards[9].set_value("linking", value)

    @property
    def sidh(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID defining part data.
        """ # nopep8
        return self._cards[10].get_value("sidh")

    @sidh.setter
    def sidh(self, value: int) -> None:
        """Set the sidh property."""
        self._cards[10].set_value("sidh", value)

    @property
    def stypeh(self) -> int:
        """Get or set the Set type EQ.0: Part
        EQ.1: Part set.
        EQ.2: part and HCONV is the *DEFINE_CPM_NPDATA ID
        EQ.3: part set and HCONV is the * DEFINE_CPM_NPDATA ID
        """ # nopep8
        return self._cards[10].get_value("stypeh")

    @stypeh.setter
    def stypeh(self, value: int) -> None:
        """Set the stypeh property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""stypeh must be `None` or one of {0,1,2,3}.""")
        self._cards[10].set_value("stypeh", value)

    @property
    def hconv(self) -> typing.Optional[float]:
        """Get or set the Heat convection coefficient used to calculate heat loss from the airbag external surface to ambient (W/K/m2).
        See *AIRBAG_HYBRID developments (Resp. P.O. Marklund).
        LT.0:	|HCONV | is a load curve ID defines heat convection coefficient as a function of time.
        When STYPEH is greater than 1, HCONV is an integer of *DEFINE_CPM_NPDATA ID.
        """ # nopep8
        return self._cards[10].get_value("hconv")

    @hconv.setter
    def hconv(self, value: float) -> None:
        """Set the hconv property."""
        self._cards[10].set_value("hconv", value)

    @property
    def pfric(self) -> float:
        """Get or set the Friction factor.
        """ # nopep8
        return self._cards[10].get_value("pfric")

    @pfric.setter
    def pfric(self, value: float) -> None:
        """Set the pfric property."""
        self._cards[10].set_value("pfric", value)

    @property
    def sdfblk(self) -> float:
        """Get or set the Scale down factor for blockage factor (Default=1, no scale down). The val-id factor will be (0,1]. If 0, it will set to 1.
        """ # nopep8
        return self._cards[10].get_value("sdfblk")

    @sdfblk.setter
    def sdfblk(self, value: float) -> None:
        """Set the sdfblk property."""
        self._cards[10].set_value("sdfblk", value)

    @property
    def kp(self) -> float:
        """Get or set the Thermal conductivity of the part.
        """ # nopep8
        return self._cards[10].get_value("kp")

    @kp.setter
    def kp(self, value: float) -> None:
        """Set the kp property."""
        self._cards[10].set_value("kp", value)

    @property
    def inip(self) -> int:
        """Get or set the Place initial air particles on surface.
        EQ.0:	yes (default)
        EQ.1:	no
        This feature exclude surfaces from initial particle placement.  This option is useful for preventing particles from being trapped between adjacent fabric layers..
        """ # nopep8
        return self._cards[10].get_value("inip")

    @inip.setter
    def inip(self, value: int) -> None:
        """Set the inip property."""
        if value not in [0, 1, None]:
            raise Exception("""inip must be `None` or one of {0,1}.""")
        self._cards[10].set_value("inip", value)

    @property
    def cp(self) -> typing.Optional[float]:
        """Get or set the Specific heat (see Remark 16).
        """ # nopep8
        return self._cards[10].get_value("cp")

    @cp.setter
    def cp(self, value: float) -> None:
        """Set the cp property."""
        self._cards[10].set_value("cp", value)

    @property
    def sid3(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID defining vent holes.
        """ # nopep8
        return self._cards[11].get_value("sid3")

    @sid3.setter
    def sid3(self, value: int) -> None:
        """Set the sid3 property."""
        self._cards[11].set_value("sid3", value)

    @property
    def stype3(self) -> int:
        """Get or set the Set type:
        EQ.0: Part
        EQ.1: Part set which each part being treated separately.
        EQ.2:	Part set and all parts are treated as one vent.  See Remark 13
        """ # nopep8
        return self._cards[11].get_value("stype3")

    @stype3.setter
    def stype3(self, value: int) -> None:
        """Set the stype3 property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""stype3 must be `None` or one of {0,1,2}.""")
        self._cards[11].set_value("stype3", value)

    @property
    def c23(self) -> float:
        """Get or set the GE.0:	Vent hole coefficient, a parameter of Wang-Nefske leakage.  A value between 0.0 and 1.0 can be input.  See Remark 1.
        LT.0:	ID for *DEFINE_CPM_VENT.
        """ # nopep8
        return self._cards[11].get_value("c23")

    @c23.setter
    def c23(self, value: float) -> None:
        """Set the c23 property."""
        self._cards[11].set_value("c23", value)

    @property
    def lctc23(self) -> typing.Optional[int]:
        """Get or set the Load curve defining vent hole coefficient as a function of time.  LCTC23 can be defined through *DEFINE_CURVE_FUNCTION.  If omitted, a curve equal to 1.0 used.
        """ # nopep8
        return self._cards[11].get_value("lctc23")

    @lctc23.setter
    def lctc23(self, value: int) -> None:
        """Set the lctc23 property."""
        self._cards[11].set_value("lctc23", value)

    @property
    def lcpc23(self) -> typing.Optional[int]:
        """Get or set the Load curve defining vent hole coefficient as a function of pressure.  If omitted a curve equal to 1.0 is used..
        """ # nopep8
        return self._cards[11].get_value("lcpc23")

    @lcpc23.setter
    def lcpc23(self, value: int) -> None:
        """Set the lcpc23 property."""
        self._cards[11].set_value("lcpc23", value)

    @property
    def enh_v(self) -> int:
        """Get or set the Enhanced venting option. See Remark 8.
        EQ.0:	Off (default)
        EQ.1:	On
        EQ.2:	Two way flow for internal vent; treated as hole for external vent .
        """ # nopep8
        return self._cards[11].get_value("enh_v")

    @enh_v.setter
    def enh_v(self, value: int) -> None:
        """Set the enh_v property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""enh_v must be `None` or one of {0,1,2}.""")
        self._cards[11].set_value("enh_v", value)

    @property
    def ppop(self) -> float:
        """Get or set the Pressure difference between interior and ambient pressure (PATM) to open the vent holes.  Once the vents are open, they will stay open.
        """ # nopep8
        return self._cards[11].get_value("ppop")

    @ppop.setter
    def ppop(self, value: float) -> None:
        """Set the ppop property."""
        self._cards[11].set_value("ppop", value)

    @property
    def pair(self) -> typing.Optional[float]:
        """Get or set the Initial pressure inside bag .
        """ # nopep8
        return self._cards[12].get_value("pair")

    @pair.setter
    def pair(self, value: float) -> None:
        """Set the pair property."""
        self._cards[12].set_value("pair", value)

    @property
    def tair(self) -> float:
        """Get or set the Initial temperature inside bag .
        """ # nopep8
        return self._cards[12].get_value("tair")

    @tair.setter
    def tair(self, value: float) -> None:
        """Set the tair property."""
        self._cards[12].set_value("tair", value)

    @property
    def xmair(self) -> typing.Optional[float]:
        """Get or set the Molar mass of gas initially inside bag.
        LT.0:	-XMAIR references the ID of a *DEFINE_CPM_GAS_PROPERTIES keyword that defines the gas thermodynamic properties.
        Note that AAIR, BAIR, and CAIR are ignored
        """ # nopep8
        return self._cards[12].get_value("xmair")

    @xmair.setter
    def xmair(self, value: float) -> None:
        """Set the xmair property."""
        self._cards[12].set_value("xmair", value)

    @property
    def aair(self) -> typing.Optional[float]:
        """Get or set the Constant, linear, and quadratic heat capacity parameters.
        """ # nopep8
        return self._cards[12].get_value("aair")

    @aair.setter
    def aair(self, value: float) -> None:
        """Set the aair property."""
        self._cards[12].set_value("aair", value)

    @property
    def bair(self) -> float:
        """Get or set the Constant, linear, and quadratic heat capacity parameters.
        """ # nopep8
        return self._cards[12].get_value("bair")

    @bair.setter
    def bair(self, value: float) -> None:
        """Set the bair property."""
        self._cards[12].set_value("bair", value)

    @property
    def cair(self) -> float:
        """Get or set the Constant, linear, and quadratic heat capacity parameters.
        """ # nopep8
        return self._cards[12].get_value("cair")

    @cair.setter
    def cair(self, value: float) -> None:
        """Set the cair property."""
        self._cards[12].set_value("cair", value)

    @property
    def npair(self) -> int:
        """Get or set the Number of particle for air.
        """ # nopep8
        return self._cards[12].get_value("npair")

    @npair.setter
    def npair(self, value: int) -> None:
        """Set the npair property."""
        self._cards[12].set_value("npair", value)

    @property
    def nprlx(self) -> str:
        """Get or set the Number of cycles to reach thermal equilibrium.  See Remark 6.
        LT.0:	If more than 50% of the collision to fabric is from initial air particles, the contact force will not apply to the fabric segment in order to keep its original shape.
        If the number contains “.”, “e” or “E”, NPRLX will treated as an end time rather than as a cycle count.
        """ # nopep8
        return self._cards[12].get_value("nprlx")

    @nprlx.setter
    def nprlx(self, value: str) -> None:
        """Set the nprlx property."""
        self._cards[12].set_value("nprlx", value)

    @property
    def lcmass(self) -> typing.Optional[int]:
        """Get or set the Total mass flow rate curve for the MOLEFRACTION option.
        """ # nopep8
        return self._cards[13].get_value("lcmass")

    @lcmass.setter
    def lcmass(self, value: int) -> None:
        """Set the lcmass property."""
        self._cards[13].set_value("lcmass", value)

    @property
    def lcmi(self) -> typing.Optional[int]:
        """Get or set the Mass flow rate curve for gas component i, unless the MOLEFRACTION option is used.
        If the MOLEFRACTION option is used, then it is the time dependent molar fraction of the total flow for gas component i.
        """ # nopep8
        return self._cards[14].get_value("lcmi")

    @lcmi.setter
    def lcmi(self, value: int) -> None:
        """Set the lcmi property."""
        self._cards[14].set_value("lcmi", value)

    @property
    def lcti(self) -> typing.Optional[int]:
        """Get or set the Temperature curve for gas component i.
        """ # nopep8
        return self._cards[14].get_value("lcti")

    @lcti.setter
    def lcti(self, value: int) -> None:
        """Set the lcti property."""
        self._cards[14].set_value("lcti", value)

    @property
    def xmi(self) -> typing.Optional[float]:
        """Get or set the Molar mass of gas component i.
        LT.0:	the absolute value of XMi references the ID of a *DEFINE_‌CPM_‌GAS_‌PROPERTIES keyword that defines the gas thermodynamic properties.
        Note that Ai, Bi, and Ci are ignored
        """ # nopep8
        return self._cards[14].get_value("xmi")

    @xmi.setter
    def xmi(self, value: float) -> None:
        """Set the xmi property."""
        self._cards[14].set_value("xmi", value)

    @property
    def ai(self) -> typing.Optional[float]:
        """Get or set the Constant, linear, and quadratic heat capacity parameters for gas component i.
        """ # nopep8
        return self._cards[14].get_value("ai")

    @ai.setter
    def ai(self, value: float) -> None:
        """Set the ai property."""
        self._cards[14].set_value("ai", value)

    @property
    def bi(self) -> float:
        """Get or set the Constant, linear, and quadratic heat capacity parameters for gas component i.
        """ # nopep8
        return self._cards[14].get_value("bi")

    @bi.setter
    def bi(self, value: float) -> None:
        """Set the bi property."""
        self._cards[14].set_value("bi", value)

    @property
    def ci(self) -> float:
        """Get or set the Constant, linear, and quadratic heat capacity parameters for gas component i.
        """ # nopep8
        return self._cards[14].get_value("ci")

    @ci.setter
    def ci(self, value: float) -> None:
        """Set the ci property."""
        self._cards[14].set_value("ci", value)

    @property
    def infgi(self) -> int:
        """Get or set the Inflator ID that this gas component belongs to (Default 1).
        """ # nopep8
        return self._cards[14].get_value("infgi")

    @infgi.setter
    def infgi(self, value: int) -> None:
        """Set the infgi property."""
        self._cards[14].set_value("infgi", value)

    @property
    def nidi(self) -> typing.Optional[int]:
        """Get or set the Node ID/Shell ID defining the location of nozzle i.
        """ # nopep8
        return self._cards[15].get_value("nidi")

    @nidi.setter
    def nidi(self, value: int) -> None:
        """Set the nidi property."""
        self._cards[15].set_value("nidi", value)

    @property
    def ani(self) -> typing.Optional[float]:
        """Get or set the Area of nozzle i (Default all nozzles are given the same area).
        """ # nopep8
        return self._cards[15].get_value("ani")

    @ani.setter
    def ani(self, value: float) -> None:
        """Set the ani property."""
        self._cards[15].set_value("ani", value)

    @property
    def vdi(self) -> typing.Optional[int]:
        """Get or set the GT.0:	Vector ID.  Initial direction of gas inflow at nozzle i.
        LT.0:	Values in the NIDi fields are interpreted as shell IDs.  See Remark 12.
        EQ.-1:	direction of gas inflow is using shell normal
        EQ.-2:	direction of gas inflow is in reversed shell normal.
        """ # nopep8
        return self._cards[15].get_value("vdi")

    @vdi.setter
    def vdi(self, value: int) -> None:
        """Set the vdi property."""
        self._cards[15].set_value("vdi", value)

    @property
    def cai(self) -> float:
        """Get or set the Cone angle in degrees (defaults to30°). This option is used only when IANG is equal to 1.
        """ # nopep8
        return self._cards[15].get_value("cai")

    @cai.setter
    def cai(self, value: float) -> None:
        """Set the cai property."""
        self._cards[15].set_value("cai", value)

    @property
    def infoi(self) -> int:
        """Get or set the Inflator ID for this orifice.  (default = 1).
        """ # nopep8
        return self._cards[15].get_value("infoi")

    @infoi.setter
    def infoi(self, value: int) -> None:
        """Set the infoi property."""
        self._cards[15].set_value("infoi", value)

    @property
    def imom(self) -> int:
        """Get or set the Inflator reaction forces
        EQ.0: Off
        EQ.1: On
        """ # nopep8
        return self._cards[15].get_value("imom")

    @imom.setter
    def imom(self, value: int) -> None:
        """Set the imom property."""
        if value not in [0, 1, None]:
            raise Exception("""imom must be `None` or one of {0,1}.""")
        self._cards[15].set_value("imom", value)

    @property
    def iang(self) -> int:
        """Get or set the Activation for cone angle to use for friction calibration(should not use in the normal runs)
        EQ.0: Off(Default)
        EQ.1: On.
        """ # nopep8
        return self._cards[15].get_value("iang")

    @iang.setter
    def iang(self, value: int) -> None:
        """Set the iang property."""
        if value not in [0, 1, None]:
            raise Exception("""iang must be `None` or one of {0,1}.""")
        self._cards[15].set_value("iang", value)

    @property
    def chm_id(self) -> typing.Optional[int]:
        """Get or set the Chamber ID where the inflator node resides.  Chambers are defined using the *DEFINE_CPM_CHAMBER keyword.
        """ # nopep8
        return self._cards[15].get_value("chm_id")

    @chm_id.setter
    def chm_id(self, value: int) -> None:
        """Set the chm_id property."""
        self._cards[15].set_value("chm_id", value)

    @property
    def nid1_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given nid1."""
        return self._get_link_by_attr("NODE", "nid", self.nid1, "parts")

    @property
    def nid2_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given nid2."""
        return self._get_link_by_attr("NODE", "nid", self.nid2, "parts")

    @property
    def nid3_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given nid3."""
        return self._get_link_by_attr("NODE", "nid", self.nid3, "parts")

    @property
    def nidi_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given nidi."""
        return self._get_link_by_attr("NODE", "nid", self.nidi, "parts")

    @property
    def hconv_link(self) -> DefineCurve:
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
    def lctc23_link(self) -> DefineCurve:
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
    def lcpc23_link(self) -> DefineCurve:
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
    def lcmass_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcmass."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcmass:
                return kwd
        return None

    @lcmass_link.setter
    def lcmass_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcmass."""
        self.lcmass = value.lcid

    @property
    def lcmi_link(self) -> DefineCurve:
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
    def lcti_link(self) -> DefineCurve:
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
    def vdi_link(self) -> DefineVector:
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

