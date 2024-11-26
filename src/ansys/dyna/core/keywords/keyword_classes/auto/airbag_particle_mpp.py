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

import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class AirbagParticleMpp(KeywordBase):
    """DYNA AIRBAG_PARTICLE_MPP keyword"""

    keyword = "AIRBAG"
    subkeyword = "PARTICLE_MPP"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "sx",
                        float,
                        0,
                        10,
                        kwargs.get("sx")
                    ),
                    Field(
                        "sy",
                        float,
                        10,
                        10,
                        kwargs.get("sy")
                    ),
                    Field(
                        "sz",
                        float,
                        20,
                        10,
                        kwargs.get("sz")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "id",
                        int,
                        0,
                        10,
                        kwargs.get("id")
                    ),
                    Field(
                        "title",
                        str,
                        10,
                        70,
                        kwargs.get("title")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sid1",
                        int,
                        0,
                        10,
                        kwargs.get("sid1")
                    ),
                    Field(
                        "stype1",
                        int,
                        10,
                        10,
                        kwargs.get("stype1", 0)
                    ),
                    Field(
                        "sid2",
                        int,
                        20,
                        10,
                        kwargs.get("sid2", 0)
                    ),
                    Field(
                        "stype2",
                        int,
                        30,
                        10,
                        kwargs.get("stype2", 0)
                    ),
                    Field(
                        "block",
                        int,
                        40,
                        10,
                        kwargs.get("block")
                    ),
                    Field(
                        "npdata",
                        int,
                        50,
                        10,
                        kwargs.get("npdata", 0)
                    ),
                    Field(
                        "fric",
                        float,
                        60,
                        10,
                        kwargs.get("fric", 0.0)
                    ),
                    Field(
                        "irdp",
                        int,
                        70,
                        10,
                        kwargs.get("irdp", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "np",
                        int,
                        0,
                        10,
                        kwargs.get("np", 200000)
                    ),
                    Field(
                        "unit",
                        int,
                        10,
                        10,
                        kwargs.get("unit", 0)
                    ),
                    Field(
                        "visflg",
                        int,
                        20,
                        10,
                        kwargs.get("visflg", 1)
                    ),
                    Field(
                        "tatm",
                        float,
                        30,
                        10,
                        kwargs.get("tatm", 293)
                    ),
                    Field(
                        "patm",
                        float,
                        40,
                        10,
                        kwargs.get("patm", 1)
                    ),
                    Field(
                        "nvent",
                        int,
                        50,
                        10,
                        kwargs.get("nvent", 0)
                    ),
                    Field(
                        "tend",
                        float,
                        60,
                        10,
                        kwargs.get("tend", 1.0E10)
                    ),
                    Field(
                        "tsw",
                        float,
                        70,
                        10,
                        kwargs.get("tsw", 1.0E10)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "tstop",
                        float,
                        1,
                        9,
                        kwargs.get("tstop", 1e11)
                    ),
                    Field(
                        "tsmth",
                        float,
                        10,
                        10,
                        kwargs.get("tsmth", 1.0)
                    ),
                    Field(
                        "occup",
                        float,
                        20,
                        10,
                        kwargs.get("occup", 0.1)
                    ),
                    Field(
                        "rebl",
                        int,
                        30,
                        10,
                        kwargs.get("rebl", 0)
                    ),
                    Field(
                        "sidsv",
                        int,
                        40,
                        10,
                        kwargs.get("sidsv")
                    ),
                    Field(
                        "psid1",
                        int,
                        50,
                        10,
                        kwargs.get("psid1")
                    ),
                    Field(
                        "tsplit",
                        float,
                        60,
                        10,
                        kwargs.get("tsplit")
                    ),
                    Field(
                        "sffdc",
                        float,
                        70,
                        10,
                        kwargs.get("sffdc", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sfiair4",
                        float,
                        1,
                        9,
                        kwargs.get("sfiair4", 1.0)
                    ),
                    Field(
                        "idfric",
                        int,
                        10,
                        10,
                        kwargs.get("idfric", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "unused",
                        int,
                        0,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "mass",
                        float,
                        10,
                        10,
                        kwargs.get("mass")
                    ),
                    Field(
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "time",
                        float,
                        30,
                        10,
                        kwargs.get("time")
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "length",
                        float,
                        50,
                        10,
                        kwargs.get("length")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "iair",
                        int,
                        0,
                        10,
                        kwargs.get("iair", 0)
                    ),
                    Field(
                        "ngas",
                        int,
                        10,
                        10,
                        kwargs.get("ngas")
                    ),
                    Field(
                        "norif",
                        int,
                        20,
                        10,
                        kwargs.get("norif")
                    ),
                    Field(
                        "nid1",
                        int,
                        30,
                        10,
                        kwargs.get("nid1", 0)
                    ),
                    Field(
                        "nid2",
                        int,
                        40,
                        10,
                        kwargs.get("nid2", 0)
                    ),
                    Field(
                        "nid3",
                        int,
                        50,
                        10,
                        kwargs.get("nid3", 0)
                    ),
                    Field(
                        "chm",
                        int,
                        60,
                        10,
                        kwargs.get("chm", 0)
                    ),
                    Field(
                        "cd_ext",
                        float,
                        70,
                        10,
                        kwargs.get("cd_ext", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sidup",
                        int,
                        0,
                        10,
                        kwargs.get("sidup")
                    ),
                    Field(
                        "styup",
                        int,
                        10,
                        10,
                        kwargs.get("styup", 0)
                    ),
                    Field(
                        "pfrac",
                        float,
                        20,
                        10,
                        kwargs.get("pfrac", 0.0)
                    ),
                    Field(
                        "linking",
                        int,
                        30,
                        10,
                        kwargs.get("linking")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sidh",
                        int,
                        0,
                        10,
                        kwargs.get("sidh")
                    ),
                    Field(
                        "stypeh",
                        int,
                        10,
                        10,
                        kwargs.get("stypeh", 0)
                    ),
                    Field(
                        "hconv",
                        float,
                        20,
                        10,
                        kwargs.get("hconv")
                    ),
                    Field(
                        "pfric",
                        float,
                        30,
                        10,
                        kwargs.get("pfric", 0.0)
                    ),
                    Field(
                        "sdfblk",
                        float,
                        40,
                        10,
                        kwargs.get("sdfblk", 1.0)
                    ),
                    Field(
                        "kp",
                        float,
                        50,
                        10,
                        kwargs.get("kp", 0.0)
                    ),
                    Field(
                        "inip",
                        int,
                        60,
                        10,
                        kwargs.get("inip", 0)
                    ),
                    Field(
                        "cp",
                        float,
                        70,
                        10,
                        kwargs.get("cp")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sid3",
                        int,
                        0,
                        10,
                        kwargs.get("sid3")
                    ),
                    Field(
                        "stype3",
                        int,
                        10,
                        10,
                        kwargs.get("stype3", 0)
                    ),
                    Field(
                        "c23",
                        float,
                        20,
                        10,
                        kwargs.get("c23", 1.0)
                    ),
                    Field(
                        "lctc23",
                        int,
                        30,
                        10,
                        kwargs.get("lctc23")
                    ),
                    Field(
                        "lcpc23",
                        int,
                        40,
                        10,
                        kwargs.get("lcpc23")
                    ),
                    Field(
                        "enh_v",
                        int,
                        50,
                        10,
                        kwargs.get("enh_v", 0)
                    ),
                    Field(
                        "ppop",
                        float,
                        60,
                        10,
                        kwargs.get("ppop", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def sx(self) -> typing.Optional[float]:
        """Get or set the Scale factor for X direction use for MPP decomposition of particle domain.
        """ # nopep8
        return self._cards[0].get_value("sx")

    @sx.setter
    def sx(self, value: float) -> None:
        self._cards[0].set_value("sx", value)

    @property
    def sy(self) -> typing.Optional[float]:
        """Get or set the Scale factor for Y direction use for MPP decomposition of particle domain.
        """ # nopep8
        return self._cards[0].get_value("sy")

    @sy.setter
    def sy(self, value: float) -> None:
        self._cards[0].set_value("sy", value)

    @property
    def sz(self) -> typing.Optional[float]:
        """Get or set the Scale factor for Z direction use for MPP decomposition of particle domain.
        """ # nopep8
        return self._cards[0].get_value("sz")

    @sz.setter
    def sz(self, value: float) -> None:
        self._cards[0].set_value("sz", value)

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Optional Airbag ID.
        """ # nopep8
        return self._cards[1].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[1].set_value("id", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Airbag id descriptor. It is suggested that unique descriptions be used.
        """ # nopep8
        return self._cards[1].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].set_value("title", value)

    @property
    def sid1(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID defining the complete airbag.
        """ # nopep8
        return self._cards[2].get_value("sid1")

    @sid1.setter
    def sid1(self, value: int) -> None:
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
        if value not in [0, 1]:
            raise Exception("""stype1 must be one of {0,1}""")
        self._cards[2].set_value("stype1", value)

    @property
    def sid2(self) -> int:
        """Get or set the Part or part set ID defining internal parts of the airbag.
        """ # nopep8
        return self._cards[2].get_value("sid2")

    @sid2.setter
    def sid2(self, value: int) -> None:
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
        if value not in [0, 1, 2]:
            raise Exception("""stype2 must be one of {0,1,2}""")
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
        self._cards[2].set_value("block", value)

    @property
    def npdata(self) -> int:
        """Get or set the Number of parts or part sets data.
        """ # nopep8
        return self._cards[2].get_value("npdata")

    @npdata.setter
    def npdata(self, value: int) -> None:
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
        if value not in [0, 1]:
            raise Exception("""irdp must be one of {0,1}""")
        self._cards[2].set_value("irdp", value)

    @property
    def np(self) -> int:
        """Get or set the Number of particles (Default 200,000).
        """ # nopep8
        return self._cards[3].get_value("np")

    @np.setter
    def np(self, value: int) -> None:
        self._cards[3].set_value("np", value)

    @property
    def unit(self) -> int:
        """Get or set the Unit system
        EQ.0: kg-mm-ms-K
        EQ.1: SI-units
        EQ.2: tonne-mm-s-K.
        EQ.3:	User defined units (see Remark 11)
        """ # nopep8
        return self._cards[3].get_value("unit")

    @unit.setter
    def unit(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""unit must be one of {0,1,2,3}""")
        self._cards[3].set_value("unit", value)

    @property
    def visflg(self) -> int:
        """Get or set the Visible particles(only support CPM database, see remark 6)
        EQ.0: Default to 1
        EQ.1: Output particle's coordinates, velocities, mass, radius, spin energy,
        translational energy
        EQ.2: Output reduce data set with corrdinates only
        EQ.3: Supress CPM database.
        """ # nopep8
        return self._cards[3].get_value("visflg")

    @visflg.setter
    def visflg(self, value: int) -> None:
        if value not in [1, 0, 2, 3]:
            raise Exception("""visflg must be one of {1,0,2,3}""")
        self._cards[3].set_value("visflg", value)

    @property
    def tatm(self) -> float:
        """Get or set the Atmospheric temperature (Default 293K).
        """ # nopep8
        return self._cards[3].get_value("tatm")

    @tatm.setter
    def tatm(self, value: float) -> None:
        self._cards[3].set_value("tatm", value)

    @property
    def patm(self) -> float:
        """Get or set the Atmospheric pressure (Default 1ATM).
        """ # nopep8
        return self._cards[3].get_value("patm")

    @patm.setter
    def patm(self, value: float) -> None:
        self._cards[3].set_value("patm", value)

    @property
    def nvent(self) -> int:
        """Get or set the Number of vent hole parts or part sets.
        """ # nopep8
        return self._cards[3].get_value("nvent")

    @nvent.setter
    def nvent(self, value: int) -> None:
        self._cards[3].set_value("nvent", value)

    @property
    def tend(self) -> float:
        """Get or set the Time when all particles (NP) have entered bag (Default 1.0e10).
        """ # nopep8
        return self._cards[3].get_value("tend")

    @tend.setter
    def tend(self, value: float) -> None:
        self._cards[3].set_value("tend", value)

    @property
    def tsw(self) -> float:
        """Get or set the Time for switch to control volume calculation (Default 1.0e10).
        """ # nopep8
        return self._cards[3].get_value("tsw")

    @tsw.setter
    def tsw(self, value: float) -> None:
        self._cards[3].set_value("tsw", value)

    @property
    def tstop(self) -> float:
        """Get or set the Time at which front tracking switches from IAIR = 4 to IAIR = 2.
        """ # nopep8
        return self._cards[4].get_value("tstop")

    @tstop.setter
    def tstop(self, value: float) -> None:
        self._cards[4].set_value("tstop", value)

    @property
    def tsmth(self) -> float:
        """Get or set the To avoid sudden jumps in the pressure signal during switching,
        the front tracking is tapered during a transition period.
        The default time of 1.0 millisecond will be applied if this value is set to zero
        """ # nopep8
        return self._cards[4].get_value("tsmth")

    @tsmth.setter
    def tsmth(self, value: float) -> None:
        self._cards[4].set_value("tsmth", value)

    @property
    def occup(self) -> float:
        """Get or set the Particles occupy OCCUP percent of the airbag’s volume.  The default value of OCCUP is 10%.
        This field can be used to balance computational cost and signal quality.  OCCUP ranges from 0.001 to 0.1..
        """ # nopep8
        return self._cards[4].get_value("occup")

    @occup.setter
    def occup(self, value: float) -> None:
        self._cards[4].set_value("occup", value)

    @property
    def rebl(self) -> int:
        """Get or set the If the option is ON, all energy stored from damping will be evenly distributed as vibrational energy to all particles.
        This improves the pressure calculation in certain applications.
        EQ.0:	Off (Default)
        EQ.1:	On.
        """ # nopep8
        return self._cards[4].get_value("rebl")

    @rebl.setter
    def rebl(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""rebl must be one of {0,1}""")
        self._cards[4].set_value("rebl", value)

    @property
    def sidsv(self) -> typing.Optional[int]:
        """Get or set the Part set ID for internal shell part.  The volume formed by this internal shell part will be excluded from the bag volume.  These internal parts must have consistent orientation to get correct excluded volume.
        """ # nopep8
        return self._cards[4].get_value("sidsv")

    @sidsv.setter
    def sidsv(self, value: int) -> None:
        self._cards[4].set_value("sidsv", value)

    @property
    def psid1(self) -> typing.Optional[int]:
        """Get or set the Part set ID for external parts which have normal pointed outward.  This option is usually used with airbag integrity check while there are two CPM bags connected with bag interaction.  Therefore, one of the bag can have the correct shell orientation but the share parts for the second bag will have wrong orientation.  This option will automatically flip the parts defined in this set in the second bag during integrity checking.
        """ # nopep8
        return self._cards[4].get_value("psid1")

    @psid1.setter
    def psid1(self, value: int) -> None:
        self._cards[4].set_value("psid1", value)

    @property
    def tsplit(self) -> typing.Optional[float]:
        """Get or set the Start time to activate particle splitting algorithm. See Remark 15.
        """ # nopep8
        return self._cards[4].get_value("tsplit")

    @tsplit.setter
    def tsplit(self, value: float) -> None:
        self._cards[4].set_value("tsplit", value)

    @property
    def sffdc(self) -> float:
        """Get or set the Scale factor for the force decay constant.  SFFDC has a range of . The default value is 1.0.  The value given here will replaced the values from *CONTROL_CPM
        """ # nopep8
        return self._cards[4].get_value("sffdc")

    @sffdc.setter
    def sffdc(self, value: float) -> None:
        self._cards[4].set_value("sffdc", value)

    @property
    def sfiair4(self) -> float:
        """Get or set the Scale factor for the ratio of initial air particles to inflator gas particles for IAIR = 4.
        Smaller values weaken the effect of gas front tracking.
        """ # nopep8
        return self._cards[5].get_value("sfiair4")

    @sfiair4.setter
    def sfiair4(self, value: float) -> None:
        self._cards[5].set_value("sfiair4", value)

    @property
    def idfric(self) -> int:
        """Get or set the Direction of P2F impact force:
        EQ.0:	No change(default)
        EQ.1 : The force is applied in the segment normal direction
        """ # nopep8
        return self._cards[5].get_value("idfric")

    @idfric.setter
    def idfric(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""idfric must be one of {0,1}""")
        self._cards[5].set_value("idfric", value)

    @property
    def mass(self) -> typing.Optional[float]:
        """Get or set the Conversion factor from current unit to MKS unit.  For example, if the current unit is using kg-mm-ms, the input should be 1.0, 0.001, 0.001.
        """ # nopep8
        return self._cards[6].get_value("mass")

    @mass.setter
    def mass(self, value: float) -> None:
        self._cards[6].set_value("mass", value)

    @property
    def time(self) -> typing.Optional[float]:
        """Get or set the Conversion factor from current unit to MKS unit.  For example, if the current unit is using kg-mm-ms, the input should be 1.0, 0.001, 0.001.
        """ # nopep8
        return self._cards[6].get_value("time")

    @time.setter
    def time(self, value: float) -> None:
        self._cards[6].set_value("time", value)

    @property
    def length(self) -> typing.Optional[float]:
        """Get or set the Conversion factor from current unit to MKS unit.  For example, if the current unit is using kg-mm-ms, the input should be 1.0, 0.001, 0.001.
        """ # nopep8
        return self._cards[6].get_value("length")

    @length.setter
    def length(self, value: float) -> None:
        self._cards[6].set_value("length", value)

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
        return self._cards[7].get_value("iair")

    @iair.setter
    def iair(self, value: int) -> None:
        self._cards[7].set_value("iair", value)

    @property
    def ngas(self) -> typing.Optional[int]:
        """Get or set the Number of gas components.
        """ # nopep8
        return self._cards[7].get_value("ngas")

    @ngas.setter
    def ngas(self, value: int) -> None:
        self._cards[7].set_value("ngas", value)

    @property
    def norif(self) -> typing.Optional[int]:
        """Get or set the Number of orifices.
        """ # nopep8
        return self._cards[7].get_value("norif")

    @norif.setter
    def norif(self, value: int) -> None:
        self._cards[7].set_value("norif", value)

    @property
    def nid1(self) -> int:
        """Get or set the NID1-NID3, Three nodes defining a moving coordinate system for the direction of flow through the gas inlet nozzles (Default fixed system).
        """ # nopep8
        return self._cards[7].get_value("nid1")

    @nid1.setter
    def nid1(self, value: int) -> None:
        self._cards[7].set_value("nid1", value)

    @property
    def nid2(self) -> int:
        """Get or set the NID1-NID3, Three nodes defining a moving coordinate system for the direction of flow through the gas inlet nozzles (Default fixed system).
        """ # nopep8
        return self._cards[7].get_value("nid2")

    @nid2.setter
    def nid2(self, value: int) -> None:
        self._cards[7].set_value("nid2", value)

    @property
    def nid3(self) -> int:
        """Get or set the NID1-NID3, Three nodes defining a moving coordinate system for the direction of flow through the gas inlet nozzles (Default fixed system).
        """ # nopep8
        return self._cards[7].get_value("nid3")

    @nid3.setter
    def nid3(self, value: int) -> None:
        self._cards[7].set_value("nid3", value)

    @property
    def chm(self) -> int:
        """Get or set the Chamber ID used in *DEFINE_CPM_CHAMBER.
        """ # nopep8
        return self._cards[7].get_value("chm")

    @chm.setter
    def chm(self, value: int) -> None:
        self._cards[7].set_value("chm", value)

    @property
    def cd_ext(self) -> float:
        """Get or set the Drag coefficient for external air. If the value is not zero, the inertial effect
        from external air will be considered and forces will be applied in the normal
        direction on the exterior airbag surface.
        """ # nopep8
        return self._cards[7].get_value("cd_ext")

    @cd_ext.setter
    def cd_ext(self, value: float) -> None:
        self._cards[7].set_value("cd_ext", value)

    @property
    def sidup(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID defining the internal parts that pressure will be applied to.
        This internal structure acts as a valve to control the external vent hole area.
        Pressure will be applied only after switch to UP (uniform pressure) using TSW.
        """ # nopep8
        return self._cards[8].get_value("sidup")

    @sidup.setter
    def sidup(self, value: int) -> None:
        self._cards[8].set_value("sidup", value)

    @property
    def styup(self) -> int:
        """Get or set the Set defining internal parts will be applied pressure
        Set type EQ.0: Part
        EQ.1: Part set.
        """ # nopep8
        return self._cards[8].get_value("styup")

    @styup.setter
    def styup(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""styup must be one of {0,1}""")
        self._cards[8].set_value("styup", value)

    @property
    def pfrac(self) -> float:
        """Get or set the Part or part set ID defining the internal parts that pressure will be applied to.
        This internal structure acts as a valve to control the external vent hole area.
        Pressure will be applied only after switch to UP (uniform pressure) using TSW.
        """ # nopep8
        return self._cards[8].get_value("pfrac")

    @pfrac.setter
    def pfrac(self, value: float) -> None:
        self._cards[8].set_value("pfrac", value)

    @property
    def linking(self) -> typing.Optional[int]:
        """Get or set the Part ID of an internal part that is coupled to the external vent definition.
        The minimum area of this part or the vent hole will be used for actual venting area.
        """ # nopep8
        return self._cards[8].get_value("linking")

    @linking.setter
    def linking(self, value: int) -> None:
        self._cards[8].set_value("linking", value)

    @property
    def sidh(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID defining part data.
        """ # nopep8
        return self._cards[9].get_value("sidh")

    @sidh.setter
    def sidh(self, value: int) -> None:
        self._cards[9].set_value("sidh", value)

    @property
    def stypeh(self) -> int:
        """Get or set the Set type EQ.0: Part
        EQ.1: Part set.
        EQ.2: part and HCONV is the *DEFINE_CPM_NPDATA ID
        EQ.3: part set and HCONV is the * DEFINE_CPM_NPDATA ID
        """ # nopep8
        return self._cards[9].get_value("stypeh")

    @stypeh.setter
    def stypeh(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""stypeh must be one of {0,1,2,3}""")
        self._cards[9].set_value("stypeh", value)

    @property
    def hconv(self) -> typing.Optional[float]:
        """Get or set the Heat convection coefficient used to calculate heat loss from the airbag external surface to ambient (W/K/m2).
        See *AIRBAG_HYBRID developments (Resp. P.O. Marklund).
        LT.0:	|HCONV | is a load curve ID defines heat convection coefficient as a function of time.
        When STYPEH is greater than 1, HCONV is an integer of *DEFINE_CPM_NPDATA ID.
        """ # nopep8
        return self._cards[9].get_value("hconv")

    @hconv.setter
    def hconv(self, value: float) -> None:
        self._cards[9].set_value("hconv", value)

    @property
    def pfric(self) -> float:
        """Get or set the Friction factor.
        """ # nopep8
        return self._cards[9].get_value("pfric")

    @pfric.setter
    def pfric(self, value: float) -> None:
        self._cards[9].set_value("pfric", value)

    @property
    def sdfblk(self) -> float:
        """Get or set the Scale down factor for blockage factor (Default=1, no scale down). The val-id factor will be (0,1]. If 0, it will set to 1.
        """ # nopep8
        return self._cards[9].get_value("sdfblk")

    @sdfblk.setter
    def sdfblk(self, value: float) -> None:
        self._cards[9].set_value("sdfblk", value)

    @property
    def kp(self) -> float:
        """Get or set the Thermal conductivity of the part.
        """ # nopep8
        return self._cards[9].get_value("kp")

    @kp.setter
    def kp(self, value: float) -> None:
        self._cards[9].set_value("kp", value)

    @property
    def inip(self) -> int:
        """Get or set the Place initial air particles on surface.
        EQ.0:	yes (default)
        EQ.1:	no
        This feature exclude surfaces from initial particle placement.  This option is useful for preventing particles from being trapped between adjacent fabric layers..
        """ # nopep8
        return self._cards[9].get_value("inip")

    @inip.setter
    def inip(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""inip must be one of {0,1}""")
        self._cards[9].set_value("inip", value)

    @property
    def cp(self) -> typing.Optional[float]:
        """Get or set the Specific heat (see Remark 16).
        """ # nopep8
        return self._cards[9].get_value("cp")

    @cp.setter
    def cp(self, value: float) -> None:
        self._cards[9].set_value("cp", value)

    @property
    def sid3(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID defining vent holes.
        """ # nopep8
        return self._cards[10].get_value("sid3")

    @sid3.setter
    def sid3(self, value: int) -> None:
        self._cards[10].set_value("sid3", value)

    @property
    def stype3(self) -> int:
        """Get or set the Set type:
        EQ.0: Part
        EQ.1: Part set which each part being treated separately.
        EQ.2:	Part set and all parts are treated as one vent.  See Remark 13
        """ # nopep8
        return self._cards[10].get_value("stype3")

    @stype3.setter
    def stype3(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""stype3 must be one of {0,1,2}""")
        self._cards[10].set_value("stype3", value)

    @property
    def c23(self) -> float:
        """Get or set the GE.0:	Vent hole coefficient, a parameter of Wang-Nefske leakage.  A value between 0.0 and 1.0 can be input.  See Remark 1.
        LT.0:	ID for *DEFINE_CPM_VENT.
        """ # nopep8
        return self._cards[10].get_value("c23")

    @c23.setter
    def c23(self, value: float) -> None:
        self._cards[10].set_value("c23", value)

    @property
    def lctc23(self) -> typing.Optional[int]:
        """Get or set the Load curve defining vent hole coefficient as a function of time.  LCTC23 can be defined through *DEFINE_CURVE_FUNCTION.  If omitted, a curve equal to 1.0 used.
        """ # nopep8
        return self._cards[10].get_value("lctc23")

    @lctc23.setter
    def lctc23(self, value: int) -> None:
        self._cards[10].set_value("lctc23", value)

    @property
    def lcpc23(self) -> typing.Optional[int]:
        """Get or set the Load curve defining vent hole coefficient as a function of pressure.  If omitted a curve equal to 1.0 is used..
        """ # nopep8
        return self._cards[10].get_value("lcpc23")

    @lcpc23.setter
    def lcpc23(self, value: int) -> None:
        self._cards[10].set_value("lcpc23", value)

    @property
    def enh_v(self) -> int:
        """Get or set the Enhanced venting option. See Remark 8.
        EQ.0:	Off (default)
        EQ.1:	On
        EQ.2:	Two way flow for internal vent; treated as hole for external vent .
        """ # nopep8
        return self._cards[10].get_value("enh_v")

    @enh_v.setter
    def enh_v(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""enh_v must be one of {0,1,2}""")
        self._cards[10].set_value("enh_v", value)

    @property
    def ppop(self) -> float:
        """Get or set the Pressure difference between interior and ambient pressure (PATM) to open the vent holes.  Once the vents are open, they will stay open.
        """ # nopep8
        return self._cards[10].get_value("ppop")

    @ppop.setter
    def ppop(self, value: float) -> None:
        self._cards[10].set_value("ppop", value)

