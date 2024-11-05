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
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DefineCpmVent(KeywordBase):
    """DYNA DEFINE_CPM_VENT keyword"""

    keyword = "DEFINE"
    subkeyword = "CPM_VENT"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
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
                        "c23",
                        float,
                        10,
                        10,
                        kwargs.get("c23", 1.0)
                    ),
                    Field(
                        "lctc23",
                        int,
                        20,
                        10,
                        kwargs.get("lctc23")
                    ),
                    Field(
                        "lcpc23",
                        int,
                        30,
                        10,
                        kwargs.get("lcpc23")
                    ),
                    Field(
                        "enh_v",
                        int,
                        40,
                        10,
                        kwargs.get("enh_v", 0)
                    ),
                    Field(
                        "ppop",
                        float,
                        50,
                        10,
                        kwargs.get("ppop")
                    ),
                    Field(
                        "c23up",
                        float,
                        60,
                        10,
                        kwargs.get("c23up")
                    ),
                    Field(
                        "iopt",
                        int,
                        70,
                        10,
                        kwargs.get("iopt")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "jt",
                        int,
                        0,
                        10,
                        kwargs.get("jt", 0)
                    ),
                    Field(
                        "ids1",
                        int,
                        10,
                        10,
                        kwargs.get("ids1")
                    ),
                    Field(
                        "ids2",
                        int,
                        20,
                        10,
                        kwargs.get("ids2")
                    ),
                    Field(
                        "iopt1",
                        int,
                        30,
                        10,
                        kwargs.get("iopt1")
                    ),
                    Field(
                        "pid1",
                        int,
                        40,
                        10,
                        kwargs.get("pid1")
                    ),
                    Field(
                        "pid2",
                        int,
                        50,
                        10,
                        kwargs.get("pid2")
                    ),
                    Field(
                        "vang",
                        float,
                        60,
                        10,
                        kwargs.get("vang", 0.0)
                    ),
                    Field(
                        "lcred",
                        int,
                        70,
                        10,
                        kwargs.get("lcred")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nid1",
                        int,
                        0,
                        10,
                        kwargs.get("nid1")
                    ),
                    Field(
                        "nid2",
                        int,
                        10,
                        10,
                        kwargs.get("nid2")
                    ),
                    Field(
                        "nid2",
                        int,
                        20,
                        10,
                        kwargs.get("nid2")
                    ),
                    Field(
                        "lcac23",
                        int,
                        30,
                        10,
                        kwargs.get("lcac23")
                    ),
                    Field(
                        "psetpv",
                        int,
                        40,
                        10,
                        kwargs.get("psetpv")
                    ),
                    Field(
                        "sfpv",
                        int,
                        50,
                        10,
                        kwargs.get("sfpv")
                    ),
                    Field(
                        "lpatm",
                        int,
                        60,
                        10,
                        kwargs.get("lpatm")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "jtnd",
                        int,
                        0,
                        10,
                        kwargs.get("jtnd")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineCpmVent.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Unique ID for this card
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def c23(self) -> float:
        """Get or set the Vent hole coefficient (Parameter for Wang-Nefske leakage) (Default 1.0) See Remark 1 below.
        """ # nopep8
        return self._cards[0].get_value("c23")

    @c23.setter
    def c23(self, value: float) -> None:
        self._cards[0].set_value("c23", value)

    @property
    def lctc23(self) -> typing.Optional[int]:
        """Get or set the Load curve defining vent hole coefficient as a function of time.
        """ # nopep8
        return self._cards[0].get_value("lctc23")

    @lctc23.setter
    def lctc23(self, value: int) -> None:
        self._cards[0].set_value("lctc23", value)

    @property
    def lcpc23(self) -> typing.Optional[int]:
        """Get or set the Load curve defining vent hole coefficient as a function of pressure.
        """ # nopep8
        return self._cards[0].get_value("lcpc23")

    @lcpc23.setter
    def lcpc23(self, value: int) -> None:
        self._cards[0].set_value("lcpc23", value)

    @property
    def enh_v(self) -> int:
        """Get or set the Enhance venting option. (Default 0)  However if Joule-Thomson effect is considered, the option will set to 1 automatically.
        EQ.0: Disable
        EQ.1: Enable
        """ # nopep8
        return self._cards[0].get_value("enh_v")

    @enh_v.setter
    def enh_v(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""enh_v must be one of {0,1}""")
        self._cards[0].set_value("enh_v", value)

    @property
    def ppop(self) -> typing.Optional[float]:
        """Get or set the Pressure difference between interior and ambient pressure to open the vent hole.Once the vent is open it will stay open.
        """ # nopep8
        return self._cards[0].get_value("ppop")

    @ppop.setter
    def ppop(self, value: float) -> None:
        self._cards[0].set_value("ppop", value)

    @property
    def c23up(self) -> typing.Optional[float]:
        """Get or set the Scale factor of C23 while switching from CPM to uniform pressure calculation
        """ # nopep8
        return self._cards[0].get_value("c23up")

    @c23up.setter
    def c23up(self, value: float) -> None:
        self._cards[0].set_value("c23up", value)

    @property
    def iopt(self) -> typing.Optional[int]:
        """Get or set the Directional venting:
        EQ.1:	In shell normal
        EQ.2:	Against shell normal
        One-way venting:
        EQ.10:	In shell normal
        EQ.20:	Against shell normal
        Special vent option::
        EQ.100:	Enable compression seal vent. Vent area is adjusted according to the formula below. See Remark 1.A_
        EQ.200:	Enable push-out vent. Particle remains active while going through this external vent within the range of 2 times of its characteristic length
        """ # nopep8
        return self._cards[0].get_value("iopt")

    @iopt.setter
    def iopt(self, value: int) -> None:
        self._cards[0].set_value("iopt", value)

    @property
    def jt(self) -> int:
        """Get or set the Joule-Thomson effect, If Joule-Thomson effect is considered, ENH_V will set to enable.
        EQ.0: Disable
        EQ.1: Use part pressure
        EQ.2: Use chamber pressure
        """ # nopep8
        return self._cards[1].get_value("jt")

    @jt.setter
    def jt(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""jt must be one of {0,1,2}""")
        self._cards[1].set_value("jt", value)

    @property
    def ids1(self) -> typing.Optional[int]:
        """Get or set the JT's up stream condition part ID/chamber ID
        """ # nopep8
        return self._cards[1].get_value("ids1")

    @ids1.setter
    def ids1(self, value: int) -> None:
        self._cards[1].set_value("ids1", value)

    @property
    def ids2(self) -> typing.Optional[int]:
        """Get or set the JT's downstream condition part ID/chamber ID
        EQ.0: airbag's PATM will be used for downstream pressure
        """ # nopep8
        return self._cards[1].get_value("ids2")

    @ids2.setter
    def ids2(self, value: int) -> None:
        self._cards[1].set_value("ids2", value)

    @property
    def iopt1(self) -> typing.Optional[int]:
        """Get or set the Upstream chamber ID for one-way vent hole.  This will help the code to determine the probability function.
        """ # nopep8
        return self._cards[1].get_value("iopt1")

    @iopt1.setter
    def iopt1(self, value: int) -> None:
        self._cards[1].set_value("iopt1", value)

    @property
    def pid1(self) -> typing.Optional[int]:
        """Get or set the PID1 and PID2 indicate parts for determining the local part pressures that can be used to evaluate the vent probability function. Depending on if a chamber is defined, how the local part pressures are evaluated changes (see *DEFINE_CPM_CHAMBER). PID1 and PID2 are optional if a chamber is defined, otherwise they are required input.
        When a chamber is defined, specifying PID1and PID2 causes the vent probability function to be evaluated from the difference of local part pressures between PID1and PID2.Otherwise the calculation involves the chamber pressure.This option is usually used for vents near a long sleeve which causes unrealistic venting using chamber pressure alone.
        When a chamber is not defined, the vent probability function is evaluated from the difference of local part pressures between PID1and PID2, using the location of the part centers to help determine vent direction..If the part is an external part, the part pressure will be used.If the part is an internal part, the pressure on the shell’s positive normal side will be used.If the vent is an external vent, PID1 should be the same as PID2 to avoid input error.
        """ # nopep8
        return self._cards[1].get_value("pid1")

    @pid1.setter
    def pid1(self, value: int) -> None:
        self._cards[1].set_value("pid1", value)

    @property
    def pid2(self) -> typing.Optional[int]:
        """Get or set the PID1 and PID2 indicate parts for determining the local part pressures that can be used to evaluate the vent probability function. Depending on if a chamber is defined, how the local part pressures are evaluated changes (see *DEFINE_CPM_CHAMBER). PID1 and PID2 are optional if a chamber is defined, otherwise they are required input.
        When a chamber is defined, specifying PID1and PID2 causes the vent probability function to be evaluated from the difference of local part pressures between PID1and PID2.Otherwise the calculation involves the chamber pressure.This option is usually used for vents near a long sleeve which causes unrealistic venting using chamber pressure alone.
        When a chamber is not defined, the vent probability function is evaluated from the difference of local part pressures between PID1and PID2, using the location of the part centers to help determine vent direction..If the part is an external part, the part pressure will be used.If the part is an internal part, the pressure on the shell’s positive normal side will be used.If the vent is an external vent, PID1 should be the same as PID2 to avoid input error.
        """ # nopep8
        return self._cards[1].get_value("pid2")

    @pid2.setter
    def pid2(self, value: int) -> None:
        self._cards[1].set_value("pid2", value)

    @property
    def vang(self) -> float:
        """Get or set the Cone angle in degrees. Particle goes through this vent will be redirection based on this angle.  This option is only valid with internal vent.
        GT.0:	cone angle (maximum 270)
        EQ.0: disabled (Default)
        EQ.-1: direction follows the vent normal
        EQ.-2: direction follows local coordinates system defined by the following three nodes
        """ # nopep8
        return self._cards[1].get_value("vang")

    @vang.setter
    def vang(self, value: float) -> None:
        self._cards[1].set_value("vang", value)

    @property
    def lcred(self) -> typing.Optional[int]:
        """Get or set the Time dependent probability curve to control CPM particle through the internal vent with VANG option.
        """ # nopep8
        return self._cards[1].get_value("lcred")

    @lcred.setter
    def lcred(self, value: int) -> None:
        self._cards[1].set_value("lcred", value)

    @property
    def nid1(self) -> typing.Optional[int]:
        """Get or set the Three nodes define a moving coordinate system for the direction of flow through the vent when VANG equals -2
        """ # nopep8
        return self._cards[2].get_value("nid1")

    @nid1.setter
    def nid1(self, value: int) -> None:
        self._cards[2].set_value("nid1", value)

    @property
    def nid2(self) -> typing.Optional[int]:
        """Get or set the Three nodes define a moving coordinate system for the direction of flow through the vent when VANG equals -2
        """ # nopep8
        return self._cards[2].get_value("nid2")

    @nid2.setter
    def nid2(self, value: int) -> None:
        self._cards[2].set_value("nid2", value)

    @property
    def nid2(self) -> typing.Optional[int]:
        """Get or set the Three nodes define a moving coordinate system for the direction of flow through the vent when VANG equals -2
        """ # nopep8
        return self._cards[2].get_value("nid2")

    @nid2.setter
    def nid2(self, value: int) -> None:
        self._cards[2].set_value("nid2", value)

    @property
    def lcac23(self) -> typing.Optional[int]:
        """Get or set the Load curve defining vent hole coefficient as a function of current vent area.
        """ # nopep8
        return self._cards[2].get_value("lcac23")

    @lcac23.setter
    def lcac23(self, value: int) -> None:
        self._cards[2].set_value("lcac23", value)

    @property
    def psetpv(self) -> typing.Optional[int]:
        """Get or set the |PSETPV | is a part set ID for internal airbag parts that interact with the push-out vent (IOPT = 200). The sign determines where the ambient pressure is applied:
        GT.0:	Ambient pressure is applied to elements in these parts that are at least a distance of SFPV×CL  away from the vent.
        LT.0 : Ambient pressure is applied to elements in these parts that are within a distance of SFPV×CL  of the vent.
        """ # nopep8
        return self._cards[2].get_value("psetpv")

    @psetpv.setter
    def psetpv(self, value: int) -> None:
        self._cards[2].set_value("psetpv", value)

    @property
    def sfpv(self) -> typing.Optional[int]:
        """Get or set the Scale factor for the characteristic length of the element.  CL is defined as sqrt(element area).
        """ # nopep8
        return self._cards[2].get_value("sfpv")

    @sfpv.setter
    def sfpv(self, value: int) -> None:
        self._cards[2].set_value("sfpv", value)

    @property
    def lpatm(self) -> typing.Optional[int]:
        """Get or set the Load curve for ambient pressure of the external vent.  This option only works for the CPM mode.
        """ # nopep8
        return self._cards[2].get_value("lpatm")

    @lpatm.setter
    def lpatm(self, value: int) -> None:
        self._cards[2].set_value("lpatm", value)

    @property
    def jtnd(self) -> typing.Optional[int]:
        """Get or set the Node/Node Set for applying vent reaction force
        GT.0:	Node ID
        LT.0 : Node Set ID.The average force is evenly applied among the nodes in the node set.
        """ # nopep8
        return self._cards[3].get_value("jtnd")

    @jtnd.setter
    def jtnd(self, value: int) -> None:
        self._cards[3].set_value("jtnd", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[4].cards[0].set_value("title", value)

