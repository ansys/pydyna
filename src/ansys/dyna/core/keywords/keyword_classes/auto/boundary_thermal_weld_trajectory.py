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

class BoundaryThermalWeldTrajectory(KeywordBase):
    """DYNA BOUNDARY_THERMAL_WELD_TRAJECTORY keyword"""

    keyword = "BOUNDARY"
    subkeyword = "THERMAL_WELD_TRAJECTORY"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "ptyp",
                        int,
                        10,
                        10,
                        kwargs.get("ptyp", 1)
                    ),
                    Field(
                        "nsid1",
                        int,
                        20,
                        10,
                        kwargs.get("nsid1", 0)
                    ),
                    Field(
                        "spd1",
                        float,
                        30,
                        10,
                        kwargs.get("spd1")
                    ),
                    Field(
                        "nsid2",
                        int,
                        40,
                        10,
                        kwargs.get("nsid2")
                    ),
                    Field(
                        "spd2",
                        float,
                        50,
                        10,
                        kwargs.get("spd2")
                    ),
                    Field(
                        "ncyc",
                        int,
                        60,
                        10,
                        kwargs.get("ncyc", 1)
                    ),
                    Field(
                        "relvel",
                        int,
                        70,
                        10,
                        kwargs.get("relvel", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "iform",
                        int,
                        0,
                        10,
                        kwargs.get("iform", 1)
                    ),
                    Field(
                        "lcid",
                        int,
                        10,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "q",
                        float,
                        20,
                        10,
                        kwargs.get("q")
                    ),
                    Field(
                        "lcrot",
                        int,
                        30,
                        10,
                        kwargs.get("lcrot")
                    ),
                    Field(
                        "lcmov",
                        int,
                        40,
                        10,
                        kwargs.get("lcmov")
                    ),
                    Field(
                        "lclat",
                        int,
                        50,
                        10,
                        kwargs.get("lclat")
                    ),
                    Field(
                        "disc",
                        float,
                        60,
                        10,
                        kwargs.get("disc")
                    ),
                    Field(
                        "enfor",
                        int,
                        70,
                        10,
                        kwargs.get("enfor")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "p1",
                        float,
                        0,
                        10,
                        kwargs.get("p1")
                    ),
                    Field(
                        "p2",
                        float,
                        10,
                        10,
                        kwargs.get("p2")
                    ),
                    Field(
                        "p3",
                        float,
                        20,
                        10,
                        kwargs.get("p3")
                    ),
                    Field(
                        "p4",
                        float,
                        30,
                        10,
                        kwargs.get("p4")
                    ),
                    Field(
                        "p5",
                        float,
                        40,
                        10,
                        kwargs.get("p5")
                    ),
                    Field(
                        "p6",
                        float,
                        50,
                        10,
                        kwargs.get("p6")
                    ),
                    Field(
                        "p7",
                        float,
                        60,
                        10,
                        kwargs.get("p7")
                    ),
                    Field(
                        "p8",
                        float,
                        70,
                        10,
                        kwargs.get("p8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "tx",
                        float,
                        0,
                        10,
                        kwargs.get("tx")
                    ),
                    Field(
                        "ty",
                        float,
                        10,
                        10,
                        kwargs.get("ty")
                    ),
                    Field(
                        "tz",
                        float,
                        20,
                        10,
                        kwargs.get("tz")
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID or Part Set ID of solids or shells to which weld source is applied.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def ptyp(self) -> int:
        """Get or set the PID type:
        EQ.1: PID defines a single part ID (default),
        EQ.2: PID defines a part set ID.
        """ # nopep8
        return self._cards[0].get_value("ptyp")

    @ptyp.setter
    def ptyp(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""ptyp must be one of {1,2}""")
        self._cards[0].set_value("ptyp", value)

    @property
    def nsid1(self) -> int:
        """Get or set the Node set ID containing the path (weld trajectory) information for the weld source movement.  A sorted node set is requested. The order defines the weld path and the direction (see Remark 1).
        """ # nopep8
        return self._cards[0].get_value("nsid1")

    @nsid1.setter
    def nsid1(self, value: int) -> None:
        self._cards[0].set_value("nsid1", value)

    @property
    def spd1(self) -> typing.Optional[float]:
        """Get or set the Speed of the heat source on the weld trajectory
        GT.0.0:	constant speed
        LT.0.0 : is a load curve ID defining weld speed as a function of  time.
        """ # nopep8
        return self._cards[0].get_value("spd1")

    @spd1.setter
    def spd1(self, value: float) -> None:
        self._cards[0].set_value("spd1", value)

    @property
    def nsid2(self) -> typing.Optional[int]:
        """Get or set the ID of second node set or segment setcontaining information for the weld source aiming direction (see Remark 2)
        GT.0:	SID2 refers to a sorted node set, the order of which defines the direction of the trajectory. The heat source is aimed from current position in SID2to current position in the weld trajectory.
        EQ.0:	beam aiming direction is (tx, ty, tz) input on optional card4.
        LT.0: 	|SID2| is a segment set. The heat source is aiming in normal direction to segments in the set.
        """ # nopep8
        return self._cards[0].get_value("nsid2")

    @nsid2.setter
    def nsid2(self, value: int) -> None:
        self._cards[0].set_value("nsid2", value)

    @property
    def spd2(self) -> typing.Optional[float]:
        """Get or set the Speed of reference point in NSID2 (ignored unless NSID2 > 0)
        GT.0:	constant speed
        LT.0 : is a load curve ID defining weld speed as a function of  time.
        """ # nopep8
        return self._cards[0].get_value("spd2")

    @spd2.setter
    def spd2(self, value: float) -> None:
        self._cards[0].set_value("spd2", value)

    @property
    def ncyc(self) -> int:
        """Get or set the Number of substeps for subcycling in evaluation of boundary condition. Allows thermal dumping (see Remark 3).
        """ # nopep8
        return self._cards[0].get_value("ncyc")

    @ncyc.setter
    def ncyc(self, value: int) -> None:
        self._cards[0].set_value("ncyc", value)

    @property
    def relvel(self) -> int:
        """Get or set the Defines if VEL1 and VEL2 are relative or absolute velocities in coupled simulations
        EQ.0:	absolute velocities
        EQ.1:	relative velocities with respect to underlying structure.
        """ # nopep8
        return self._cards[0].get_value("relvel")

    @relvel.setter
    def relvel(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""relvel must be one of {0,1}""")
        self._cards[0].set_value("relvel", value)

    @property
    def iform(self) -> int:
        """Get or set the Geometry description for energy rate density distribution (see Remark 4):
        EQ.1: Goldak-type heat source
        EQ.2: double ellipsoidal heat source with constant density
        EQ.3: double conical heat source with constant density
        EQ.4: frustum-shaped heat source with constant density.
        EQ.5: user-defined function
        """ # nopep8
        return self._cards[1].get_value("iform")

    @iform.setter
    def iform(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5]:
            raise Exception("""iform must be one of {1,2,3,4,5}""")
        self._cards[1].set_value("iform", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for weld energy input rate vs. time
        EQ.0: use constant multiplier value Q.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[1].set_value("lcid", value)

    @property
    def q(self) -> typing.Optional[float]:
        """Get or set the Curve multiplier for weld energy input rate [energy/time]
        LT.0:	take absolute value and accurate integration of heat using integration cells with edge length DISC
        """ # nopep8
        return self._cards[1].get_value("q")

    @q.setter
    def q(self, value: float) -> None:
        self._cards[1].set_value("q", value)

    @property
    def lcrot(self) -> typing.Optional[int]:
        """Get or set the Load curve defining the rotation (angle in degree) of weld source around the trajectory as function of time(see Remark 2).
        """ # nopep8
        return self._cards[1].get_value("lcrot")

    @lcrot.setter
    def lcrot(self, value: int) -> None:
        self._cards[1].set_value("lcrot", value)

    @property
    def lcmov(self) -> typing.Optional[int]:
        """Get or set the Load curve for offset of weld source in direction of the weld beam as function of time (see Remark 2).
        """ # nopep8
        return self._cards[1].get_value("lcmov")

    @lcmov.setter
    def lcmov(self, value: int) -> None:
        self._cards[1].set_value("lcmov", value)

    @property
    def lclat(self) -> typing.Optional[int]:
        """Get or set the Load curve for lateral offset of weld sourceas function of time (see Remark 2)
        """ # nopep8
        return self._cards[1].get_value("lclat")

    @lclat.setter
    def lclat(self, value: int) -> None:
        self._cards[1].set_value("lclat", value)

    @property
    def disc(self) -> typing.Optional[float]:
        """Get or set the Resolution for accurate integration, parameter defines edge length for integration cubes.  Default is 5% of weld pool depth.
        """ # nopep8
        return self._cards[1].get_value("disc")

    @disc.setter
    def disc(self, value: float) -> None:
        self._cards[1].set_value("disc", value)

    @property
    def enfor(self) -> typing.Optional[int]:
        """Get or set the Flag for heat input enforcement option.  If set, the nodal heat input is scaled
        such that the resulting heat inputs equals the user input as given by Q and LCID.
        """ # nopep8
        return self._cards[1].get_value("enfor")

    @enfor.setter
    def enfor(self, value: int) -> None:
        self._cards[1].set_value("enfor", value)

    @property
    def p1(self) -> typing.Optional[float]:
        """Get or set the Parameters defining for weld pool geometry, depending on parameter IFORM.See Remark 4 for details.
        """ # nopep8
        return self._cards[2].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        self._cards[2].set_value("p1", value)

    @property
    def p2(self) -> typing.Optional[float]:
        """Get or set the Parameters defining for weld pool geometry, depending on parameter IFORM.See Remark 4 for details.
        """ # nopep8
        return self._cards[2].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        self._cards[2].set_value("p2", value)

    @property
    def p3(self) -> typing.Optional[float]:
        """Get or set the Parameters defining for weld pool geometry, depending on parameter IFORM.See Remark 4 for details.
        """ # nopep8
        return self._cards[2].get_value("p3")

    @p3.setter
    def p3(self, value: float) -> None:
        self._cards[2].set_value("p3", value)

    @property
    def p4(self) -> typing.Optional[float]:
        """Get or set the Parameters defining for weld pool geometry, depending on parameter IFORM.See Remark 4 for details.
        """ # nopep8
        return self._cards[2].get_value("p4")

    @p4.setter
    def p4(self, value: float) -> None:
        self._cards[2].set_value("p4", value)

    @property
    def p5(self) -> typing.Optional[float]:
        """Get or set the Parameters defining for weld pool geometry, depending on parameter IFORM.See Remark 4 for details.
        """ # nopep8
        return self._cards[2].get_value("p5")

    @p5.setter
    def p5(self, value: float) -> None:
        self._cards[2].set_value("p5", value)

    @property
    def p6(self) -> typing.Optional[float]:
        """Get or set the Parameters defining for weld pool geometry, depending on parameter IFORM.See Remark 4 for details
        """ # nopep8
        return self._cards[2].get_value("p6")

    @p6.setter
    def p6(self, value: float) -> None:
        self._cards[2].set_value("p6", value)

    @property
    def p7(self) -> typing.Optional[float]:
        """Get or set the Parameters defining for weld pool geometry, depending on parameter IFORM.See Remark 4 for details.
        """ # nopep8
        return self._cards[2].get_value("p7")

    @p7.setter
    def p7(self, value: float) -> None:
        self._cards[2].set_value("p7", value)

    @property
    def p8(self) -> typing.Optional[float]:
        """Get or set the Parameters defining for weld pool geometry, depending on parameter IFORM.See Remark 4 for details.
        """ # nopep8
        return self._cards[2].get_value("p8")

    @p8.setter
    def p8(self, value: float) -> None:
        self._cards[2].set_value("p8", value)

    @property
    def tx(self) -> typing.Optional[float]:
        """Get or set the Weld beam direction vector in global coordinates (SID2 = 0 only).
        """ # nopep8
        return self._cards[3].get_value("tx")

    @tx.setter
    def tx(self, value: float) -> None:
        self._cards[3].set_value("tx", value)

    @property
    def ty(self) -> typing.Optional[float]:
        """Get or set the Weld beam direction vector in global coordinates (SID2 = 0 only).
        """ # nopep8
        return self._cards[3].get_value("ty")

    @ty.setter
    def ty(self, value: float) -> None:
        self._cards[3].set_value("ty", value)

    @property
    def tz(self) -> typing.Optional[float]:
        """Get or set the Weld beam direction vector in global coordinates (SID2 = 0 only).
        """ # nopep8
        return self._cards[3].get_value("tz")

    @tz.setter
    def tz(self, value: float) -> None:
        self._cards[3].set_value("tz", value)

