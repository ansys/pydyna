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

class BoundaryFluxTrajectory(KeywordBase):
    """DYNA BOUNDARY_FLUX_TRAJECTORY keyword"""

    keyword = "BOUNDARY"
    subkeyword = "FLUX_TRAJECTORY"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ssid",
                        int,
                        0,
                        10,
                        kwargs.get("ssid")
                    ),
                    Field(
                        "pserod",
                        int,
                        10,
                        10,
                        kwargs.get("pserod")
                    ),
                    Field(
                        "nsid1",
                        int,
                        20,
                        10,
                        kwargs.get("nsid1")
                    ),
                    Field(
                        "spd1",
                        float,
                        30,
                        10,
                        kwargs.get("spd1", 0.)
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
                        kwargs.get("spd2", 0.)
                    ),
                    Field(
                        "relvel",
                        int,
                        60,
                        10,
                        kwargs.get("relvel", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "erod",
                        int,
                        0,
                        10,
                        kwargs.get("erod", 0)
                    ),
                    Field(
                        "loc",
                        int,
                        10,
                        10,
                        kwargs.get("loc")
                    ),
                    Field(
                        "lcrot",
                        int,
                        20,
                        10,
                        kwargs.get("lcrot")
                    ),
                    Field(
                        "lclat",
                        int,
                        30,
                        10,
                        kwargs.get("lclat")
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
                        kwargs.get("iform")
                    ),
                    Field(
                        "lctim",
                        int,
                        10,
                        10,
                        kwargs.get("lctim")
                    ),
                    Field(
                        "q",
                        float,
                        20,
                        10,
                        kwargs.get("q", 0.)
                    ),
                    Field(
                        "lcinc",
                        int,
                        30,
                        10,
                        kwargs.get("lcinc")
                    ),
                    Field(
                        "enfor",
                        int,
                        40,
                        10,
                        kwargs.get("enfor", 0)
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
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID containing segments that are potentially heated
        by surface heat source (flux)..
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        self._cards[0].set_value("ssid", value)

    @property
    def pserod(self) -> typing.Optional[int]:
        """Get or set the Part set ID for updating boundary segments exposed to the environment as solid elements erode.
        """ # nopep8
        return self._cards[0].get_value("pserod")

    @pserod.setter
    def pserod(self, value: int) -> None:
        self._cards[0].set_value("pserod", value)

    @property
    def nsid1(self) -> typing.Optional[int]:
        """Get or set the Node set defining the path of the weld source. The source travels
        along the path at speed SPD1. The nodes are traversed according
        to their ordering in the node set. See Remark 1.
        """ # nopep8
        return self._cards[0].get_value("nsid1")

    @nsid1.setter
    def nsid1(self, value: int) -> None:
        self._cards[0].set_value("nsid1", value)

    @property
    def spd1(self) -> float:
        """Get or set the Speed of the heat source on the weld trajectory
        GT.0.0: constant speed
        LT.0.0: |SPD1| is a load curve ID defining weld speed as a function of time.
        """ # nopep8
        return self._cards[0].get_value("spd1")

    @spd1.setter
    def spd1(self, value: float) -> None:
        self._cards[0].set_value("spd1", value)

    @property
    def nsid2(self) -> typing.Optional[int]:
        """Get or set the Node or segment set containing information for the weld source aiming direction:
        GT.0: NSID2 together with SPD2 define a curve in the same
        way that NSID1 and SPD1 define a curve. Aiming direction
        is taken to be the vector pointing from the current
        position along NSID2 (for example your hand holding
        the torch) to the current position on NSID1 (the weld source).
        EQ.0: beam aiming direction is (TX, TY, TZ) input on Card 5.
        """ # nopep8
        return self._cards[0].get_value("nsid2")

    @nsid2.setter
    def nsid2(self, value: int) -> None:
        self._cards[0].set_value("nsid2", value)

    @property
    def spd2(self) -> float:
        """Get or set the Speed of reference point in NSID2 (ignored unless NSID2 > 0)
        GT.0: constant speed
        LT.0: |SPD2| is a load curve ID defining weld speed as a
        function of time.
        """ # nopep8
        return self._cards[0].get_value("spd2")

    @spd2.setter
    def spd2(self, value: float) -> None:
        self._cards[0].set_value("spd2", value)

    @property
    def relvel(self) -> int:
        """Get or set the Defines if SPD1 and SPD2 are relative or absolute speeds in
        coupled simulations
        EQ.0: absolute speeds
        EQ.1: relative speeds with respect to underlying structure.
        """ # nopep8
        return self._cards[0].get_value("relvel")

    @relvel.setter
    def relvel(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""relvel must be one of {0,1}""")
        self._cards[0].set_value("relvel", value)

    @property
    def erod(self) -> int:
        """Get or set the Flag for updating boundary segments exposed to the
        environment as solid elements (defined in PSEROD) erode; see Remark 5.
        EQ.0: no propagation onto new segments
        EQ.1: propagation onto new segments.
        """ # nopep8
        return self._cards[1].get_value("erod")

    @erod.setter
    def erod(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""erod must be one of {0,1}""")
        self._cards[1].set_value("erod", value)

    @property
    def loc(self) -> typing.Optional[int]:
        """Get or set the For a thick thermal shell, the flux will be applied to the surface
        identified by LOC. See field THSHEL on the *CONTROL_SHELL keyword.
        EQ.-1: lower surface of thermal shell element
        EQ.0: middle surface of thermal shell element
        EQ.1: upper surface of thermal shell element.
        """ # nopep8
        return self._cards[1].get_value("loc")

    @loc.setter
    def loc(self, value: int) -> None:
        self._cards[1].set_value("loc", value)

    @property
    def lcrot(self) -> typing.Optional[int]:
        """Get or set the Load curve defining the rotation (angle in degrees) of weld source
        around the trajectory as function of time. See Remark 2.
        """ # nopep8
        return self._cards[1].get_value("lcrot")

    @lcrot.setter
    def lcrot(self, value: int) -> None:
        self._cards[1].set_value("lcrot", value)

    @property
    def lclat(self) -> typing.Optional[int]:
        """Get or set the Load curve for lateral offset of weld source as function of time. See Remark 2.
        """ # nopep8
        return self._cards[1].get_value("lclat")

    @lclat.setter
    def lclat(self, value: int) -> None:
        self._cards[1].set_value("lclat", value)

    @property
    def iform(self) -> typing.Optional[int]:
        """Get or set the Geometry description for energy rate density distribution (see Remark 3):
        EQ.1: double elliptic with constant density
        EQ.2: double elliptic with Gaussian distribution
        EQ.3: user defined function.
        """ # nopep8
        return self._cards[2].get_value("iform")

    @iform.setter
    def iform(self, value: int) -> None:
        self._cards[2].set_value("iform", value)

    @property
    def lctim(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for flux energy input rate multiplier q1(t) as a
        function of time, see Remark 4.
        EQ.0: use constant multiplier value q1(t) = 1.0.
        """ # nopep8
        return self._cards[2].get_value("lctim")

    @lctim.setter
    def lctim(self, value: int) -> None:
        self._cards[2].set_value("lctim", value)

    @property
    def q(self) -> float:
        """Get or set the Base energy input rate Qb [energy/time], see Remark 4.
        """ # nopep8
        return self._cards[2].get_value("q")

    @q.setter
    def q(self, value: float) -> None:
        self._cards[2].set_value("q", value)

    @property
    def lcinc(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for flux energy input rate multiplier q2(a) as a. The value of the angle α range from 0° (segment normal and heat source point in the same direction) to 180° (segment normal and heat source point in opposite directions).
        function of inclination angle a, see Remark 4
        EQ.0: use constant multiplier value q2(a) = 1.0.
        """ # nopep8
        return self._cards[2].get_value("lcinc")

    @lcinc.setter
    def lcinc(self, value: int) -> None:
        self._cards[2].set_value("lcinc", value)

    @property
    def enfor(self) -> int:
        """Get or set the Flag for heat input enforcement option (see Remark 4)
        EQ.0: no additional scaling of heat source
        EQ.1: account for inclination angle a of heat source by energy
        input rate multiplier q3(a) = cos(a)
        EQ.2: scale the nodal fluxes by a multiplier q4(t) such that the
        resulting heat input equals the user input Q(t) =Qb*q1(t)
        EQ.3: apply option 1 and 2.
        """ # nopep8
        return self._cards[2].get_value("enfor")

    @enfor.setter
    def enfor(self, value: int) -> None:
        self._cards[2].set_value("enfor", value)

    @property
    def p1(self) -> typing.Optional[float]:
        """Get or set the Parameters defining flux geometry, depending on field IFORM.
        See Remark 3 for details.
        """ # nopep8
        return self._cards[3].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        self._cards[3].set_value("p1", value)

    @property
    def p2(self) -> typing.Optional[float]:
        """Get or set the Parameters defining flux geometry, depending on field IFORM.
        See Remark 3 for details.
        """ # nopep8
        return self._cards[3].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        self._cards[3].set_value("p2", value)

    @property
    def p3(self) -> typing.Optional[float]:
        """Get or set the Parameters defining flux geometry, depending on field IFORM.
        See Remark 3 for details.
        """ # nopep8
        return self._cards[3].get_value("p3")

    @p3.setter
    def p3(self, value: float) -> None:
        self._cards[3].set_value("p3", value)

    @property
    def p4(self) -> typing.Optional[float]:
        """Get or set the Parameters defining flux geometry, depending on field IFORM.
        See Remark 3 for details.
        """ # nopep8
        return self._cards[3].get_value("p4")

    @p4.setter
    def p4(self, value: float) -> None:
        self._cards[3].set_value("p4", value)

    @property
    def p5(self) -> typing.Optional[float]:
        """Get or set the Parameters defining flux geometry, depending on field IFORM.
        See Remark 3 for details.
        """ # nopep8
        return self._cards[3].get_value("p5")

    @p5.setter
    def p5(self, value: float) -> None:
        self._cards[3].set_value("p5", value)

    @property
    def p6(self) -> typing.Optional[float]:
        """Get or set the Parameters defining flux geometry, depending on field IFORM.
        See Remark 3 for details.
        """ # nopep8
        return self._cards[3].get_value("p6")

    @p6.setter
    def p6(self, value: float) -> None:
        self._cards[3].set_value("p6", value)

    @property
    def p7(self) -> typing.Optional[float]:
        """Get or set the Parameters defining flux geometry, depending on field IFORM.
        See Remark 3 for details.
        """ # nopep8
        return self._cards[3].get_value("p7")

    @p7.setter
    def p7(self, value: float) -> None:
        self._cards[3].set_value("p7", value)

    @property
    def p8(self) -> typing.Optional[float]:
        """Get or set the Parameters defining flux geometry, depending on field IFORM.
        See Remark 3 for details.
        """ # nopep8
        return self._cards[3].get_value("p8")

    @p8.setter
    def p8(self, value: float) -> None:
        self._cards[3].set_value("p8", value)

    @property
    def tx(self) -> typing.Optional[float]:
        """Get or set the Weld beam direction vector in global coordinates (NSID2 = 0 only).
        """ # nopep8
        return self._cards[4].get_value("tx")

    @tx.setter
    def tx(self, value: float) -> None:
        self._cards[4].set_value("tx", value)

    @property
    def ty(self) -> typing.Optional[float]:
        """Get or set the Weld beam direction vector in global coordinates (NSID2 = 0 only).
        """ # nopep8
        return self._cards[4].get_value("ty")

    @ty.setter
    def ty(self, value: float) -> None:
        self._cards[4].set_value("ty", value)

    @property
    def tz(self) -> typing.Optional[float]:
        """Get or set the Weld beam direction vector in global coordinates (NSID2 = 0 only).
        """ # nopep8
        return self._cards[4].get_value("tz")

    @tz.setter
    def tz(self, value: float) -> None:
        self._cards[4].set_value("tz", value)

