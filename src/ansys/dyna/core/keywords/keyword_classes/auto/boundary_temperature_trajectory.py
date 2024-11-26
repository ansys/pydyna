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

class BoundaryTemperatureTrajectory(KeywordBase):
    """DYNA BOUNDARY_TEMPERATURE_TRAJECTORY keyword"""

    keyword = "BOUNDARY"
    subkeyword = "TEMPERATURE_TRAJECTORY"

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
                        "pype",
                        int,
                        10,
                        10,
                        kwargs.get("pype", 1)
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
                        "unused",
                        int,
                        60,
                        10,
                        kwargs.get("unused")
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
                        "tmult",
                        float,
                        20,
                        10,
                        kwargs.get("tmult")
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
                        "unused",
                        int,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        70,
                        10,
                        kwargs.get("unused")
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
        """Get or set the Part ID or part set ID to what the temperature boundary condition will be applied on.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
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
        if value not in [1, 2]:
            raise Exception("""pype must be one of {1,2}""")
        self._cards[0].set_value("pype", value)

    @property
    def nsid1(self) -> typing.Optional[int]:
        """Get or set the Node set defining the path of the moving volume.  The moving volume travels along the path at speed SPD1.
        The nodes are traversed according to their order in the node set.  See Remark 1.
        """ # nopep8
        return self._cards[0].get_value("nsid1")

    @nsid1.setter
    def nsid1(self, value: int) -> None:
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
        if value not in [1, 2]:
            raise Exception("""iform must be one of {1,2}""")
        self._cards[1].set_value("iform", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for temperature as a function of time
        EQ.0:	temperature is a constant defined by the value TMULT.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[1].set_value("lcid", value)

    @property
    def tmult(self) -> typing.Optional[float]:
        """Get or set the Curve multiplier for temperature.
        """ # nopep8
        return self._cards[1].get_value("tmult")

    @tmult.setter
    def tmult(self, value: float) -> None:
        self._cards[1].set_value("tmult", value)

    @property
    def lcrot(self) -> typing.Optional[int]:
        """Get or set the Load curve defining the rotation angle (in degrees) of the moving volume around the trajectory as a function of time.  See Remark 2.
        """ # nopep8
        return self._cards[1].get_value("lcrot")

    @lcrot.setter
    def lcrot(self, value: int) -> None:
        self._cards[1].set_value("lcrot", value)

    @property
    def lcmov(self) -> typing.Optional[int]:
        """Get or set the Load curve defining the offset of the moving volume along its center axis as a function of time.  See Remark 2.
        """ # nopep8
        return self._cards[1].get_value("lcmov")

    @lcmov.setter
    def lcmov(self, value: int) -> None:
        self._cards[1].set_value("lcmov", value)

    @property
    def lclat(self) -> typing.Optional[int]:
        """Get or set the Load curve defining the lateral offset of the moving volume as a function of time.  See Remark 2.
        """ # nopep8
        return self._cards[1].get_value("lclat")

    @lclat.setter
    def lclat(self, value: int) -> None:
        self._cards[1].set_value("lclat", value)

    @property
    def p1(self) -> typing.Optional[float]:
        """Get or set the Parameters defining the moving volume's geometry.
        The meaning of each parameter depends on field IFORM.  See Remark 3 for details.
        """ # nopep8
        return self._cards[2].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        self._cards[2].set_value("p1", value)

    @property
    def p2(self) -> typing.Optional[float]:
        """Get or set the Parameters defining the moving volume's geometry.
        The meaning of each parameter depends on field IFORM.  See Remark 3 for details.
        """ # nopep8
        return self._cards[2].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        self._cards[2].set_value("p2", value)

    @property
    def p3(self) -> typing.Optional[float]:
        """Get or set the Parameters defining the moving volume's geometry.
        The meaning of each parameter depends on field IFORM.  See Remark 3 for details.
        """ # nopep8
        return self._cards[2].get_value("p3")

    @p3.setter
    def p3(self, value: float) -> None:
        self._cards[2].set_value("p3", value)

    @property
    def p4(self) -> typing.Optional[float]:
        """Get or set the Parameters defining the moving volume's geometry.
        The meaning of each parameter depends on field IFORM.  See Remark 3 for details.
        """ # nopep8
        return self._cards[2].get_value("p4")

    @p4.setter
    def p4(self, value: float) -> None:
        self._cards[2].set_value("p4", value)

    @property
    def p5(self) -> typing.Optional[float]:
        """Get or set the Parameters defining the moving volume's geometry.
        The meaning of each parameter depends on field IFORM.  See Remark 3 for details.
        """ # nopep8
        return self._cards[2].get_value("p5")

    @p5.setter
    def p5(self, value: float) -> None:
        self._cards[2].set_value("p5", value)

    @property
    def p6(self) -> typing.Optional[float]:
        """Get or set the Parameters defining the moving volume's geometry.
        The meaning of each parameter depends on field IFORM.  See Remark 3 for details.
        """ # nopep8
        return self._cards[2].get_value("p6")

    @p6.setter
    def p6(self, value: float) -> None:
        self._cards[2].set_value("p6", value)

    @property
    def p7(self) -> typing.Optional[float]:
        """Get or set the Parameters defining the moving volume's geometry.
        The meaning of each parameter depends on field IFORM.  See Remark 3 for details.
        """ # nopep8
        return self._cards[2].get_value("p7")

    @p7.setter
    def p7(self, value: float) -> None:
        self._cards[2].set_value("p7", value)

    @property
    def p8(self) -> typing.Optional[float]:
        """Get or set the Parameters defining the moving volume's geometry.
        The meaning of each parameter depends on field IFORM.  See Remark 3 for details.
        """ # nopep8
        return self._cards[2].get_value("p8")

    @p8.setter
    def p8(self, value: float) -> None:
        self._cards[2].set_value("p8", value)

    @property
    def tx(self) -> typing.Optional[float]:
        """Get or set the Orientation vector of the moving volume's center axis in global coordinates (NSID2 = 0 only).
        """ # nopep8
        return self._cards[3].get_value("tx")

    @tx.setter
    def tx(self, value: float) -> None:
        self._cards[3].set_value("tx", value)

    @property
    def ty(self) -> typing.Optional[float]:
        """Get or set the Orientation vector of the moving volume's center axis in global coordinates (NSID2 = 0 only).
        """ # nopep8
        return self._cards[3].get_value("ty")

    @ty.setter
    def ty(self, value: float) -> None:
        self._cards[3].set_value("ty", value)

    @property
    def tz(self) -> typing.Optional[float]:
        """Get or set the Orientation vector of the moving volume's center axis in global coordinates (NSID2 = 0 only).
        """ # nopep8
        return self._cards[3].get_value("tz")

    @tz.setter
    def tz(self, value: float) -> None:
        self._cards[3].set_value("tz", value)

