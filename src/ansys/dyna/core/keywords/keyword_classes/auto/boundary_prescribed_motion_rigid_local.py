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

class BoundaryPrescribedMotionRigidLocal(KeywordBase):
    """DYNA BOUNDARY_PRESCRIBED_MOTION_RIGID_LOCAL keyword"""

    keyword = "BOUNDARY"
    subkeyword = "PRESCRIBED_MOTION_RIGID_LOCAL"

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
                        "dof",
                        int,
                        10,
                        10,
                        kwargs.get("dof", 0)
                    ),
                    Field(
                        "vad",
                        int,
                        20,
                        10,
                        kwargs.get("vad", 0)
                    ),
                    Field(
                        "lcid",
                        int,
                        30,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "sf",
                        float,
                        40,
                        10,
                        kwargs.get("sf", 1.0)
                    ),
                    Field(
                        "vid",
                        int,
                        50,
                        10,
                        kwargs.get("vid")
                    ),
                    Field(
                        "death",
                        float,
                        60,
                        10,
                        kwargs.get("death", 1.0E+28)
                    ),
                    Field(
                        "birth",
                        float,
                        70,
                        10,
                        kwargs.get("birth", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "offset1",
                        float,
                        0,
                        10,
                        kwargs.get("offset1", 0.0)
                    ),
                    Field(
                        "offset2",
                        float,
                        10,
                        10,
                        kwargs.get("offset2", 0.0)
                    ),
                    Field(
                        "lrb",
                        int,
                        20,
                        10,
                        kwargs.get("lrb", 0)
                    ),
                    Field(
                        "node1",
                        int,
                        30,
                        10,
                        kwargs.get("node1", 0)
                    ),
                    Field(
                        "node2",
                        int,
                        40,
                        10,
                        kwargs.get("node2", 0)
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID, see *PART.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def dof(self) -> int:
        """Get or set the Applicable degrees-of-freedom:
        EQ.0: Not valid, please use any of the other available options,
        EQ.1: x-translational DOF,
        EQ.2: y-translational DOF,
        EQ.3: z-translational DOF,
        EQ.4: translational motion only in direction given by the VID. Movement on plane normal to the vector is permitted,
        EQ.-4: Same as 4, except translation on the plane normal to the vector is NOT permitted,
        EQ.5: x-rotational DOF,
        EQ.6: y-rotational DOF,
        EQ.7: z-rotational DOF,
        EQ.8: rotational motion about an axis which is passing through the center-of-gravity of the node, node set, or rigid body and is parallel to vector VID.  Rotation about the normal axes is permitted,
        EQ.-8:rotational motion about an axis which is passing through the center-of-gravity of the node or node set and is parallel to vector VID.  Rotation about the normal axes is not permitted.  This option does not apply to rigid bodies.,
        EQ.9: y/z DOF for node rotating about the x-axis at location (OFFSET1,OFFSET2) in the yz-plane, point (y,z). Radial motion is NOT permitted,
        EQ.-9: Same as 9, except radial motion is permitted,
        EQ.10: z/x DOF for node rotating about the y-axis at location (OFFSET1,OFFSET2) in the zx-plane, point(z,x). Radial motion is NOT permitted,
        EQ.-10:Same as  10, except radial motion is permitted,
        EQ.11: x/y DOF for node rotating about the z-axis at location (OFFSET1,OFFSET2) in the xy-plane, point (x,y). Radial motion is NOT permitted,
        EQ.-11: Same as 11, except radial motion is permitted.
        """ # nopep8
        return self._cards[0].get_value("dof")

    @dof.setter
    def dof(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, -4, 5, 6, 7, 8, -8, 9, -9, 10, -10, 11, -11]:
            raise Exception("""dof must be one of {0,1,2,3,4,-4,5,6,7,8,-8,9,-9,10,-10,11,-11}""")
        self._cards[0].set_value("dof", value)

    @property
    def vad(self) -> int:
        """Get or set the Velocity/Acceleration/Displacement flag:
        EQ.0: velocity(rigid bodies and nodes),
        EQ.1: acceleration(nodes only),
        EQ.2: displacement(rigid bodies and nodes).
        EQ.3: velocity versus displacement(rigid bodies),
        EQ.4: relative displacement(rigid bodies only)
        """ # nopep8
        return self._cards[0].get_value("vad")

    @vad.setter
    def vad(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4]:
            raise Exception("""vad must be one of {0,1,2,3,4}""")
        self._cards[0].set_value("vad", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Curve ID or function ID to describe motion value as a function of time; see *DEFINE_CURVE, *DEFINE_CURVE_FUNCTION, or *DEFINE_FUNCTION.  If LCID refers to *DEFINE_FUNCTION, the function has four arguments: time and x, y and z coordinates of the node or rigid body, such as f(t,x,y,z)=10.0×t+max⁡(x-100,0.). If VAD = 2, the function has one argument which is time, such as f(t)=10.0×t (see Remark 2). See BIRTH below.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def sf(self) -> float:
        """Get or set the Load curve scale factor (default=1.0).
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        self._cards[0].set_value("sf", value)

    @property
    def vid(self) -> typing.Optional[int]:
        """Get or set the Vector ID for DOF values of 4 or 8, see *DEFINE_VECTOR.
        """ # nopep8
        return self._cards[0].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        self._cards[0].set_value("vid", value)

    @property
    def death(self) -> float:
        """Get or set the Time imposed motion/constraint is removed (default=1.0E+28).
        """ # nopep8
        return self._cards[0].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        self._cards[0].set_value("death", value)

    @property
    def birth(self) -> float:
        """Get or set the Time imposed motion/constraint is activated (default=0.0).
        """ # nopep8
        return self._cards[0].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        self._cards[0].set_value("birth", value)

    @property
    def offset1(self) -> float:
        """Get or set the Offset for DOF types 9-11 (y, z, x direction).
        """ # nopep8
        return self._cards[1].get_value("offset1")

    @offset1.setter
    def offset1(self, value: float) -> None:
        self._cards[1].set_value("offset1", value)

    @property
    def offset2(self) -> float:
        """Get or set the Offset for DOF types 9-11 (z, x, y direction).
        """ # nopep8
        return self._cards[1].get_value("offset2")

    @offset2.setter
    def offset2(self, value: float) -> None:
        self._cards[1].set_value("offset2", value)

    @property
    def lrb(self) -> int:
        """Get or set the lead rigid body for measuring the relative displacement.
        """ # nopep8
        return self._cards[1].get_value("lrb")

    @lrb.setter
    def lrb(self, value: int) -> None:
        self._cards[1].set_value("lrb", value)

    @property
    def node1(self) -> int:
        """Get or set the Optional orientation node, n1, for relative displacement.
        """ # nopep8
        return self._cards[1].get_value("node1")

    @node1.setter
    def node1(self, value: int) -> None:
        self._cards[1].set_value("node1", value)

    @property
    def node2(self) -> int:
        """Get or set the Optional orientation node, n2, for relative displacement.
        """ # nopep8
        return self._cards[1].get_value("node2")

    @node2.setter
    def node2(self, value: int) -> None:
        self._cards[1].set_value("node2", value)

