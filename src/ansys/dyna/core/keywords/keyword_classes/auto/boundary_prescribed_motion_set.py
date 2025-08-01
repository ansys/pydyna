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

"""Module providing the BoundaryPrescribedMotionSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class BoundaryPrescribedMotionSet(KeywordBase):
    """DYNA BOUNDARY_PRESCRIBED_MOTION_SET keyword"""

    keyword = "BOUNDARY"
    subkeyword = "PRESCRIBED_MOTION_SET"
    option_specs = [
        OptionSpec("ID", -2, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the BoundaryPrescribedMotionSet class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "nsid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "dof",
                        int,
                        10,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "vad",
                        int,
                        20,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "lcid",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "sf",
                        float,
                        40,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "vid",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "death",
                        float,
                        60,
                        10,
                        1.0E+28,
                        **kwargs,
                    ),
                    Field(
                        "birth",
                        float,
                        70,
                        10,
                        0.0,
                        **kwargs,
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
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "offset2",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "lrb",
                        int,
                        20,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "node1",
                        int,
                        30,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "node2",
                        int,
                        40,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
                lambda: abs(self.dof) in [9, 10, 11] or self.vad==4,
            ),
            OptionCardSet(
                option_spec = BoundaryPrescribedMotionSet.option_specs[0],
                cards = [
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
                                "heading",
                                str,
                                10,
                                70,
                                kwargs.get("heading")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Nodal set ID, see *SET_NODE.
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        """Set the nsid property."""
        self._cards[0].set_value("nsid", value)

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
        EQ.12: Translational motion in direction given by the normals to the segments. Applicable to SET_SEGMENT option only
        """ # nopep8
        return self._cards[0].get_value("dof")

    @dof.setter
    def dof(self, value: int) -> None:
        """Set the dof property."""
        if value not in [0, 1, 2, 3, 4, -4, 5, 6, 7, 8, -8, 9, -9, 10, -10, 11, -11, 12, None]:
            raise Exception("""dof must be `None` or one of {0,1,2,3,4,-4,5,6,7,8,-8,9,-9,10,-10,11,-11,12}.""")
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
        """Set the vad property."""
        if value not in [0, 1, 2, 3, 4, None]:
            raise Exception("""vad must be `None` or one of {0,1,2,3,4}.""")
        self._cards[0].set_value("vad", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Curve ID or function ID to describe motion value as a function of time; see *DEFINE_CURVE, *DEFINE_CURVE_FUNCTION, or *DEFINE_FUNCTION.  If LCID refers to *DEFINE_FUNCTION, the function has four arguments: time and x, y and z coordinates of the node or rigid body, such as f(t,x,y,z)=10.0×t+max⁡(x-100,0.). If VAD = 2, the function has one argument which is time, such as f(t)=10.0×t (see Remark 2). See BIRTH below.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def sf(self) -> float:
        """Get or set the Load curve scale factor (default=1.0).
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[0].set_value("sf", value)

    @property
    def vid(self) -> typing.Optional[int]:
        """Get or set the Vector ID for DOF values of 4 or 8, see *DEFINE_VECTOR.
        """ # nopep8
        return self._cards[0].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        """Set the vid property."""
        self._cards[0].set_value("vid", value)

    @property
    def death(self) -> float:
        """Get or set the Time imposed motion/constraint is removed (default=1.0E+28).
        """ # nopep8
        return self._cards[0].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        """Set the death property."""
        self._cards[0].set_value("death", value)

    @property
    def birth(self) -> float:
        """Get or set the Time imposed motion/constraint is activated (default=0.0).
        """ # nopep8
        return self._cards[0].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        """Set the birth property."""
        self._cards[0].set_value("birth", value)

    @property
    def offset1(self) -> float:
        """Get or set the Offset for DOF types 9-11 (y, z, x direction).
        """ # nopep8
        return self._cards[1].get_value("offset1")

    @offset1.setter
    def offset1(self, value: float) -> None:
        """Set the offset1 property."""
        self._cards[1].set_value("offset1", value)

    @property
    def offset2(self) -> float:
        """Get or set the Offset for DOF types 9-11 (z, x, y direction.)
        """ # nopep8
        return self._cards[1].get_value("offset2")

    @offset2.setter
    def offset2(self, value: float) -> None:
        """Set the offset2 property."""
        self._cards[1].set_value("offset2", value)

    @property
    def lrb(self) -> int:
        """Get or set the lead rigid body for measuring the relative displacement.
        """ # nopep8
        return self._cards[1].get_value("lrb")

    @lrb.setter
    def lrb(self, value: int) -> None:
        """Set the lrb property."""
        self._cards[1].set_value("lrb", value)

    @property
    def node1(self) -> int:
        """Get or set the Optional orientation node, n1, for relative displacement.
        """ # nopep8
        return self._cards[1].get_value("node1")

    @node1.setter
    def node1(self, value: int) -> None:
        """Set the node1 property."""
        self._cards[1].set_value("node1", value)

    @property
    def node2(self) -> int:
        """Get or set the Optional orientation node, n2, for relative displacement.
        """ # nopep8
        return self._cards[1].get_value("node2")

    @node2.setter
    def node2(self, value: int) -> None:
        """Set the node2 property."""
        self._cards[1].set_value("node2", value)

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the ID keyword option
        """ # nopep8
        return self._cards[2].cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[2].cards[0].set_value("id", value)

        if value:
            self.activate_option("ID")

    @property
    def heading(self) -> typing.Optional[str]:
        """Get or set the Descriptor. We suggest using unique descriptions.
        """ # nopep8
        return self._cards[2].cards[0].get_value("heading")

    @heading.setter
    def heading(self, value: str) -> None:
        """Set the heading property."""
        self._cards[2].cards[0].set_value("heading", value)

        if value:
            self.activate_option("HEADING")

