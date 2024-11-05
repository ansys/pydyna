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

class ConstrainedJointCoorUniversal(KeywordBase):
    """DYNA CONSTRAINED_JOINT_COOR_UNIVERSAL keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "JOINT_COOR_UNIVERSAL"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "rbid_a",
                        int,
                        0,
                        10,
                        kwargs.get("rbid_a")
                    ),
                    Field(
                        "rbid_b",
                        int,
                        10,
                        10,
                        kwargs.get("rbid_b")
                    ),
                    Field(
                        "rps",
                        float,
                        20,
                        10,
                        kwargs.get("rps", 1.0)
                    ),
                    Field(
                        "damp",
                        float,
                        30,
                        10,
                        kwargs.get("damp")
                    ),
                    Field(
                        "tmass",
                        float,
                        40,
                        10,
                        kwargs.get("tmass")
                    ),
                    Field(
                        "rmass",
                        float,
                        50,
                        10,
                        kwargs.get("rmass")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "x1",
                        float,
                        0,
                        10,
                        kwargs.get("x1")
                    ),
                    Field(
                        "y1",
                        float,
                        10,
                        10,
                        kwargs.get("y1")
                    ),
                    Field(
                        "z1",
                        float,
                        20,
                        10,
                        kwargs.get("z1")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "x2",
                        float,
                        0,
                        10,
                        kwargs.get("x2")
                    ),
                    Field(
                        "y2",
                        float,
                        10,
                        10,
                        kwargs.get("y2")
                    ),
                    Field(
                        "z2",
                        float,
                        20,
                        10,
                        kwargs.get("z2")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "x3",
                        float,
                        0,
                        10,
                        kwargs.get("x3")
                    ),
                    Field(
                        "y3",
                        float,
                        10,
                        10,
                        kwargs.get("y3")
                    ),
                    Field(
                        "z3",
                        float,
                        20,
                        10,
                        kwargs.get("z3")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "x4",
                        float,
                        0,
                        10,
                        kwargs.get("x4")
                    ),
                    Field(
                        "y4",
                        float,
                        10,
                        10,
                        kwargs.get("y4")
                    ),
                    Field(
                        "z4",
                        float,
                        20,
                        10,
                        kwargs.get("z4")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "x5",
                        float,
                        0,
                        10,
                        kwargs.get("x5")
                    ),
                    Field(
                        "y5",
                        float,
                        10,
                        10,
                        kwargs.get("y5")
                    ),
                    Field(
                        "z5",
                        float,
                        20,
                        10,
                        kwargs.get("z5")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "x6",
                        float,
                        0,
                        10,
                        kwargs.get("x6")
                    ),
                    Field(
                        "y6",
                        float,
                        10,
                        10,
                        kwargs.get("y6")
                    ),
                    Field(
                        "z6",
                        float,
                        20,
                        10,
                        kwargs.get("z6")
                    ),
                ],
            ),
        ]

    @property
    def rbid_a(self) -> typing.Optional[int]:
        """Get or set the Part ID of rigid body A
        """ # nopep8
        return self._cards[0].get_value("rbid_a")

    @rbid_a.setter
    def rbid_a(self, value: int) -> None:
        self._cards[0].set_value("rbid_a", value)

    @property
    def rbid_b(self) -> typing.Optional[int]:
        """Get or set the Part ID of rigid body
        """ # nopep8
        return self._cards[0].get_value("rbid_b")

    @rbid_b.setter
    def rbid_b(self, value: int) -> None:
        self._cards[0].set_value("rbid_b", value)

    @property
    def rps(self) -> float:
        """Get or set the Relative penalty stiffness (default = 1.0).
        """ # nopep8
        return self._cards[0].get_value("rps")

    @rps.setter
    def rps(self, value: float) -> None:
        self._cards[0].set_value("rps", value)

    @property
    def damp(self) -> typing.Optional[float]:
        """Get or set the Damping scale factor on default damping value. (Revolute and Spherical Joints):
        EQ.0.0: default is set to 1.0,
        LE.0.01 and GT.0.0: no damping is used.
        """ # nopep8
        return self._cards[0].get_value("damp")

    @damp.setter
    def damp(self, value: float) -> None:
        self._cards[0].set_value("damp", value)

    @property
    def tmass(self) -> typing.Optional[float]:
        """Get or set the Lumped translational mass.  The mass is equally split between the first points defined for rigid bodies A and B.
        """ # nopep8
        return self._cards[0].get_value("tmass")

    @tmass.setter
    def tmass(self, value: float) -> None:
        self._cards[0].set_value("tmass", value)

    @property
    def rmass(self) -> typing.Optional[float]:
        """Get or set the Lumped rotational inertia.  The inertia is equally split between the first points defined for rigid bodies A and B.
        """ # nopep8
        return self._cards[0].get_value("rmass")

    @rmass.setter
    def rmass(self, value: float) -> None:
        self._cards[0].set_value("rmass", value)

    @property
    def x1(self) -> typing.Optional[float]:
        """Get or set the Coordinate of point 1, in rigid body A.  Define for all joint types.
        """ # nopep8
        return self._cards[1].get_value("x1")

    @x1.setter
    def x1(self, value: float) -> None:
        self._cards[1].set_value("x1", value)

    @property
    def y1(self) -> typing.Optional[float]:
        """Get or set the Coordinate of point 1, in rigid body A.  Define for all joint types.
        """ # nopep8
        return self._cards[1].get_value("y1")

    @y1.setter
    def y1(self, value: float) -> None:
        self._cards[1].set_value("y1", value)

    @property
    def z1(self) -> typing.Optional[float]:
        """Get or set the Coordinate of point 1, in rigid body A.  Define for all joint types.
        """ # nopep8
        return self._cards[1].get_value("z1")

    @z1.setter
    def z1(self, value: float) -> None:
        self._cards[1].set_value("z1", value)

    @property
    def x2(self) -> typing.Optional[float]:
        """Get or set the Coordinate of  point 2, in rigid body B.  If points 1 and 2 are coincident in the specified joint type, the coordinate for point 1 is used.
        """ # nopep8
        return self._cards[2].get_value("x2")

    @x2.setter
    def x2(self, value: float) -> None:
        self._cards[2].set_value("x2", value)

    @property
    def y2(self) -> typing.Optional[float]:
        """Get or set the Coordinate of  point 2, in rigid body B.  If points 1 and 2 are coincident in the specified joint type, the coordinate for point 1 is used.
        """ # nopep8
        return self._cards[2].get_value("y2")

    @y2.setter
    def y2(self, value: float) -> None:
        self._cards[2].set_value("y2", value)

    @property
    def z2(self) -> typing.Optional[float]:
        """Get or set the Coordinate of  point 2, in rigid body B.  If points 1 and 2 are coincident in the specified joint type, the coordinate for point 1 is used.
        """ # nopep8
        return self._cards[2].get_value("z2")

    @z2.setter
    def z2(self, value: float) -> None:
        self._cards[2].set_value("z2", value)

    @property
    def x3(self) -> typing.Optional[float]:
        """Get or set the Coordinate of point 3, in rigid body A.  Define for all joint types.
        """ # nopep8
        return self._cards[3].get_value("x3")

    @x3.setter
    def x3(self, value: float) -> None:
        self._cards[3].set_value("x3", value)

    @property
    def y3(self) -> typing.Optional[float]:
        """Get or set the Coordinate of point 3, in rigid body A.  Define for all joint types.
        """ # nopep8
        return self._cards[3].get_value("y3")

    @y3.setter
    def y3(self, value: float) -> None:
        self._cards[3].set_value("y3", value)

    @property
    def z3(self) -> typing.Optional[float]:
        """Get or set the Coordinate of point 3, in rigid body A.  Define for all joint types.
        """ # nopep8
        return self._cards[3].get_value("z3")

    @z3.setter
    def z3(self, value: float) -> None:
        self._cards[3].set_value("z3", value)

    @property
    def x4(self) -> typing.Optional[float]:
        """Get or set the Coordinate of  point 4, in rigid body B.  If points 3 and 4 are coincident in the specified joint type, the coordinate for point 3 is used.
        """ # nopep8
        return self._cards[4].get_value("x4")

    @x4.setter
    def x4(self, value: float) -> None:
        self._cards[4].set_value("x4", value)

    @property
    def y4(self) -> typing.Optional[float]:
        """Get or set the Coordinate of  point 4, in rigid body B.  If points 3 and 4 are coincident in the specified joint type, the coordinate for point 3 is used.
        """ # nopep8
        return self._cards[4].get_value("y4")

    @y4.setter
    def y4(self, value: float) -> None:
        self._cards[4].set_value("y4", value)

    @property
    def z4(self) -> typing.Optional[float]:
        """Get or set the Coordinate of  point 4, in rigid body B.  If points 3 and 4 are coincident in the specified joint type, the coordinate for point 3 is used.
        """ # nopep8
        return self._cards[4].get_value("z4")

    @z4.setter
    def z4(self, value: float) -> None:
        self._cards[4].set_value("z4", value)

    @property
    def x5(self) -> typing.Optional[float]:
        """Get or set the Coordinate of point 5, in rigid body A.  Define for all joint types.
        """ # nopep8
        return self._cards[5].get_value("x5")

    @x5.setter
    def x5(self, value: float) -> None:
        self._cards[5].set_value("x5", value)

    @property
    def y5(self) -> typing.Optional[float]:
        """Get or set the Coordinate of point 5, in rigid body A.  Define for all joint types.
        """ # nopep8
        return self._cards[5].get_value("y5")

    @y5.setter
    def y5(self, value: float) -> None:
        self._cards[5].set_value("y5", value)

    @property
    def z5(self) -> typing.Optional[float]:
        """Get or set the Coordinate of point 5, in rigid body A.  Define for all joint types.
        """ # nopep8
        return self._cards[5].get_value("z5")

    @z5.setter
    def z5(self, value: float) -> None:
        self._cards[5].set_value("z5", value)

    @property
    def x6(self) -> typing.Optional[float]:
        """Get or set the Coordinate of  point 6, in rigid body B.  If points 5 and 6 are coincident in the specified joint type, the coordinate for point 5 is used.
        """ # nopep8
        return self._cards[6].get_value("x6")

    @x6.setter
    def x6(self, value: float) -> None:
        self._cards[6].set_value("x6", value)

    @property
    def y6(self) -> typing.Optional[float]:
        """Get or set the Coordinate of  point 6, in rigid body B.  If points 5 and 6 are coincident in the specified joint type, the coordinate for point 5 is used.
        """ # nopep8
        return self._cards[6].get_value("y6")

    @y6.setter
    def y6(self, value: float) -> None:
        self._cards[6].set_value("y6", value)

    @property
    def z6(self) -> typing.Optional[float]:
        """Get or set the Coordinate of  point 6, in rigid body B.  If points 5 and 6 are coincident in the specified joint type, the coordinate for point 5 is used.
        """ # nopep8
        return self._cards[6].get_value("z6")

    @z6.setter
    def z6(self, value: float) -> None:
        self._cards[6].set_value("z6", value)

