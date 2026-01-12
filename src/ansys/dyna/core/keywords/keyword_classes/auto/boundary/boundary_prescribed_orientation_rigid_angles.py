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

"""Module providing the BoundaryPrescribedOrientationRigidAngles class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BOUNDARYPRESCRIBEDORIENTATIONRIGIDANGLES_CARD0 = (
    FieldSchema("pidb", int, 0, 10, None),
    FieldSchema("pida", int, 10, 10, None),
    FieldSchema("intrp", int, 20, 10, 1),
    FieldSchema("birth", float, 30, 10, 0.0),
    FieldSchema("death", float, 40, 10, 1e+20),
    FieldSchema("toffset", int, 50, 10, 0),
)

_BOUNDARYPRESCRIBEDORIENTATIONRIGIDANGLES_CARD1 = (
    FieldSchema("lcidq1", int, 0, 10, None),
    FieldSchema("lcidq2", int, 10, 10, None),
    FieldSchema("lcidq3", int, 20, 10, None),
    FieldSchema("iseq", int, 30, 10, 123),
    FieldSchema("ishft", int, 40, 10, 1),
    FieldSchema("body", int, 50, 10, 0),
)

class BoundaryPrescribedOrientationRigidAngles(KeywordBase):
    """DYNA BOUNDARY_PRESCRIBED_ORIENTATION_RIGID_ANGLES keyword"""

    keyword = "BOUNDARY"
    subkeyword = "PRESCRIBED_ORIENTATION_RIGID_ANGLES"

    def __init__(self, **kwargs):
        """Initialize the BoundaryPrescribedOrientationRigidAngles class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYPRESCRIBEDORIENTATIONRIGIDANGLES_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _BOUNDARYPRESCRIBEDORIENTATIONRIGIDANGLES_CARD1,
                **kwargs,
            ),        ]
    @property
    def pidb(self) -> typing.Optional[int]:
        """Get or set the Part ID for rigid body B whose orientation is prescribed
        """ # nopep8
        return self._cards[0].get_value("pidb")

    @pidb.setter
    def pidb(self, value: int) -> None:
        """Set the pidb property."""
        self._cards[0].set_value("pidb", value)

    @property
    def pida(self) -> typing.Optional[int]:
        """Get or set the Part ID for rigid body A.  The orientation of PIDB is measured with respect to the coordinate system of PIDA, as defined by LCO on *MAT_RIGID.  If zero then orientation of PIDB is measured with respect to the global reference frame except for BODY=1 in the ANGLES option
        """ # nopep8
        return self._cards[0].get_value("pida")

    @pida.setter
    def pida(self, value: int) -> None:
        """Set the pida property."""
        self._cards[0].set_value("pida", value)

    @property
    def intrp(self) -> int:
        """Get or set the Interpolation method used on time history curves:
        EQ.1: Linear interpolation (default)
        """ # nopep8
        return self._cards[0].get_value("intrp")

    @intrp.setter
    def intrp(self, value: int) -> None:
        """Set the intrp property."""
        self._cards[0].set_value("intrp", value)

    @property
    def birth(self) -> float:
        """Get or set the Prior to this time the body moves freely under the action of other agents.
        """ # nopep8
        return self._cards[0].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        """Set the birth property."""
        self._cards[0].set_value("birth", value)

    @property
    def death(self) -> float:
        """Get or set the The body is freed at this time and subsequently allowed to move under the action of other agents
        """ # nopep8
        return self._cards[0].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        """Set the death property."""
        self._cards[0].set_value("death", value)

    @property
    def toffset(self) -> int:
        """Get or set the Time offset flag:
        EQ.0:   No time offset is applied.
        EQ.1:	The time value of all load curves will be offset by the birth time,
        EQ.0:	no time offset is applied
        """ # nopep8
        return self._cards[0].get_value("toffset")

    @toffset.setter
    def toffset(self, value: int) -> None:
        """Set the toffset property."""
        if value not in [0, 1, None]:
            raise Exception("""toffset must be `None` or one of {0,1}.""")
        self._cards[0].set_value("toffset", value)

    @property
    def lcidq1(self) -> typing.Optional[int]:
        """Get or set the Load curve ID.
        """ # nopep8
        return self._cards[1].get_value("lcidq1")

    @lcidq1.setter
    def lcidq1(self, value: int) -> None:
        """Set the lcidq1 property."""
        self._cards[1].set_value("lcidq1", value)

    @property
    def lcidq2(self) -> typing.Optional[int]:
        """Get or set the Load curve ID.
        """ # nopep8
        return self._cards[1].get_value("lcidq2")

    @lcidq2.setter
    def lcidq2(self, value: int) -> None:
        """Set the lcidq2 property."""
        self._cards[1].set_value("lcidq2", value)

    @property
    def lcidq3(self) -> typing.Optional[int]:
        """Get or set the Load curve ID.
        """ # nopep8
        return self._cards[1].get_value("lcidq3")

    @lcidq3.setter
    def lcidq3(self, value: int) -> None:
        """Set the lcidq3 property."""
        self._cards[1].set_value("lcidq3", value)

    @property
    def iseq(self) -> int:
        """Get or set the Specifies the sequence in which the rotations are effected.  In this first set of sequences three unique axes are involved.
        EQ.123:  the first rotation is performed about the x axis an amount q1, the second about the y axis an amount q2 and the third about the z axis an amount q3.
        EQ.231:  the first rotation is performed about the y axis an amount q1, the second about the z axis an amount q2 and the third about the x axis an amount q3.
        EQ.312:  the first rotation is performed about the z axis an amount q1, the second about the x axis an amount q2 and the third about the y axis an amount q3.
        EQ.132:  the first rotation is performed about the x axis an amount q1, the second about the z axis an amount q2 and the third about the y axis an amount q3.
        EQ.213:  the first rotation is performed about the y axis an amount q1, the second about the x axis an amount q2 and the third about the z axis an amount q3.
        EQ.321:  the first rotation is performed about the z axis an amount q1, the second about the y axis an amount q2 and the third about the x axis an amount q3.
        The second set of sequences involve only two unique axes where the first and third are repeated.
        EQ.121:  the first rotation is performed about the x axis an amount q1, the second about the y axis an amount q2 and the third about the x axis an amount q3.
        EQ.131:  the first rotation is performed about the x axis an amount q1, the second about the z axis an amount q2 and the third about the x axis an amount q3.
        VARIABLE DESCRIPTION
        EQ.212:  the first rotation is performed about the y axis an amount q1, the second about the x axis an amount q2 and the third about the y axis an amount q3.
        EQ.232:  the first rotation is performed about the y axis an amount q1, the second about the z axis an amount q2 and the third about the y axis an amount q3.
        EQ.313:  the first rotation is performed about the z axis an amount q1, the second about the x axis an amount q2 and the third about the z axis an amount q3.
        EQ.323:  the first rotation is performed about the z axis an amount q1, the second about the x axis an amount q2 and the third about the z axis an amount q3..
        """ # nopep8
        return self._cards[1].get_value("iseq")

    @iseq.setter
    def iseq(self, value: int) -> None:
        """Set the iseq property."""
        if value not in [123, 231, 312, 132, 213, 321, 121, 131, 212, 232, 313, 323, None]:
            raise Exception("""iseq must be `None` or one of {123,231,312,132,213,321,121,131,212,232,313,323}.""")
        self._cards[1].set_value("iseq", value)

    @property
    def ishft(self) -> int:
        """Get or set the Angle shift.
        EQ.1:  Angle curves are unaltered.
        EQ.2: Shifts angle data in the LCIDQi curves as necessary to eliminate discontinuities. If angles are confined to the range [- , ] and the data contains excursions exceeding   then set ISHFT=2.
        """ # nopep8
        return self._cards[1].get_value("ishft")

    @ishft.setter
    def ishft(self, value: int) -> None:
        """Set the ishft property."""
        if value not in [1, 2, None]:
            raise Exception("""ishft must be `None` or one of {1,2}.""")
        self._cards[1].set_value("ishft", value)

    @property
    def body(self) -> int:
        """Get or set the Reference axes.
        EQ.0: Rotations are performed about axes fixed in PIDA (extrinsic rotation, default).
        EQ.1: Rotations are performed about axes fixed in PIDB (intrinsic rotation).
        """ # nopep8
        return self._cards[1].get_value("body")

    @body.setter
    def body(self, value: int) -> None:
        """Set the body property."""
        if value not in [0, 1, None]:
            raise Exception("""body must be `None` or one of {0,1}.""")
        self._cards[1].set_value("body", value)

