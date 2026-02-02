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

"""Module providing the ConstrainedJointCoorPulley class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_CONSTRAINEDJOINTCOORPULLEY_CARD0 = (
    FieldSchema("rbid_a", int, 0, 10, None),
    FieldSchema("rbid_b", int, 10, 10, None),
    FieldSchema("rps", float, 20, 10, 1.0),
    FieldSchema("damp", float, 30, 10, None),
    FieldSchema("tmass", float, 40, 10, None),
    FieldSchema("rmass", float, 50, 10, None),
)

_CONSTRAINEDJOINTCOORPULLEY_CARD1 = (
    FieldSchema("x1", float, 0, 10, None),
    FieldSchema("y1", float, 10, 10, None),
    FieldSchema("z1", float, 20, 10, None),
)

_CONSTRAINEDJOINTCOORPULLEY_CARD2 = (
    FieldSchema("x2", float, 0, 10, None),
    FieldSchema("y2", float, 10, 10, None),
    FieldSchema("z2", float, 20, 10, None),
)

_CONSTRAINEDJOINTCOORPULLEY_CARD3 = (
    FieldSchema("x3", float, 0, 10, None),
    FieldSchema("y3", float, 10, 10, None),
    FieldSchema("z3", float, 20, 10, None),
)

_CONSTRAINEDJOINTCOORPULLEY_CARD4 = (
    FieldSchema("x4", float, 0, 10, None),
    FieldSchema("y4", float, 10, 10, None),
    FieldSchema("z4", float, 20, 10, None),
)

_CONSTRAINEDJOINTCOORPULLEY_CARD5 = (
    FieldSchema("x5", float, 0, 10, None),
    FieldSchema("y5", float, 10, 10, None),
    FieldSchema("z5", float, 20, 10, None),
)

_CONSTRAINEDJOINTCOORPULLEY_CARD6 = (
    FieldSchema("x6", float, 0, 10, None),
    FieldSchema("y6", float, 10, 10, None),
    FieldSchema("z6", float, 20, 10, None),
)

_CONSTRAINEDJOINTCOORPULLEY_CARD7 = (
    FieldSchema("parm", float, 0, 10, None),
    FieldSchema("lcid", int, 10, 10, 0),
    FieldSchema("type", int, 20, 10, 0),
    FieldSchema("r1", float, 30, 10, None),
)

class ConstrainedJointCoorPulley(KeywordBase):
    """DYNA CONSTRAINED_JOINT_COOR_PULLEY keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "JOINT_COOR_PULLEY"
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the ConstrainedJointCoorPulley class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDJOINTCOORPULLEY_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDJOINTCOORPULLEY_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDJOINTCOORPULLEY_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDJOINTCOORPULLEY_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDJOINTCOORPULLEY_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDJOINTCOORPULLEY_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDJOINTCOORPULLEY_CARD6,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDJOINTCOORPULLEY_CARD7,
                **kwargs,
            ),        ]
    @property
    def rbid_a(self) -> typing.Optional[int]:
        """Get or set the Part ID of rigid body A
        """ # nopep8
        return self._cards[0].get_value("rbid_a")

    @rbid_a.setter
    def rbid_a(self, value: int) -> None:
        """Set the rbid_a property."""
        self._cards[0].set_value("rbid_a", value)

    @property
    def rbid_b(self) -> typing.Optional[int]:
        """Get or set the Part ID of rigid body
        """ # nopep8
        return self._cards[0].get_value("rbid_b")

    @rbid_b.setter
    def rbid_b(self, value: int) -> None:
        """Set the rbid_b property."""
        self._cards[0].set_value("rbid_b", value)

    @property
    def rps(self) -> float:
        """Get or set the Relative penalty stiffness (default = 1.0).
        """ # nopep8
        return self._cards[0].get_value("rps")

    @rps.setter
    def rps(self, value: float) -> None:
        """Set the rps property."""
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
        """Set the damp property."""
        self._cards[0].set_value("damp", value)

    @property
    def tmass(self) -> typing.Optional[float]:
        """Get or set the Lumped translational mass.  The mass is equally split between the first points defined for rigid bodies A and B.
        """ # nopep8
        return self._cards[0].get_value("tmass")

    @tmass.setter
    def tmass(self, value: float) -> None:
        """Set the tmass property."""
        self._cards[0].set_value("tmass", value)

    @property
    def rmass(self) -> typing.Optional[float]:
        """Get or set the Lumped rotational inertia.  The inertia is equally split between the first points defined for rigid bodies A and B.
        """ # nopep8
        return self._cards[0].get_value("rmass")

    @rmass.setter
    def rmass(self, value: float) -> None:
        """Set the rmass property."""
        self._cards[0].set_value("rmass", value)

    @property
    def x1(self) -> typing.Optional[float]:
        """Get or set the Coordinate of point 1, in rigid body A.  Define for all joint types.
        """ # nopep8
        return self._cards[1].get_value("x1")

    @x1.setter
    def x1(self, value: float) -> None:
        """Set the x1 property."""
        self._cards[1].set_value("x1", value)

    @property
    def y1(self) -> typing.Optional[float]:
        """Get or set the Coordinate of point 1, in rigid body A.  Define for all joint types.
        """ # nopep8
        return self._cards[1].get_value("y1")

    @y1.setter
    def y1(self, value: float) -> None:
        """Set the y1 property."""
        self._cards[1].set_value("y1", value)

    @property
    def z1(self) -> typing.Optional[float]:
        """Get or set the Coordinate of point 1, in rigid body A.  Define for all joint types.
        """ # nopep8
        return self._cards[1].get_value("z1")

    @z1.setter
    def z1(self, value: float) -> None:
        """Set the z1 property."""
        self._cards[1].set_value("z1", value)

    @property
    def x2(self) -> typing.Optional[float]:
        """Get or set the Coordinate of  point 2, in rigid body B.  If points 1 and 2 are coincident in the specified joint type, the coordinate for point 1 is used.
        """ # nopep8
        return self._cards[2].get_value("x2")

    @x2.setter
    def x2(self, value: float) -> None:
        """Set the x2 property."""
        self._cards[2].set_value("x2", value)

    @property
    def y2(self) -> typing.Optional[float]:
        """Get or set the Coordinate of  point 2, in rigid body B.  If points 1 and 2 are coincident in the specified joint type, the coordinate for point 1 is used.
        """ # nopep8
        return self._cards[2].get_value("y2")

    @y2.setter
    def y2(self, value: float) -> None:
        """Set the y2 property."""
        self._cards[2].set_value("y2", value)

    @property
    def z2(self) -> typing.Optional[float]:
        """Get or set the Coordinate of  point 2, in rigid body B.  If points 1 and 2 are coincident in the specified joint type, the coordinate for point 1 is used.
        """ # nopep8
        return self._cards[2].get_value("z2")

    @z2.setter
    def z2(self, value: float) -> None:
        """Set the z2 property."""
        self._cards[2].set_value("z2", value)

    @property
    def x3(self) -> typing.Optional[float]:
        """Get or set the Coordinate of point 3, in rigid body A.  Define for all joint types.
        """ # nopep8
        return self._cards[3].get_value("x3")

    @x3.setter
    def x3(self, value: float) -> None:
        """Set the x3 property."""
        self._cards[3].set_value("x3", value)

    @property
    def y3(self) -> typing.Optional[float]:
        """Get or set the Coordinate of point 3, in rigid body A.  Define for all joint types.
        """ # nopep8
        return self._cards[3].get_value("y3")

    @y3.setter
    def y3(self, value: float) -> None:
        """Set the y3 property."""
        self._cards[3].set_value("y3", value)

    @property
    def z3(self) -> typing.Optional[float]:
        """Get or set the Coordinate of point 3, in rigid body A.  Define for all joint types.
        """ # nopep8
        return self._cards[3].get_value("z3")

    @z3.setter
    def z3(self, value: float) -> None:
        """Set the z3 property."""
        self._cards[3].set_value("z3", value)

    @property
    def x4(self) -> typing.Optional[float]:
        """Get or set the Coordinate of  point 4, in rigid body B.  If points 3 and 4 are coincident in the specified joint type, the coordinate for point 3 is used.
        """ # nopep8
        return self._cards[4].get_value("x4")

    @x4.setter
    def x4(self, value: float) -> None:
        """Set the x4 property."""
        self._cards[4].set_value("x4", value)

    @property
    def y4(self) -> typing.Optional[float]:
        """Get or set the Coordinate of  point 4, in rigid body B.  If points 3 and 4 are coincident in the specified joint type, the coordinate for point 3 is used.
        """ # nopep8
        return self._cards[4].get_value("y4")

    @y4.setter
    def y4(self, value: float) -> None:
        """Set the y4 property."""
        self._cards[4].set_value("y4", value)

    @property
    def z4(self) -> typing.Optional[float]:
        """Get or set the Coordinate of  point 4, in rigid body B.  If points 3 and 4 are coincident in the specified joint type, the coordinate for point 3 is used.
        """ # nopep8
        return self._cards[4].get_value("z4")

    @z4.setter
    def z4(self, value: float) -> None:
        """Set the z4 property."""
        self._cards[4].set_value("z4", value)

    @property
    def x5(self) -> typing.Optional[float]:
        """Get or set the Coordinate of point 5, in rigid body A.  Define for all joint types.
        """ # nopep8
        return self._cards[5].get_value("x5")

    @x5.setter
    def x5(self, value: float) -> None:
        """Set the x5 property."""
        self._cards[5].set_value("x5", value)

    @property
    def y5(self) -> typing.Optional[float]:
        """Get or set the Coordinate of point 5, in rigid body A.  Define for all joint types.
        """ # nopep8
        return self._cards[5].get_value("y5")

    @y5.setter
    def y5(self, value: float) -> None:
        """Set the y5 property."""
        self._cards[5].set_value("y5", value)

    @property
    def z5(self) -> typing.Optional[float]:
        """Get or set the Coordinate of point 5, in rigid body A.  Define for all joint types.
        """ # nopep8
        return self._cards[5].get_value("z5")

    @z5.setter
    def z5(self, value: float) -> None:
        """Set the z5 property."""
        self._cards[5].set_value("z5", value)

    @property
    def x6(self) -> typing.Optional[float]:
        """Get or set the Coordinate of  point 6, in rigid body B.  If points 5 and 6 are coincident in the specified joint type, the coordinate for point 5 is used.
        """ # nopep8
        return self._cards[6].get_value("x6")

    @x6.setter
    def x6(self, value: float) -> None:
        """Set the x6 property."""
        self._cards[6].set_value("x6", value)

    @property
    def y6(self) -> typing.Optional[float]:
        """Get or set the Coordinate of  point 6, in rigid body B.  If points 5 and 6 are coincident in the specified joint type, the coordinate for point 5 is used.
        """ # nopep8
        return self._cards[6].get_value("y6")

    @y6.setter
    def y6(self, value: float) -> None:
        """Set the y6 property."""
        self._cards[6].set_value("y6", value)

    @property
    def z6(self) -> typing.Optional[float]:
        """Get or set the Coordinate of  point 6, in rigid body B.  If points 5 and 6 are coincident in the specified joint type, the coordinate for point 5 is used.
        """ # nopep8
        return self._cards[6].get_value("z6")

    @z6.setter
    def z6(self, value: float) -> None:
        """Set the z6 property."""
        self._cards[6].set_value("z6", value)

    @property
    def parm(self) -> typing.Optional[float]:
        """Get or set the Parameter which a function of joint type.  Leave blank for MOTORS
        """ # nopep8
        return self._cards[7].get_value("parm")

    @parm.setter
    def parm(self, value: float) -> None:
        """Set the parm property."""
        self._cards[7].set_value("parm", value)

    @property
    def lcid(self) -> int:
        """Get or set the Define load curve ID for MOTOR joints.
        """ # nopep8
        return self._cards[7].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[7].set_value("lcid", value)

    @property
    def type(self) -> int:
        """Get or set the Define integer flag for MOTOR joints as follows:
        EQ.0:  translational/rotational velocity
        EQ.1:  translational/rotational acceleration
        EQ.2:  translational/rotational displacement
        """ # nopep8
        return self._cards[7].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        """Set the type property."""
        self._cards[7].set_value("type", value)

    @property
    def r1(self) -> typing.Optional[float]:
        """Get or set the Radius, R_1, for the gear and pulley joint type.  If undefined, nodal points 5 and 6 are assumed to be on the outer radius. The values of R1 and R2 affect the outputted reaction forces. The forces are calculated from the moments by dividing them by the radii
        """ # nopep8
        return self._cards[7].get_value("r1")

    @r1.setter
    def r1(self, value: float) -> None:
        """Set the r1 property."""
        self._cards[7].set_value("r1", value)

    @property
    def lcid_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid:
                return kwd
        return None

    @lcid_link.setter
    def lcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid."""
        self.lcid = value.lcid

