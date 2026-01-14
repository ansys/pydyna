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

"""Module providing the BoundaryPrescribedOrientationRigidVector class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BOUNDARYPRESCRIBEDORIENTATIONRIGIDVECTOR_CARD0 = (
    FieldSchema("pidb", int, 0, 10, None),
    FieldSchema("pida", int, 10, 10, None),
    FieldSchema("intrp", int, 20, 10, 1),
    FieldSchema("birth", float, 30, 10, 0.0),
    FieldSchema("death", float, 40, 10, 1e+20),
    FieldSchema("toffset", int, 50, 10, 0),
)

_BOUNDARYPRESCRIBEDORIENTATIONRIGIDVECTOR_CARD1 = (
    FieldSchema("lcidv1", int, 0, 10, None),
    FieldSchema("lcidv2", int, 10, 10, None),
    FieldSchema("lcidv3", int, 20, 10, None),
    FieldSchema("lcids", int, 30, 10, None),
    FieldSchema("valspin", float, 40, 10, None),
)

class BoundaryPrescribedOrientationRigidVector(KeywordBase):
    """DYNA BOUNDARY_PRESCRIBED_ORIENTATION_RIGID_VECTOR keyword"""

    keyword = "BOUNDARY"
    subkeyword = "PRESCRIBED_ORIENTATION_RIGID_VECTOR"

    def __init__(self, **kwargs):
        """Initialize the BoundaryPrescribedOrientationRigidVector class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYPRESCRIBEDORIENTATIONRIGIDVECTOR_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _BOUNDARYPRESCRIBEDORIENTATIONRIGIDVECTOR_CARD1,
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
    def lcidv1(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying the vector measure number vi as a function of time
        """ # nopep8
        return self._cards[1].get_value("lcidv1")

    @lcidv1.setter
    def lcidv1(self, value: int) -> None:
        """Set the lcidv1 property."""
        self._cards[1].set_value("lcidv1", value)

    @property
    def lcidv2(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying the vector measure number vi as a function of time
        """ # nopep8
        return self._cards[1].get_value("lcidv2")

    @lcidv2.setter
    def lcidv2(self, value: int) -> None:
        """Set the lcidv2 property."""
        self._cards[1].set_value("lcidv2", value)

    @property
    def lcidv3(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying the vector measure number vi as a function of time
        """ # nopep8
        return self._cards[1].get_value("lcidv3")

    @lcidv3.setter
    def lcidv3(self, value: int) -> None:
        """Set the lcidv3 property."""
        self._cards[1].set_value("lcidv3", value)

    @property
    def lcids(self) -> typing.Optional[int]:
        """Get or set the Load curve ID which specifies the spin speed of PIDB about an axis parallel to the vector
        """ # nopep8
        return self._cards[1].get_value("lcids")

    @lcids.setter
    def lcids(self, value: int) -> None:
        """Set the lcids property."""
        self._cards[1].set_value("lcids", value)

    @property
    def valspin(self) -> typing.Optional[float]:
        """Get or set the Value for constant the spin speed of PIDB (radians per unit time).  This option is bypassed if the load curve number defined above is non zero.
        """ # nopep8
        return self._cards[1].get_value("valspin")

    @valspin.setter
    def valspin(self, value: float) -> None:
        """Set the valspin property."""
        self._cards[1].set_value("valspin", value)

