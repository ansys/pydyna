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

"""Module providing the IcfdDefineTurbsource class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDDEFINETURBSOURCE_CARD0 = (
    FieldSchema("sid", int, 0, 10, None),
    FieldSchema("lcidx", int, 10, 10, None),
    FieldSchema("lcidy", int, 20, 10, None),
    FieldSchema("lcidz", int, 30, 10, None),
    FieldSchema("shape", int, 40, 10, 1),
    FieldSchema("r", float, 50, 10, None),
    FieldSchema("ptid1", int, 60, 10, None),
    FieldSchema("ptid2", int, 70, 10, None),
)

class IcfdDefineTurbsource(KeywordBase):
    """DYNA ICFD_DEFINE_TURBSOURCE keyword"""

    keyword = "ICFD"
    subkeyword = "DEFINE_TURBSOURCE"

    def __init__(self, **kwargs):
        """Initialize the IcfdDefineTurbsource class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDDEFINETURBSOURCE_CARD0,
                **kwargs,
            ),        ]
    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the source ID
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def lcidx(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying the evolution of the external source term function of time for the turbulent kinetic energy k equation, see *DEFINE_CURVE,*DEFINE_CURVE_FUNCTION or *DEFINE_FUNCTION. If a DEFINE_FUNCTION is used, the following parameters are allowed:  f(x,y,z,vx,vy,vz,temp,pres,time
        """ # nopep8
        return self._cards[0].get_value("lcidx")

    @lcidx.setter
    def lcidx(self, value: int) -> None:
        """Set the lcidx property."""
        self._cards[0].set_value("lcidx", value)

    @property
    def lcidy(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying the evolution of the external source term function of time for the turbulent diffusion ε or specific rate of dissipation w equation, see *DEFINE_CURVE,*DEFINE_CURVE_FUNCTION or *DEFINE_FUNCTION. If a DEFINE_FUNCTION is used, the following parameters are allowed:  f(x,y,z,vx,vy,vz,temp,pres,time).
        """ # nopep8
        return self._cards[0].get_value("lcidy")

    @lcidy.setter
    def lcidy(self, value: int) -> None:
        """Set the lcidy property."""
        self._cards[0].set_value("lcidy", value)

    @property
    def lcidz(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying the evolution of the external source term function of time for the kinematic eddy turbulent viscosity equation used in the Spalart-Allmaras model, see *DEFINE_CURVE,*DEFINE_CURVE_FUNCTION or *DEFINE_FUNCTION. If a DEFINE_FUNCTION is used, the following parameters are allowed:  f(x,y,z,vx,vy,vz,temp,pres,time).
        """ # nopep8
        return self._cards[0].get_value("lcidz")

    @lcidz.setter
    def lcidz(self, value: int) -> None:
        """Set the lcidz property."""
        self._cards[0].set_value("lcidz", value)

    @property
    def shape(self) -> int:
        """Get or set the Shape of the external source:
        EQ.1 :	Box shape
        EQ.2 :	Cylinder shape
        EQ.3 :	Sphere shape
        """ # nopep8
        return self._cards[0].get_value("shape")

    @shape.setter
    def shape(self, value: int) -> None:
        """Set the shape property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""shape must be `None` or one of {1,2,3}.""")
        self._cards[0].set_value("shape", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Radius of the sphere is SHAPE=3
        """ # nopep8
        return self._cards[0].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        """Set the r property."""
        self._cards[0].set_value("r", value)

    @property
    def ptid1(self) -> typing.Optional[int]:
        """Get or set the ID of point (See ICFD_DEFINE_POINT) of minimum coordinates if SHAPE=1, tail point if SHAPE=2, origin if SHAPE=3.
        """ # nopep8
        return self._cards[0].get_value("ptid1")

    @ptid1.setter
    def ptid1(self, value: int) -> None:
        """Set the ptid1 property."""
        self._cards[0].set_value("ptid1", value)

    @property
    def ptid2(self) -> typing.Optional[int]:
        """Get or set the ID of point of maximum coordinates if SHAPE=2, head point if SHAPE=2.
        """ # nopep8
        return self._cards[0].get_value("ptid2")

    @ptid2.setter
    def ptid2(self, value: int) -> None:
        """Set the ptid2 property."""
        self._cards[0].set_value("ptid2", value)

