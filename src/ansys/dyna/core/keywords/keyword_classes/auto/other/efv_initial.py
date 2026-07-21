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

"""Module providing the EfvInitial class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVINITIAL_CARD0 = (
    FieldSchema("inid", int, 0, 10, None),
)

_EFVINITIAL_CARD1 = (
    FieldSchema("rho0", float, 0, 10, None),
    FieldSchema("e0", float, 10, 10, None),
)

_EFVINITIAL_CARD2 = (
    FieldSchema("v0x", float, 0, 10, None),
    FieldSchema("v0y", float, 10, 10, None),
    FieldSchema("v0z", float, 20, 10, None),
)

_EFVINITIAL_CARD3 = (
    FieldSchema("vra", float, 0, 10, None),
    FieldSchema("sym", int, 10, 10, 0),
)

_EFVINITIAL_CARD4 = (
    FieldSchema("v0r", float, 0, 10, None),
    FieldSchema("xo", float, 10, 10, None),
    FieldSchema("yo", float, 20, 10, None),
    FieldSchema("zo", float, 30, 10, None),
    FieldSchema("xdr", float, 40, 10, None),
    FieldSchema("ydr", float, 50, 10, None),
    FieldSchema("zdr", float, 60, 10, None),
)

class EfvInitial(KeywordBase):
    """DYNA EFV_INITIAL keyword"""

    keyword = "EFV"
    subkeyword = "INITIAL"

    def __init__(self, **kwargs):
        """Initialize the EfvInitial class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVINITIAL_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVINITIAL_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVINITIAL_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVINITIAL_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVINITIAL_CARD4,
                **kwargs,
            ),
        ]
    @property
    def inid(self) -> typing.Optional[int]:
        """Get or set the ID of *EFV_INITIAL.  A unique number must be specified.
        """ # nopep8
        return self._cards[0].get_value("inid")

    @inid.setter
    def inid(self, value: int) -> None:
        """Set the inid property."""
        self._cards[0].set_value("inid", value)

    @property
    def rho0(self) -> typing.Optional[float]:
        """Get or set the Initial density
        """ # nopep8
        return self._cards[1].get_value("rho0")

    @rho0.setter
    def rho0(self, value: float) -> None:
        """Set the rho0 property."""
        self._cards[1].set_value("rho0", value)

    @property
    def e0(self) -> typing.Optional[float]:
        """Get or set the Initial internal energy
        """ # nopep8
        return self._cards[1].get_value("e0")

    @e0.setter
    def e0(self, value: float) -> None:
        """Set the e0 property."""
        self._cards[1].set_value("e0", value)

    @property
    def v0x(self) -> typing.Optional[float]:
        """Get or set the Initial translational velocities
        """ # nopep8
        return self._cards[2].get_value("v0x")

    @v0x.setter
    def v0x(self, value: float) -> None:
        """Set the v0x property."""
        self._cards[2].set_value("v0x", value)

    @property
    def v0y(self) -> typing.Optional[float]:
        """Get or set the Initial translational velocities
        """ # nopep8
        return self._cards[2].get_value("v0y")

    @v0y.setter
    def v0y(self, value: float) -> None:
        """Set the v0y property."""
        self._cards[2].set_value("v0y", value)

    @property
    def v0z(self) -> typing.Optional[float]:
        """Get or set the Initial translational velocities
        """ # nopep8
        return self._cards[2].get_value("v0z")

    @v0z.setter
    def v0z(self, value: float) -> None:
        """Set the v0z property."""
        self._cards[2].set_value("v0z", value)

    @property
    def vra(self) -> typing.Optional[float]:
        """Get or set the Initial radial velocity. Its direction depends on SYM
        """ # nopep8
        return self._cards[3].get_value("vra")

    @vra.setter
    def vra(self, value: float) -> None:
        """Set the vra property."""
        self._cards[3].set_value("vra", value)

    @property
    def sym(self) -> int:
        """Get or set the Symmetry flag giving a direction to the radial velocity VRA:
        EQ.0: Spherical
        EQ.1: Cylindrical around the global x - axis
        EQ.2: Cylindrical around the global y - axis
        EQ.3: Cylindrical around the global z - axis
        """ # nopep8
        return self._cards[3].get_value("sym")

    @sym.setter
    def sym(self, value: int) -> None:
        """Set the sym property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""sym must be `None` or one of {0,1,2,3}.""")
        self._cards[3].set_value("sym", value)

    @property
    def v0r(self) -> typing.Optional[float]:
        """Get or set the Initial velocity of rotation around an axis oriented by the vector (XDR,YDR,ZDR) and passing through the position (XO,YO,ZO).
        """ # nopep8
        return self._cards[4].get_value("v0r")

    @v0r.setter
    def v0r(self, value: float) -> None:
        """Set the v0r property."""
        self._cards[4].set_value("v0r", value)

    @property
    def xo(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point on the axis of rotation
        """ # nopep8
        return self._cards[4].get_value("xo")

    @xo.setter
    def xo(self, value: float) -> None:
        """Set the xo property."""
        self._cards[4].set_value("xo", value)

    @property
    def yo(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point on the axis of rotation
        """ # nopep8
        return self._cards[4].get_value("yo")

    @yo.setter
    def yo(self, value: float) -> None:
        """Set the yo property."""
        self._cards[4].set_value("yo", value)

    @property
    def zo(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point on the axis of rotation
        """ # nopep8
        return self._cards[4].get_value("zo")

    @zo.setter
    def zo(self, value: float) -> None:
        """Set the zo property."""
        self._cards[4].set_value("zo", value)

    @property
    def xdr(self) -> typing.Optional[float]:
        """Get or set the Coordinates of the vector orienting the axis of rotation
        """ # nopep8
        return self._cards[4].get_value("xdr")

    @xdr.setter
    def xdr(self, value: float) -> None:
        """Set the xdr property."""
        self._cards[4].set_value("xdr", value)

    @property
    def ydr(self) -> typing.Optional[float]:
        """Get or set the Coordinates of the vector orienting the axis of rotation
        """ # nopep8
        return self._cards[4].get_value("ydr")

    @ydr.setter
    def ydr(self, value: float) -> None:
        """Set the ydr property."""
        self._cards[4].set_value("ydr", value)

    @property
    def zdr(self) -> typing.Optional[float]:
        """Get or set the Coordinates of the vector orienting the axis of rotation
        """ # nopep8
        return self._cards[4].get_value("zdr")

    @zdr.setter
    def zdr(self, value: float) -> None:
        """Set the zdr property."""
        self._cards[4].set_value("zdr", value)

