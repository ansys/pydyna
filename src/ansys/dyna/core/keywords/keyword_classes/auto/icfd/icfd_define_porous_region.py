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

"""Module providing the IcfdDefinePorousRegion class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_ICFDDEFINEPOROUSREGION_CARD0 = (
    FieldSchema("prid", int, 0, 10, None),
    FieldSchema("ishape", int, 10, 10, None),
    FieldSchema("r", float, 20, 10, None),
    FieldSchema("ptid1", int, 30, 10, None),
    FieldSchema("ptid2", int, 40, 10, None),
    FieldSchema("perm", float, 50, 10, None),
    FieldSchema("lcid", int, 60, 10, None),
    FieldSchema("sensfl", int, 70, 10, 0),
)

_ICFDDEFINEPOROUSREGION_CARD1 = (
    FieldSchema("sensid1", int, 0, 10, None),
    FieldSchema("sensid2", int, 10, 10, None),
    FieldSchema("kmin", float, 20, 10, None),
    FieldSchema("kmax", float, 30, 10, None),
    FieldSchema("dltapref", float, 40, 10, 0.0),
    FieldSchema("slope", float, 50, 10, 0.0),
)

class IcfdDefinePorousRegion(KeywordBase):
    """DYNA ICFD_DEFINE_POROUS_REGION keyword"""

    keyword = "ICFD"
    subkeyword = "DEFINE_POROUS_REGION"
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the IcfdDefinePorousRegion class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDDEFINEPOROUSREGION_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ICFDDEFINEPOROUSREGION_CARD1,
                **kwargs,
            ),
        ]
    @property
    def prid(self) -> typing.Optional[int]:
        """Get or set the Porous region ID
        """ # nopep8
        return self._cards[0].get_value("prid")

    @prid.setter
    def prid(self, value: int) -> None:
        """Set the prid property."""
        self._cards[0].set_value("prid", value)

    @property
    def ishape(self) -> typing.Optional[int]:
        """Get or set the Shape of the volumetric porous region:
        EQ.1: Box shape,
        EQ.2: Cylinder shape,
        EQ.3: Sphere shape,
        """ # nopep8
        return self._cards[0].get_value("ishape")

    @ishape.setter
    def ishape(self, value: int) -> None:
        """Set the ishape property."""
        self._cards[0].set_value("ishape", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Radius of the cylinder if SHAPE =2, radius of the sphere if SHAPE = 3
        """ # nopep8
        return self._cards[0].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        """Set the r property."""
        self._cards[0].set_value("r", value)

    @property
    def ptid1(self) -> typing.Optional[int]:
        """Get or set the Point ID (see *ICFD_DEFINE_POINT) with meaning depending on ISHAPE:
        ISHAPE.EQ.1: Minimum coordinates of the box
        ISHAPE.EQ.2: Tail point of the cylinder
        """ # nopep8
        return self._cards[0].get_value("ptid1")

    @ptid1.setter
    def ptid1(self, value: int) -> None:
        """Set the ptid1 property."""
        self._cards[0].set_value("ptid1", value)

    @property
    def ptid2(self) -> typing.Optional[int]:
        """Get or set the Point ID with meaning depending on ISHAPE (ignored for ISHAPE = 3):
        ISHAPE.EQ.1: Maximum coordinates of the box
        ISHAPE.EQ.2: Head point of the cylinder.
        """ # nopep8
        return self._cards[0].get_value("ptid2")

    @ptid2.setter
    def ptid2(self, value: int) -> None:
        """Set the ptid2 property."""
        self._cards[0].set_value("ptid2", value)

    @property
    def perm(self) -> typing.Optional[float]:
        """Get or set the Isotropic permeability of the region
        """ # nopep8
        return self._cards[0].get_value("perm")

    @perm.setter
    def perm(self, value: float) -> None:
        """Set the perm property."""
        self._cards[0].set_value("perm", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying the evolution of the permeability as a function of time for the X, Y, and Z degrees of freedom; see *DEFINE_CURVE, *DEFINE_CURVE_FUNCTION, or *DEFINE_FUNCTION. If a using a *DEFINE_FUNCTION, the following parameters are allowed: f(x, y, z, vx, vy, vz, temp, pres, time).
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def sensfl(self) -> int:
        """Get or set the Sensor flag determining whether the permeability is a function of the pressure drop computed between the sensors:
        EQ.0: Not a function of pressure drop
        EQ.1: Function of a pressure drop between sensors
        """ # nopep8
        return self._cards[0].get_value("sensfl")

    @sensfl.setter
    def sensfl(self, value: int) -> None:
        """Set the sensfl property."""
        if value not in [0, 1, None]:
            raise Exception("""sensfl must be `None` or one of {0,1}.""")
        self._cards[0].set_value("sensfl", value)

    @property
    def sensid1(self) -> typing.Optional[int]:
        """Get or set the Sensor (ICFD_DEFINE_POINT ID) upstream of the porous region
        """ # nopep8
        return self._cards[1].get_value("sensid1")

    @sensid1.setter
    def sensid1(self, value: int) -> None:
        """Set the sensid1 property."""
        self._cards[1].set_value("sensid1", value)

    @property
    def sensid2(self) -> typing.Optional[int]:
        """Get or set the Sensor (ICFD_DEFINE_POINT ID) downstream, of the porous region
        """ # nopep8
        return self._cards[1].get_value("sensid2")

    @sensid2.setter
    def sensid2(self, value: int) -> None:
        """Set the sensid2 property."""
        self._cards[1].set_value("sensid2", value)

    @property
    def kmin(self) -> typing.Optional[float]:
        """Get or set the Minimum permeability for the valve (closed position ),
        """ # nopep8
        return self._cards[1].get_value("kmin")

    @kmin.setter
    def kmin(self, value: float) -> None:
        """Set the kmin property."""
        self._cards[1].set_value("kmin", value)

    @property
    def kmax(self) -> typing.Optional[float]:
        """Get or set the Maximum permebility for the valve (open position),
        """ # nopep8
        return self._cards[1].get_value("kmax")

    @kmax.setter
    def kmax(self, value: float) -> None:
        """Set the kmax property."""
        self._cards[1].set_value("kmax", value)

    @property
    def dltapref(self) -> float:
        """Get or set the Pressure drop that opens/closes the valve,
        """ # nopep8
        return self._cards[1].get_value("dltapref")

    @dltapref.setter
    def dltapref(self, value: float) -> None:
        """Set the dltapref property."""
        self._cards[1].set_value("dltapref", value)

    @property
    def slope(self) -> float:
        """Get or set the Slope of the transition from KMAX to KMIN
        """ # nopep8
        return self._cards[1].get_value("slope")

    @slope.setter
    def slope(self, value: float) -> None:
        """Set the slope property."""
        self._cards[1].set_value("slope", value)

    @property
    def lcid_link(self) -> typing.Optional[DefineCurve]:
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

