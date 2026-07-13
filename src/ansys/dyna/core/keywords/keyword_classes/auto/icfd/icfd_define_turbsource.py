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
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_ICFDDEFINETURBSOURCE_CARD0 = (
    FieldSchema("sid", int, 0, 10, None),
    FieldSchema("lcidk", int, 10, 10, None),
    FieldSchema("lcidep", int, 20, 10, None),
    FieldSchema("lcidnu", int, 30, 10, None),
    FieldSchema("ishape", int, 40, 10, 1),
    FieldSchema("r", float, 50, 10, None),
    FieldSchema("ptid1", int, 60, 10, None),
    FieldSchema("ptid2", int, 70, 10, None),
)

class IcfdDefineTurbsource(KeywordBase):
    """DYNA ICFD_DEFINE_TURBSOURCE keyword"""

    keyword = "ICFD"
    subkeyword = "DEFINE_TURBSOURCE"
    _link_fields = {
        "lcidk": LinkType.DEFINE_CURVE,
        "lcidep": LinkType.DEFINE_CURVE,
        "lcidnu": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the IcfdDefineTurbsource class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDDEFINETURBSOURCE_CARD0,
                **kwargs,
            ),
        ]
    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Turbulent external source ID
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def lcidk(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying the evolution of the external source term function of time for the turbulent kinetic energy, k, equation (see *DEFINE_CURVE,*DEFINE_CURVE_FUNCTION, or *DEFINE_FUNCTION). If using a *DEFINE_FUNCTION, the following arguments are allowed: f(x, y, z, vx, vy, vz, temp, pres, time).
        """ # nopep8
        return self._cards[0].get_value("lcidk")

    @lcidk.setter
    def lcidk(self, value: int) -> None:
        """Set the lcidk property."""
        self._cards[0].set_value("lcidk", value)

    @property
    def lcidep(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying the evolution of the external source term function of time for the turbulent diffusion, e, or specific rate of dissipation, w, equation (see *DEFINE_CURVE,*DEFINE_CURVE_FUNCTION, or *DEFINE_FUNCTION). If using a *DEFINE_FUNCTION, the following arguments are allowed: f(x, y, z, vx, vy, vz, temp, pres, time).
        """ # nopep8
        return self._cards[0].get_value("lcidep")

    @lcidep.setter
    def lcidep(self, value: int) -> None:
        """Set the lcidep property."""
        self._cards[0].set_value("lcidep", value)

    @property
    def lcidnu(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying the evolution of the external source term function of time for the kinematic eddy turbulent viscosity equation used in the Spalart-Allmaras model (see *DEFINE_CURVE,*DEFINE_CURVE_FUNCTION, or *DEFINE_FUNCTION). If using a *DEFINE_FUNCTION, the following arguments are allowed: f(x, y, z, vx, vy, vz, temp, pres, time).
        """ # nopep8
        return self._cards[0].get_value("lcidnu")

    @lcidnu.setter
    def lcidnu(self, value: int) -> None:
        """Set the lcidnu property."""
        self._cards[0].set_value("lcidnu", value)

    @property
    def ishape(self) -> int:
        """Get or set the Shape of the external source:
        EQ.1: Box shape
        EQ.2: Cylinder shape
        EQ.3: Sphere shape
        """ # nopep8
        return self._cards[0].get_value("ishape")

    @ishape.setter
    def ishape(self, value: int) -> None:
        """Set the ishape property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""ishape must be `None` or one of {1,2,3}.""")
        self._cards[0].set_value("ishape", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Radius of the cylinder if ISHAPE = 2 or radius of the sphere if ISHAPE = 3
        """ # nopep8
        return self._cards[0].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        """Set the r property."""
        self._cards[0].set_value("r", value)

    @property
    def ptid1(self) -> typing.Optional[int]:
        """Get or set the Point ID (see *ICFD_DEFINE_POINT) whose meaning depends on ISHAPE:
        ISHAPE.EQ.1: Minimum coordinates of the boxISHAPE.
        EQ.2: Point in the center of one base of the cylinderISHAPE.
        EQ.3: Center of the sphere
        """ # nopep8
        return self._cards[0].get_value("ptid1")

    @ptid1.setter
    def ptid1(self, value: int) -> None:
        """Set the ptid1 property."""
        self._cards[0].set_value("ptid1", value)

    @property
    def ptid2(self) -> typing.Optional[int]:
        """Get or set the Point ID (see *ICFD_DEFINE_POINT) whose meaning depends on ISHAPE:
        ISHAPE.EQ.1: Maximum coordinates of the boxISHAPE.
        EQ.2: Point in the center of the other base of the cylinder
        """ # nopep8
        return self._cards[0].get_value("ptid2")

    @ptid2.setter
    def ptid2(self, value: int) -> None:
        """Set the ptid2 property."""
        self._cards[0].set_value("ptid2", value)

    @property
    def lcidk_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidk."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidk:
                return kwd
        return None

    @lcidk_link.setter
    def lcidk_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidk."""
        self.lcidk = value.lcid

    @property
    def lcidep_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidep."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidep:
                return kwd
        return None

    @lcidep_link.setter
    def lcidep_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidep."""
        self.lcidep = value.lcid

    @property
    def lcidnu_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidnu."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidnu:
                return kwd
        return None

    @lcidnu_link.setter
    def lcidnu_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidnu."""
        self.lcidnu = value.lcid

