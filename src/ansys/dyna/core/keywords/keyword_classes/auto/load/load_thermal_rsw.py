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

"""Module providing the LoadThermalRsw class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_LOADTHERMALRSW_CARD0 = (
    FieldSchema("deftemp", int, 0, 10, None),
)

_LOADTHERMALRSW_CARD1 = (
    FieldSchema("sid", int, 0, 10, None),
    FieldSchema("option", int, 10, 10, 0),
    FieldSchema("nid1", int, 20, 10, None),
    FieldSchema("nid2", int, 30, 10, None),
    FieldSchema("tdeath", float, 40, 10, 1e+20),
    FieldSchema("tbirth", float, 50, 10, 0.0),
    FieldSchema("loc", int, 60, 10, 0),
    FieldSchema("geoup", int, 70, 10, 0),
)

_LOADTHERMALRSW_CARD2 = (
    FieldSchema("dist", float, 0, 10, 0.0),
    FieldSchema("h1", float, 10, 10, 0.0),
    FieldSchema("h2", float, 20, 10, 0.0),
    FieldSchema("r", float, 30, 10, 0.0),
    FieldSchema("tempc", float, 40, 10, 0.0),
    FieldSchema("tempb", float, 50, 10, 0.0),
    FieldSchema("lcidt", int, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

_LOADTHERMALRSW_CARD3 = (
    FieldSchema("hz1", float, 0, 10, None),
    FieldSchema("hz2", float, 10, 10, 0.0),
    FieldSchema("rz", float, 20, 10, 0.0),
    FieldSchema("tempzb", float, 30, 10, 0.0),
)

class LoadThermalRsw(KeywordBase):
    """DYNA LOAD_THERMAL_RSW keyword"""

    keyword = "LOAD"
    subkeyword = "THERMAL_RSW"
    _link_fields = {
        "nid1": LinkType.NODE,
        "nid2": LinkType.NODE,
        "h2": LinkType.NODE,
        "r": LinkType.NODE,
        "lcidt": LinkType.DEFINE_CURVE,
        "sid": LinkType.SET_NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the LoadThermalRsw class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADTHERMALRSW_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADTHERMALRSW_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADTHERMALRSW_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADTHERMALRSW_CARD3,
                **kwargs,
            ),        ]
    @property
    def deftemp(self) -> typing.Optional[int]:
        """Get or set the Default temperature outside the nuggets and heat affected zones
        """ # nopep8
        return self._cards[0].get_value("deftemp")

    @deftemp.setter
    def deftemp(self, value: int) -> None:
        """Set the deftemp property."""
        self._cards[0].set_value("deftemp", value)

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Node Set ID; see *SET_‌NODE_‌OPTION. Nodes in the set will be checked to see if they are in the nugget or heat affected zone. If they are, the boundary condition will be applied. The boundary condition will not be applied to nodes in these regions if they are not included in the set..
        """ # nopep8
        return self._cards[1].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[1].set_value("sid", value)

    @property
    def option(self) -> int:
        """Get or set the Option for heat affected zone around the weld nugget:
        EQ.0: no heat affected zone
        EQ.1: ellipsoidal region considered
        """ # nopep8
        return self._cards[1].get_value("option")

    @option.setter
    def option(self, value: int) -> None:
        """Set the option property."""
        if value not in [0, 1, None]:
            raise Exception("""option must be `None` or one of {0,1}.""")
        self._cards[1].set_value("option", value)

    @property
    def nid1(self) -> typing.Optional[int]:
        """Get or set the Node defining the tail of the orientation vector (axis of rotation of
        the ellipsoidal region) and the base for positioning of the nugget.
        See Remarks 1 and 2.
        """ # nopep8
        return self._cards[1].get_value("nid1")

    @nid1.setter
    def nid1(self, value: int) -> None:
        """Set the nid1 property."""
        self._cards[1].set_value("nid1", value)

    @property
    def nid2(self) -> typing.Optional[int]:
        """Get or set the Node defining the head of the orientation vector (axis of rotation
        of the ellipsoidal region). See Remarks 1 and 2.
        """ # nopep8
        return self._cards[1].get_value("nid2")

    @nid2.setter
    def nid2(self, value: int) -> None:
        """Set the nid2 property."""
        self._cards[1].set_value("nid2", value)

    @property
    def tdeath(self) -> float:
        """Get or set the Deactivation time for temperature boundary condition. At this
        point in time the temperature constraint is removed.
        """ # nopep8
        return self._cards[1].get_value("tdeath")

    @tdeath.setter
    def tdeath(self, value: float) -> None:
        """Set the tdeath property."""
        self._cards[1].set_value("tdeath", value)

    @property
    def tbirth(self) -> float:
        """Get or set the Activation time for temperature boundary condition. Before this
        point in time the temperature constraint is ignored
        """ # nopep8
        return self._cards[1].get_value("tbirth")

    @tbirth.setter
    def tbirth(self, value: float) -> None:
        """Set the tbirth property."""
        self._cards[1].set_value("tbirth", value)

    @property
    def loc(self) -> int:
        """Get or set the Application of surface for thermal shell elements, see parameter,
        THSHEL, in the *CONTROL_SHELL input:
        EQ.-1: lower surface of thermal shell element
        EQ.0: middle surface of thermal shell element
        EQ.1: upper surface of thermal shell element.
        """ # nopep8
        return self._cards[1].get_value("loc")

    @loc.setter
    def loc(self, value: int) -> None:
        """Set the loc property."""
        if value not in [0, -1, 1, None]:
            raise Exception("""loc must be `None` or one of {0,-1,1}.""")
        self._cards[1].set_value("loc", value)

    @property
    def geoup(self) -> int:
        """Get or set the Number of times the geometry of the spot weld is updated between TBIRTH and TDEATH
        EQ.0:	Update geometry every time step
        """ # nopep8
        return self._cards[1].get_value("geoup")

    @geoup.setter
    def geoup(self, value: int) -> None:
        """Set the geoup property."""
        self._cards[1].set_value("geoup", value)

    @property
    def dist(self) -> float:
        """Get or set the Position of center of nugget on the axis of rotation. Parameter
        defines the distance to NID1 along the orientation vector. See	Remark 1..
        """ # nopep8
        return self._cards[2].get_value("dist")

    @dist.setter
    def dist(self, value: float) -> None:
        """Set the dist property."""
        self._cards[2].set_value("dist", value)

    @property
    def h1(self) -> float:
        """Get or set the Half width h1 of nugget in the lower half, i.e. in direction to NID1.	See Remark 2.
        """ # nopep8
        return self._cards[2].get_value("h1")

    @h1.setter
    def h1(self, value: float) -> None:
        """Set the h1 property."""
        self._cards[2].set_value("h1", value)

    @property
    def h2(self) -> float:
        """Get or set the Half width h2 of nugget in the upper half, i.e. in direction to NID2. See Remark 2.
        """ # nopep8
        return self._cards[2].get_value("h2")

    @h2.setter
    def h2(self, value: float) -> None:
        """Set the h2 property."""
        self._cards[2].set_value("h2", value)

    @property
    def r(self) -> float:
        """Get or set the Radius rweld of the nugget in surface normal to orientation vector. See Remark 2.
        """ # nopep8
        return self._cards[2].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        """Set the r property."""
        self._cards[2].set_value("r", value)

    @property
    def tempc(self) -> float:
        """Get or set the Base temperature at the center of the nugget. See Remark 3.
        """ # nopep8
        return self._cards[2].get_value("tempc")

    @tempc.setter
    def tempc(self, value: float) -> None:
        """Set the tempc property."""
        self._cards[2].set_value("tempc", value)

    @property
    def tempb(self) -> float:
        """Get or set the Base temperature at the boundary of the nugget. See Remark 3.
        """ # nopep8
        return self._cards[2].get_value("tempb")

    @tempb.setter
    def tempb(self, value: float) -> None:
        """Set the tempb property."""
        self._cards[2].set_value("tempb", value)

    @property
    def lcidt(self) -> typing.Optional[int]:
        """Get or set the |LCIDT| refers to the load curve ID prescribing the temperature evolution in the nugget as a function of time. The abscissa of the load curve will be normalized between the birth and death times of the boundary condition.
        GT.0:	The ordinate values of the load curve scale the respective base temperature of a particular point.
        EQ.0:	No temperature evolution. Base temperatures are used.
        LT.0:	The ordinate values of the load curve are used to define a linear combination between the temperature at the birth time and the base temperature of a particular point.Load curve ordinate values should range between 0.0 and 1.0.We recommend LCIDT < 0 to ensure a smooth temperature evolution.
        """ # nopep8
        return self._cards[2].get_value("lcidt")

    @lcidt.setter
    def lcidt(self, value: int) -> None:
        """Set the lcidt property."""
        self._cards[2].set_value("lcidt", value)

    @property
    def hz1(self) -> typing.Optional[float]:
        """Get or set the Half width hz1 of heat affected zone in the lower half, meaning in
        direction to NID1. Only active for OPTION = 1. See Remark 4.
        """ # nopep8
        return self._cards[3].get_value("hz1")

    @hz1.setter
    def hz1(self, value: float) -> None:
        """Set the hz1 property."""
        self._cards[3].set_value("hz1", value)

    @property
    def hz2(self) -> float:
        """Get or set the Half width hz2 of heat affected zone in the upper half, meaning in
        direction to NID1. Only active for OPTION = 1. See Remark 4.
        """ # nopep8
        return self._cards[3].get_value("hz2")

    @hz2.setter
    def hz2(self, value: float) -> None:
        """Set the hz2 property."""
        self._cards[3].set_value("hz2", value)

    @property
    def rz(self) -> float:
        """Get or set the Radius Rhaz of the heat affected zone in surface normal to
        orientation vector. See Remark 4.
        """ # nopep8
        return self._cards[3].get_value("rz")

    @rz.setter
    def rz(self, value: float) -> None:
        """Set the rz property."""
        self._cards[3].set_value("rz", value)

    @property
    def tempzb(self) -> float:
        """Get or set the Base temperature at the boundary of the heat affected zone
        for OPTION = 1. See Remark 4.
        """ # nopep8
        return self._cards[3].get_value("tempzb")

    @tempzb.setter
    def tempzb(self, value: float) -> None:
        """Set the tempzb property."""
        self._cards[3].set_value("tempzb", value)

    @property
    def nid1_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid1."""
        return self._get_link_by_attr("NODE", "nid", self.nid1, "parts")

    @property
    def nid2_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid2."""
        return self._get_link_by_attr("NODE", "nid", self.nid2, "parts")

    @property
    def h2_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given h2."""
        return self._get_link_by_attr("NODE", "nid", self.h2, "parts")

    @property
    def r_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given r."""
        return self._get_link_by_attr("NODE", "nid", self.r, "parts")

    @property
    def lcidt_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidt."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidt:
                return kwd
        return None

    @lcidt_link.setter
    def lcidt_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidt."""
        self.lcidt = value.lcid

    @property
    def sid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for sid."""
        return self._get_set_link("NODE", self.sid)

    @sid_link.setter
    def sid_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for sid."""
        self.sid = value.sid

