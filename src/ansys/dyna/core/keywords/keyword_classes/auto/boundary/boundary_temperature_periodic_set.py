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

"""Module providing the BoundaryTemperaturePeriodicSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_BOUNDARYTEMPERATUREPERIODICSET_CARD0 = (
    FieldSchema("ssid1", int, 0, 10, None),
    FieldSchema("ptype", int, 10, 10, None),
    FieldSchema("ssid2", int, 20, 10, None),
    FieldSchema("tdlcid", int, 30, 10, None),
    FieldSchema("axe", int, 40, 10, None),
    FieldSchema("nid", int, 50, 10, None),
    FieldSchema("angle", float, 60, 10, None),
)

class BoundaryTemperaturePeriodicSet(KeywordBase):
    """DYNA BOUNDARY_TEMPERATURE_PERIODIC_SET keyword"""

    keyword = "BOUNDARY"
    subkeyword = "TEMPERATURE_PERIODIC_SET"
    _link_fields = {
        "nid": LinkType.NODE,
        "tdlcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the BoundaryTemperaturePeriodicSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYTEMPERATUREPERIODICSET_CARD0,
                **kwargs,
            ),        ]
    @property
    def ssid1(self) -> typing.Optional[int]:
        """Get or set the First Segment set on which the periodic temperature boundary condition will be applied.
        """ # nopep8
        return self._cards[0].get_value("ssid1")

    @ssid1.setter
    def ssid1(self, value: int) -> None:
        """Set the ssid1 property."""
        self._cards[0].set_value("ssid1", value)

    @property
    def ptype(self) -> typing.Optional[int]:
        """Get or set the Type of periodic boundary condition:
        EQ.1:	Rotation boundary condition defined by an axis, an origin pointand a rotation angle.
        EQ.2 : Reflective boundary condition defined by an axis and origin point.
        EQ.3 : Sliding boundary condition.
        """ # nopep8
        return self._cards[0].get_value("ptype")

    @ptype.setter
    def ptype(self, value: int) -> None:
        """Set the ptype property."""
        self._cards[0].set_value("ptype", value)

    @property
    def ssid2(self) -> typing.Optional[int]:
        """Get or set the Second Segment set on which the periodic temperature boundary condition will be applied.
        """ # nopep8
        return self._cards[0].get_value("ssid2")

    @ssid2.setter
    def ssid2(self, value: int) -> None:
        """Set the ssid2 property."""
        self._cards[0].set_value("ssid2", value)

    @property
    def tdlcid(self) -> typing.Optional[int]:
        """Get or set the Optional load curve specifying the temperature drop, T_drop, between the two surfaces in the periodic boundary condition as a function of time. Note that T_drop =T_1-T_2 where T_1 is the temperature of the surface specified with SSID1 and T_2 is the temperature of the surface specified with SSID2.
        EQ.0:	No temperature drop between that surfacs, that is, T_drop = 0.0
        """ # nopep8
        return self._cards[0].get_value("tdlcid")

    @tdlcid.setter
    def tdlcid(self, value: int) -> None:
        """Set the tdlcid property."""
        self._cards[0].set_value("tdlcid", value)

    @property
    def axe(self) -> typing.Optional[int]:
        """Get or set the Axis for Ptype=1 or 2 EQ.1:	X-axis
        EQ.2:	Y - axis
        EQ.3 : Z - axis.
        Flag for meaning of ANGLE for PTYPE = 3. Setting AXE = 1 means that ANGLE is the contact distance. Otherwise, it is a scale factor on the contact distance search
        """ # nopep8
        return self._cards[0].get_value("axe")

    @axe.setter
    def axe(self, value: int) -> None:
        """Set the axe property."""
        self._cards[0].set_value("axe", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node ID giving the origin point coordinates
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[0].set_value("nid", value)

    @property
    def angle(self) -> typing.Optional[float]:
        """Get or set the Rotation angle if PTYPE=1. Scaling factor on contact distance search if PTYPE=3 (default applies a
        scale factor of 0.3 on local element size). If AXE=1 and PTYPE=3, then ANGLE becomes the contact distance
        """ # nopep8
        return self._cards[0].get_value("angle")

    @angle.setter
    def angle(self, value: float) -> None:
        """Set the angle property."""
        self._cards[0].set_value("angle", value)

    @property
    def nid_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given nid."""
        return self._get_link_by_attr("NODE", "nid", self.nid, "parts")

    @property
    def tdlcid_link(self) -> DefineCurve:
        """Get the DefineCurve object for tdlcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.tdlcid:
                return kwd
        return None

    @tdlcid_link.setter
    def tdlcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for tdlcid."""
        self.tdlcid = value.lcid

