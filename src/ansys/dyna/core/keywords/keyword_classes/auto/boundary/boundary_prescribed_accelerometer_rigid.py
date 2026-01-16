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

"""Module providing the BoundaryPrescribedAccelerometerRigid class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_BOUNDARYPRESCRIBEDACCELEROMETERRIGID_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
)

_BOUNDARYPRESCRIBEDACCELEROMETERRIGID_CARD1 = (
    FieldSchema("nid", int, 0, 10, None),
    FieldSchema("cid", int, 10, 10, None),
    FieldSchema("lcidx", int, 20, 10, None),
    FieldSchema("lcidy", int, 30, 10, None),
    FieldSchema("lcidz", int, 40, 10, None),
)

class BoundaryPrescribedAccelerometerRigid(KeywordBase):
    """DYNA BOUNDARY_PRESCRIBED_ACCELEROMETER_RIGID keyword"""

    keyword = "BOUNDARY"
    subkeyword = "PRESCRIBED_ACCELEROMETER_RIGID"
    _link_fields = {
        "nid": LinkType.NODE,
        "lcidx": LinkType.DEFINE_CURVE,
        "lcidy": LinkType.DEFINE_CURVE,
        "lcidz": LinkType.DEFINE_CURVE,
        "cid": LinkType.DEFINE_COORDINATE_SYSTEM,
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the BoundaryPrescribedAccelerometerRigid class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYPRESCRIBEDACCELEROMETERRIGID_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _BOUNDARYPRESCRIBEDACCELEROMETERRIGID_CARD1,
                **kwargs,
            ),        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID for rigid body whose motion is prescribed.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node ID corresponding to the location of the accelerometer
        """ # nopep8
        return self._cards[1].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[1].set_value("nid", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Coordinate system ID describing the orientation of the accelerometer's local axes
        """ # nopep8
        return self._cards[1].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[1].set_value("cid", value)

    @property
    def lcidx(self) -> typing.Optional[int]:
        """Get or set the Load curve ID containing the local x-acceleration time history from the accelerometer.
        """ # nopep8
        return self._cards[1].get_value("lcidx")

    @lcidx.setter
    def lcidx(self, value: int) -> None:
        """Set the lcidx property."""
        self._cards[1].set_value("lcidx", value)

    @property
    def lcidy(self) -> typing.Optional[int]:
        """Get or set the Load curve ID containing the local y-acceleration time history from the accelerometer.
        """ # nopep8
        return self._cards[1].get_value("lcidy")

    @lcidy.setter
    def lcidy(self, value: int) -> None:
        """Set the lcidy property."""
        self._cards[1].set_value("lcidy", value)

    @property
    def lcidz(self) -> typing.Optional[int]:
        """Get or set the Load curve ID containing the local z-acceleration time history from the accelerometer.
        """ # nopep8
        return self._cards[1].get_value("lcidz")

    @lcidz.setter
    def lcidz(self, value: int) -> None:
        """Set the lcidz property."""
        self._cards[1].set_value("lcidz", value)

    @property
    def nid_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given nid."""
        return self._get_link_by_attr("NODE", "nid", self.nid, "parts")

    @property
    def lcidx_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidx."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidx:
                return kwd
        return None

    @lcidx_link.setter
    def lcidx_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidx."""
        self.lcidx = value.lcid

    @property
    def lcidy_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidy."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidy:
                return kwd
        return None

    @lcidy_link.setter
    def lcidy_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidy."""
        self.lcidy = value.lcid

    @property
    def lcidz_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidz."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidz:
                return kwd
        return None

    @lcidz_link.setter
    def lcidz_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidz."""
        self.lcidz = value.lcid

    @property
    def cid_link(self) -> DefineCoordinateSystem:
        """Get the DefineCoordinateSystem object for cid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "COORDINATE_SYSTEM"):
            if kwd.cid == self.cid:
                return kwd
        return None

    @cid_link.setter
    def cid_link(self, value: DefineCoordinateSystem) -> None:
        """Set the DefineCoordinateSystem object for cid."""
        self.cid = value.cid

    @property
    def pid_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

