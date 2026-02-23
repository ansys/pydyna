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

"""Module providing the BoundaryFreeFieldGroundMotionPoint class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_BOUNDARYFREEFIELDGROUNDMOTIONPOINT_CARD0 = (
    FieldSchema("ssid", int, 0, 8, None),
    FieldSchema("xp", float, 8, 16, 0.0),
    FieldSchema("yp", float, 24, 16, 0.0),
    FieldSchema("zp", float, 40, 16, 0.0),
    FieldSchema("gmx", int, 56, 8, None),
    FieldSchema("gmy", int, 64, 8, None),
    FieldSchema("gmz", int, 72, 8, None),
)

_BOUNDARYFREEFIELDGROUNDMOTIONPOINT_CARD1 = (
    FieldSchema("sf", float, 0, 10, 1.0),
    FieldSchema("cid", int, 10, 10, 0),
    FieldSchema("birth", float, 20, 10, 0.0),
    FieldSchema("death", float, 30, 10, 1e+28),
    FieldSchema("isg", int, 40, 10, 0),
    FieldSchema("igm", int, 50, 10, 0),
)

class BoundaryFreeFieldGroundMotionPoint(KeywordBase):
    """DYNA BOUNDARY_FREE_FIELD_GROUND_MOTION_POINT keyword"""

    keyword = "BOUNDARY"
    subkeyword = "FREE_FIELD_GROUND_MOTION_POINT"
    _link_fields = {
        "cid": LinkType.DEFINE_COORDINATE_SYSTEM,
    }

    def __init__(self, **kwargs):
        """Initialize the BoundaryFreeFieldGroundMotionPoint class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYFREEFIELDGROUNDMOTIONPOINT_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _BOUNDARYFREEFIELDGROUNDMOTIONPOINT_CARD1,
                **kwargs,
            ),        ]
    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Soil-structure interface ID.
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[0].set_value("ssid", value)

    @property
    def xp(self) -> float:
        """Get or set the Curve multiplier at node N1.
        """ # nopep8
        return self._cards[0].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[0].set_value("xp", value)

    @property
    def yp(self) -> float:
        """Get or set the Curve multiplier at node N2.
        """ # nopep8
        return self._cards[0].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[0].set_value("yp", value)

    @property
    def zp(self) -> float:
        """Get or set the Curve multiplier at node N3.
        """ # nopep8
        return self._cards[0].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[0].set_value("zp", value)

    @property
    def gmx(self) -> typing.Optional[int]:
        """Get or set the Curve multiplier at node N4.
        """ # nopep8
        return self._cards[0].get_value("gmx")

    @gmx.setter
    def gmx(self, value: int) -> None:
        """Set the gmx property."""
        self._cards[0].set_value("gmx", value)

    @property
    def gmy(self) -> typing.Optional[int]:
        """Get or set the Curve multiplier at node N4.
        """ # nopep8
        return self._cards[0].get_value("gmy")

    @gmy.setter
    def gmy(self, value: int) -> None:
        """Set the gmy property."""
        self._cards[0].set_value("gmy", value)

    @property
    def gmz(self) -> typing.Optional[int]:
        """Get or set the Curve multiplier at node N4.
        """ # nopep8
        return self._cards[0].get_value("gmz")

    @gmz.setter
    def gmz(self, value: int) -> None:
        """Set the gmz property."""
        self._cards[0].set_value("gmz", value)

    @property
    def sf(self) -> float:
        """Get or set the Ground motion scale factor.
        """ # nopep8
        return self._cards[1].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[1].set_value("sf", value)

    @property
    def cid(self) -> int:
        """Get or set the Coordinate system ID, see *DEFINE_COORDINATE_SYSTEM.
        """ # nopep8
        return self._cards[1].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[1].set_value("cid", value)

    @property
    def birth(self) -> float:
        """Get or set the Time at which specified ground motion is activated.
        """ # nopep8
        return self._cards[1].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        """Set the birth property."""
        self._cards[1].set_value("birth", value)

    @property
    def death(self) -> float:
        """Get or set the Time at which specified ground motion is removed.
        """ # nopep8
        return self._cards[1].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        """Set the death property."""
        self._cards[1].set_value("death", value)

    @property
    def isg(self) -> int:
        """Get or set the Definition of soil-structure interface:
        EQ.0: SSID is ID for soil-structure interface defined by *INTERFACE_SSI_ID for non-matching mesh between soil and structure.
        EQ.1: SSID is segment set ID identifying soil-structure interface for merged meshes between soil and structure
        .
        """ # nopep8
        return self._cards[1].get_value("isg")

    @isg.setter
    def isg(self, value: int) -> None:
        """Set the isg property."""
        if value not in [0, 1, None]:
            raise Exception("""isg must be `None` or one of {0,1}.""")
        self._cards[1].set_value("isg", value)

    @property
    def igm(self) -> int:
        """Get or set the Specification of ground motions GMX, GMY, GMZ:
        EQ.0: ground motions are specified as acceleration load curves. See *DEFINE_CURVE
        EQ.1: Both ground accelerations and velocities specified using *DEFINE_GROUND_MOTION
        .
        """ # nopep8
        return self._cards[1].get_value("igm")

    @igm.setter
    def igm(self, value: int) -> None:
        """Set the igm property."""
        if value not in [0, 1, None]:
            raise Exception("""igm must be `None` or one of {0,1}.""")
        self._cards[1].set_value("igm", value)

    @property
    def cid_link(self) -> typing.Optional[DefineCoordinateSystem]:
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

