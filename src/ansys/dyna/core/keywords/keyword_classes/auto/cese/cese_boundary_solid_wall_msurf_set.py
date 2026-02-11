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

"""Module providing the CeseBoundarySolidWallMsurfSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_CESEBOUNDARYSOLIDWALLMSURFSET_CARD0 = (
    FieldSchema("surfsid", int, 0, 10, None),
    FieldSchema("lcid", int, 10, 10, 0),
    FieldSchema("vx", float, 20, 10, 0.0),
    FieldSchema("vy", float, 30, 10, 0.0),
    FieldSchema("vz", float, 40, 10, 0.0),
)

class CeseBoundarySolidWallMsurfSet(KeywordBase):
    """DYNA CESE_BOUNDARY_SOLID_WALL_MSURF_SET keyword"""

    keyword = "CESE"
    subkeyword = "BOUNDARY_SOLID_WALL_MSURF_SET"
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the CeseBoundarySolidWallMsurfSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CESEBOUNDARYSOLIDWALLMSURFSET_CARD0,
                **kwargs,
            ),        ]
    @property
    def surfsid(self) -> typing.Optional[int]:
        """Get or set the Identifier of a set of surface part IDs created with a *LSO_ID_SET card, where each surface part ID in the set is referenced in *MESH_SURFACE_ELEMENT cards.
        """ # nopep8
        return self._cards[0].get_value("surfsid")

    @surfsid.setter
    def surfsid(self, value: int) -> None:
        """Set the surfsid property."""
        self._cards[0].set_value("surfsid", value)

    @property
    def lcid(self) -> int:
        """Get or set the Load curve ID to define this solid wall boundary movement.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def vx(self) -> float:
        """Get or set the velocity vector of the solid wall:
        LCID.EQ.0: it is defined by (Vx,Vy,Vz) itself.
        LCID.NE.0: it will be defined by both of the load curve and (Vx,Vy,Vz).
        """ # nopep8
        return self._cards[0].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        """Set the vx property."""
        self._cards[0].set_value("vx", value)

    @property
    def vy(self) -> float:
        """Get or set the velocity vector of the solid wall:
        LCID.EQ.0: it is defined by (Vx,Vy,Vz) itself.
        LCID.NE.0: it will be defined by both of the load curve and (Vx,Vy,Vz).
        """ # nopep8
        return self._cards[0].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        """Set the vy property."""
        self._cards[0].set_value("vy", value)

    @property
    def vz(self) -> float:
        """Get or set the velocity vector of the solid wall:
        LCID.EQ.0: it is defined by (Vx,Vy,Vz) itself.
        LCID.NE.0: it will be defined by both of the load curve and (Vx,Vy,Vz).
        """ # nopep8
        return self._cards[0].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        """Set the vz property."""
        self._cards[0].set_value("vz", value)

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

