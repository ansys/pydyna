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

"""Module providing the DualceseBoundarySolidWallMsurfRotate class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_DUALCESEBOUNDARYSOLIDWALLMSURFROTATE_CARD0 = (
    FieldSchema("mspid", int, 0, 10, None),
    FieldSchema("lcid", int, 10, 10, None),
    FieldSchema("xp", float, 20, 10, 0.0),
    FieldSchema("yp", float, 30, 10, 0.0),
    FieldSchema("zp", float, 40, 10, 0.0),
    FieldSchema("nx", float, 50, 10, 0.0),
    FieldSchema("ny", float, 60, 10, 0.0),
    FieldSchema("nz", float, 70, 10, 0.0),
)

class DualceseBoundarySolidWallMsurfRotate(KeywordBase):
    """DYNA DUALCESE_BOUNDARY_SOLID_WALL_MSURF_ROTATE keyword"""

    keyword = "DUALCESE"
    subkeyword = "BOUNDARY_SOLID_WALL_MSURF_ROTATE"
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the DualceseBoundarySolidWallMsurfRotate class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEBOUNDARYSOLIDWALLMSURFROTATE_CARD0,
                **kwargs,
            ),        ]
    @property
    def mspid(self) -> typing.Optional[int]:
        """Get or set the Mesh surface part ID that is referenced by *MESH_SURFACE_ELEMENT cards
        """ # nopep8
        return self._cards[0].get_value("mspid")

    @mspid.setter
    def mspid(self, value: int) -> None:
        """Set the mspid property."""
        self._cards[0].set_value("mspid", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for specifying the rotating speed frequency in Hz. This input is required
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def xp(self) -> float:
        """Get or set the Coordinates for a point on the axis of rotation
        """ # nopep8
        return self._cards[0].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[0].set_value("xp", value)

    @property
    def yp(self) -> float:
        """Get or set the Coordinates for a point on the axis of rotation
        """ # nopep8
        return self._cards[0].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[0].set_value("yp", value)

    @property
    def zp(self) -> float:
        """Get or set the Coordinates for a point on the axis of rotation
        """ # nopep8
        return self._cards[0].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[0].set_value("zp", value)

    @property
    def nx(self) -> float:
        """Get or set the Coordinates for a point on the axis of rotation
        """ # nopep8
        return self._cards[0].get_value("nx")

    @nx.setter
    def nx(self, value: float) -> None:
        """Set the nx property."""
        self._cards[0].set_value("nx", value)

    @property
    def ny(self) -> float:
        """Get or set the Coordinates for a point on the axis of rotation
        """ # nopep8
        return self._cards[0].get_value("ny")

    @ny.setter
    def ny(self, value: float) -> None:
        """Set the ny property."""
        self._cards[0].set_value("ny", value)

    @property
    def nz(self) -> float:
        """Get or set the Coordinates for a point on the axis of rotation
        """ # nopep8
        return self._cards[0].get_value("nz")

    @nz.setter
    def nz(self, value: float) -> None:
        """Set the nz property."""
        self._cards[0].set_value("nz", value)

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

