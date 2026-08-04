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

"""Module providing the DualceseEnsightRegion class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_DUALCESEENSIGHTREGION_CARD0 = (
    FieldSchema("regid", int, 0, 10, None),
)

_DUALCESEENSIGHTREGION_CARD1 = (
    FieldSchema("region_type", str, 0, 80, "BOX"),
)

_DUALCESEENSIGHTREGION_CARD2 = (
    FieldSchema("x1", float, 0, 10, None),
    FieldSchema("y1", float, 10, 10, None),
    FieldSchema("z1", float, 20, 10, None),
    FieldSchema("x2", float, 30, 10, None),
    FieldSchema("y2", float, 40, 10, None),
    FieldSchema("z2", float, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

_DUALCESEENSIGHTREGION_CARD3 = (
    FieldSchema("bz3", float, 0, 10, None),
    FieldSchema("ra3", float, 10, 10, None),
    FieldSchema("rb3", float, 20, 10, None),
    FieldSchema("rc3", float, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

_DUALCESEENSIGHTREGION_CARD4 = (
    FieldSchema("side", str, 0, 10, "INSIDE"),
    FieldSchema("unused", str, 10, 10, None),
)

_DUALCESEENSIGHTREGION_CARD5 = (
    FieldSchema("dist", float, 0, 10, None),
    FieldSchema("lcid", int, 10, 10, None),
    FieldSchema("isetuse", int, 20, 10, None),
)

class DualceseEnsightRegion(KeywordBase):
    """DYNA DUALCESE_ENSIGHT_REGION keyword"""

    keyword = "DUALCESE"
    subkeyword = "ENSIGHT_REGION"
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the DualceseEnsightRegion class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEENSIGHTREGION_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DUALCESEENSIGHTREGION_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DUALCESEENSIGHTREGION_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DUALCESEENSIGHTREGION_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DUALCESEENSIGHTREGION_CARD4,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DUALCESEENSIGHTREGION_CARD5,
                **kwargs,
            ),
        ]
    @property
    def regid(self) -> typing.Optional[int]:
        """Get or set the ID of the dual CESE region used for output
        """ # nopep8
        return self._cards[0].get_value("regid")

    @regid.setter
    def regid(self, value: int) -> None:
        """Set the regid property."""
        self._cards[0].set_value("regid", value)

    @property
    def region_type(self) -> str:
        """Get or set the Type of region for selecting a portion of the dual CESE mesh. The available options are: BOX, SPHERE, CYLINDER, ELLIPSOID, DISTANCE_FROM_FSI_INTERFACE, and DISTANCE_FROM_CHT_INTERFACE. DISTANCE_FROM_FSI_INTERFACE is the region defined by the fluid-structure interaction (FSI) interface to a distance specified by DIST and LCID from the interface (see Figure 0-1). Similarly, DISTANCE_FROM_CHT_INTERFACE is the region defined by the conjugate heat transfer (CHT) interface to a distance specified by DIST and LCID from the interface.
        """ # nopep8
        return self._cards[1].get_value("region_type")

    @region_type.setter
    def region_type(self, value: str) -> None:
        """Set the region_type property."""
        if value not in ["BOX", "SPHERE", "CYLINDER", "ELLIPSOID", "DISTANCE_FROM_FSI_INTERFACE", "DISTANCE_FROM_CHT_INTERFACE", None]:
            raise Exception("""region_type must be `None` or one of {"BOX","SPHERE","CYLINDER","ELLIPSOID","DISTANCE_FROM_FSI_INTERFACE","DISTANCE_FROM_CHT_INTERFACE"}.""")
        self._cards[1].set_value("region_type", value)

    @property
    def x1(self) -> typing.Optional[float]:
        """Get or set the Position of one corner of the BOX geometry
        """ # nopep8
        return self._cards[2].get_value("x1")

    @x1.setter
    def x1(self, value: float) -> None:
        """Set the x1 property."""
        self._cards[2].set_value("x1", value)

    @property
    def y1(self) -> typing.Optional[float]:
        """Get or set the Position of one corner of the BOX geometry
        """ # nopep8
        return self._cards[2].get_value("y1")

    @y1.setter
    def y1(self, value: float) -> None:
        """Set the y1 property."""
        self._cards[2].set_value("y1", value)

    @property
    def z1(self) -> typing.Optional[float]:
        """Get or set the Position of one corner of the BOX geometry
        """ # nopep8
        return self._cards[2].get_value("z1")

    @z1.setter
    def z1(self, value: float) -> None:
        """Set the z1 property."""
        self._cards[2].set_value("z1", value)

    @property
    def x2(self) -> typing.Optional[float]:
        """Get or set the Position of the opposite corner from (X1, Y1, Z1) for the box geometry
        """ # nopep8
        return self._cards[2].get_value("x2")

    @x2.setter
    def x2(self, value: float) -> None:
        """Set the x2 property."""
        self._cards[2].set_value("x2", value)

    @property
    def y2(self) -> typing.Optional[float]:
        """Get or set the Position of the opposite corner from (X1, Y1, Z1) for the box geometry
        """ # nopep8
        return self._cards[2].get_value("y2")

    @y2.setter
    def y2(self, value: float) -> None:
        """Set the y2 property."""
        self._cards[2].set_value("y2", value)

    @property
    def z2(self) -> typing.Optional[float]:
        """Get or set the Position of the opposite corner from (X1, Y1, Z1) for the box geometry
        """ # nopep8
        return self._cards[2].get_value("z2")

    @z2.setter
    def z2(self, value: float) -> None:
        """Set the z2 property."""
        self._cards[2].set_value("z2", value)

    @property
    def bz3(self) -> typing.Optional[float]:
        """Get or set the A vector, b ?, in the plane of the first and second axes of the ellipsoid.  The third axis of the ellipsoid (axis c) will be in the direction of a�b ?, and finally, the second axis b=c�a.
        """ # nopep8
        return self._cards[3].get_value("bz3")

    @bz3.setter
    def bz3(self, value: float) -> None:
        """Set the bz3 property."""
        self._cards[3].set_value("bz3", value)

    @property
    def ra3(self) -> typing.Optional[float]:
        """Get or set the The semi-axis lengths of the ellipsoid
        """ # nopep8
        return self._cards[3].get_value("ra3")

    @ra3.setter
    def ra3(self, value: float) -> None:
        """Set the ra3 property."""
        self._cards[3].set_value("ra3", value)

    @property
    def rb3(self) -> typing.Optional[float]:
        """Get or set the The semi-axis lengths of the ellipsoid
        """ # nopep8
        return self._cards[3].get_value("rb3")

    @rb3.setter
    def rb3(self, value: float) -> None:
        """Set the rb3 property."""
        self._cards[3].set_value("rb3", value)

    @property
    def rc3(self) -> typing.Optional[float]:
        """Get or set the The semi-axis lengths of the ellipsoid
        """ # nopep8
        return self._cards[3].get_value("rc3")

    @rc3.setter
    def rc3(self, value: float) -> None:
        """Set the rc3 property."""
        self._cards[3].set_value("rc3", value)

    @property
    def side(self) -> str:
        """Get or set the Flag to specify which side of the geometry the region comes from:
        EQ.INSIDE:	Region comes from inside the geometry(inside).For DISTANCE_FROM_FSI_INTERFACE and DISTANCE_FROM_CHT_INTERFACE, inside is the region that includes the FSI / CHT interface out to a distance specified with DIST and LCID.See Figure 0 - 1.
        EQ.OUTSIDE : Region comes from outside the geometry.
        """ # nopep8
        return self._cards[4].get_value("side")

    @side.setter
    def side(self, value: str) -> None:
        """Set the side property."""
        if value not in ["INSIDE", "OUTSIDE", None]:
            raise Exception("""side must be `None` or one of {"INSIDE","OUTSIDE"}.""")
        self._cards[4].set_value("side", value)

    @property
    def dist(self) -> typing.Optional[float]:
        """Get or set the Distance measure from the selected mesh object. This field only applies to DISTANCE_FROM_FSI_INTERFACE and DISTANCE_FROM_CHT_INTERFACE.
        """ # nopep8
        return self._cards[5].get_value("dist")

    @dist.setter
    def dist(self, value: float) -> None:
        """Set the dist property."""
        self._cards[5].set_value("dist", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve that adjusts the distance from the selected mesh object as a function of time. This field only applies to DISTANCE_FROM_FSI_INTERFACE and DISTANCE_FROM_CHT_INTERFACE.
        """ # nopep8
        return self._cards[5].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[5].set_value("lcid", value)

    @property
    def isetuse(self) -> typing.Optional[int]:
        """Get or set the undefined.
        """ # nopep8
        return self._cards[5].get_value("isetuse")

    @isetuse.setter
    def isetuse(self, value: int) -> None:
        """Set the isetuse property."""
        self._cards[5].set_value("isetuse", value)

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

