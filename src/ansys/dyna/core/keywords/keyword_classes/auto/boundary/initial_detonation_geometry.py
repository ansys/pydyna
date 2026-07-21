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

"""Module providing the InitialDetonationGeometry class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_box import DefineBox
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_vector import DefineVector

_INITIALDETONATIONGEOMETRY_CARD0 = (
    FieldSchema("heid", int, 0, 10, None),
    FieldSchema("hetyp", int, 10, 10, 0),
    FieldSchema("mmgse", int, 20, 10, 0),
)

_INITIALDETONATIONGEOMETRY_CARD1 = (
    FieldSchema("geotyp", int, 0, 10, 0),
    FieldSchema("lt", float, 10, 10, 0.0),
    FieldSchema("dgeo", float, 20, 10, None),
    FieldSchema("boxid", int, 20, 10, None),
)

_INITIALDETONATIONGEOMETRY_CARD2 = (
    FieldSchema("vid1", int, 0, 10, None),
    FieldSchema("vid2", int, 10, 10, None),
    FieldSchema("vid3", int, 20, 10, None),
    FieldSchema("vid4", int, 30, 10, None),
)

class InitialDetonationGeometry(KeywordBase):
    """DYNA INITIAL_DETONATION_GEOMETRY keyword"""

    keyword = "INITIAL"
    subkeyword = "DETONATION_GEOMETRY"
    _link_fields = {
        "boxid": LinkType.DEFINE_BOX,
        "vid1": LinkType.DEFINE_VECTOR,
        "vid2": LinkType.DEFINE_VECTOR,
        "vid3": LinkType.DEFINE_VECTOR,
        "vid4": LinkType.DEFINE_VECTOR,
    }

    def __init__(self, **kwargs):
        """Initialize the InitialDetonationGeometry class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INITIALDETONATIONGEOMETRY_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _INITIALDETONATIONGEOMETRY_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _INITIALDETONATIONGEOMETRY_CARD2,
                **kwargs,
            ),
        ]
    @property
    def heid(self) -> typing.Optional[int]:
        """Get or set the ID specifying the high explosives to be lit
        """ # nopep8
        return self._cards[0].get_value("heid")

    @heid.setter
    def heid(self, value: int) -> None:
        """Set the heid property."""
        self._cards[0].set_value("heid", value)

    @property
    def hetyp(self) -> int:
        """Get or set the Type of HEID:
        EQ.0: Part set(*SET_PART)
        EQ.1: Part (*PART)
        EQ.2: Element set ID (*SET_SOLID in 3D, *SET_SHELL in 2D)
        """ # nopep8
        return self._cards[0].get_value("hetyp")

    @hetyp.setter
    def hetyp(self, value: int) -> None:
        """Set the hetyp property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""hetyp must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("hetyp", value)

    @property
    def mmgse(self) -> int:
        """Get or set the ID of *SET_MULTI-MATERIAL_GROUP_LIST selecting the explosive ALE groups to be lit
        """ # nopep8
        return self._cards[0].get_value("mmgse")

    @mmgse.setter
    def mmgse(self, value: int) -> None:
        """Set the mmgse property."""
        self._cards[0].set_value("mmgse", value)

    @property
    def geotyp(self) -> int:
        """Get or set the Type of geometry formed by the detonation points:
        EQ.1: Plane
        EQ.2: Cylindrical or truncated conical surface
        EQ.3: Spherical or elliptical surface
        """ # nopep8
        return self._cards[1].get_value("geotyp")

    @geotyp.setter
    def geotyp(self, value: int) -> None:
        """Set the geotyp property."""
        self._cards[1].set_value("geotyp", value)

    @property
    def lt(self) -> float:
        """Get or set the Lighting time for detonation point
        """ # nopep8
        return self._cards[1].get_value("lt")

    @lt.setter
    def lt(self, value: float) -> None:
        """Set the lt property."""
        self._cards[1].set_value("lt", value)

    @property
    def dgeo(self) -> typing.Optional[float]:
        """Get or set the Maximum distance from the detonation geometry for determining which HE elements become detonation points. If the element center for the specified HE is less than this distance away from the detonation geometry, then the element center becomes detonation point. If zero or undefined, DGEO becomes the length of the largest specified HE element.Note that this condition can be limited to only elements inside a box specified with BOXID below.
        """ # nopep8
        return self._cards[1].get_value("dgeo")

    @dgeo.setter
    def dgeo(self, value: float) -> None:
        """Set the dgeo property."""
        self._cards[1].set_value("dgeo", value)

    @property
    def boxid(self) -> typing.Optional[int]:
        """Get or set the ID of a box (see *DEFINE_BOX). If used, the element center becomes a detonation point if it is both within the box and within the distance specified with DGEO relative to the detonation geometry.
        """ # nopep8
        return self._cards[1].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        """Set the boxid property."""
        self._cards[1].set_value("boxid", value)

    @property
    def vid1(self) -> typing.Optional[int]:
        """Get or set the IDs of vectors (see *DEFINE_VECTOR) for specifying the orientation and location of a cylinder or truncate cone with either circular or elliptical bases. The cylinder or truncated cone can also be oblique. For all cases, the tail of VID1 gives the center of one of the bases, the length of VID2 gives the length of the cylinder or truncated cones axis, and the direction of VID2 gives the direction of the cylinder or truncated cones axis.
        For a cylinder with circular bases, the length of VID1 is the radius of the circle.The bases lie in planes formed by VID1and the cross product of VID1 with VID2.VID3 should not be input for this case.See Figure 0 - 1.
        For a cylinder with elliptical bases, the lengths of VID1and VID3 are the lengths of the two axes.The direction of the axis with the length of VID1 is in the direction of VID1.The direction for the axis with the length given by VID3 is in the direction given by the cross product of VID1 with VID2.See Figure 0 - 2.
        For a truncated cone, you must specify VID4 in addition to VID1and VID2.The bottom base determines if the bases are circular or elliptical based on whether you specify VID3 as previously discussed.VID4 gives the direction and length of the first axis of the second base.It does not need to be parallel to VID1.The second base is scaled uniformly from the first base.See Figure 0 - 3.
        """ # nopep8
        return self._cards[2].get_value("vid1")

    @vid1.setter
    def vid1(self, value: int) -> None:
        """Set the vid1 property."""
        self._cards[2].set_value("vid1", value)

    @property
    def vid2(self) -> typing.Optional[int]:
        """Get or set the IDs of vectors (see *DEFINE_VECTOR) for specifying the orientation and location of a cylinder or truncate cone with either circular or elliptical bases. The cylinder or truncated cone can also be oblique. For all cases, the tail of VID1 gives the center of one of the bases, the length of VID2 gives the length of the cylinder or truncated cones axis, and the direction of VID2 gives the direction of the cylinder or truncated cones axis.
        For a cylinder with circular bases, the length of VID1 is the radius of the circle.The bases lie in planes formed by VID1and the cross product of VID1 with VID2.VID3 should not be input for this case.See Figure 0 - 1.
        For a cylinder with elliptical bases, the lengths of VID1and VID3 are the lengths of the two axes.The direction of the axis with the length of VID1 is in the direction of VID1.The direction for the axis with the length given by VID3 is in the direction given by the cross product of VID1 with VID2.See Figure 0 - 2.
        For a truncated cone, you must specify VID4 in addition to VID1and VID2.The bottom base determines if the bases are circular or elliptical based on whether you specify VID3 as previously discussed.VID4 gives the direction and length of the first axis of the second base.It does not need to be parallel to VID1.The second base is scaled uniformly from the first base.See Figure 0 - 3.
        """ # nopep8
        return self._cards[2].get_value("vid2")

    @vid2.setter
    def vid2(self, value: int) -> None:
        """Set the vid2 property."""
        self._cards[2].set_value("vid2", value)

    @property
    def vid3(self) -> typing.Optional[int]:
        """Get or set the IDs of vectors (see *DEFINE_VECTOR) for specifying the orientation and location of a cylinder or truncate cone with either circular or elliptical bases. The cylinder or truncated cone can also be oblique. For all cases, the tail of VID1 gives the center of one of the bases, the length of VID2 gives the length of the cylinder or truncated cones axis, and the direction of VID2 gives the direction of the cylinder or truncated cones axis.
        For a cylinder with circular bases, the length of VID1 is the radius of the circle.The bases lie in planes formed by VID1and the cross product of VID1 with VID2.VID3 should not be input for this case.See Figure 0 - 1.
        For a cylinder with elliptical bases, the lengths of VID1and VID3 are the lengths of the two axes.The direction of the axis with the length of VID1 is in the direction of VID1.The direction for the axis with the length given by VID3 is in the direction given by the cross product of VID1 with VID2.See Figure 0 - 2.
        For a truncated cone, you must specify VID4 in addition to VID1and VID2.The bottom base determines if the bases are circular or elliptical based on whether you specify VID3 as previously discussed.VID4 gives the direction and length of the first axis of the second base.It does not need to be parallel to VID1.The second base is scaled uniformly from the first base.See Figure 0 - 3.
        """ # nopep8
        return self._cards[2].get_value("vid3")

    @vid3.setter
    def vid3(self, value: int) -> None:
        """Set the vid3 property."""
        self._cards[2].set_value("vid3", value)

    @property
    def vid4(self) -> typing.Optional[int]:
        """Get or set the IDs of vectors (see *DEFINE_VECTOR) for specifying the orientation and location of a cylinder or truncate cone with either circular or elliptical bases. The cylinder or truncated cone can also be oblique. For all cases, the tail of VID1 gives the center of one of the bases, the length of VID2 gives the length of the cylinder or truncated cones axis, and the direction of VID2 gives the direction of the cylinder or truncated cones axis.
        For a cylinder with circular bases, the length of VID1 is the radius of the circle.The bases lie in planes formed by VID1and the cross product of VID1 with VID2.VID3 should not be input for this case.See Figure 0 - 1.
        For a cylinder with elliptical bases, the lengths of VID1and VID3 are the lengths of the two axes.The direction of the axis with the length of VID1 is in the direction of VID1.The direction for the axis with the length given by VID3 is in the direction given by the cross product of VID1 with VID2.See Figure 0 - 2.
        For a truncated cone, you must specify VID4 in addition to VID1and VID2.The bottom base determines if the bases are circular or elliptical based on whether you specify VID3 as previously discussed.VID4 gives the direction and length of the first axis of the second base.It does not need to be parallel to VID1.The second base is scaled uniformly from the first base.See Figure 0 - 3.
        """ # nopep8
        return self._cards[2].get_value("vid4")

    @vid4.setter
    def vid4(self, value: int) -> None:
        """Set the vid4 property."""
        self._cards[2].set_value("vid4", value)

    @property
    def boxid_link(self) -> typing.Optional[DefineBox]:
        """Get the DefineBox object for boxid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "BOX"):
            if kwd.boxid == self.boxid:
                return kwd
        return None

    @boxid_link.setter
    def boxid_link(self, value: DefineBox) -> None:
        """Set the DefineBox object for boxid."""
        self.boxid = value.boxid

    @property
    def vid1_link(self) -> typing.Optional[DefineVector]:
        """Get the DefineVector object for vid1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.vid1:
                return kwd
        return None

    @vid1_link.setter
    def vid1_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for vid1."""
        self.vid1 = value.vid

    @property
    def vid2_link(self) -> typing.Optional[DefineVector]:
        """Get the DefineVector object for vid2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.vid2:
                return kwd
        return None

    @vid2_link.setter
    def vid2_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for vid2."""
        self.vid2 = value.vid

    @property
    def vid3_link(self) -> typing.Optional[DefineVector]:
        """Get the DefineVector object for vid3."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.vid3:
                return kwd
        return None

    @vid3_link.setter
    def vid3_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for vid3."""
        self.vid3 = value.vid

    @property
    def vid4_link(self) -> typing.Optional[DefineVector]:
        """Get the DefineVector object for vid4."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.vid4:
                return kwd
        return None

    @vid4_link.setter
    def vid4_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for vid4."""
        self.vid4 = value.vid

