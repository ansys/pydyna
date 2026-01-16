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
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_vector import DefineVector

_INITIALDETONATIONGEOMETRY_CARD0 = (
    FieldSchema("heid", int, 0, 10, None),
    FieldSchema("hetyp", float, 10, 10, 0.0),
    FieldSchema("mmgse", float, 20, 10, 0.0),
)

_INITIALDETONATIONGEOMETRY_CARD1 = (
    FieldSchema("geotyp", int, 0, 10, 0),
    FieldSchema("lt", float, 10, 10, 0.0),
    FieldSchema("dgeo", float, 20, 10, 0.0),
)

_INITIALDETONATIONGEOMETRY_CARD2 = (
    FieldSchema("v1", int, 0, 10, None),
    FieldSchema("v2", int, 10, 10, None),
    FieldSchema("v3", int, 20, 10, None),
    FieldSchema("v4", int, 30, 10, None),
)

class InitialDetonationGeometry(KeywordBase):
    """DYNA INITIAL_DETONATION_GEOMETRY keyword"""

    keyword = "INITIAL"
    subkeyword = "DETONATION_GEOMETRY"
    _link_fields = {
        "v1": LinkType.DEFINE_VECTOR,
        "v2": LinkType.DEFINE_VECTOR,
        "v3": LinkType.DEFINE_VECTOR,
        "v4": LinkType.DEFINE_VECTOR,
        "heid": LinkType.SET_NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the InitialDetonationGeometry class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INITIALDETONATIONGEOMETRY_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INITIALDETONATIONGEOMETRY_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INITIALDETONATIONGEOMETRY_CARD2,
                **kwargs,
            ),        ]
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
    def hetyp(self) -> float:
        """Get or set the Type of HEID :
        EQ.0:	Part set(*SET_PART)
        """ # nopep8
        return self._cards[0].get_value("hetyp")

    @hetyp.setter
    def hetyp(self, value: float) -> None:
        """Set the hetyp property."""
        self._cards[0].set_value("hetyp", value)

    @property
    def mmgse(self) -> float:
        """Get or set the ID of *SET_‌MULTI-MATERIAL_‌GROUP_LIST selecting the explosive ALE groups to be lit
        """ # nopep8
        return self._cards[0].get_value("mmgse")

    @mmgse.setter
    def mmgse(self, value: float) -> None:
        """Set the mmgse property."""
        self._cards[0].set_value("mmgse", value)

    @property
    def geotyp(self) -> int:
        """Get or set the Type of geometry formed by the detonation points:
        EQ.1: Plane
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
    def dgeo(self) -> float:
        """Get or set the Maximum distance from the detonation geometry for determining which HE elements become detonation points. If the element center for the specified HE is less than this distance away from the detonation geometry, then the element center becomes detonation point. If zero or undefined, DGEO becomes the length of the largest specified HE element.
        """ # nopep8
        return self._cards[1].get_value("dgeo")

    @dgeo.setter
    def dgeo(self, value: float) -> None:
        """Set the dgeo property."""
        self._cards[1].set_value("dgeo", value)

    @property
    def v1(self) -> typing.Optional[int]:
        """Get or set the IDs of vectors (*DEFINE_VECTOR) for specifying orientation and location of the geometry selected with GEOTYP.
        GEOTYP.EQ.1:	V1 is the plane’s normal vector.The tail of V1 is a point in this plane.V2, V3,and V4 are unused
        """ # nopep8
        return self._cards[2].get_value("v1")

    @v1.setter
    def v1(self, value: int) -> None:
        """Set the v1 property."""
        self._cards[2].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[int]:
        """Get or set the IDs of vectors (*DEFINE_VECTOR) for specifying orientation and location of the geometry selected with GEOTYP.
        GEOTYP.EQ.1:	V1 is the plane’s normal vector.The tail of V1 is a point in this plane.V2, V3,and V4 are unused
        """ # nopep8
        return self._cards[2].get_value("v2")

    @v2.setter
    def v2(self, value: int) -> None:
        """Set the v2 property."""
        self._cards[2].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[int]:
        """Get or set the IDs of vectors (*DEFINE_VECTOR) for specifying orientation and location of the geometry selected with GEOTYP.
        GEOTYP.EQ.1:	V1 is the plane’s normal vector.The tail of V1 is a point in this plane.V2, V3,and V4 are unused
        """ # nopep8
        return self._cards[2].get_value("v3")

    @v3.setter
    def v3(self, value: int) -> None:
        """Set the v3 property."""
        self._cards[2].set_value("v3", value)

    @property
    def v4(self) -> typing.Optional[int]:
        """Get or set the IDs of vectors (*DEFINE_VECTOR) for specifying orientation and location of the geometry selected with GEOTYP.
        GEOTYP.EQ.1:	V1 is the plane’s normal vector.The tail of V1 is a point in this plane.V2, V3,and V4 are unused
        """ # nopep8
        return self._cards[2].get_value("v4")

    @v4.setter
    def v4(self, value: int) -> None:
        """Set the v4 property."""
        self._cards[2].set_value("v4", value)

    @property
    def v1_link(self) -> DefineVector:
        """Get the DefineVector object for v1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.v1:
                return kwd
        return None

    @v1_link.setter
    def v1_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for v1."""
        self.v1 = value.vid

    @property
    def v2_link(self) -> DefineVector:
        """Get the DefineVector object for v2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.v2:
                return kwd
        return None

    @v2_link.setter
    def v2_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for v2."""
        self.v2 = value.vid

    @property
    def v3_link(self) -> DefineVector:
        """Get the DefineVector object for v3."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.v3:
                return kwd
        return None

    @v3_link.setter
    def v3_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for v3."""
        self.v3 = value.vid

    @property
    def v4_link(self) -> DefineVector:
        """Get the DefineVector object for v4."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.v4:
                return kwd
        return None

    @v4_link.setter
    def v4_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for v4."""
        self.v4 = value.vid

    @property
    def heid_link(self) -> KeywordBase:
        """Get the SET_NODE_* keyword for heid."""
        return self._get_set_link("NODE", self.heid)

    @heid_link.setter
    def heid_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for heid."""
        self.heid = value.sid

