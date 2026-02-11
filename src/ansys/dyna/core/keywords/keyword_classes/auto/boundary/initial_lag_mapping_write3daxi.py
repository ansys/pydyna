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

"""Module providing the InitialLagMappingWrite3Daxi class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_vector import DefineVector

_INITIALLAGMAPPINGWRITE3DAXI_CARD0 = (
    FieldSchema("setid", int, 0, 10, None),
)

_INITIALLAGMAPPINGWRITE3DAXI_CARD1 = (
    FieldSchema("xp", float, 0, 10, 0.0),
    FieldSchema("yp", float, 10, 10, 0.0),
    FieldSchema("zp", float, 20, 10, 0.0),
    FieldSchema("vecid", int, 30, 10, None),
    FieldSchema("angle", float, 40, 10, None),
    FieldSchema("nelangl", int, 50, 10, None),
)

class InitialLagMappingWrite3Daxi(KeywordBase):
    """DYNA INITIAL_LAG_MAPPING_WRITE3DAXI keyword"""

    keyword = "INITIAL"
    subkeyword = "LAG_MAPPING_WRITE3DAXI"
    _link_fields = {
        "vecid": LinkType.DEFINE_VECTOR,
        "setid": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the InitialLagMappingWrite3Daxi class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INITIALLAGMAPPINGWRITE3DAXI_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INITIALLAGMAPPINGWRITE3DAXI_CARD1,
                **kwargs,
            ),        ]
    @property
    def setid(self) -> typing.Optional[int]:
        """Get or set the part set ID
        """ # nopep8
        return self._cards[0].get_value("setid")

    @setid.setter
    def setid(self, value: int) -> None:
        """Set the setid property."""
        self._cards[0].set_value("setid", value)

    @property
    def xp(self) -> float:
        """Get or set the x-position of a point belonging to the plane from which the 3D mesh is generated
        """ # nopep8
        return self._cards[1].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[1].set_value("xp", value)

    @property
    def yp(self) -> float:
        """Get or set the y-position of a point belonging to the plane from which the 3D mesh is generated
        """ # nopep8
        return self._cards[1].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[1].set_value("yp", value)

    @property
    def zp(self) -> float:
        """Get or set the z-position of a point belonging to the plane from which the 3D mesh is generated
        """ # nopep8
        return self._cards[1].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[1].set_value("zp", value)

    @property
    def vecid(self) -> typing.Optional[int]:
        """Get or set the ID of the symmetric axis defined by *DEFINE_VECTOR
        """ # nopep8
        return self._cards[1].get_value("vecid")

    @vecid.setter
    def vecid(self, value: int) -> None:
        """Set the vecid property."""
        self._cards[1].set_value("vecid", value)

    @property
    def angle(self) -> typing.Optional[float]:
        """Get or set the Angle of rotation around an axis defined by *DEFINE_VECTOR
        """ # nopep8
        return self._cards[1].get_value("angle")

    @angle.setter
    def angle(self, value: float) -> None:
        """Set the angle property."""
        self._cards[1].set_value("angle", value)

    @property
    def nelangl(self) -> typing.Optional[int]:
        """Get or set the Mapping parameter.  See Remark 5.
        GT. 0:	For a 2D to 3D mapping, number of elements to create in the azimuthal direction for ANGLE
        EQ.-1:	No mesh is generated or projected.
        EQ.-2:	For a 3D to 3D mapping, ANGLE only rotates the data from the mapping file (not the current mesh).
        EQ.-3:	No mesh is generated or projected except that the boundary nodes of the current mesh are projected on the boundary faces of the previous mesh
        """ # nopep8
        return self._cards[1].get_value("nelangl")

    @nelangl.setter
    def nelangl(self, value: int) -> None:
        """Set the nelangl property."""
        self._cards[1].set_value("nelangl", value)

    @property
    def vecid_link(self) -> typing.Optional[DefineVector]:
        """Get the DefineVector object for vecid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.vecid:
                return kwd
        return None

    @vecid_link.setter
    def vecid_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for vecid."""
        self.vecid = value.vid

    @property
    def setid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for setid."""
        return self._get_set_link("PART", self.setid)

    @setid_link.setter
    def setid_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for setid."""
        self.setid = value.sid

