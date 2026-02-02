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

"""Module providing the BoundaryPrescribedOrientationRigidDircos class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_BOUNDARYPRESCRIBEDORIENTATIONRIGIDDIRCOS_CARD0 = (
    FieldSchema("pidb", int, 0, 10, None),
    FieldSchema("pida", int, 10, 10, None),
    FieldSchema("intrp", int, 20, 10, 1),
    FieldSchema("birth", float, 30, 10, 0.0),
    FieldSchema("death", float, 40, 10, 1e+20),
    FieldSchema("toffset", int, 50, 10, 0),
)

_BOUNDARYPRESCRIBEDORIENTATIONRIGIDDIRCOS_CARD1 = (
    FieldSchema("lcidc11", int, 0, 10, None),
    FieldSchema("lcidc12", int, 10, 10, None),
    FieldSchema("lcidc13", int, 20, 10, None),
    FieldSchema("lcidc21", int, 30, 10, None),
    FieldSchema("lcidc22", int, 40, 10, None),
    FieldSchema("lcidc23", int, 50, 10, None),
    FieldSchema("lcidc31", int, 60, 10, None),
    FieldSchema("lcidc32", int, 70, 10, None),
)

_BOUNDARYPRESCRIBEDORIENTATIONRIGIDDIRCOS_CARD2 = (
    FieldSchema("lcidc33", int, 0, 10, None),
)

class BoundaryPrescribedOrientationRigidDircos(KeywordBase):
    """DYNA BOUNDARY_PRESCRIBED_ORIENTATION_RIGID_DIRCOS keyword"""

    keyword = "BOUNDARY"
    subkeyword = "PRESCRIBED_ORIENTATION_RIGID_DIRCOS"
    _link_fields = {
        "lcidc11": LinkType.DEFINE_CURVE,
        "lcidc12": LinkType.DEFINE_CURVE,
        "lcidc13": LinkType.DEFINE_CURVE,
        "lcidc21": LinkType.DEFINE_CURVE,
        "lcidc22": LinkType.DEFINE_CURVE,
        "lcidc23": LinkType.DEFINE_CURVE,
        "lcidc31": LinkType.DEFINE_CURVE,
        "lcidc32": LinkType.DEFINE_CURVE,
        "lcidc33": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the BoundaryPrescribedOrientationRigidDircos class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYPRESCRIBEDORIENTATIONRIGIDDIRCOS_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _BOUNDARYPRESCRIBEDORIENTATIONRIGIDDIRCOS_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _BOUNDARYPRESCRIBEDORIENTATIONRIGIDDIRCOS_CARD2,
                **kwargs,
            ),        ]
    @property
    def pidb(self) -> typing.Optional[int]:
        """Get or set the Part ID for rigid body B whose orientation is prescribed
        """ # nopep8
        return self._cards[0].get_value("pidb")

    @pidb.setter
    def pidb(self, value: int) -> None:
        """Set the pidb property."""
        self._cards[0].set_value("pidb", value)

    @property
    def pida(self) -> typing.Optional[int]:
        """Get or set the Part ID for rigid body A.  The orientation of PIDB is measured with respect to the coordinate system of PIDA, as defined by LCO on *MAT_RIGID.  If zero then orientation of PIDB is measured with respect to the global reference frame except for BODY=1 in the ANGLES option
        """ # nopep8
        return self._cards[0].get_value("pida")

    @pida.setter
    def pida(self, value: int) -> None:
        """Set the pida property."""
        self._cards[0].set_value("pida", value)

    @property
    def intrp(self) -> int:
        """Get or set the Interpolation method used on time history curves:
        EQ.1: Linear interpolation (default)
        """ # nopep8
        return self._cards[0].get_value("intrp")

    @intrp.setter
    def intrp(self, value: int) -> None:
        """Set the intrp property."""
        self._cards[0].set_value("intrp", value)

    @property
    def birth(self) -> float:
        """Get or set the Prior to this time the body moves freely under the action of other agents.
        """ # nopep8
        return self._cards[0].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        """Set the birth property."""
        self._cards[0].set_value("birth", value)

    @property
    def death(self) -> float:
        """Get or set the The body is freed at this time and subsequently allowed to move under the action of other agents
        """ # nopep8
        return self._cards[0].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        """Set the death property."""
        self._cards[0].set_value("death", value)

    @property
    def toffset(self) -> int:
        """Get or set the Time offset flag:
        EQ.0:   No time offset is applied.
        EQ.1:	The time value of all load curves will be offset by the birth time,
        EQ.0:	no time offset is applied
        """ # nopep8
        return self._cards[0].get_value("toffset")

    @toffset.setter
    def toffset(self, value: int) -> None:
        """Set the toffset property."""
        if value not in [0, 1, None]:
            raise Exception("""toffset must be `None` or one of {0,1}.""")
        self._cards[0].set_value("toffset", value)

    @property
    def lcidc11(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying direction cosine Cij as a function of time.  Cij is defined as:where ai (i=1,2,3) are mutually perpendicular unit vectors fixed in PIDA and bj (j=1,2,3) are mutually perpendicular unit vectors fixed in PIDB.  If PIDA=0 then aj (j=1,2,3) are unit vectors aligned, respectively, with the global axes X, Y, and Z.
        """ # nopep8
        return self._cards[1].get_value("lcidc11")

    @lcidc11.setter
    def lcidc11(self, value: int) -> None:
        """Set the lcidc11 property."""
        self._cards[1].set_value("lcidc11", value)

    @property
    def lcidc12(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying direction cosine Cij as a function of time.  Cij is defined as:where ai (i=1,2,3) are mutually perpendicular unit vectors fixed in PIDA and bj (j=1,2,3) are mutually perpendicular unit vectors fixed in PIDB.  If PIDA=0 then aj (j=1,2,3) are unit vectors aligned, respectively, with the global axes X, Y, and Z.
        """ # nopep8
        return self._cards[1].get_value("lcidc12")

    @lcidc12.setter
    def lcidc12(self, value: int) -> None:
        """Set the lcidc12 property."""
        self._cards[1].set_value("lcidc12", value)

    @property
    def lcidc13(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying direction cosine Cij as a function of time.  Cij is defined as:where ai (i=1,2,3) are mutually perpendicular unit vectors fixed in PIDA and bj (j=1,2,3) are mutually perpendicular unit vectors fixed in PIDB.  If PIDA=0 then aj (j=1,2,3) are unit vectors aligned, respectively, with the global axes X, Y, and Z.
        """ # nopep8
        return self._cards[1].get_value("lcidc13")

    @lcidc13.setter
    def lcidc13(self, value: int) -> None:
        """Set the lcidc13 property."""
        self._cards[1].set_value("lcidc13", value)

    @property
    def lcidc21(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying direction cosine Cij as a function of time.  Cij is defined as:where ai (i=1,2,3) are mutually perpendicular unit vectors fixed in PIDA and bj (j=1,2,3) are mutually perpendicular unit vectors fixed in PIDB.  If PIDA=0 then aj (j=1,2,3) are unit vectors aligned, respectively, with the global axes X, Y, and Z.
        """ # nopep8
        return self._cards[1].get_value("lcidc21")

    @lcidc21.setter
    def lcidc21(self, value: int) -> None:
        """Set the lcidc21 property."""
        self._cards[1].set_value("lcidc21", value)

    @property
    def lcidc22(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying direction cosine Cij as a function of time.  Cij is defined as:where ai (i=1,2,3) are mutually perpendicular unit vectors fixed in PIDA and bj (j=1,2,3) are mutually perpendicular unit vectors fixed in PIDB.  If PIDA=0 then aj (j=1,2,3) are unit vectors aligned, respectively, with the global axes X, Y, and Z.
        """ # nopep8
        return self._cards[1].get_value("lcidc22")

    @lcidc22.setter
    def lcidc22(self, value: int) -> None:
        """Set the lcidc22 property."""
        self._cards[1].set_value("lcidc22", value)

    @property
    def lcidc23(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying direction cosine Cij as a function of time.  Cij is defined as:where ai (i=1,2,3) are mutually perpendicular unit vectors fixed in PIDA and bj (j=1,2,3) are mutually perpendicular unit vectors fixed in PIDB.  If PIDA=0 then aj (j=1,2,3) are unit vectors aligned, respectively, with the global axes X, Y, and Z.
        """ # nopep8
        return self._cards[1].get_value("lcidc23")

    @lcidc23.setter
    def lcidc23(self, value: int) -> None:
        """Set the lcidc23 property."""
        self._cards[1].set_value("lcidc23", value)

    @property
    def lcidc31(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying direction cosine Cij as a function of time.  Cij is defined as:where ai (i=1,2,3) are mutually perpendicular unit vectors fixed in PIDA and bj (j=1,2,3) are mutually perpendicular unit vectors fixed in PIDB.  If PIDA=0 then aj (j=1,2,3) are unit vectors aligned, respectively, with the global axes X, Y, and Z.
        """ # nopep8
        return self._cards[1].get_value("lcidc31")

    @lcidc31.setter
    def lcidc31(self, value: int) -> None:
        """Set the lcidc31 property."""
        self._cards[1].set_value("lcidc31", value)

    @property
    def lcidc32(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying direction cosine Cij as a function of time.  Cij is defined as:where ai (i=1,2,3) are mutually perpendicular unit vectors fixed in PIDA and bj (j=1,2,3) are mutually perpendicular unit vectors fixed in PIDB.  If PIDA=0 then aj (j=1,2,3) are unit vectors aligned, respectively, with the global axes X, Y, and Z.
        """ # nopep8
        return self._cards[1].get_value("lcidc32")

    @lcidc32.setter
    def lcidc32(self, value: int) -> None:
        """Set the lcidc32 property."""
        self._cards[1].set_value("lcidc32", value)

    @property
    def lcidc33(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying direction cosine Cij as a function of time.  Cij is defined as:where ai (i=1,2,3) are mutually perpendicular unit vectors fixed in PIDA and bj (j=1,2,3) are mutually perpendicular unit vectors fixed in PIDB.  If PIDA=0 then aj (j=1,2,3) are unit vectors aligned, respectively, with the global axes X, Y, and Z.
        """ # nopep8
        return self._cards[2].get_value("lcidc33")

    @lcidc33.setter
    def lcidc33(self, value: int) -> None:
        """Set the lcidc33 property."""
        self._cards[2].set_value("lcidc33", value)

    @property
    def lcidc11_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidc11."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidc11:
                return kwd
        return None

    @lcidc11_link.setter
    def lcidc11_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidc11."""
        self.lcidc11 = value.lcid

    @property
    def lcidc12_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidc12."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidc12:
                return kwd
        return None

    @lcidc12_link.setter
    def lcidc12_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidc12."""
        self.lcidc12 = value.lcid

    @property
    def lcidc13_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidc13."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidc13:
                return kwd
        return None

    @lcidc13_link.setter
    def lcidc13_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidc13."""
        self.lcidc13 = value.lcid

    @property
    def lcidc21_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidc21."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidc21:
                return kwd
        return None

    @lcidc21_link.setter
    def lcidc21_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidc21."""
        self.lcidc21 = value.lcid

    @property
    def lcidc22_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidc22."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidc22:
                return kwd
        return None

    @lcidc22_link.setter
    def lcidc22_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidc22."""
        self.lcidc22 = value.lcid

    @property
    def lcidc23_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidc23."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidc23:
                return kwd
        return None

    @lcidc23_link.setter
    def lcidc23_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidc23."""
        self.lcidc23 = value.lcid

    @property
    def lcidc31_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidc31."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidc31:
                return kwd
        return None

    @lcidc31_link.setter
    def lcidc31_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidc31."""
        self.lcidc31 = value.lcid

    @property
    def lcidc32_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidc32."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidc32:
                return kwd
        return None

    @lcidc32_link.setter
    def lcidc32_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidc32."""
        self.lcidc32 = value.lcid

    @property
    def lcidc33_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidc33."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidc33:
                return kwd
        return None

    @lcidc33_link.setter
    def lcidc33_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidc33."""
        self.lcidc33 = value.lcid

