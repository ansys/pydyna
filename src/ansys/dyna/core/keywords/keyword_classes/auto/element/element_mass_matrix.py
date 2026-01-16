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

"""Module providing the ElementMassMatrix class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_ELEMENTMASSMATRIX_CARD0 = (
    FieldSchema("eid", int, 0, 10, None),
    FieldSchema("id", int, 10, 10, None),
    FieldSchema("cid", float, 20, 10, None),
)

_ELEMENTMASSMATRIX_CARD1 = (
    FieldSchema("m11", float, 0, 10, 0.0),
    FieldSchema("m21", float, 10, 10, 0.0),
    FieldSchema("m22", float, 20, 10, 0.0),
    FieldSchema("m31", float, 30, 10, 0.0),
    FieldSchema("m32", float, 40, 10, 0.0),
    FieldSchema("m33", float, 50, 10, 0.0),
    FieldSchema("m41", float, 60, 10, 0.0),
)

_ELEMENTMASSMATRIX_CARD2 = (
    FieldSchema("m41", float, 0, 10, 0.0),
    FieldSchema("m43", float, 10, 10, 0.0),
    FieldSchema("m44", float, 20, 10, 0.0),
    FieldSchema("m51", float, 30, 10, 0.0),
    FieldSchema("m52", float, 40, 10, 0.0),
    FieldSchema("m53", float, 50, 10, 0.0),
    FieldSchema("m54", float, 60, 10, 0.0),
)

_ELEMENTMASSMATRIX_CARD3 = (
    FieldSchema("m55", float, 0, 10, 0.0),
    FieldSchema("m61", float, 10, 10, 0.0),
    FieldSchema("m62", float, 20, 10, 0.0),
    FieldSchema("m63", float, 30, 10, 0.0),
    FieldSchema("m64", float, 40, 10, 0.0),
    FieldSchema("m65", float, 50, 10, 0.0),
    FieldSchema("m66", float, 60, 10, 0.0),
)

class ElementMassMatrix(KeywordBase):
    """DYNA ELEMENT_MASS_MATRIX keyword"""

    keyword = "ELEMENT"
    subkeyword = "MASS_MATRIX"
    _link_fields = {
        "id": LinkType.NODE,
        "cid": LinkType.DEFINE_COORDINATE_SYSTEM,
    }

    def __init__(self, **kwargs):
        """Initialize the ElementMassMatrix class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ELEMENTMASSMATRIX_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTMASSMATRIX_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTMASSMATRIX_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTMASSMATRIX_CARD3,
                **kwargs,
            ),        ]
    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Element ID.  A unique number is recommended.  The nodes in a node set share the same element ID
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        """Set the eid property."""
        self._cards[0].set_value("eid", value)

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Node ID, This is the node to which the mass is assigned.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def cid(self) -> typing.Optional[float]:
        """Get or set the Local coordinate ID which defines the orientation of the mass matrix
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: float) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def m11(self) -> float:
        """Get or set the The IJth term of the symmetric mass matrix.  The lower triangular part of the matrix is defined
        """ # nopep8
        return self._cards[1].get_value("m11")

    @m11.setter
    def m11(self, value: float) -> None:
        """Set the m11 property."""
        self._cards[1].set_value("m11", value)

    @property
    def m21(self) -> float:
        """Get or set the The IJth term of the symmetric mass matrix.  The lower triangular part of the matrix is defined
        """ # nopep8
        return self._cards[1].get_value("m21")

    @m21.setter
    def m21(self, value: float) -> None:
        """Set the m21 property."""
        self._cards[1].set_value("m21", value)

    @property
    def m22(self) -> float:
        """Get or set the The IJth term of the symmetric mass matrix.  The lower triangular part of the matrix is defined
        """ # nopep8
        return self._cards[1].get_value("m22")

    @m22.setter
    def m22(self, value: float) -> None:
        """Set the m22 property."""
        self._cards[1].set_value("m22", value)

    @property
    def m31(self) -> float:
        """Get or set the The IJth term of the symmetric mass matrix.  The lower triangular part of the matrix is defined
        """ # nopep8
        return self._cards[1].get_value("m31")

    @m31.setter
    def m31(self, value: float) -> None:
        """Set the m31 property."""
        self._cards[1].set_value("m31", value)

    @property
    def m32(self) -> float:
        """Get or set the The IJth term of the symmetric mass matrix.  The lower triangular part of the matrix is defined
        """ # nopep8
        return self._cards[1].get_value("m32")

    @m32.setter
    def m32(self, value: float) -> None:
        """Set the m32 property."""
        self._cards[1].set_value("m32", value)

    @property
    def m33(self) -> float:
        """Get or set the The IJth term of the symmetric mass matrix.  The lower triangular part of the matrix is defined
        """ # nopep8
        return self._cards[1].get_value("m33")

    @m33.setter
    def m33(self, value: float) -> None:
        """Set the m33 property."""
        self._cards[1].set_value("m33", value)

    @property
    def m41(self) -> float:
        """Get or set the The IJth term of the symmetric mass matrix.  The lower triangular part of the matrix is defined
        """ # nopep8
        return self._cards[1].get_value("m41")

    @m41.setter
    def m41(self, value: float) -> None:
        """Set the m41 property."""
        self._cards[1].set_value("m41", value)

    @property
    def m41(self) -> float:
        """Get or set the The IJth term of the symmetric mass matrix.  The lower triangular part of the matrix is defined
        """ # nopep8
        return self._cards[2].get_value("m41")

    @m41.setter
    def m41(self, value: float) -> None:
        """Set the m41 property."""
        self._cards[2].set_value("m41", value)

    @property
    def m43(self) -> float:
        """Get or set the The IJth term of the symmetric mass matrix.  The lower triangular part of the matrix is defined
        """ # nopep8
        return self._cards[2].get_value("m43")

    @m43.setter
    def m43(self, value: float) -> None:
        """Set the m43 property."""
        self._cards[2].set_value("m43", value)

    @property
    def m44(self) -> float:
        """Get or set the The IJth term of the symmetric mass matrix.  The lower triangular part of the matrix is defined
        """ # nopep8
        return self._cards[2].get_value("m44")

    @m44.setter
    def m44(self, value: float) -> None:
        """Set the m44 property."""
        self._cards[2].set_value("m44", value)

    @property
    def m51(self) -> float:
        """Get or set the The IJth term of the symmetric mass matrix.  The lower triangular part of the matrix is defined
        """ # nopep8
        return self._cards[2].get_value("m51")

    @m51.setter
    def m51(self, value: float) -> None:
        """Set the m51 property."""
        self._cards[2].set_value("m51", value)

    @property
    def m52(self) -> float:
        """Get or set the The IJth term of the symmetric mass matrix.  The lower triangular part of the matrix is defined
        """ # nopep8
        return self._cards[2].get_value("m52")

    @m52.setter
    def m52(self, value: float) -> None:
        """Set the m52 property."""
        self._cards[2].set_value("m52", value)

    @property
    def m53(self) -> float:
        """Get or set the The IJth term of the symmetric mass matrix.  The lower triangular part of the matrix is defined
        """ # nopep8
        return self._cards[2].get_value("m53")

    @m53.setter
    def m53(self, value: float) -> None:
        """Set the m53 property."""
        self._cards[2].set_value("m53", value)

    @property
    def m54(self) -> float:
        """Get or set the The IJth term of the symmetric mass matrix.  The lower triangular part of the matrix is defined
        """ # nopep8
        return self._cards[2].get_value("m54")

    @m54.setter
    def m54(self, value: float) -> None:
        """Set the m54 property."""
        self._cards[2].set_value("m54", value)

    @property
    def m55(self) -> float:
        """Get or set the The IJth term of the symmetric mass matrix.  The lower triangular part of the matrix is defined
        """ # nopep8
        return self._cards[3].get_value("m55")

    @m55.setter
    def m55(self, value: float) -> None:
        """Set the m55 property."""
        self._cards[3].set_value("m55", value)

    @property
    def m61(self) -> float:
        """Get or set the The IJth term of the symmetric mass matrix.  The lower triangular part of the matrix is defined
        """ # nopep8
        return self._cards[3].get_value("m61")

    @m61.setter
    def m61(self, value: float) -> None:
        """Set the m61 property."""
        self._cards[3].set_value("m61", value)

    @property
    def m62(self) -> float:
        """Get or set the The IJth term of the symmetric mass matrix.  The lower triangular part of the matrix is defined
        """ # nopep8
        return self._cards[3].get_value("m62")

    @m62.setter
    def m62(self, value: float) -> None:
        """Set the m62 property."""
        self._cards[3].set_value("m62", value)

    @property
    def m63(self) -> float:
        """Get or set the The IJth term of the symmetric mass matrix.  The lower triangular part of the matrix is defined
        """ # nopep8
        return self._cards[3].get_value("m63")

    @m63.setter
    def m63(self, value: float) -> None:
        """Set the m63 property."""
        self._cards[3].set_value("m63", value)

    @property
    def m64(self) -> float:
        """Get or set the The IJth term of the symmetric mass matrix.  The lower triangular part of the matrix is defined
        """ # nopep8
        return self._cards[3].get_value("m64")

    @m64.setter
    def m64(self, value: float) -> None:
        """Set the m64 property."""
        self._cards[3].set_value("m64", value)

    @property
    def m65(self) -> float:
        """Get or set the The IJth term of the symmetric mass matrix.  The lower triangular part of the matrix is defined
        """ # nopep8
        return self._cards[3].get_value("m65")

    @m65.setter
    def m65(self, value: float) -> None:
        """Set the m65 property."""
        self._cards[3].set_value("m65", value)

    @property
    def m66(self) -> float:
        """Get or set the The IJth term of the symmetric mass matrix.  The lower triangular part of the matrix is defined
        """ # nopep8
        return self._cards[3].get_value("m66")

    @m66.setter
    def m66(self, value: float) -> None:
        """Set the m66 property."""
        self._cards[3].set_value("m66", value)

    @property
    def id_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given id."""
        return self._get_link_by_attr("NODE", "nid", self.id, "parts")

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

