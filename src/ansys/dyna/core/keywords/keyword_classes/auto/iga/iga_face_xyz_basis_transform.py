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

"""Module providing the IgaFaceXyzBasisTransform class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_IGAFACEXYZBASISTRANSFORM_CARD0 = (
    FieldSchema("fid", int, 0, 10, None),
    FieldSchema("patchid", int, 10, 10, None),
    FieldSchema("ori", int, 20, 10, 0),
    FieldSchema("psid", int, 30, 10, None),
    FieldSchema("esid", int, 40, 10, None),
)

_IGAFACEXYZBASISTRANSFORM_CARD1 = (
    FieldSchema("option", str, 0, 10, None),
    FieldSchema("e1", int, 10, 10, None),
    FieldSchema("e2", int, 20, 10, None),
    FieldSchema("e3", int, 30, 10, None),
    FieldSchema("e4", int, 40, 10, None),
    FieldSchema("e5", int, 50, 10, None),
    FieldSchema("e6", int, 60, 10, None),
    FieldSchema("e7", int, 70, 10, None),
)

class IgaFaceXyzBasisTransform(KeywordBase):
    """DYNA IGA_FACE_XYZ_BASIS_TRANSFORM keyword"""

    keyword = "IGA"
    subkeyword = "FACE_XYZ_BASIS_TRANSFORM"

    def __init__(self, **kwargs):
        """Initialize the IgaFaceXyzBasisTransform class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _IGAFACEXYZBASISTRANSFORM_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _IGAFACEXYZBASISTRANSFORM_CARD1,
                **kwargs,
            ),
        ]
    @property
    def fid(self) -> typing.Optional[int]:
        """Get or set the Physical face ID. A unique number must be chosen.
        """ # nopep8
        return self._cards[0].get_value("fid")

    @fid.setter
    def fid(self, value: int) -> None:
        """Set the fid property."""
        self._cards[0].set_value("fid", value)

    @property
    def patchid(self) -> typing.Optional[int]:
        """Get or set the Depending on the keyword option either a physical bivariate NURBS patch ID for no keyword option (see *IGA_2D_NURBS_XYZ) or a bivariate basis transform patch ID for BASIS_TRANSFORM keyword option (see *IGA_2D_BASIS_TRANSFORM_XYZ).
        """ # nopep8
        return self._cards[0].get_value("patchid")

    @patchid.setter
    def patchid(self, value: int) -> None:
        """Set the patchid property."""
        self._cards[0].set_value("patchid", value)

    @property
    def ori(self) -> int:
        """Get or set the Orientation with respect to the physical bivariate NURBS.
        EQ.0: Same
        EQ.1: Reversed.
        """ # nopep8
        return self._cards[0].get_value("ori")

    @ori.setter
    def ori(self, value: int) -> None:
        """Set the ori property."""
        if value not in [0, 1, None]:
            raise Exception("""ori must be `None` or one of {0,1}.""")
        self._cards[0].set_value("ori", value)

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Parametric point set ID, see *IGA_POINT_UVW, *SET_IGA_POINT_UVW.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[0].set_value("psid", value)

    @property
    def esid(self) -> typing.Optional[int]:
        """Get or set the Parametric edge set ID, see *IGA_EDGE_UVW, *SET_IGA_EDGE_UVW.
        """ # nopep8
        return self._cards[0].get_value("esid")

    @esid.setter
    def esid(self, value: int) -> None:
        """Set the esid property."""
        self._cards[0].set_value("esid", value)

    @property
    def option(self) -> typing.Optional[str]:
        """Get or set the Option for specifying the list of elements included in this *IGA_FACE_XYZ. If this card is blank, all elements of the patch will be included. See the table below.
        """ # nopep8
        return self._cards[1].get_value("option")

    @option.setter
    def option(self, value: str) -> None:
        """Set the option property."""
        self._cards[1].set_value("option", value)

    @property
    def e1(self) -> typing.Optional[int]:
        """Get or set the Specified entity. If this card is blank, all elements of the patch will be included. See table below.
        """ # nopep8
        return self._cards[1].get_value("e1")

    @e1.setter
    def e1(self, value: int) -> None:
        """Set the e1 property."""
        self._cards[1].set_value("e1", value)

    @property
    def e2(self) -> typing.Optional[int]:
        """Get or set the Specified entity. If this card is blank, all elements of the patch will be included. See table below.
        """ # nopep8
        return self._cards[1].get_value("e2")

    @e2.setter
    def e2(self, value: int) -> None:
        """Set the e2 property."""
        self._cards[1].set_value("e2", value)

    @property
    def e3(self) -> typing.Optional[int]:
        """Get or set the Specified entity. If this card is blank, all elements of the patch will be included. See table below.
        """ # nopep8
        return self._cards[1].get_value("e3")

    @e3.setter
    def e3(self, value: int) -> None:
        """Set the e3 property."""
        self._cards[1].set_value("e3", value)

    @property
    def e4(self) -> typing.Optional[int]:
        """Get or set the Specified entity. If this card is blank, all elements of the patch will be included. See table below.
        """ # nopep8
        return self._cards[1].get_value("e4")

    @e4.setter
    def e4(self, value: int) -> None:
        """Set the e4 property."""
        self._cards[1].set_value("e4", value)

    @property
    def e5(self) -> typing.Optional[int]:
        """Get or set the Specified entity. If this card is blank, all elements of the patch will be included. See table below.
        """ # nopep8
        return self._cards[1].get_value("e5")

    @e5.setter
    def e5(self, value: int) -> None:
        """Set the e5 property."""
        self._cards[1].set_value("e5", value)

    @property
    def e6(self) -> typing.Optional[int]:
        """Get or set the Specified entity. If this card is blank, all elements of the patch will be included. See table below.
        """ # nopep8
        return self._cards[1].get_value("e6")

    @e6.setter
    def e6(self, value: int) -> None:
        """Set the e6 property."""
        self._cards[1].set_value("e6", value)

    @property
    def e7(self) -> typing.Optional[int]:
        """Get or set the Specified entity. If this card is blank, all elements of the patch will be included. See table below.
        """ # nopep8
        return self._cards[1].get_value("e7")

    @e7.setter
    def e7(self, value: int) -> None:
        """Set the e7 property."""
        self._cards[1].set_value("e7", value)

