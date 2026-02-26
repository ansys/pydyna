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

"""Module providing the IgaFaceUvw class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_IGAFACEUVW_CARD0 = (
    FieldSchema("fid", int, 0, 10, None),
    FieldSchema("fxyzid", int, 10, 10, None),
    FieldSchema("nid", int, 20, 10, None),
    FieldSchema("sense", int, 30, 10, 0),
)

_IGAFACEUVW_CARD1 = (
    FieldSchema("brid1", int, 0, 10, None),
    FieldSchema("brid2", int, 10, 10, None),
    FieldSchema("brid3", int, 20, 10, None),
    FieldSchema("brid4", int, 30, 10, None),
    FieldSchema("brid5", int, 40, 10, None),
    FieldSchema("brid6", int, 50, 10, None),
    FieldSchema("brid7", int, 60, 10, None),
    FieldSchema("brid8", int, 70, 10, None),
)

class IgaFaceUvw(KeywordBase):
    """DYNA IGA_FACE_UVW keyword"""

    keyword = "IGA"
    subkeyword = "FACE_UVW"

    def __init__(self, **kwargs):
        """Initialize the IgaFaceUvw class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _IGAFACEUVW_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _IGAFACEUVW_CARD1,
                **kwargs,
            ),        ]
    @property
    def fid(self) -> typing.Optional[int]:
        """Get or set the Parametric face ID. A unique number must be chosen.
        """ # nopep8
        return self._cards[0].get_value("fid")

    @fid.setter
    def fid(self, value: int) -> None:
        """Set the fid property."""
        self._cards[0].set_value("fid", value)

    @property
    def fxyzid(self) -> typing.Optional[int]:
        """Get or set the Physical face IDs, see *IGA_FACE_XYZ.
        """ # nopep8
        return self._cards[0].get_value("fxyzid")

    @fxyzid.setter
    def fxyzid(self, value: int) -> None:
        """Set the fxyzid property."""
        self._cards[0].set_value("fxyzid", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Parametric bivariate NURBS ID, see *IGA_2D_NURBS_UVW.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[0].set_value("nid", value)

    @property
    def sense(self) -> int:
        """Get or set the Sense of Orientation with respect to the ppysical face.
        EQ.0: Same(Default)
        EQ.1 : Reversed.
        """ # nopep8
        return self._cards[0].get_value("sense")

    @sense.setter
    def sense(self, value: int) -> None:
        """Set the sense property."""
        if value not in [0, 1, None]:
            raise Exception("""sense must be `None` or one of {0,1}.""")
        self._cards[0].set_value("sense", value)

    @property
    def brid1(self) -> typing.Optional[int]:
        """Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
        """ # nopep8
        return self._cards[1].get_value("brid1")

    @brid1.setter
    def brid1(self, value: int) -> None:
        """Set the brid1 property."""
        self._cards[1].set_value("brid1", value)

    @property
    def brid2(self) -> typing.Optional[int]:
        """Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
        """ # nopep8
        return self._cards[1].get_value("brid2")

    @brid2.setter
    def brid2(self, value: int) -> None:
        """Set the brid2 property."""
        self._cards[1].set_value("brid2", value)

    @property
    def brid3(self) -> typing.Optional[int]:
        """Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
        """ # nopep8
        return self._cards[1].get_value("brid3")

    @brid3.setter
    def brid3(self, value: int) -> None:
        """Set the brid3 property."""
        self._cards[1].set_value("brid3", value)

    @property
    def brid4(self) -> typing.Optional[int]:
        """Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
        """ # nopep8
        return self._cards[1].get_value("brid4")

    @brid4.setter
    def brid4(self, value: int) -> None:
        """Set the brid4 property."""
        self._cards[1].set_value("brid4", value)

    @property
    def brid5(self) -> typing.Optional[int]:
        """Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
        """ # nopep8
        return self._cards[1].get_value("brid5")

    @brid5.setter
    def brid5(self, value: int) -> None:
        """Set the brid5 property."""
        self._cards[1].set_value("brid5", value)

    @property
    def brid6(self) -> typing.Optional[int]:
        """Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
        """ # nopep8
        return self._cards[1].get_value("brid6")

    @brid6.setter
    def brid6(self, value: int) -> None:
        """Set the brid6 property."""
        self._cards[1].set_value("brid6", value)

    @property
    def brid7(self) -> typing.Optional[int]:
        """Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
        """ # nopep8
        return self._cards[1].get_value("brid7")

    @brid7.setter
    def brid7(self, value: int) -> None:
        """Set the brid7 property."""
        self._cards[1].set_value("brid7", value)

    @property
    def brid8(self) -> typing.Optional[int]:
        """Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
        """ # nopep8
        return self._cards[1].get_value("brid8")

    @brid8.setter
    def brid8(self, value: int) -> None:
        """Set the brid8 property."""
        self._cards[1].set_value("brid8", value)

