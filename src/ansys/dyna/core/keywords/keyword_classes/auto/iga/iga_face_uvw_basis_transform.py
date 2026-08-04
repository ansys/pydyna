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

"""Module providing the IgaFaceUvwBasisTransform class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_IGAFACEUVWBASISTRANSFORM_CARD0 = (
    FieldSchema("fid", int, 0, 10, None),
    FieldSchema("fxyzid", int, 10, 10, None),
    FieldSchema("patchid", int, 20, 10, None),
    FieldSchema("sense", int, 30, 10, 0),
)

_IGAFACEUVWBASISTRANSFORM_CARD1 = (
    FieldSchema("elid", int, 0, 10, None),
    FieldSchema("faceid", int, 10, 10, None),
)

class IgaFaceUvwBasisTransform(KeywordBase):
    """DYNA IGA_FACE_UVW_BASIS_TRANSFORM keyword"""

    keyword = "IGA"
    subkeyword = "FACE_UVW_BASIS_TRANSFORM"

    def __init__(self, **kwargs):
        """Initialize the IgaFaceUvwBasisTransform class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _IGAFACEUVWBASISTRANSFORM_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _IGAFACEUVWBASISTRANSFORM_CARD1,
                **kwargs,
            ),
        ]
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
    def patchid(self) -> typing.Optional[int]:
        """Get or set the Parametric bivariate NURBS patch ID, see *IGA_2D_NURBS_UVW.
        """ # nopep8
        return self._cards[0].get_value("patchid")

    @patchid.setter
    def patchid(self, value: int) -> None:
        """Set the patchid property."""
        self._cards[0].set_value("patchid", value)

    @property
    def sense(self) -> int:
        """Get or set the Sense of Orientation with respect to the physical face.
        EQ.0: Same(Default)
        EQ.1: Reversed.
        """ # nopep8
        return self._cards[0].get_value("sense")

    @sense.setter
    def sense(self, value: int) -> None:
        """Set the sense property."""
        if value not in [0, 1, None]:
            raise Exception("""sense must be `None` or one of {0,1}.""")
        self._cards[0].set_value("sense", value)

    @property
    def elid(self) -> typing.Optional[int]:
        """Get or set the Element ID local to a trivariate basis transform. See *IGA_3D_BASIS_TRANSFORM_XYZ and Remark 5. A unique number must be chosen
        """ # nopep8
        return self._cards[1].get_value("elid")

    @elid.setter
    def elid(self, value: int) -> None:
        """Set the elid property."""
        self._cards[1].set_value("elid", value)

    @property
    def faceid(self) -> typing.Optional[int]:
        """Get or set the Face ID (see Remark 6).
        """ # nopep8
        return self._cards[1].get_value("faceid")

    @faceid.setter
    def faceid(self, value: int) -> None:
        """Set the faceid property."""
        self._cards[1].set_value("faceid", value)

