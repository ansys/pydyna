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

"""Module providing the Iga2DBrep class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_IGA2DBREP_CARD0 = (
    FieldSchema("brid", int, 0, 10, None),
)

_IGA2DBREP_CARD1 = (
    FieldSchema("fid1", int, 0, 10, None),
    FieldSchema("fid2", int, 10, 10, None),
    FieldSchema("fid3", int, 20, 10, None),
    FieldSchema("fid4", int, 30, 10, None),
    FieldSchema("fid5", int, 40, 10, None),
    FieldSchema("fid6", int, 50, 10, None),
    FieldSchema("fid7", int, 60, 10, None),
    FieldSchema("fid8", int, 70, 10, None),
)

class Iga2DBrep(KeywordBase):
    """DYNA IGA_2D_BREP keyword"""

    keyword = "IGA"
    subkeyword = "2D_BREP"

    def __init__(self, **kwargs):
        """Initialize the Iga2DBrep class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _IGA2DBREP_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _IGA2DBREP_CARD1,
                **kwargs,
            ),        ]
    @property
    def brid(self) -> typing.Optional[int]:
        """Get or set the Two-dimensional boundary representation ID. A unique number must be chosen.
        """ # nopep8
        return self._cards[0].get_value("brid")

    @brid.setter
    def brid(self, value: int) -> None:
        """Set the brid property."""
        self._cards[0].set_value("brid", value)

    @property
    def fid1(self) -> typing.Optional[int]:
        """Get or set the Parametric face IDs, see *IGA_FACE_UVW.
        """ # nopep8
        return self._cards[1].get_value("fid1")

    @fid1.setter
    def fid1(self, value: int) -> None:
        """Set the fid1 property."""
        self._cards[1].set_value("fid1", value)

    @property
    def fid2(self) -> typing.Optional[int]:
        """Get or set the Parametric face IDs, see *IGA_FACE_UVW.
        """ # nopep8
        return self._cards[1].get_value("fid2")

    @fid2.setter
    def fid2(self, value: int) -> None:
        """Set the fid2 property."""
        self._cards[1].set_value("fid2", value)

    @property
    def fid3(self) -> typing.Optional[int]:
        """Get or set the Parametric face IDs, see *IGA_FACE_UVW.
        """ # nopep8
        return self._cards[1].get_value("fid3")

    @fid3.setter
    def fid3(self, value: int) -> None:
        """Set the fid3 property."""
        self._cards[1].set_value("fid3", value)

    @property
    def fid4(self) -> typing.Optional[int]:
        """Get or set the Parametric face IDs, see *IGA_FACE_UVW.
        """ # nopep8
        return self._cards[1].get_value("fid4")

    @fid4.setter
    def fid4(self, value: int) -> None:
        """Set the fid4 property."""
        self._cards[1].set_value("fid4", value)

    @property
    def fid5(self) -> typing.Optional[int]:
        """Get or set the Parametric face IDs, see *IGA_FACE_UVW.
        """ # nopep8
        return self._cards[1].get_value("fid5")

    @fid5.setter
    def fid5(self, value: int) -> None:
        """Set the fid5 property."""
        self._cards[1].set_value("fid5", value)

    @property
    def fid6(self) -> typing.Optional[int]:
        """Get or set the Parametric face IDs, see *IGA_FACE_UVW.
        """ # nopep8
        return self._cards[1].get_value("fid6")

    @fid6.setter
    def fid6(self, value: int) -> None:
        """Set the fid6 property."""
        self._cards[1].set_value("fid6", value)

    @property
    def fid7(self) -> typing.Optional[int]:
        """Get or set the Parametric face IDs, see *IGA_FACE_UVW.
        """ # nopep8
        return self._cards[1].get_value("fid7")

    @fid7.setter
    def fid7(self, value: int) -> None:
        """Set the fid7 property."""
        self._cards[1].set_value("fid7", value)

    @property
    def fid8(self) -> typing.Optional[int]:
        """Get or set the Parametric face IDs, see *IGA_FACE_UVW.
        """ # nopep8
        return self._cards[1].get_value("fid8")

    @fid8.setter
    def fid8(self, value: int) -> None:
        """Set the fid8 property."""
        self._cards[1].set_value("fid8", value)

