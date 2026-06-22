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

"""Module providing the IgaFaceXyz class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.series_card import SeriesCard
from ansys.dyna.core.lib.keyword_base import KeywordBase

_IGAFACEXYZ_CARD0 = (
    FieldSchema("fid", int, 0, 10, None),
    FieldSchema("nid", int, 10, 10, None),
    FieldSchema("ori", int, 20, 10, 0),
    FieldSchema("psid", int, 30, 10, None),
    FieldSchema("esid", int, 40, 10, None),
)

class IgaFaceXyz(KeywordBase):
    """DYNA IGA_FACE_XYZ keyword"""

    keyword = "IGA"
    subkeyword = "FACE_XYZ"

    def __init__(self, **kwargs):
        """Initialize the IgaFaceXyz class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _IGAFACEXYZ_CARD0,
                **kwargs,
            ),
            SeriesCard(
                "brid",
                8,
                10,
                int,
                None,
                data = kwargs.get("brid")),
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
    def nid(self) -> typing.Optional[int]:
        """Get or set the Physical bivariate NURBS ID, see *IGA_2D_NURBS_XYZ.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[0].set_value("nid", value)

    @property
    def ori(self) -> int:
        """Get or set the Orientation with respect to the physical bivariate NURBS.
        EQ.0: Same
        EQ.1 : Reversed.
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
    def brid(self) -> SeriesCard:
        """boundary representation IDs.."""
        return self._cards[1]

    @brid.setter
    def brid(self, value: typing.List) -> None:
        self._cards[1].data = value

