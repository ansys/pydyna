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

"""Module providing the Iga1DBrep class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_IGA1DBREP_CARD0 = (
    FieldSchema("brid", int, 0, 10, None),
)

_IGA1DBREP_CARD1 = (
    FieldSchema("eid1", int, 0, 10, None),
    FieldSchema("eid2", int, 10, 10, None),
    FieldSchema("eid3", int, 20, 10, None),
    FieldSchema("eid4", int, 30, 10, None),
    FieldSchema("eid5", int, 40, 10, None),
    FieldSchema("eid6", int, 50, 10, None),
    FieldSchema("eid7", int, 60, 10, None),
    FieldSchema("eid8", int, 70, 10, None),
)

class Iga1DBrep(KeywordBase):
    """DYNA IGA_1D_BREP keyword"""

    keyword = "IGA"
    subkeyword = "1D_BREP"

    def __init__(self, **kwargs):
        """Initialize the Iga1DBrep class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _IGA1DBREP_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _IGA1DBREP_CARD1,
                **kwargs,
            ),        ]
    @property
    def brid(self) -> typing.Optional[int]:
        """Get or set the One-dimensional boundary representation ID. A unique number must be chosen.
        """ # nopep8
        return self._cards[0].get_value("brid")

    @brid.setter
    def brid(self, value: int) -> None:
        """Set the brid property."""
        self._cards[0].set_value("brid", value)

    @property
    def eid1(self) -> typing.Optional[int]:
        """Get or set the Parametric edge IDs, see *IGA_EDGE_UVW.
        """ # nopep8
        return self._cards[1].get_value("eid1")

    @eid1.setter
    def eid1(self, value: int) -> None:
        """Set the eid1 property."""
        self._cards[1].set_value("eid1", value)

    @property
    def eid2(self) -> typing.Optional[int]:
        """Get or set the Parametric edge IDs, see *IGA_EDGE_UVW.
        """ # nopep8
        return self._cards[1].get_value("eid2")

    @eid2.setter
    def eid2(self, value: int) -> None:
        """Set the eid2 property."""
        self._cards[1].set_value("eid2", value)

    @property
    def eid3(self) -> typing.Optional[int]:
        """Get or set the Parametric edge IDs, see *IGA_EDGE_UVW.
        """ # nopep8
        return self._cards[1].get_value("eid3")

    @eid3.setter
    def eid3(self, value: int) -> None:
        """Set the eid3 property."""
        self._cards[1].set_value("eid3", value)

    @property
    def eid4(self) -> typing.Optional[int]:
        """Get or set the Parametric edge IDs, see *IGA_EDGE_UVW.
        """ # nopep8
        return self._cards[1].get_value("eid4")

    @eid4.setter
    def eid4(self, value: int) -> None:
        """Set the eid4 property."""
        self._cards[1].set_value("eid4", value)

    @property
    def eid5(self) -> typing.Optional[int]:
        """Get or set the Parametric edge IDs, see *IGA_EDGE_UVW.
        """ # nopep8
        return self._cards[1].get_value("eid5")

    @eid5.setter
    def eid5(self, value: int) -> None:
        """Set the eid5 property."""
        self._cards[1].set_value("eid5", value)

    @property
    def eid6(self) -> typing.Optional[int]:
        """Get or set the Parametric edge IDs, see *IGA_EDGE_UVW.
        """ # nopep8
        return self._cards[1].get_value("eid6")

    @eid6.setter
    def eid6(self, value: int) -> None:
        """Set the eid6 property."""
        self._cards[1].set_value("eid6", value)

    @property
    def eid7(self) -> typing.Optional[int]:
        """Get or set the Parametric edge IDs, see *IGA_EDGE_UVW.
        """ # nopep8
        return self._cards[1].get_value("eid7")

    @eid7.setter
    def eid7(self, value: int) -> None:
        """Set the eid7 property."""
        self._cards[1].set_value("eid7", value)

    @property
    def eid8(self) -> typing.Optional[int]:
        """Get or set the Parametric edge IDs, see *IGA_EDGE_UVW.
        """ # nopep8
        return self._cards[1].get_value("eid8")

    @eid8.setter
    def eid8(self, value: int) -> None:
        """Set the eid8 property."""
        self._cards[1].set_value("eid8", value)

