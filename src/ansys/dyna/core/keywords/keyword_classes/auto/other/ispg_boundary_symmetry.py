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

"""Module providing the IspgBoundarySymmetry class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ISPGBOUNDARYSYMMETRY_CARD0 = (
    FieldSchema("ndir", int, 0, 10, None),
    FieldSchema("coord1", float, 10, 10, None),
    FieldSchema("coord2", float, 20, 10, None),
    FieldSchema("iloc", int, 30, 10, 0),
)

class IspgBoundarySymmetry(KeywordBase):
    """DYNA ISPG_BOUNDARY_SYMMETRY keyword"""

    keyword = "ISPG"
    subkeyword = "BOUNDARY_SYMMETRY"

    def __init__(self, **kwargs):
        """Initialize the IspgBoundarySymmetry class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ISPGBOUNDARYSYMMETRY_CARD0,
                **kwargs,
            ),
        ]
    @property
    def ndir(self) -> typing.Optional[int]:
        """Get or set the Normal direction of the symmetric planes used to define the symmetric boundary condition:
        EQ.1: Global X - axis
        EQ.2: Global Y - axis
        EQ.3: Global Z - axis
        """ # nopep8
        return self._cards[0].get_value("ndir")

    @ndir.setter
    def ndir(self, value: int) -> None:
        """Set the ndir property."""
        self._cards[0].set_value("ndir", value)

    @property
    def coord1(self) -> typing.Optional[float]:
        """Get or set the Minimum coordinate value along the direction set with NDIR (ignored if ILOC = 2)
        """ # nopep8
        return self._cards[0].get_value("coord1")

    @coord1.setter
    def coord1(self, value: float) -> None:
        """Set the coord1 property."""
        self._cards[0].set_value("coord1", value)

    @property
    def coord2(self) -> typing.Optional[float]:
        """Get or set the Maximum coordinate value along the direction set with NDIR (ignored if ILOC = 1)
        """ # nopep8
        return self._cards[0].get_value("coord2")

    @coord2.setter
    def coord2(self, value: float) -> None:
        """Set the coord2 property."""
        self._cards[0].set_value("coord2", value)

    @property
    def iloc(self) -> int:
        """Get or set the Location of symmetric planes:
        EQ.0: Symmetric planes at both coordinates COORD1 and COORD2
        EQ.1: Symmetric plane at coordinate COORD1
        EQ.2: Symmetric plane at coordinate COORD2
        """ # nopep8
        return self._cards[0].get_value("iloc")

    @iloc.setter
    def iloc(self, value: int) -> None:
        """Set the iloc property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""iloc must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("iloc", value)

