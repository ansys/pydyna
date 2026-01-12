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

"""Module providing the LoadFaceXyzSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_LOADFACEXYZSET_CARD0 = (
    FieldSchema("fxyzsid", int, 0, 10, None),
    FieldSchema("lcid", int, 10, 10, None),
    FieldSchema("sf", float, 20, 10, 1.0),
    FieldSchema("at", float, 30, 10, 0.0),
)

class LoadFaceXyzSet(KeywordBase):
    """DYNA LOAD_FACE_XYZ_SET keyword"""

    keyword = "LOAD"
    subkeyword = "FACE_XYZ_SET"

    def __init__(self, **kwargs):
        """Initialize the LoadFaceXyzSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADFACEXYZSET_CARD0,
                **kwargs,
            ),        ]
    @property
    def fxyzsid(self) -> typing.Optional[int]:
        """Get or set the Physical face set ID; see *SET_IGA_FACE_XYZ
        """ # nopep8
        return self._cards[0].get_value("fxyzsid")

    @fxyzsid.setter
    def fxyzsid(self, value: int) -> None:
        """Set the fxyzsid property."""
        self._cards[0].set_value("fxyzsid", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID (see *DEFINE_CURVE). The load curve must provide pressure as a function of time
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def sf(self) -> float:
        """Get or set the Load curve scale factor
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[0].set_value("sf", value)

    @property
    def at(self) -> float:
        """Get or set the Arrival time for pressure or birth time of pressure
        """ # nopep8
        return self._cards[0].get_value("at")

    @at.setter
    def at(self, value: float) -> None:
        """Set the at property."""
        self._cards[0].set_value("at", value)

