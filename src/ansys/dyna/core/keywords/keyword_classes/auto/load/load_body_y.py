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

"""Module providing the LoadBodyY class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_LOADBODYY_CARD0 = (
    FieldSchema("lcid", int, 0, 10, None),
    FieldSchema("sf", float, 10, 10, 1.0),
    FieldSchema("lciddr", int, 20, 10, 0),
    FieldSchema("xc", float, 30, 10, 0.0),
    FieldSchema("yc", float, 40, 10, 0.0),
    FieldSchema("zc", float, 50, 10, 0.0),
    FieldSchema("cid", int, 60, 10, 0),
)

class LoadBodyY(KeywordBase):
    """DYNA LOAD_BODY_Y keyword"""

    keyword = "LOAD"
    subkeyword = "BODY_Y"

    def __init__(self, **kwargs):
        """Initialize the LoadBodyY class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADBODYY_CARD0,
                **kwargs,
            ),        ]
    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def sf(self) -> float:
        """Get or set the Load curve scale factori which applies to both LCID and LCIDDR.
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[0].set_value("sf", value)

    @property
    def lciddr(self) -> int:
        """Get or set the Load curve ID for dynamic relaxation phase. Only if dynamic relaxation is defined.
        """ # nopep8
        return self._cards[0].get_value("lciddr")

    @lciddr.setter
    def lciddr(self, value: int) -> None:
        """Set the lciddr property."""
        self._cards[0].set_value("lciddr", value)

    @property
    def xc(self) -> float:
        """Get or set the X-center of rotation, define for angular velocities.
        """ # nopep8
        return self._cards[0].get_value("xc")

    @xc.setter
    def xc(self, value: float) -> None:
        """Set the xc property."""
        self._cards[0].set_value("xc", value)

    @property
    def yc(self) -> float:
        """Get or set the Y-center of rotation, define for angular velocities.
        """ # nopep8
        return self._cards[0].get_value("yc")

    @yc.setter
    def yc(self, value: float) -> None:
        """Set the yc property."""
        self._cards[0].set_value("yc", value)

    @property
    def zc(self) -> float:
        """Get or set the Z-center of rotation, define for angular velocities.
        """ # nopep8
        return self._cards[0].get_value("zc")

    @zc.setter
    def zc(self, value: float) -> None:
        """Set the zc property."""
        self._cards[0].set_value("zc", value)

    @property
    def cid(self) -> int:
        """Get or set the Coordinate system ID to define acceleration in local coordinate system.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

