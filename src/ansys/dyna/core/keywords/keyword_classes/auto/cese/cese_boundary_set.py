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

"""Module providing the CeseBoundarySet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CESEBOUNDARYSET_CARD0 = (
    FieldSchema("ssid", int, 0, 10, None),
    FieldSchema("dof ", int, 10, 10, 101),
    FieldSchema("lcid", int, 20, 10, None),
    FieldSchema("sf", float, 30, 10, 1.0),
)

class CeseBoundarySet(KeywordBase):
    """DYNA CESE_BOUNDARY_SET keyword"""

    keyword = "CESE"
    subkeyword = "BOUNDARY_SET"

    def __init__(self, **kwargs):
        """Initialize the CeseBoundarySet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CESEBOUNDARYSET_CARD0,
                **kwargs,
            ),        ]
    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Segment set  ID.
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[0].set_value("ssid", value)

    @property
    def dof_(self) -> int:
        """Get or set the Applicable degrees-of-freedom:
        EQ.101:  x-velocity,
        EQ.102:  y-velocity,
        EQ.103:  z-velocity,
        EQ.104:  density,
        EQ.105:  pressure ,
        EQ.106:  temperature,
        EQ.201:  x, y & z-velocity,
        EQ.202: x & y-velocity,
        EQ.203:  x & z-velocity,
        EQ.204:  y & z-velocity.
        .
        """ # nopep8
        return self._cards[0].get_value("dof ")

    @dof_.setter
    def dof_(self, value: int) -> None:
        """Set the dof_ property."""
        if value not in [101, 102, 103, 104, 105, 106, 201, 202, 203, 204, None]:
            raise Exception("""dof_ must be `None` or one of {101,102,103,104,105,106,201,202,203,204}.""")
        self._cards[0].set_value("dof ", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the variable value versus time, see *DEFINE_ CURVE.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def sf(self) -> float:
        """Get or set the Load curve scale factor.  (default=1.0).
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[0].set_value("sf", value)

