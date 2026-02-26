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

"""Module providing the DatabasePblastSensor class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DATABASEPBLASTSENSOR_CARD0 = (
    FieldSchema("dt", float, 0, 10, 0.0),
    FieldSchema("binary", int, 10, 10, 3),
)

_DATABASEPBLASTSENSOR_CARD1 = (
    FieldSchema("id", int, 0, 10, 0),
    FieldSchema("itype", int, 10, 10, 0),
    FieldSchema("offset", float, 20, 10, 0.0),
    FieldSchema("radius", float, 30, 10, 0.0),
)

class DatabasePblastSensor(KeywordBase):
    """DYNA DATABASE_PBLAST_SENSOR keyword"""

    keyword = "DATABASE"
    subkeyword = "PBLAST_SENSOR"

    def __init__(self, **kwargs):
        """Initialize the DatabasePblastSensor class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DATABASEPBLASTSENSOR_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DATABASEPBLASTSENSOR_CARD1,
                **kwargs,
            ),        ]
    @property
    def dt(self) -> float:
        """Get or set the Output interval.
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        """Set the dt property."""
        self._cards[0].set_value("dt", value)

    @property
    def binary(self) -> int:
        """Get or set the Flag for the binary file:
        EQ.3:	Data is written to the binary file binout.
        """ # nopep8
        return self._cards[0].get_value("binary")

    @binary.setter
    def binary(self, value: int) -> None:
        """Set the binary property."""
        self._cards[0].set_value("binary", value)

    @property
    def id(self) -> int:
        """Get or set the Set ID.
        """ # nopep8
        return self._cards[1].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[1].set_value("id", value)

    @property
    def itype(self) -> int:
        """Get or set the EQ.0: *SET_SHELL ID
        EQ.1: Shell ID.
        """ # nopep8
        return self._cards[1].get_value("itype")

    @itype.setter
    def itype(self, value: int) -> None:
        """Set the itype property."""
        if value not in [0, 1, None]:
            raise Exception("""itype must be `None` or one of {0,1}.""")
        self._cards[1].set_value("itype", value)

    @property
    def offset(self) -> float:
        """Get or set the Offset distance, d, between sensor and the segment center. Where d>0 is along shell normal and d<0 is against shell normal.
        """ # nopep8
        return self._cards[1].get_value("offset")

    @offset.setter
    def offset(self, value: float) -> None:
        """Set the offset property."""
        self._cards[1].set_value("offset", value)

    @property
    def radius(self) -> float:
        """Get or set the Radius of sphere of the sensor.  Please see *DATABASE_CPM_SENSOR for sphere sensor.
        """ # nopep8
        return self._cards[1].get_value("radius")

    @radius.setter
    def radius(self, value: float) -> None:
        """Set the radius property."""
        self._cards[1].set_value("radius", value)

