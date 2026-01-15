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

"""Module providing the DatabaseCpmSensor class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DATABASECPMSENSOR_CARD0 = (
    FieldSchema("dt", float, 0, 10, None),
    FieldSchema("binary", int, 10, 10, 1),
)

_DATABASECPMSENSOR_CARD1 = (
    FieldSchema("segsid", int, 0, 10, None),
    FieldSchema("offset", float, 10, 10, None),
    FieldSchema("r_lx", float, 20, 10, None, "r/lx"),
    FieldSchema("len_ly", float, 30, 10, None, "len/ly"),
    FieldSchema("lz", float, 40, 10, None),
)

class DatabaseCpmSensor(KeywordBase):
    """DYNA DATABASE_CPM_SENSOR keyword"""

    keyword = "DATABASE"
    subkeyword = "CPM_SENSOR"

    def __init__(self, **kwargs):
        """Initialize the DatabaseCpmSensor class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DATABASECPMSENSOR_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DATABASECPMSENSOR_CARD1,
                **kwargs,
            ),        ]
    @property
    def dt(self) -> typing.Optional[float]:
        """Get or set the Output interval
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        """Set the dt property."""
        self._cards[0].set_value("dt", value)

    @property
    def binary(self) -> int:
        """Get or set the Flag for the binary file
        EQ.1:  ASCII file is written,
        EQ.2:  Data written to the binary file binout,
        EQ.3:  ASCII file  is written and the data written to the binary file binout
        """ # nopep8
        return self._cards[0].get_value("binary")

    @binary.setter
    def binary(self, value: int) -> None:
        """Set the binary property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""binary must be `None` or one of {1,2,3}.""")
        self._cards[0].set_value("binary", value)

    @property
    def segsid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID
        """ # nopep8
        return self._cards[1].get_value("segsid")

    @segsid.setter
    def segsid(self, value: int) -> None:
        """Set the segsid property."""
        self._cards[1].set_value("segsid", value)

    @property
    def offset(self) -> typing.Optional[float]:
        """Get or set the Offset distance between the center of the sensor and  the segment center. If it is positive, it is on the side pointed to by the segment normal vector.
        """ # nopep8
        return self._cards[1].get_value("offset")

    @offset.setter
    def offset(self, value: float) -> None:
        """Set the offset property."""
        self._cards[1].set_value("offset", value)

    @property
    def r_lx(self) -> typing.Optional[float]:
        """Get or set the Radius(sphere)/length in local X direction(rectangular) of the sensor.
        """ # nopep8
        return self._cards[1].get_value("r_lx")

    @r_lx.setter
    def r_lx(self, value: float) -> None:
        """Set the r_lx property."""
        self._cards[1].set_value("r_lx", value)

    @property
    def len_ly(self) -> typing.Optional[float]:
        """Get or set the Length(cylinder)/length in local Y direction(rectangular) of the sensor.
        """ # nopep8
        return self._cards[1].get_value("len_ly")

    @len_ly.setter
    def len_ly(self, value: float) -> None:
        """Set the len_ly property."""
        self._cards[1].set_value("len_ly", value)

    @property
    def lz(self) -> typing.Optional[float]:
        """Get or set the Length in local Z direction(rectangular) of the sensor.
        """ # nopep8
        return self._cards[1].get_value("lz")

    @lz.setter
    def lz(self, value: float) -> None:
        """Set the lz property."""
        self._cards[1].set_value("lz", value)

