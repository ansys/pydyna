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

"""Module providing the IcfdDatabaseTpd class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDDATABASETPD_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("x1", float, 10, 10, None),
    FieldSchema("y1", float, 20, 10, None),
    FieldSchema("z1", float, 30, 10, None),
    FieldSchema("x2", float, 40, 10, None),
    FieldSchema("y2", float, 50, 10, None),
    FieldSchema("z2", float, 60, 10, None),
)

_ICFDDATABASETPD_CARD1 = (
    FieldSchema("dtout", float, 0, 10, None),
    FieldSchema("rad1", float, 10, 10, None),
    FieldSchema("rad2", float, 20, 10, None),
)

class IcfdDatabaseTpd(KeywordBase):
    """DYNA ICFD_DATABASE_TPD keyword"""

    keyword = "ICFD"
    subkeyword = "DATABASE_TPD"

    def __init__(self, **kwargs):
        """Initialize the IcfdDatabaseTpd class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDDATABASETPD_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ICFDDATABASETPD_CARD1,
                **kwargs,
            ),
        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID used to plot multiple files if multiple *ICFD_DATABASE_TPD keywords are defined.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def x1(self) -> typing.Optional[float]:
        """Get or set the Coordinates of the center of the upstream sphere where the upstream average pressure is computed.
        """ # nopep8
        return self._cards[0].get_value("x1")

    @x1.setter
    def x1(self, value: float) -> None:
        """Set the x1 property."""
        self._cards[0].set_value("x1", value)

    @property
    def y1(self) -> typing.Optional[float]:
        """Get or set the Coordinates of the center of the upstream sphere where the upstream average pressure is computed.
        """ # nopep8
        return self._cards[0].get_value("y1")

    @y1.setter
    def y1(self, value: float) -> None:
        """Set the y1 property."""
        self._cards[0].set_value("y1", value)

    @property
    def z1(self) -> typing.Optional[float]:
        """Get or set the Coordinates of the center of the upstream sphere where the upstream average pressure is computed.
        """ # nopep8
        return self._cards[0].get_value("z1")

    @z1.setter
    def z1(self, value: float) -> None:
        """Set the z1 property."""
        self._cards[0].set_value("z1", value)

    @property
    def x2(self) -> typing.Optional[float]:
        """Get or set the Coordinates of the center of the downstream sphere where the downstream average pressure is computed.
        """ # nopep8
        return self._cards[0].get_value("x2")

    @x2.setter
    def x2(self, value: float) -> None:
        """Set the x2 property."""
        self._cards[0].set_value("x2", value)

    @property
    def y2(self) -> typing.Optional[float]:
        """Get or set the Coordinates of the center of the downstream sphere where the downstream average pressure is computed.
        """ # nopep8
        return self._cards[0].get_value("y2")

    @y2.setter
    def y2(self, value: float) -> None:
        """Set the y2 property."""
        self._cards[0].set_value("y2", value)

    @property
    def z2(self) -> typing.Optional[float]:
        """Get or set the Coordinates of the center of the downstream sphere where the downstream average pressure is computed.
        """ # nopep8
        return self._cards[0].get_value("z2")

    @z2.setter
    def z2(self, value: float) -> None:
        """Set the z2 property."""
        self._cards[0].set_value("z2", value)

    @property
    def dtout(self) -> typing.Optional[float]:
        """Get or set the Time interval to print the output. If DTOUT is equal to 0.0, then the ICFD timestep will be used.
        """ # nopep8
        return self._cards[1].get_value("dtout")

    @dtout.setter
    def dtout(self, value: float) -> None:
        """Set the dtout property."""
        self._cards[1].set_value("dtout", value)

    @property
    def rad1(self) -> typing.Optional[float]:
        """Get or set the Radius of the upstream sphere
        """ # nopep8
        return self._cards[1].get_value("rad1")

    @rad1.setter
    def rad1(self, value: float) -> None:
        """Set the rad1 property."""
        self._cards[1].set_value("rad1", value)

    @property
    def rad2(self) -> typing.Optional[float]:
        """Get or set the Radius of the downstream sphere
        """ # nopep8
        return self._cards[1].get_value("rad2")

    @rad2.setter
    def rad2(self, value: float) -> None:
        """Set the rad2 property."""
        self._cards[1].set_value("rad2", value)

