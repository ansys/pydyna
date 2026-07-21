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

"""Module providing the BatteryEchemCellGeometry class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BATTERYECHEMCELLGEOMETRY_CARD0 = (
    FieldSchema("cellid", int, 0, 10, None),
    FieldSchema("alen", float, 10, 10, None),
    FieldSchema("slen", float, 20, 10, None),
    FieldSchema("clen", float, 30, 10, None),
    FieldSchema("cnlen", float, 40, 10, None),
    FieldSchema("cplen", float, 50, 10, None),
)

_BATTERYECHEMCELLGEOMETRY_CARD1 = (
    FieldSchema("na", int, 0, 10, None),
    FieldSchema("ns", int, 10, 10, None),
    FieldSchema("nc", int, 20, 10, None),
    FieldSchema("nrj", int, 30, 10, 30),
    FieldSchema("qrad", float, 40, 10, 1.0),
)

class BatteryEchemCellGeometry(KeywordBase):
    """DYNA BATTERY_ECHEM_CELL_GEOMETRY keyword"""

    keyword = "BATTERY"
    subkeyword = "ECHEM_CELL_GEOMETRY"

    def __init__(self, **kwargs):
        """Initialize the BatteryEchemCellGeometry class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMCELLGEOMETRY_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMCELLGEOMETRY_CARD1,
                **kwargs,
            ),
        ]
    @property
    def cellid(self) -> typing.Optional[int]:
        """Get or set the Optional Battery Cell ID
        """ # nopep8
        return self._cards[0].get_value("cellid")

    @cellid.setter
    def cellid(self, value: int) -> None:
        """Set the cellid property."""
        self._cards[0].set_value("cellid", value)

    @property
    def alen(self) -> typing.Optional[float]:
        """Get or set the Length of the anode side electrode
        """ # nopep8
        return self._cards[0].get_value("alen")

    @alen.setter
    def alen(self, value: float) -> None:
        """Set the alen property."""
        self._cards[0].set_value("alen", value)

    @property
    def slen(self) -> typing.Optional[float]:
        """Get or set the Length of the separator
        """ # nopep8
        return self._cards[0].get_value("slen")

    @slen.setter
    def slen(self, value: float) -> None:
        """Set the slen property."""
        self._cards[0].set_value("slen", value)

    @property
    def clen(self) -> typing.Optional[float]:
        """Get or set the Length of the cathode side electrode
        """ # nopep8
        return self._cards[0].get_value("clen")

    @clen.setter
    def clen(self, value: float) -> None:
        """Set the clen property."""
        self._cards[0].set_value("clen", value)

    @property
    def cnlen(self) -> typing.Optional[float]:
        """Get or set the Length of the negative current collector
        """ # nopep8
        return self._cards[0].get_value("cnlen")

    @cnlen.setter
    def cnlen(self, value: float) -> None:
        """Set the cnlen property."""
        self._cards[0].set_value("cnlen", value)

    @property
    def cplen(self) -> typing.Optional[float]:
        """Get or set the Length of the positive current collector
        """ # nopep8
        return self._cards[0].get_value("cplen")

    @cplen.setter
    def cplen(self, value: float) -> None:
        """Set the cplen property."""
        self._cards[0].set_value("cplen", value)

    @property
    def na(self) -> typing.Optional[int]:
        """Get or set the Number of elements in the anode electrode
        """ # nopep8
        return self._cards[1].get_value("na")

    @na.setter
    def na(self, value: int) -> None:
        """Set the na property."""
        self._cards[1].set_value("na", value)

    @property
    def ns(self) -> typing.Optional[int]:
        """Get or set the Number of elements in the separator.
        """ # nopep8
        return self._cards[1].get_value("ns")

    @ns.setter
    def ns(self, value: int) -> None:
        """Set the ns property."""
        self._cards[1].set_value("ns", value)

    @property
    def nc(self) -> typing.Optional[int]:
        """Get or set the Number of elements in the cathode electrode
        """ # nopep8
        return self._cards[1].get_value("nc")

    @nc.setter
    def nc(self, value: int) -> None:
        """Set the nc property."""
        self._cards[1].set_value("nc", value)

    @property
    def nrj(self) -> int:
        """Get or set the Number of particles in the radial direction when solving Solid Phase diffusion equation.
        """ # nopep8
        return self._cards[1].get_value("nrj")

    @nrj.setter
    def nrj(self, value: int) -> None:
        """Set the nrj property."""
        self._cards[1].set_value("nrj", value)

    @property
    def qrad(self) -> float:
        """Get or set the Radial mesh geometric grading
        """ # nopep8
        return self._cards[1].get_value("qrad")

    @qrad.setter
    def qrad(self, value: float) -> None:
        """Set the qrad property."""
        self._cards[1].set_value("qrad", value)

