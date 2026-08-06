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

"""Module providing the BatteryEchemThermal class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BATTERYECHEMTHERMAL_CARD0 = (
    FieldSchema("tname", str, 0, 10, None),
    FieldSchema("ittype", int, 10, 10, 0),
    FieldSchema("iprt", int, 20, 10, 0),
    FieldSchema("unused", float, 30, 10, None),
    FieldSchema("unused", float, 40, 10, None),
    FieldSchema("unused", float, 50, 10, None),
    FieldSchema("iarr", float, 60, 10, 0.0),
)

_BATTERYECHEMTHERMAL_CARD1 = (
    FieldSchema("filename", str, 0, 256, None),
)

class BatteryEchemThermal(KeywordBase):
    """DYNA BATTERY_ECHEM_THERMAL keyword"""

    keyword = "BATTERY"
    subkeyword = "ECHEM_THERMAL"

    def __init__(self, **kwargs):
        """Initialize the BatteryEchemThermal class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMTHERMAL_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMTHERMAL_CARD1,
                **kwargs,
            ),
        ]
    @property
    def tname(self) -> typing.Optional[str]:
        """Get or set the Thermal material identifier
        """ # nopep8
        return self._cards[0].get_value("tname")

    @tname.setter
    def tname(self, value: str) -> None:
        """Set the tname property."""
        self._cards[0].set_value("tname", value)

    @property
    def ittype(self) -> int:
        """Get or set the Flag for how temperature is determined:
        EQ.0:Constant temperature mode.
        EQ.1:Isothermal temperature with time.
        EQ.2:Thermal coupling with LS - DYNA thermal solver
        """ # nopep8
        return self._cards[0].get_value("ittype")

    @ittype.setter
    def ittype(self, value: int) -> None:
        """Set the ittype property."""
        if value not in [0, 1, None]:
            raise Exception("""ittype must be `None` or one of {0,1}.""")
        self._cards[0].set_value("ittype", value)

    @property
    def iprt(self) -> int:
        """Get or set the Data print in ASCII format
        EQ.0:No data print out.
        EQ.1:Time versus heat flux print out for thermal solver.
        """ # nopep8
        return self._cards[0].get_value("iprt")

    @iprt.setter
    def iprt(self, value: int) -> None:
        """Set the iprt property."""
        if value not in [0, 1, None]:
            raise Exception("""iprt must be `None` or one of {0,1}.""")
        self._cards[0].set_value("iprt", value)

    @property
    def iarr(self) -> float:
        """Get or set the Arrhenius temperature corrections (See Remark 2):
        EQ.0 : Off
        EQ.1 : On
        """ # nopep8
        return self._cards[0].get_value("iarr")

    @iarr.setter
    def iarr(self, value: float) -> None:
        """Set the iarr property."""
        if value not in [0, 1, None]:
            raise Exception("""iarr must be `None` or one of {0,1}.""")
        self._cards[0].set_value("iarr", value)

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the Name of the battery cell output file (ASCII)
        """ # nopep8
        return self._cards[1].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        """Set the filename property."""
        self._cards[1].set_value("filename", value)

