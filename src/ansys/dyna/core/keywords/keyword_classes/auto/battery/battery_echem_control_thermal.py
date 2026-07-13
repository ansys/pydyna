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

"""Module providing the BatteryEchemControlThermal class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BATTERYECHEMCONTROLTHERMAL_CARD0 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("ittype", int, 10, 10, 0),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("iarr", int, 60, 10, 0),
    FieldSchema("iheat", int, 70, 10, 0),
)

class BatteryEchemControlThermal(KeywordBase):
    """DYNA BATTERY_ECHEM_CONTROL_THERMAL keyword"""

    keyword = "BATTERY"
    subkeyword = "ECHEM_CONTROL_THERMAL"

    def __init__(self, **kwargs):
        """Initialize the BatteryEchemControlThermal class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMCONTROLTHERMAL_CARD0,
                **kwargs,
            ),
        ]
    @property
    def ittype(self) -> int:
        """Get or set the Flag for how temperature is determined (see Remark 1):
        EQ.0 : Constant temperature mode
        EQ.1 : Isothermal temperature with time
        """ # nopep8
        return self._cards[0].get_value("ittype")

    @ittype.setter
    def ittype(self, value: int) -> None:
        """Set the ittype property."""
        if value not in [0, 1, None]:
            raise Exception("""ittype must be `None` or one of {0,1}.""")
        self._cards[0].set_value("ittype", value)

    @property
    def iarr(self) -> int:
        """Get or set the Arrhenius temperature corrections (See Remark 2):
        EQ.0 : Off
        EQ.1 : On
        """ # nopep8
        return self._cards[0].get_value("iarr")

    @iarr.setter
    def iarr(self, value: int) -> None:
        """Set the iarr property."""
        if value not in [0, 1, None]:
            raise Exception("""iarr must be `None` or one of {0,1}.""")
        self._cards[0].set_value("iarr", value)

    @property
    def iheat(self) -> int:
        """Get or set the Heat Generation Method:
        EQ.0 : Energy balance method.
        EQ.1 : Local Evaluation method
        """ # nopep8
        return self._cards[0].get_value("iheat")

    @iheat.setter
    def iheat(self, value: int) -> None:
        """Set the iheat property."""
        if value not in [0, 1, None]:
            raise Exception("""iheat must be `None` or one of {0,1}.""")
        self._cards[0].set_value("iheat", value)

