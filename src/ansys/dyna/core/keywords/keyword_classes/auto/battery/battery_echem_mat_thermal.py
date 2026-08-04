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

"""Module providing the BatteryEchemMatThermal class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BATTERYECHEMMATTHERMAL_CARD0 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("cp", float, 10, 10, None),
    FieldSchema("htc", float, 20, 10, None),
    FieldSchema("t0", float, 30, 10, None),
    FieldSchema("tamb", float, 40, 10, None),
    FieldSchema("cmass", float, 50, 10, None),
    FieldSchema("emiss", float, 60, 10, None),
)

class BatteryEchemMatThermal(KeywordBase):
    """DYNA BATTERY_ECHEM_MAT_THERMAL keyword"""

    keyword = "BATTERY"
    subkeyword = "ECHEM_MAT_THERMAL"

    def __init__(self, **kwargs):
        """Initialize the BatteryEchemMatThermal class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMMATTHERMAL_CARD0,
                **kwargs,
            ),
        ]
    @property
    def cp(self) -> typing.Optional[float]:
        """Get or set the Cell heat capacity C_p. (units: J?kg?^(-1) K^(-1))
        """ # nopep8
        return self._cards[0].get_value("cp")

    @cp.setter
    def cp(self, value: float) -> None:
        """Set the cp property."""
        self._cards[0].set_value("cp", value)

    @property
    def htc(self) -> typing.Optional[float]:
        """Get or set the Heat Transfer coefficient h. See Remark 1. (units: W.m^(-2) K^(-1))
        """ # nopep8
        return self._cards[0].get_value("htc")

    @htc.setter
    def htc(self, value: float) -> None:
        """Set the htc property."""
        self._cards[0].set_value("htc", value)

    @property
    def t0(self) -> typing.Optional[float]:
        """Get or set the Initial Temperature T. (units: K)
        """ # nopep8
        return self._cards[0].get_value("t0")

    @t0.setter
    def t0(self, value: float) -> None:
        """Set the t0 property."""
        self._cards[0].set_value("t0", value)

    @property
    def tamb(self) -> typing.Optional[float]:
        """Get or set the Ambient temperature T_amb. (units: K)
        """ # nopep8
        return self._cards[0].get_value("tamb")

    @tamb.setter
    def tamb(self, value: float) -> None:
        """Set the tamb property."""
        self._cards[0].set_value("tamb", value)

    @property
    def cmass(self) -> typing.Optional[float]:
        """Get or set the Mass per Area of the cell m_t. See Remark 2. (units: kg.m^(-2))
        """ # nopep8
        return self._cards[0].get_value("cmass")

    @cmass.setter
    def cmass(self, value: float) -> None:
        """Set the cmass property."""
        self._cards[0].set_value("cmass", value)

    @property
    def emiss(self) -> typing.Optional[float]:
        """Get or set the Surface emission coefficient ?_rad.
        """ # nopep8
        return self._cards[0].get_value("emiss")

    @emiss.setter
    def emiss(self, value: float) -> None:
        """Set the emiss property."""
        self._cards[0].set_value("emiss", value)

