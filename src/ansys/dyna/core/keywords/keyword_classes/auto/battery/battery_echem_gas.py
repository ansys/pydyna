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

"""Module providing the BatteryEchemGas class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BATTERYECHEMGAS_CARD0 = (
    FieldSchema("ic2h4", float, 0, 10, 1e-05),
    FieldSchema("io2", float, 10, 10, 1e-05),
    FieldSchema("ico2", float, 20, 10, 1e-05),
    FieldSchema("ih2o", float, 30, 10, 1e-05),
)

_BATTERYECHEMGAS_CARD1 = (
    FieldSchema("ag1", float, 0, 10, None),
    FieldSchema("ag2", float, 10, 10, 5.028e-06),
    FieldSchema("eg1", float, 20, 10, 250000.0),
    FieldSchema("eg2", float, 30, 10, 251000.0),
)

_BATTERYECHEMGAS_CARD2 = (
    FieldSchema("hic2h4", float, 0, 10, None),
    FieldSchema("unused", float, 10, 10, None),
    FieldSchema("hco2", float, 20, 10, None),
    FieldSchema("hh2o", float, 30, 10, None),
    FieldSchema("hilc6", float, 40, 10, None),
    FieldSchema("unused", float, 50, 10, None),
    FieldSchema("hlioh", float, 60, 10, None),
)

class BatteryEchemGas(KeywordBase):
    """DYNA BATTERY_ECHEM_GAS keyword"""

    keyword = "BATTERY"
    subkeyword = "ECHEM_GAS"

    def __init__(self, **kwargs):
        """Initialize the BatteryEchemGas class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMGAS_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMGAS_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMGAS_CARD2,
                **kwargs,
            ),
        ]
    @property
    def ic2h4(self) -> float:
        """Get or set the Initial concentration of C2H2 gas (units: mol/m3)
        """ # nopep8
        return self._cards[0].get_value("ic2h4")

    @ic2h4.setter
    def ic2h4(self, value: float) -> None:
        """Set the ic2h4 property."""
        self._cards[0].set_value("ic2h4", value)

    @property
    def io2(self) -> float:
        """Get or set the Initial concentration of O2 gas (units: mol/m3)
        """ # nopep8
        return self._cards[0].get_value("io2")

    @io2.setter
    def io2(self, value: float) -> None:
        """Set the io2 property."""
        self._cards[0].set_value("io2", value)

    @property
    def ico2(self) -> float:
        """Get or set the Initial concentration of CO2 gas (units: mol/m3)
        """ # nopep8
        return self._cards[0].get_value("ico2")

    @ico2.setter
    def ico2(self, value: float) -> None:
        """Set the ico2 property."""
        self._cards[0].set_value("ico2", value)

    @property
    def ih2o(self) -> float:
        """Get or set the Initial concentration of H2O gas (units: mol/m3)
        """ # nopep8
        return self._cards[0].get_value("ih2o")

    @ih2o.setter
    def ih2o(self, value: float) -> None:
        """Set the ih2o property."""
        self._cards[0].set_value("ih2o", value)

    @property
    def ag1(self) -> typing.Optional[float]:
        """Get or set the Frequency factor for Ethylene oxidation (units: m/s)
        """ # nopep8
        return self._cards[1].get_value("ag1")

    @ag1.setter
    def ag1(self, value: float) -> None:
        """Set the ag1 property."""
        self._cards[1].set_value("ag1", value)

    @property
    def ag2(self) -> float:
        """Get or set the Frequency factor for Lithium hydration (units: m/s)
        """ # nopep8
        return self._cards[1].get_value("ag2")

    @ag2.setter
    def ag2(self, value: float) -> None:
        """Set the ag2 property."""
        self._cards[1].set_value("ag2", value)

    @property
    def eg1(self) -> float:
        """Get or set the Activation energy of Ethylene oxidation (units: J/mol)
        """ # nopep8
        return self._cards[1].get_value("eg1")

    @eg1.setter
    def eg1(self, value: float) -> None:
        """Set the eg1 property."""
        self._cards[1].set_value("eg1", value)

    @property
    def eg2(self) -> float:
        """Get or set the Activation energy of Ethylene hydration (units: J/mol)
        """ # nopep8
        return self._cards[1].get_value("eg2")

    @eg2.setter
    def eg2(self, value: float) -> None:
        """Set the eg2 property."""
        self._cards[1].set_value("eg2", value)

    @property
    def hic2h4(self) -> typing.Optional[float]:
        """Get or set the Formation enthalpies of different species participating in gas generation model. See Remark 3. (units: kJ.?mol?^(-1))
        """ # nopep8
        return self._cards[2].get_value("hic2h4")

    @hic2h4.setter
    def hic2h4(self, value: float) -> None:
        """Set the hic2h4 property."""
        self._cards[2].set_value("hic2h4", value)

    @property
    def hco2(self) -> typing.Optional[float]:
        """Get or set the Formation enthalpies of different species participating in gas generation model. See Remark 3. (units: kJ.?mol?^(-1))
        """ # nopep8
        return self._cards[2].get_value("hco2")

    @hco2.setter
    def hco2(self, value: float) -> None:
        """Set the hco2 property."""
        self._cards[2].set_value("hco2", value)

    @property
    def hh2o(self) -> typing.Optional[float]:
        """Get or set the Formation enthalpies of different species participating in gas generation model. See Remark 3. (units: kJ.?mol?^(-1))
        """ # nopep8
        return self._cards[2].get_value("hh2o")

    @hh2o.setter
    def hh2o(self, value: float) -> None:
        """Set the hh2o property."""
        self._cards[2].set_value("hh2o", value)

    @property
    def hilc6(self) -> typing.Optional[float]:
        """Get or set the Formation enthalpies of different species participating in gas generation model. See Remark 3. (units: kJ.?mol?^(-1))
        """ # nopep8
        return self._cards[2].get_value("hilc6")

    @hilc6.setter
    def hilc6(self, value: float) -> None:
        """Set the hilc6 property."""
        self._cards[2].set_value("hilc6", value)

    @property
    def hlioh(self) -> typing.Optional[float]:
        """Get or set the Formation enthalpies of different species participating in gas generation model. See Remark 3. (units: kJ.?mol?^(-1))
        """ # nopep8
        return self._cards[2].get_value("hlioh")

    @hlioh.setter
    def hlioh(self, value: float) -> None:
        """Set the hlioh property."""
        self._cards[2].set_value("hlioh", value)

