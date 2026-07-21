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

"""Module providing the BatteryEchemAging class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BATTERYECHEMAGING_CARD0 = (
    FieldSchema("seimw", float, 0, 10, None),
    FieldSchema("seirho", float, 10, 10, None),
    FieldSchema("seibrug", float, 20, 10, None),
    FieldSchema("seieps", float, 30, 10, None),
    FieldSchema("seic0", float, 40, 10, None),
    FieldSchema("seit0", float, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

_BATTERYECHEMAGING_CARD1 = (
    FieldSchema("seii0", float, 0, 10, None),
    FieldSchema("seirka", float, 10, 10, None),
    FieldSchema("seicon", float, 20, 10, None),
    FieldSchema("ecc0", float, 30, 10, None),
    FieldSchema("dec", float, 40, 10, None),
    FieldSchema("istype", float, 50, 10, None),
)

_BATTERYECHEMAGING_CARD2 = (
    FieldSchema("afi", float, 0, 10, None),
    FieldSchema("eat", float, 10, 10, None),
)

_BATTERYECHEMAGING_CARD3 = (
    FieldSchema("hoflec", float, 0, 10, None),
    FieldSchema("unused", float, 10, 10, None),
    FieldSchema("hofled", float, 20, 10, None),
    FieldSchema("hofc2h4", float, 30, 10, None),
    FieldSchema("hoflc", float, 40, 10, None),
    FieldSchema("hofco2", float, 50, 10, None),
    FieldSchema("unused", float, 60, 10, None),
)

class BatteryEchemAging(KeywordBase):
    """DYNA BATTERY_ECHEM_AGING keyword"""

    keyword = "BATTERY"
    subkeyword = "ECHEM_AGING"

    def __init__(self, **kwargs):
        """Initialize the BatteryEchemAging class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMAGING_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMAGING_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMAGING_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMAGING_CARD3,
                **kwargs,
            ),
        ]
    @property
    def seimw(self) -> typing.Optional[float]:
        """Get or set the Molecular weight of the solid electrolyte interphase(SEI)
        """ # nopep8
        return self._cards[0].get_value("seimw")

    @seimw.setter
    def seimw(self, value: float) -> None:
        """Set the seimw property."""
        self._cards[0].set_value("seimw", value)

    @property
    def seirho(self) -> typing.Optional[float]:
        """Get or set the Density of the SEI
        """ # nopep8
        return self._cards[0].get_value("seirho")

    @seirho.setter
    def seirho(self, value: float) -> None:
        """Set the seirho property."""
        self._cards[0].set_value("seirho", value)

    @property
    def seibrug(self) -> typing.Optional[float]:
        """Get or set the The Bruggeman constant of the SEI
        """ # nopep8
        return self._cards[0].get_value("seibrug")

    @seibrug.setter
    def seibrug(self, value: float) -> None:
        """Set the seibrug property."""
        self._cards[0].set_value("seibrug", value)

    @property
    def seieps(self) -> typing.Optional[float]:
        """Get or set the Initial SEI porosity
        """ # nopep8
        return self._cards[0].get_value("seieps")

    @seieps.setter
    def seieps(self, value: float) -> None:
        """Set the seieps property."""
        self._cards[0].set_value("seieps", value)

    @property
    def seic0(self) -> typing.Optional[float]:
        """Get or set the Initial SEI concentration (units: mol/m3)
        """ # nopep8
        return self._cards[0].get_value("seic0")

    @seic0.setter
    def seic0(self, value: float) -> None:
        """Set the seic0 property."""
        self._cards[0].set_value("seic0", value)

    @property
    def seit0(self) -> typing.Optional[float]:
        """Get or set the Initial thickness of the SEI layer (m)
        """ # nopep8
        return self._cards[0].get_value("seit0")

    @seit0.setter
    def seit0(self, value: float) -> None:
        """Set the seit0 property."""
        self._cards[0].set_value("seit0", value)

    @property
    def seii0(self) -> typing.Optional[float]:
        """Get or set the Exchange current density for the SEI reaction
        """ # nopep8
        return self._cards[1].get_value("seii0")

    @seii0.setter
    def seii0(self, value: float) -> None:
        """Set the seii0 property."""
        self._cards[1].set_value("seii0", value)

    @property
    def seirka(self) -> typing.Optional[float]:
        """Get or set the Reaction rate constant for the SEI reaction (ignored if SEII0 ? 0.0)
        """ # nopep8
        return self._cards[1].get_value("seirka")

    @seirka.setter
    def seirka(self, value: float) -> None:
        """Set the seirka property."""
        self._cards[1].set_value("seirka", value)

    @property
    def seicon(self) -> typing.Optional[float]:
        """Get or set the Ionic conductivity (units: S/m)
        """ # nopep8
        return self._cards[1].get_value("seicon")

    @seicon.setter
    def seicon(self, value: float) -> None:
        """Set the seicon property."""
        self._cards[1].set_value("seicon", value)

    @property
    def ecc0(self) -> typing.Optional[float]:
        """Get or set the Initial concentration of EC (ethylene carbonate). This field is ignored if SEII0 ? 0.0.
        """ # nopep8
        return self._cards[1].get_value("ecc0")

    @ecc0.setter
    def ecc0(self, value: float) -> None:
        """Set the ecc0 property."""
        self._cards[1].set_value("ecc0", value)

    @property
    def dec(self) -> typing.Optional[float]:
        """Get or set the Diffusion coefficient of EC
        """ # nopep8
        return self._cards[1].get_value("dec")

    @dec.setter
    def dec(self, value: float) -> None:
        """Set the dec property."""
        self._cards[1].set_value("dec", value)

    @property
    def istype(self) -> typing.Optional[float]:
        """Get or set the Formulation variation for SEI side reactions kinetics:
        EQ.0:	SEII0 is used for SEI side reaction.
        EQ.1:	SEIRKA and ECCO are used for SEI side reaction. This allows a feedback mechanism to occur if DEC is also defined.
        """ # nopep8
        return self._cards[1].get_value("istype")

    @istype.setter
    def istype(self, value: float) -> None:
        """Set the istype property."""
        self._cards[1].set_value("istype", value)

    @property
    def afi(self) -> typing.Optional[float]:
        """Get or set the Frequency factor for the reaction
        """ # nopep8
        return self._cards[2].get_value("afi")

    @afi.setter
    def afi(self, value: float) -> None:
        """Set the afi property."""
        self._cards[2].set_value("afi", value)

    @property
    def eat(self) -> typing.Optional[float]:
        """Get or set the Activation energy for the reaction
        """ # nopep8
        return self._cards[2].get_value("eat")

    @eat.setter
    def eat(self, value: float) -> None:
        """Set the eat property."""
        self._cards[2].set_value("eat", value)

    @property
    def hoflec(self) -> typing.Optional[float]:
        """Get or set the Formation enthalpy of the EC (units: kJ/mol)
        """ # nopep8
        return self._cards[3].get_value("hoflec")

    @hoflec.setter
    def hoflec(self, value: float) -> None:
        """Set the hoflec property."""
        self._cards[3].set_value("hoflec", value)

    @property
    def hofled(self) -> typing.Optional[float]:
        """Get or set the Formation enthalpy of the SEI layer (units: kJ/mol)
        """ # nopep8
        return self._cards[3].get_value("hofled")

    @hofled.setter
    def hofled(self, value: float) -> None:
        """Set the hofled property."""
        self._cards[3].set_value("hofled", value)

    @property
    def hofc2h4(self) -> typing.Optional[float]:
        """Get or set the Formation enthalpy of ethylene (units: kJ/mol)
        """ # nopep8
        return self._cards[3].get_value("hofc2h4")

    @hofc2h4.setter
    def hofc2h4(self, value: float) -> None:
        """Set the hofc2h4 property."""
        self._cards[3].set_value("hofc2h4", value)

    @property
    def hoflc(self) -> typing.Optional[float]:
        """Get or set the Formation enthalpy of LC (Li2CO3; units: kJ/mol)
        """ # nopep8
        return self._cards[3].get_value("hoflc")

    @hoflc.setter
    def hoflc(self, value: float) -> None:
        """Set the hoflc property."""
        self._cards[3].set_value("hoflc", value)

    @property
    def hofco2(self) -> typing.Optional[float]:
        """Get or set the Formation enthalpy of CO2 (units: kJ/mol)
        """ # nopep8
        return self._cards[3].get_value("hofco2")

    @hofco2.setter
    def hofco2(self, value: float) -> None:
        """Set the hofco2 property."""
        self._cards[3].set_value("hofco2", value)

