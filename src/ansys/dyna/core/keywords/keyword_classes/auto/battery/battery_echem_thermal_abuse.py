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

"""Module providing the BatteryEchemThermalAbuse class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BATTERYECHEMTHERMALABUSE_CARD0 = (
    FieldSchema("model", int, 0, 10, 0),
    FieldSchema("feedbk", int, 10, 10, 0),
    FieldSchema("tsei0", float, 20, 10, None),
    FieldSchema("tseir", int, 30, 10, 0),
    FieldSchema("sf5", float, 40, 10, None),
    FieldSchema("igasgen", int, 50, 10, None),
)

_BATTERYECHEMTHERMALABUSE_CARD1 = (
    FieldSchema("omc2h4", float, 0, 10, None),
    FieldSchema("omo2", float, 10, 10, None),
    FieldSchema("omh2o", float, 20, 10, None),
    FieldSchema("omco2", float, 30, 10, None),
    FieldSchema("omh2", float, 40, 10, None),
)

_BATTERYECHEMTHERMALABUSE_CARD2 = (
    FieldSchema("a", float, 0, 10, None),
    FieldSchema("ear", float, 10, 10, None),
    FieldSchema("n", float, 20, 10, 1.0),
    FieldSchema("hw", float, 30, 10, None),
    FieldSchema("ton", float, 40, 10, None),
    FieldSchema("m", float, 50, 10, None),
    FieldSchema("p", float, 60, 10, None),
    FieldSchema("alfa0", float, 70, 10, 1.0),
)

_BATTERYECHEMTHERMALABUSE_CARD3 = (
    FieldSchema("a", float, 0, 10, None),
    FieldSchema("ear", float, 10, 10, None),
    FieldSchema("n", float, 20, 10, 1.0),
    FieldSchema("hw", float, 30, 10, None),
    FieldSchema("ton", float, 40, 10, None),
    FieldSchema("m", float, 50, 10, None),
    FieldSchema("p", float, 60, 10, None),
    FieldSchema("alfa0", float, 70, 10, 1.0),
)

_BATTERYECHEMTHERMALABUSE_CARD4 = (
    FieldSchema("a", float, 0, 10, None),
    FieldSchema("ear", float, 10, 10, None),
    FieldSchema("n", float, 20, 10, 1.0),
    FieldSchema("hw", float, 30, 10, None),
    FieldSchema("ton", float, 40, 10, None),
    FieldSchema("m", float, 50, 10, None),
    FieldSchema("p", float, 60, 10, None),
    FieldSchema("alfa0", float, 70, 10, 1.0),
)

_BATTERYECHEMTHERMALABUSE_CARD5 = (
    FieldSchema("a", float, 0, 10, None),
    FieldSchema("ear", float, 10, 10, None),
    FieldSchema("n", float, 20, 10, 1.0),
    FieldSchema("hw", float, 30, 10, None),
    FieldSchema("ton", float, 40, 10, None),
    FieldSchema("m", float, 50, 10, None),
    FieldSchema("p", float, 60, 10, None),
    FieldSchema("alfa0", float, 70, 10, 1.0),
)

_BATTERYECHEMTHERMALABUSE_CARD6 = (
    FieldSchema("pkap", float, 0, 10, None),
    FieldSchema("pdsa", float, 10, 10, None),
    FieldSchema("pdsc", float, 20, 10, None),
    FieldSchema("pioa", float, 30, 10, None),
    FieldSchema("pioc", float, 40, 10, None),
    FieldSchema("prfa", float, 50, 10, None),
    FieldSchema("prfc", float, 60, 10, None),
    FieldSchema("psigc", float, 70, 10, None),
)

_BATTERYECHEMTHERMALABUSE_CARD7 = (
    FieldSchema("peps1", float, 0, 10, None),
    FieldSchema("peps2", float, 10, 10, None),
)

class BatteryEchemThermalAbuse(KeywordBase):
    """DYNA BATTERY_ECHEM_THERMAL_ABUSE keyword"""

    keyword = "BATTERY"
    subkeyword = "ECHEM_THERMAL_ABUSE"

    def __init__(self, **kwargs):
        """Initialize the BatteryEchemThermalAbuse class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMTHERMALABUSE_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMTHERMALABUSE_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMTHERMALABUSE_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMTHERMALABUSE_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMTHERMALABUSE_CARD4,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMTHERMALABUSE_CARD5,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMTHERMALABUSE_CARD6,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMTHERMALABUSE_CARD7,
                **kwargs,
            ),
        ]
    @property
    def model(self) -> int:
        """Get or set the Flag to select thermal runaway model (see Remark 1):
        EQ.0 : Off
        EQ.1 : 1 - Eq model
        EQ.4 : 4 - Eq model
        """ # nopep8
        return self._cards[0].get_value("model")

    @model.setter
    def model(self, value: int) -> None:
        """Set the model property."""
        if value not in [0, 1, 4, None]:
            raise Exception("""model must be `None` or one of {0,1,4}.""")
        self._cards[0].set_value("model", value)

    @property
    def feedbk(self) -> int:
        """Get or set the Feedback model:
        EQ.0 : Off
        EQ.1 : On.See Remark 5.
        """ # nopep8
        return self._cards[0].get_value("feedbk")

    @feedbk.setter
    def feedbk(self, value: int) -> None:
        """Set the feedbk property."""
        if value not in [0, 1, None]:
            raise Exception("""feedbk must be `None` or one of {0,1}.""")
        self._cards[0].set_value("feedbk", value)

    @property
    def tsei0(self) -> typing.Optional[float]:
        """Get or set the Initial SEI reference thickness
        """ # nopep8
        return self._cards[0].get_value("tsei0")

    @tsei0.setter
    def tsei0(self, value: float) -> None:
        """Set the tsei0 property."""
        self._cards[0].set_value("tsei0", value)

    @property
    def tseir(self) -> int:
        """Get or set the SEI thickness coupling (see Remark 2):
        EQ.0 : Tunneling regime.
        EQ.1 : Diffusion regime.
        EQ.2 : Auto regime.
        """ # nopep8
        return self._cards[0].get_value("tseir")

    @tseir.setter
    def tseir(self, value: int) -> None:
        """Set the tseir property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""tseir must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("tseir", value)

    @property
    def sf5(self) -> typing.Optional[float]:
        """Get or set the Optional scaling coefficient on Reaction 5
        """ # nopep8
        return self._cards[0].get_value("sf5")

    @sf5.setter
    def sf5(self, value: float) -> None:
        """Set the sf5 property."""
        self._cards[0].set_value("sf5", value)

    @property
    def igasgen(self) -> typing.Optional[int]:
        """Get or set the Thermal abuse gas generated mole tracking model
        EQ.0:Off.
        EQ.1:On
        """ # nopep8
        return self._cards[0].get_value("igasgen")

    @igasgen.setter
    def igasgen(self, value: int) -> None:
        """Set the igasgen property."""
        self._cards[0].set_value("igasgen", value)

    @property
    def omc2h4(self) -> typing.Optional[float]:
        """Get or set the Gas yield values for the different species (mol.?kg?^(-1)). Needed for tracking generated moles when IGASGEN=1
        """ # nopep8
        return self._cards[1].get_value("omc2h4")

    @omc2h4.setter
    def omc2h4(self, value: float) -> None:
        """Set the omc2h4 property."""
        self._cards[1].set_value("omc2h4", value)

    @property
    def omo2(self) -> typing.Optional[float]:
        """Get or set the Gas yield values for the different species (mol.?kg?^(-1)). Needed for tracking generated moles when IGASGEN=1
        """ # nopep8
        return self._cards[1].get_value("omo2")

    @omo2.setter
    def omo2(self, value: float) -> None:
        """Set the omo2 property."""
        self._cards[1].set_value("omo2", value)

    @property
    def omh2o(self) -> typing.Optional[float]:
        """Get or set the Gas yield values for the different species (mol.?kg?^(-1)). Needed for tracking generated moles when IGASGEN=1
        """ # nopep8
        return self._cards[1].get_value("omh2o")

    @omh2o.setter
    def omh2o(self, value: float) -> None:
        """Set the omh2o property."""
        self._cards[1].set_value("omh2o", value)

    @property
    def omco2(self) -> typing.Optional[float]:
        """Get or set the Gas yield values for the different species (mol.?kg?^(-1)). Needed for tracking generated moles when IGASGEN=1
        """ # nopep8
        return self._cards[1].get_value("omco2")

    @omco2.setter
    def omco2(self, value: float) -> None:
        """Set the omco2 property."""
        self._cards[1].set_value("omco2", value)

    @property
    def omh2(self) -> typing.Optional[float]:
        """Get or set the Gas yield values for the different species (mol.?kg?^(-1)). Needed for tracking generated moles when IGASGEN=1
        """ # nopep8
        return self._cards[1].get_value("omh2")

    @omh2.setter
    def omh2(self, value: float) -> None:
        """Set the omh2 property."""
        self._cards[1].set_value("omh2", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Frequency factor
        """ # nopep8
        return self._cards[2].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[2].set_value("a", value)

    @property
    def ear(self) -> typing.Optional[float]:
        """Get or set the Activation energy
        """ # nopep8
        return self._cards[2].get_value("ear")

    @ear.setter
    def ear(self, value: float) -> None:
        """Set the ear property."""
        self._cards[2].set_value("ear", value)

    @property
    def n(self) -> float:
        """Get or set the Reaction order.
        """ # nopep8
        return self._cards[2].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[2].set_value("n", value)

    @property
    def hw(self) -> typing.Optional[float]:
        """Get or set the Enthalpy times mass loading
        """ # nopep8
        return self._cards[2].get_value("hw")

    @hw.setter
    def hw(self, value: float) -> None:
        """Set the hw property."""
        self._cards[2].set_value("hw", value)

    @property
    def ton(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[2].get_value("ton")

    @ton.setter
    def ton(self, value: float) -> None:
        """Set the ton property."""
        self._cards[2].set_value("ton", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Second reaction order.
        """ # nopep8
        return self._cards[2].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        """Set the m property."""
        self._cards[2].set_value("m", value)

    @property
    def p(self) -> typing.Optional[float]:
        """Get or set the Third reaction order.
        """ # nopep8
        return self._cards[2].get_value("p")

    @p.setter
    def p(self, value: float) -> None:
        """Set the p property."""
        self._cards[2].set_value("p", value)

    @property
    def alfa0(self) -> float:
        """Get or set the Initial ?.
        """ # nopep8
        return self._cards[2].get_value("alfa0")

    @alfa0.setter
    def alfa0(self, value: float) -> None:
        """Set the alfa0 property."""
        self._cards[2].set_value("alfa0", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Frequency factor
        """ # nopep8
        return self._cards[3].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[3].set_value("a", value)

    @property
    def ear(self) -> typing.Optional[float]:
        """Get or set the Activation energy
        """ # nopep8
        return self._cards[3].get_value("ear")

    @ear.setter
    def ear(self, value: float) -> None:
        """Set the ear property."""
        self._cards[3].set_value("ear", value)

    @property
    def n(self) -> float:
        """Get or set the Reaction order.
        """ # nopep8
        return self._cards[3].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[3].set_value("n", value)

    @property
    def hw(self) -> typing.Optional[float]:
        """Get or set the Enthalpy times mass loading
        """ # nopep8
        return self._cards[3].get_value("hw")

    @hw.setter
    def hw(self, value: float) -> None:
        """Set the hw property."""
        self._cards[3].set_value("hw", value)

    @property
    def ton(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[3].get_value("ton")

    @ton.setter
    def ton(self, value: float) -> None:
        """Set the ton property."""
        self._cards[3].set_value("ton", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Second reaction order.
        """ # nopep8
        return self._cards[3].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        """Set the m property."""
        self._cards[3].set_value("m", value)

    @property
    def p(self) -> typing.Optional[float]:
        """Get or set the Third reaction order.
        """ # nopep8
        return self._cards[3].get_value("p")

    @p.setter
    def p(self, value: float) -> None:
        """Set the p property."""
        self._cards[3].set_value("p", value)

    @property
    def alfa0(self) -> float:
        """Get or set the Initial ?.
        """ # nopep8
        return self._cards[3].get_value("alfa0")

    @alfa0.setter
    def alfa0(self, value: float) -> None:
        """Set the alfa0 property."""
        self._cards[3].set_value("alfa0", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Frequency factor
        """ # nopep8
        return self._cards[4].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[4].set_value("a", value)

    @property
    def ear(self) -> typing.Optional[float]:
        """Get or set the Activation energy
        """ # nopep8
        return self._cards[4].get_value("ear")

    @ear.setter
    def ear(self, value: float) -> None:
        """Set the ear property."""
        self._cards[4].set_value("ear", value)

    @property
    def n(self) -> float:
        """Get or set the Reaction order.
        """ # nopep8
        return self._cards[4].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[4].set_value("n", value)

    @property
    def hw(self) -> typing.Optional[float]:
        """Get or set the Enthalpy times mass loading
        """ # nopep8
        return self._cards[4].get_value("hw")

    @hw.setter
    def hw(self, value: float) -> None:
        """Set the hw property."""
        self._cards[4].set_value("hw", value)

    @property
    def ton(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[4].get_value("ton")

    @ton.setter
    def ton(self, value: float) -> None:
        """Set the ton property."""
        self._cards[4].set_value("ton", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Second reaction order.
        """ # nopep8
        return self._cards[4].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        """Set the m property."""
        self._cards[4].set_value("m", value)

    @property
    def p(self) -> typing.Optional[float]:
        """Get or set the Third reaction order.
        """ # nopep8
        return self._cards[4].get_value("p")

    @p.setter
    def p(self, value: float) -> None:
        """Set the p property."""
        self._cards[4].set_value("p", value)

    @property
    def alfa0(self) -> float:
        """Get or set the Initial ?.
        """ # nopep8
        return self._cards[4].get_value("alfa0")

    @alfa0.setter
    def alfa0(self, value: float) -> None:
        """Set the alfa0 property."""
        self._cards[4].set_value("alfa0", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Frequency factor
        """ # nopep8
        return self._cards[5].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[5].set_value("a", value)

    @property
    def ear(self) -> typing.Optional[float]:
        """Get or set the Activation energy
        """ # nopep8
        return self._cards[5].get_value("ear")

    @ear.setter
    def ear(self, value: float) -> None:
        """Set the ear property."""
        self._cards[5].set_value("ear", value)

    @property
    def n(self) -> float:
        """Get or set the Reaction order.
        """ # nopep8
        return self._cards[5].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[5].set_value("n", value)

    @property
    def hw(self) -> typing.Optional[float]:
        """Get or set the Enthalpy times mass loading
        """ # nopep8
        return self._cards[5].get_value("hw")

    @hw.setter
    def hw(self, value: float) -> None:
        """Set the hw property."""
        self._cards[5].set_value("hw", value)

    @property
    def ton(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[5].get_value("ton")

    @ton.setter
    def ton(self, value: float) -> None:
        """Set the ton property."""
        self._cards[5].set_value("ton", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Second reaction order.
        """ # nopep8
        return self._cards[5].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        """Set the m property."""
        self._cards[5].set_value("m", value)

    @property
    def p(self) -> typing.Optional[float]:
        """Get or set the Third reaction order.
        """ # nopep8
        return self._cards[5].get_value("p")

    @p.setter
    def p(self, value: float) -> None:
        """Set the p property."""
        self._cards[5].set_value("p", value)

    @property
    def alfa0(self) -> float:
        """Get or set the Initial ?.
        """ # nopep8
        return self._cards[5].get_value("alfa0")

    @alfa0.setter
    def alfa0(self, value: float) -> None:
        """Set the alfa0 property."""
        self._cards[5].set_value("alfa0", value)

    @property
    def pkap(self) -> typing.Optional[float]:
        """Get or set the Degradation parameters to be defined if FEEDBK=1. See Remark 5
        """ # nopep8
        return self._cards[6].get_value("pkap")

    @pkap.setter
    def pkap(self, value: float) -> None:
        """Set the pkap property."""
        self._cards[6].set_value("pkap", value)

    @property
    def pdsa(self) -> typing.Optional[float]:
        """Get or set the Degradation parameters to be defined if FEEDBK=1. See Remark 5
        """ # nopep8
        return self._cards[6].get_value("pdsa")

    @pdsa.setter
    def pdsa(self, value: float) -> None:
        """Set the pdsa property."""
        self._cards[6].set_value("pdsa", value)

    @property
    def pdsc(self) -> typing.Optional[float]:
        """Get or set the Degradation parameters to be defined if FEEDBK=1. See Remark 5
        """ # nopep8
        return self._cards[6].get_value("pdsc")

    @pdsc.setter
    def pdsc(self, value: float) -> None:
        """Set the pdsc property."""
        self._cards[6].set_value("pdsc", value)

    @property
    def pioa(self) -> typing.Optional[float]:
        """Get or set the Degradation parameters to be defined if FEEDBK=1. See Remark 5
        """ # nopep8
        return self._cards[6].get_value("pioa")

    @pioa.setter
    def pioa(self, value: float) -> None:
        """Set the pioa property."""
        self._cards[6].set_value("pioa", value)

    @property
    def pioc(self) -> typing.Optional[float]:
        """Get or set the Degradation parameters to be defined if FEEDBK=1. See Remark 5
        """ # nopep8
        return self._cards[6].get_value("pioc")

    @pioc.setter
    def pioc(self, value: float) -> None:
        """Set the pioc property."""
        self._cards[6].set_value("pioc", value)

    @property
    def prfa(self) -> typing.Optional[float]:
        """Get or set the Degradation parameters to be defined if FEEDBK=1. See Remark 5
        """ # nopep8
        return self._cards[6].get_value("prfa")

    @prfa.setter
    def prfa(self, value: float) -> None:
        """Set the prfa property."""
        self._cards[6].set_value("prfa", value)

    @property
    def prfc(self) -> typing.Optional[float]:
        """Get or set the Degradation parameters to be defined if FEEDBK=1. See Remark 5
        """ # nopep8
        return self._cards[6].get_value("prfc")

    @prfc.setter
    def prfc(self, value: float) -> None:
        """Set the prfc property."""
        self._cards[6].set_value("prfc", value)

    @property
    def psigc(self) -> typing.Optional[float]:
        """Get or set the Degradation parameters to be defined if FEEDBK=1. See Remark 5
        """ # nopep8
        return self._cards[6].get_value("psigc")

    @psigc.setter
    def psigc(self, value: float) -> None:
        """Set the psigc property."""
        self._cards[6].set_value("psigc", value)

    @property
    def peps1(self) -> typing.Optional[float]:
        """Get or set the Degradation parameters to be defined if FEEDBK=1. See Remark 5
        """ # nopep8
        return self._cards[7].get_value("peps1")

    @peps1.setter
    def peps1(self, value: float) -> None:
        """Set the peps1 property."""
        self._cards[7].set_value("peps1", value)

    @property
    def peps2(self) -> typing.Optional[float]:
        """Get or set the Degradation parameters to be defined if FEEDBK=1. See Remark 5
        """ # nopep8
        return self._cards[7].get_value("peps2")

    @peps2.setter
    def peps2(self, value: float) -> None:
        """Set the peps2 property."""
        self._cards[7].set_value("peps2", value)

