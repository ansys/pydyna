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

"""Module providing the BatteryEchemVent class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BATTERYECHEMVENT_CARD0 = (
    FieldSchema("ivent", int, 0, 10, 0),
    FieldSchema("pburst", float, 10, 10, None),
    FieldSchema("kvent", float, 20, 10, None),
    FieldSchema("pamb", float, 30, 10, 1.013e-05),
    FieldSchema("vpor", float, 40, 10, None),
)

_BATTERYECHEMVENT_CARD1 = (
    FieldSchema("avent", float, 0, 10, None),
    FieldSchema("gamma", float, 10, 10, 1.4),
    FieldSchema("mwg", float, 20, 10, 0.029),
    FieldSchema("mej", float, 30, 10, None),
    FieldSchema("cpej", float, 40, 10, 830.0),
    FieldSchema("sfej", float, 50, 10, 1.0),
)

_BATTERYECHEMVENT_CARD2 = (
    FieldSchema("avent", float, 0, 10, None),
    FieldSchema("gamma", float, 10, 10, 1.4),
    FieldSchema("mwg", float, 20, 10, 0.029),
    FieldSchema("mej", float, 30, 10, None),
    FieldSchema("cpej", float, 40, 10, 830.0),
    FieldSchema("sfej", float, 50, 10, 1.0),
)

_BATTERYECHEMVENT_CARD3 = (
    FieldSchema("cpc2h4", float, 0, 10, None),
    FieldSchema("coo2", float, 10, 10, None),
    FieldSchema("cphh2o", float, 20, 10, None),
    FieldSchema("cpco2", float, 30, 10, None),
    FieldSchema("cph2", float, 40, 10, None),
)

class BatteryEchemVent(KeywordBase):
    """DYNA BATTERY_ECHEM_VENT keyword"""

    keyword = "BATTERY"
    subkeyword = "ECHEM_VENT"

    def __init__(self, **kwargs):
        """Initialize the BatteryEchemVent class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMVENT_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMVENT_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMVENT_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMVENT_CARD3,
                **kwargs,
            ),
        ]
    @property
    def ivent(self) -> int:
        """Get or set the Flag for turn on vent model:
        EQ.0 : Off
        EQ.1 : On
        """ # nopep8
        return self._cards[0].get_value("ivent")

    @ivent.setter
    def ivent(self, value: int) -> None:
        """Set the ivent property."""
        if value not in [0, 1, None]:
            raise Exception("""ivent must be `None` or one of {0,1}.""")
        self._cards[0].set_value("ivent", value)

    @property
    def pburst(self) -> typing.Optional[float]:
        """Get or set the Burst pressure P_burst
        """ # nopep8
        return self._cards[0].get_value("pburst")

    @pburst.setter
    def pburst(self, value: float) -> None:
        """Set the pburst property."""
        self._cards[0].set_value("pburst", value)

    @property
    def kvent(self) -> typing.Optional[float]:
        """Get or set the Venting coefficient K_vent
        """ # nopep8
        return self._cards[0].get_value("kvent")

    @kvent.setter
    def kvent(self, value: float) -> None:
        """Set the kvent property."""
        self._cards[0].set_value("kvent", value)

    @property
    def pamb(self) -> float:
        """Get or set the Ambient pressure
        """ # nopep8
        return self._cards[0].get_value("pamb")

    @pamb.setter
    def pamb(self, value: float) -> None:
        """Set the pamb property."""
        self._cards[0].set_value("pamb", value)

    @property
    def vpor(self) -> typing.Optional[float]:
        """Get or set the effective pore volume per unit electrode area V_pore
        """ # nopep8
        return self._cards[0].get_value("vpor")

    @vpor.setter
    def vpor(self, value: float) -> None:
        """Set the vpor property."""
        self._cards[0].set_value("vpor", value)

    @property
    def avent(self) -> typing.Optional[float]:
        """Get or set the Vent electrode area ratio A_vent
        """ # nopep8
        return self._cards[1].get_value("avent")

    @avent.setter
    def avent(self, value: float) -> None:
        """Set the avent property."""
        self._cards[1].set_value("avent", value)

    @property
    def gamma(self) -> float:
        """Get or set the Heat capacity ratio ?.
        """ # nopep8
        return self._cards[1].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        """Set the gamma property."""
        self._cards[1].set_value("gamma", value)

    @property
    def mwg(self) -> float:
        """Get or set the Mean Molar mass of venting gas mixture M_wg
        """ # nopep8
        return self._cards[1].get_value("mwg")

    @mwg.setter
    def mwg(self, value: float) -> None:
        """Set the mwg property."""
        self._cards[1].set_value("mwg", value)

    @property
    def mej(self) -> typing.Optional[float]:
        """Get or set the Mass per unit area ejected during venting M_ej
        """ # nopep8
        return self._cards[1].get_value("mej")

    @mej.setter
    def mej(self, value: float) -> None:
        """Set the mej property."""
        self._cards[1].set_value("mej", value)

    @property
    def cpej(self) -> float:
        """Get or set the Heat capacity of ejected solid fragments ?Cp?_ej
        """ # nopep8
        return self._cards[1].get_value("cpej")

    @cpej.setter
    def cpej(self, value: float) -> None:
        """Set the cpej property."""
        self._cards[1].set_value("cpej", value)

    @property
    def sfej(self) -> float:
        """Get or set the Scale factor on ejected mass
        """ # nopep8
        return self._cards[1].get_value("sfej")

    @sfej.setter
    def sfej(self, value: float) -> None:
        """Set the sfej property."""
        self._cards[1].set_value("sfej", value)

    @property
    def avent(self) -> typing.Optional[float]:
        """Get or set the Vent electrode area ratio A_vent
        """ # nopep8
        return self._cards[2].get_value("avent")

    @avent.setter
    def avent(self, value: float) -> None:
        """Set the avent property."""
        self._cards[2].set_value("avent", value)

    @property
    def gamma(self) -> float:
        """Get or set the Heat capacity ratio ?.
        """ # nopep8
        return self._cards[2].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        """Set the gamma property."""
        self._cards[2].set_value("gamma", value)

    @property
    def mwg(self) -> float:
        """Get or set the Mean Molar mass of venting gas mixture M_wg
        """ # nopep8
        return self._cards[2].get_value("mwg")

    @mwg.setter
    def mwg(self, value: float) -> None:
        """Set the mwg property."""
        self._cards[2].set_value("mwg", value)

    @property
    def mej(self) -> typing.Optional[float]:
        """Get or set the Mass per unit area ejected during venting M_ej
        """ # nopep8
        return self._cards[2].get_value("mej")

    @mej.setter
    def mej(self, value: float) -> None:
        """Set the mej property."""
        self._cards[2].set_value("mej", value)

    @property
    def cpej(self) -> float:
        """Get or set the Heat capacity of ejected solid fragments ?Cp?_ej
        """ # nopep8
        return self._cards[2].get_value("cpej")

    @cpej.setter
    def cpej(self, value: float) -> None:
        """Set the cpej property."""
        self._cards[2].set_value("cpej", value)

    @property
    def sfej(self) -> float:
        """Get or set the Scale factor on ejected mass
        """ # nopep8
        return self._cards[2].get_value("sfej")

    @sfej.setter
    def sfej(self, value: float) -> None:
        """Set the sfej property."""
        self._cards[2].set_value("sfej", value)

    @property
    def cpc2h4(self) -> typing.Optional[float]:
        """Get or set the Molar heat capacities of different species participating in venting model. See Remark 1. (units: J.?mol?^(-1) K^(-1))
        """ # nopep8
        return self._cards[3].get_value("cpc2h4")

    @cpc2h4.setter
    def cpc2h4(self, value: float) -> None:
        """Set the cpc2h4 property."""
        self._cards[3].set_value("cpc2h4", value)

    @property
    def coo2(self) -> typing.Optional[float]:
        """Get or set the Molar heat capacities of different species participating in venting model. See Remark 1. (units: J.?mol?^(-1) K^(-1))
        """ # nopep8
        return self._cards[3].get_value("coo2")

    @coo2.setter
    def coo2(self, value: float) -> None:
        """Set the coo2 property."""
        self._cards[3].set_value("coo2", value)

    @property
    def cphh2o(self) -> typing.Optional[float]:
        """Get or set the Molar heat capacities of different species participating in venting model. See Remark 1. (units: J.?mol?^(-1) K^(-1))
        """ # nopep8
        return self._cards[3].get_value("cphh2o")

    @cphh2o.setter
    def cphh2o(self, value: float) -> None:
        """Set the cphh2o property."""
        self._cards[3].set_value("cphh2o", value)

    @property
    def cpco2(self) -> typing.Optional[float]:
        """Get or set the Molar heat capacities of different species participating in venting model. See Remark 1. (units: J.?mol?^(-1) K^(-1))
        """ # nopep8
        return self._cards[3].get_value("cpco2")

    @cpco2.setter
    def cpco2(self, value: float) -> None:
        """Set the cpco2 property."""
        self._cards[3].set_value("cpco2", value)

    @property
    def cph2(self) -> typing.Optional[float]:
        """Get or set the Molar heat capacities of different species participating in venting model. See Remark 1. (units: J.?mol?^(-1) K^(-1))
        """ # nopep8
        return self._cards[3].get_value("cph2")

    @cph2.setter
    def cph2(self, value: float) -> None:
        """Set the cph2 property."""
        self._cards[3].set_value("cph2", value)

