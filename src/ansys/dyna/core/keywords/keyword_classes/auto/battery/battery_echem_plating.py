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

"""Module providing the BatteryEchemPlating class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BATTERYECHEMPLATING_CARD0 = (
    FieldSchema("iplat", int, 0, 10, 0),
    FieldSchema("kli", float, 10, 10, None),
    FieldSchema("gamma0", float, 20, 10, None),
    FieldSchema("alphaa", float, 30, 10, None),
    FieldSchema("alphac", float, 40, 10, None),
    FieldSchema("lsei0", float, 50, 10, None),
    FieldSchema("ksei", float, 60, 10, None),
)

class BatteryEchemPlating(KeywordBase):
    """DYNA BATTERY_ECHEM_PLATING keyword"""

    keyword = "BATTERY"
    subkeyword = "ECHEM_PLATING"

    def __init__(self, **kwargs):
        """Initialize the BatteryEchemPlating class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMPLATING_CARD0,
                **kwargs,
            ),
        ]
    @property
    def iplat(self) -> int:
        """Get or set the Flag for turn on plating model (See Remark 1):
        EQ.0: Off
        EQ.1 : On.Diagnostic only.
        EQ.2: On.Electrolyte concentration correction.
        EQ.3: On.SEI resistance in plating overpotential.
        """ # nopep8
        return self._cards[0].get_value("iplat")

    @iplat.setter
    def iplat(self, value: int) -> None:
        """Set the iplat property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""iplat must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("iplat", value)

    @property
    def kli(self) -> typing.Optional[float]:
        """Get or set the Plating stripping rate constant  (m.s^(-1))
        """ # nopep8
        return self._cards[0].get_value("kli")

    @kli.setter
    def kli(self, value: float) -> None:
        """Set the kli property."""
        self._cards[0].set_value("kli", value)

    @property
    def gamma0(self) -> typing.Optional[float]:
        """Get or set the Dead Li formation rate (s^(-1))
        """ # nopep8
        return self._cards[0].get_value("gamma0")

    @gamma0.setter
    def gamma0(self, value: float) -> None:
        """Set the gamma0 property."""
        self._cards[0].set_value("gamma0", value)

    @property
    def alphaa(self) -> typing.Optional[float]:
        """Get or set the Anodic transfer coefficient for plating model
        """ # nopep8
        return self._cards[0].get_value("alphaa")

    @alphaa.setter
    def alphaa(self, value: float) -> None:
        """Set the alphaa property."""
        self._cards[0].set_value("alphaa", value)

    @property
    def alphac(self) -> typing.Optional[float]:
        """Get or set the Cathodic transfer coefficient for plating model
        """ # nopep8
        return self._cards[0].get_value("alphac")

    @alphac.setter
    def alphac(self, value: float) -> None:
        """Set the alphac property."""
        self._cards[0].set_value("alphac", value)

    @property
    def lsei0(self) -> typing.Optional[float]:
        """Get or set the Initial SEI thickness  (m)
        """ # nopep8
        return self._cards[0].get_value("lsei0")

    @lsei0.setter
    def lsei0(self, value: float) -> None:
        """Set the lsei0 property."""
        self._cards[0].set_value("lsei0", value)

    @property
    def ksei(self) -> typing.Optional[float]:
        """Get or set the SEI ionic conductivity (S.m^(-1))
        """ # nopep8
        return self._cards[0].get_value("ksei")

    @ksei.setter
    def ksei(self, value: float) -> None:
        """Set the ksei property."""
        self._cards[0].set_value("ksei", value)

