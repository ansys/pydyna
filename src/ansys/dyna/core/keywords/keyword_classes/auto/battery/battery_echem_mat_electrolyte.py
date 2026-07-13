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

"""Module providing the BatteryEchemMatElectrolyte class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BATTERYECHEMMATELECTROLYTE_CARD0 = (
    FieldSchema("cellid", int, 0, 10, None),
    FieldSchema("iocpe", int, 10, 10, None),
    FieldSchema("ieltype", int, 20, 10, None),
    FieldSchema("rhoel", float, 30, 10, None),
    FieldSchema("rhop", float, 40, 10, None),
    FieldSchema("rhos", float, 50, 10, None),
)

_BATTERYECHEMMATELECTROLYTE_CARD1 = (
    FieldSchema("vfes", float, 0, 10, None),
    FieldSchema("vfps", float, 10, 10, None),
    FieldSchema("vfgs", float, 20, 10, None),
    FieldSchema("lics", float, 30, 10, None),
    FieldSchema("bruge", float, 40, 10, 1.5),
    FieldSchema("clmax", float, 50, 10, 0.0),
)

class BatteryEchemMatElectrolyte(KeywordBase):
    """DYNA BATTERY_ECHEM_MAT_ELECTROLYTE keyword"""

    keyword = "BATTERY"
    subkeyword = "ECHEM_MAT_ELECTROLYTE"

    def __init__(self, **kwargs):
        """Initialize the BatteryEchemMatElectrolyte class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMMATELECTROLYTE_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMMATELECTROLYTE_CARD1,
                **kwargs,
            ),
        ]
    @property
    def cellid(self) -> typing.Optional[int]:
        """Get or set the Part ID.Unused in EM-P2D coupling mode.
        """ # nopep8
        return self._cards[0].get_value("cellid")

    @cellid.setter
    def cellid(self, value: int) -> None:
        """Set the cellid property."""
        self._cards[0].set_value("cellid", value)

    @property
    def iocpe(self) -> typing.Optional[int]:
        """Get or set the Material type for the open-circuit potential:
        LT.0:	Absolute value points to define function ID. See Remark 1.
        EQ.1:LiPF6 in EC:DMC(1:1).
        EQ.2:LiPF6 in EC:DMC(2:1).
        EQ.3:LiPF6 in EC:DMC(1:2).
        EQ.4:LiPF6 in PC
        EQ.5:LiClO4 in PC
        EQ.10:	Simplified Model
        EQ.13:30% KOH in H2O (NiMH system)
        Here, EC is ethylene carbonate, DMC is dimethyl carbonate,and PC is propylene carbonate.
        """ # nopep8
        return self._cards[0].get_value("iocpe")

    @iocpe.setter
    def iocpe(self, value: int) -> None:
        """Set the iocpe property."""
        self._cards[0].set_value("iocpe", value)

    @property
    def ieltype(self) -> typing.Optional[int]:
        """Get or set the Type of electrolyte:
        EQ.0: Liquid electrolyte.
        EQ.1:Polumer electrolyte
        """ # nopep8
        return self._cards[0].get_value("ieltype")

    @ieltype.setter
    def ieltype(self, value: int) -> None:
        """Set the ieltype property."""
        self._cards[0].set_value("ieltype", value)

    @property
    def rhoel(self) -> typing.Optional[float]:
        """Get or set the Density of the electrolyte.
        """ # nopep8
        return self._cards[0].get_value("rhoel")

    @rhoel.setter
    def rhoel(self, value: float) -> None:
        """Set the rhoel property."""
        self._cards[0].set_value("rhoel", value)

    @property
    def rhop(self) -> typing.Optional[float]:
        """Get or set the Density of the polymer phase.
        """ # nopep8
        return self._cards[0].get_value("rhop")

    @rhop.setter
    def rhop(self, value: float) -> None:
        """Set the rhop property."""
        self._cards[0].set_value("rhop", value)

    @property
    def rhos(self) -> typing.Optional[float]:
        """Get or set the Density of the separator material.
        """ # nopep8
        return self._cards[0].get_value("rhos")

    @rhos.setter
    def rhos(self, value: float) -> None:
        """Set the rhos property."""
        self._cards[0].set_value("rhos", value)

    @property
    def vfes(self) -> typing.Optional[float]:
        """Get or set the Volume fraction of electrolyte in the separator
        """ # nopep8
        return self._cards[1].get_value("vfes")

    @vfes.setter
    def vfes(self, value: float) -> None:
        """Set the vfes property."""
        self._cards[1].set_value("vfes", value)

    @property
    def vfps(self) -> typing.Optional[float]:
        """Get or set the Volume fraction of the polymer phase in the separator
        """ # nopep8
        return self._cards[1].get_value("vfps")

    @vfps.setter
    def vfps(self, value: float) -> None:
        """Set the vfps property."""
        self._cards[1].set_value("vfps", value)

    @property
    def vfgs(self) -> typing.Optional[float]:
        """Get or set the Volume fraction of the gas in the separator
        """ # nopep8
        return self._cards[1].get_value("vfgs")

    @vfgs.setter
    def vfgs(self, value: float) -> None:
        """Set the vfgs property."""
        self._cards[1].set_value("vfgs", value)

    @property
    def lics(self) -> typing.Optional[float]:
        """Get or set the Initial concentration of Lithium c_e  (mol.m^(-3)) electrolyte
        """ # nopep8
        return self._cards[1].get_value("lics")

    @lics.setter
    def lics(self, value: float) -> None:
        """Set the lics property."""
        self._cards[1].set_value("lics", value)

    @property
    def bruge(self) -> float:
        """Get or set the Bruggeman coefficient in electrolyte
        """ # nopep8
        return self._cards[1].get_value("bruge")

    @bruge.setter
    def bruge(self, value: float) -> None:
        """Set the bruge property."""
        self._cards[1].set_value("bruge", value)

    @property
    def clmax(self) -> float:
        """Get or set the Maximum electrolyte salt concentration (mol.m^(-3)). Required when IELTYPE=1
        """ # nopep8
        return self._cards[1].get_value("clmax")

    @clmax.setter
    def clmax(self, value: float) -> None:
        """Set the clmax property."""
        self._cards[1].set_value("clmax", value)

