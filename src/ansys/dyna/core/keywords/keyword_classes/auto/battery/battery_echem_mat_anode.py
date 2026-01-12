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

"""Module providing the BatteryEchemMatAnode class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BATTERYECHEMMATANODE_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("iocpa", int, 10, 10, None),
    FieldSchema("capta", float, 20, 10, None),
    FieldSchema("s_xa", float, 30, 10, None),
    FieldSchema("rada", float, 40, 10, None),
    FieldSchema("ratea", float, 50, 10, None),
    FieldSchema("ranode", float, 60, 10, None),
)

_BATTERYECHEMMATANODE_CARD1 = (
    FieldSchema("rhoea", float, 0, 10, None),
    FieldSchema("rhofa", float, 10, 10, None),
    FieldSchema("rhocca", float, 20, 10, None),
    FieldSchema("diffa", float, 30, 10, None),
    FieldSchema("conda", float, 40, 10, None),
)

_BATTERYECHEMMATANODE_CARD2 = (
    FieldSchema("vfea", float, 0, 10, None),
    FieldSchema("vfpa", float, 10, 10, None),
    FieldSchema("vffa", float, 20, 10, None),
    FieldSchema("vfga", float, 30, 10, None),
)

class BatteryEchemMatAnode(KeywordBase):
    """DYNA BATTERY_ECHEM_MAT_ANODE keyword"""

    keyword = "BATTERY"
    subkeyword = "ECHEM_MAT_ANODE"

    def __init__(self, **kwargs):
        """Initialize the BatteryEchemMatAnode class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMMATANODE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMMATANODE_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMMATANODE_CARD2,
                **kwargs,
            ),        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part IDr
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def iocpa(self) -> typing.Optional[int]:
        """Get or set the Material type for the open-circuit potential.
        EQ.1:	Lithium metal foil.
        EQ.2 : Titanium disulfide, LixTiS2(0 < x < 1).
        EQ.3 : Petroleum coke, Carbon.
        EQ.4 : MCMB 2510 carbon.
        EQ.5 : MCMB 2528 carbon
        """ # nopep8
        return self._cards[0].get_value("iocpa")

    @iocpa.setter
    def iocpa(self, value: int) -> None:
        """Set the iocpa property."""
        self._cards[0].set_value("iocpa", value)

    @property
    def capta(self) -> typing.Optional[float]:
        """Get or set the Coulombic capacity of anode material.
        """ # nopep8
        return self._cards[0].get_value("capta")

    @capta.setter
    def capta(self, value: float) -> None:
        """Set the capta property."""
        self._cards[0].set_value("capta", value)

    @property
    def s_xa(self) -> typing.Optional[float]:
        """Get or set the Initial Lithium stoichiometric coefficient of the anode side active material. For example LixWO3 (0<x<0.67).
        """ # nopep8
        return self._cards[0].get_value("s_xa")

    @s_xa.setter
    def s_xa(self, value: float) -> None:
        """Set the s_xa property."""
        self._cards[0].set_value("s_xa", value)

    @property
    def rada(self) -> typing.Optional[float]:
        """Get or set the Radius of spherical particles in the anode side active material. (m)
        """ # nopep8
        return self._cards[0].get_value("rada")

    @rada.setter
    def rada(self, value: float) -> None:
        """Set the rada property."""
        self._cards[0].set_value("rada", value)

    @property
    def ratea(self) -> typing.Optional[float]:
        """Get or set the Reaction rate constant for the anode electrode
        """ # nopep8
        return self._cards[0].get_value("ratea")

    @ratea.setter
    def ratea(self, value: float) -> None:
        """Set the ratea property."""
        self._cards[0].set_value("ratea", value)

    @property
    def ranode(self) -> typing.Optional[float]:
        """Get or set the Film resistance for the anode electrode
        """ # nopep8
        return self._cards[0].get_value("ranode")

    @ranode.setter
    def ranode(self, value: float) -> None:
        """Set the ranode property."""
        self._cards[0].set_value("ranode", value)

    @property
    def rhoea(self) -> typing.Optional[float]:
        """Get or set the Density of anode insertion material (electrode particles). (Kg/m3)
        """ # nopep8
        return self._cards[1].get_value("rhoea")

    @rhoea.setter
    def rhoea(self, value: float) -> None:
        """Set the rhoea property."""
        self._cards[1].set_value("rhoea", value)

    @property
    def rhofa(self) -> typing.Optional[float]:
        """Get or set the Density of the anode side inert filler. (Kg/m3)
        """ # nopep8
        return self._cards[1].get_value("rhofa")

    @rhofa.setter
    def rhofa(self, value: float) -> None:
        """Set the rhofa property."""
        self._cards[1].set_value("rhofa", value)

    @property
    def rhocca(self) -> typing.Optional[float]:
        """Get or set the Density of the anode side current collector. (Kg/m3)
        """ # nopep8
        return self._cards[1].get_value("rhocca")

    @rhocca.setter
    def rhocca(self, value: float) -> None:
        """Set the rhocca property."""
        self._cards[1].set_value("rhocca", value)

    @property
    def diffa(self) -> typing.Optional[float]:
        """Get or set the Diffusion coefficient of Lithium ions in the anode insertion material
        """ # nopep8
        return self._cards[1].get_value("diffa")

    @diffa.setter
    def diffa(self, value: float) -> None:
        """Set the diffa property."""
        self._cards[1].set_value("diffa", value)

    @property
    def conda(self) -> typing.Optional[float]:
        """Get or set the Effective electronic conductivity of the anode porous electrode
        """ # nopep8
        return self._cards[1].get_value("conda")

    @conda.setter
    def conda(self, value: float) -> None:
        """Set the conda property."""
        self._cards[1].set_value("conda", value)

    @property
    def vfea(self) -> typing.Optional[float]:
        """Get or set the Volume fraction of electrolyte in the anode electrode
        """ # nopep8
        return self._cards[2].get_value("vfea")

    @vfea.setter
    def vfea(self, value: float) -> None:
        """Set the vfea property."""
        self._cards[2].set_value("vfea", value)

    @property
    def vfpa(self) -> typing.Optional[float]:
        """Get or set the Volume fraction of the polymer phase in the anode electrode
        """ # nopep8
        return self._cards[2].get_value("vfpa")

    @vfpa.setter
    def vfpa(self, value: float) -> None:
        """Set the vfpa property."""
        self._cards[2].set_value("vfpa", value)

    @property
    def vffa(self) -> typing.Optional[float]:
        """Get or set the Volume fraction of the inert filler in the anode electrode
        """ # nopep8
        return self._cards[2].get_value("vffa")

    @vffa.setter
    def vffa(self, value: float) -> None:
        """Set the vffa property."""
        self._cards[2].set_value("vffa", value)

    @property
    def vfga(self) -> typing.Optional[float]:
        """Get or set the Volume fraction of the gas in the anode electrode
        """ # nopep8
        return self._cards[2].get_value("vfga")

    @vfga.setter
    def vfga(self, value: float) -> None:
        """Set the vfga property."""
        self._cards[2].set_value("vfga", value)

