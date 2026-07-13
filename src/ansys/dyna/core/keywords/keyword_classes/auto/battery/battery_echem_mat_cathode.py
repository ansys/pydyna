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

"""Module providing the BatteryEchemMatCathode class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BATTERYECHEMMATCATHODE_CARD0 = (
    FieldSchema("pidc", int, 0, 10, None),
    FieldSchema("iocpc", int, 10, 10, None),
    FieldSchema("captc", float, 20, 10, None),
    FieldSchema("socc", float, 30, 10, None),
    FieldSchema("radc", float, 40, 10, None),
    FieldSchema("ratec", float, 50, 10, None),
    FieldSchema("rcathde", float, 60, 10, None),
)

_BATTERYECHEMMATCATHODE_CARD1 = (
    FieldSchema("rhoec", float, 0, 10, None),
    FieldSchema("rhofc", float, 10, 10, None),
    FieldSchema("rhoccc", float, 20, 10, None),
    FieldSchema("dfsc", float, 30, 10, None),
    FieldSchema("condc", float, 40, 10, None),
    FieldSchema("mwc", float, 50, 10, None),
)

_BATTERYECHEMMATCATHODE_CARD2 = (
    FieldSchema("vfec", float, 0, 10, None),
    FieldSchema("vfpc", float, 10, 10, None),
    FieldSchema("vffc", float, 20, 10, None),
    FieldSchema("vfgc", float, 30, 10, None),
    FieldSchema("brugc", float, 40, 10, 1.5),
    FieldSchema("b1star", float, 50, 10, None),
    FieldSchema("lpore", float, 60, 10, None),
)

_BATTERYECHEMMATCATHODE_CARD3 = (
    FieldSchema("ibvtyp", int, 0, 10, 0),
    FieldSchema("alfaa", float, 10, 10, 0.5),
    FieldSchema("alfac", float, 20, 10, 0.5),
)

_BATTERYECHEMMATCATHODE_CARD4 = (
    FieldSchema("ecrk", float, 0, 10, None),
    FieldSchema("ecrds", float, 10, 10, None),
    FieldSchema("ecrf", float, 20, 10, None),
)

_BATTERYECHEMMATCATHODE_CARD5 = (
    FieldSchema("s0c", float, 0, 10, None),
    FieldSchema("s100c", float, 10, 10, None),
    FieldSchema("pmv", float, 20, 10, None),
)

class BatteryEchemMatCathode(KeywordBase):
    """DYNA BATTERY_ECHEM_MAT_CATHODE keyword"""

    keyword = "BATTERY"
    subkeyword = "ECHEM_MAT_CATHODE"

    def __init__(self, **kwargs):
        """Initialize the BatteryEchemMatCathode class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMMATCATHODE_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMMATCATHODE_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMMATCATHODE_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMMATCATHODE_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMMATCATHODE_CARD4,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMMATCATHODE_CARD5,
                **kwargs,
            ),
        ]
    @property
    def pidc(self) -> typing.Optional[int]:
        """Get or set the Part ID
        """ # nopep8
        return self._cards[0].get_value("pidc")

    @pidc.setter
    def pidc(self, value: int) -> None:
        """Set the pidc property."""
        self._cards[0].set_value("pidc", value)

    @property
    def iocpc(self) -> typing.Optional[int]:
        """Get or set the Material type for the open-circuit potential:
        EQ.1: Mn2O4(lower plateau) (1.1 < y < 1.99)
        EQ.2: Mn2O4(upper plateau) (0.17 < y < 0.99)
        EQ.3: Cobalt dioxide 1, LiyCoO2(0.0 < y < 0.99)
        EQ.4: Cobalt dioxide 2, LiyCoO2(0.0 < y < 0.99)
        EQ.5: Mn2O4(literature version) (0.17 < y < 0.99)
        EQ.6: NMC - 111
        EQ.7: NMC - 811
        EQ.8:	Ni0.8Co0.2O2
        EQ.9:LiFePO4
        EQ.10:Nernst Ideal intercalation
        EQ.12:NiOOH/Ni(OH)2
        EQ.13:LiCoO2
        """ # nopep8
        return self._cards[0].get_value("iocpc")

    @iocpc.setter
    def iocpc(self, value: int) -> None:
        """Set the iocpc property."""
        self._cards[0].set_value("iocpc", value)

    @property
    def captc(self) -> typing.Optional[float]:
        """Get or set the Coulombic capacity of anode material  (units: mAh/g).
        """ # nopep8
        return self._cards[0].get_value("captc")

    @captc.setter
    def captc(self, value: float) -> None:
        """Set the captc property."""
        self._cards[0].set_value("captc", value)

    @property
    def socc(self) -> typing.Optional[float]:
        """Get or set the Initial Lithium stoichiometric coefficient of the anode side active material. For example LixWO3 (0<x<0.67).
        """ # nopep8
        return self._cards[0].get_value("socc")

    @socc.setter
    def socc(self, value: float) -> None:
        """Set the socc property."""
        self._cards[0].set_value("socc", value)

    @property
    def radc(self) -> typing.Optional[float]:
        """Get or set the Radius of spherical particle in the cathode side active material.
        """ # nopep8
        return self._cards[0].get_value("radc")

    @radc.setter
    def radc(self, value: float) -> None:
        """Set the radc property."""
        self._cards[0].set_value("radc", value)

    @property
    def ratec(self) -> typing.Optional[float]:
        """Get or set the Reaction rate constant for the cathode electrode
        """ # nopep8
        return self._cards[0].get_value("ratec")

    @ratec.setter
    def ratec(self, value: float) -> None:
        """Set the ratec property."""
        self._cards[0].set_value("ratec", value)

    @property
    def rcathde(self) -> typing.Optional[float]:
        """Get or set the Film resistance for the cathode electrode
        """ # nopep8
        return self._cards[0].get_value("rcathde")

    @rcathde.setter
    def rcathde(self, value: float) -> None:
        """Set the rcathde property."""
        self._cards[0].set_value("rcathde", value)

    @property
    def rhoec(self) -> typing.Optional[float]:
        """Get or set the Density of the cathode insertion material (electrode particles).
        """ # nopep8
        return self._cards[1].get_value("rhoec")

    @rhoec.setter
    def rhoec(self, value: float) -> None:
        """Set the rhoec property."""
        self._cards[1].set_value("rhoec", value)

    @property
    def rhofc(self) -> typing.Optional[float]:
        """Get or set the Density of the cathode side inert filler.
        """ # nopep8
        return self._cards[1].get_value("rhofc")

    @rhofc.setter
    def rhofc(self, value: float) -> None:
        """Set the rhofc property."""
        self._cards[1].set_value("rhofc", value)

    @property
    def rhoccc(self) -> typing.Optional[float]:
        """Get or set the Density of the cathode side current collector.
        """ # nopep8
        return self._cards[1].get_value("rhoccc")

    @rhoccc.setter
    def rhoccc(self, value: float) -> None:
        """Set the rhoccc property."""
        self._cards[1].set_value("rhoccc", value)

    @property
    def dfsc(self) -> typing.Optional[float]:
        """Get or set the Diffusion coefficient of Lithium ions in the cathode insertion material.
        """ # nopep8
        return self._cards[1].get_value("dfsc")

    @dfsc.setter
    def dfsc(self, value: float) -> None:
        """Set the dfsc property."""
        self._cards[1].set_value("dfsc", value)

    @property
    def condc(self) -> typing.Optional[float]:
        """Get or set the Effective electronic conductivity of the cathode porous electrode
        """ # nopep8
        return self._cards[1].get_value("condc")

    @condc.setter
    def condc(self, value: float) -> None:
        """Set the condc property."""
        self._cards[1].set_value("condc", value)

    @property
    def mwc(self) -> typing.Optional[float]:
        """Get or set the Molecular weight of the cathode electrode
        """ # nopep8
        return self._cards[1].get_value("mwc")

    @mwc.setter
    def mwc(self, value: float) -> None:
        """Set the mwc property."""
        self._cards[1].set_value("mwc", value)

    @property
    def vfec(self) -> typing.Optional[float]:
        """Get or set the Volume fraction of electrolyte in the anode electrode
        """ # nopep8
        return self._cards[2].get_value("vfec")

    @vfec.setter
    def vfec(self, value: float) -> None:
        """Set the vfec property."""
        self._cards[2].set_value("vfec", value)

    @property
    def vfpc(self) -> typing.Optional[float]:
        """Get or set the Volume fraction of the polymer phase in the anode electrode
        """ # nopep8
        return self._cards[2].get_value("vfpc")

    @vfpc.setter
    def vfpc(self, value: float) -> None:
        """Set the vfpc property."""
        self._cards[2].set_value("vfpc", value)

    @property
    def vffc(self) -> typing.Optional[float]:
        """Get or set the Volume fraction of the inert filler in the anode electrode
        """ # nopep8
        return self._cards[2].get_value("vffc")

    @vffc.setter
    def vffc(self, value: float) -> None:
        """Set the vffc property."""
        self._cards[2].set_value("vffc", value)

    @property
    def vfgc(self) -> typing.Optional[float]:
        """Get or set the Volume fraction of the gas in the anode electrode
        """ # nopep8
        return self._cards[2].get_value("vfgc")

    @vfgc.setter
    def vfgc(self, value: float) -> None:
        """Set the vfgc property."""
        self._cards[2].set_value("vfgc", value)

    @property
    def brugc(self) -> float:
        """Get or set the Bruggeman coefficient in the anode
        """ # nopep8
        return self._cards[2].get_value("brugc")

    @brugc.setter
    def brugc(self, value: float) -> None:
        """Set the brugc property."""
        self._cards[2].set_value("brugc", value)

    @property
    def b1star(self) -> typing.Optional[float]:
        """Get or set the Thiele closure coefficient..
        """ # nopep8
        return self._cards[2].get_value("b1star")

    @b1star.setter
    def b1star(self, value: float) -> None:
        """Set the b1star property."""
        self._cards[2].set_value("b1star", value)

    @property
    def lpore(self) -> typing.Optional[float]:
        """Get or set the Optional pore length scale (m). Used when B1STAR defined
        """ # nopep8
        return self._cards[2].get_value("lpore")

    @lpore.setter
    def lpore(self, value: float) -> None:
        """Set the lpore property."""
        self._cards[2].set_value("lpore", value)

    @property
    def ibvtyp(self) -> int:
        """Get or set the Butler�Volmer equation formulation:
        EQ.0:  Symmetric
        EQ.1:	Asymmetric
        EQ.2:Linearized
        EQ.3:Inverse formulation
        """ # nopep8
        return self._cards[3].get_value("ibvtyp")

    @ibvtyp.setter
    def ibvtyp(self, value: int) -> None:
        """Set the ibvtyp property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""ibvtyp must be `None` or one of {0,1,2,3}.""")
        self._cards[3].set_value("ibvtyp", value)

    @property
    def alfaa(self) -> float:
        """Get or set the Butler�Volmer equation alpha parameters ?_a and ?_c.
        """ # nopep8
        return self._cards[3].get_value("alfaa")

    @alfaa.setter
    def alfaa(self, value: float) -> None:
        """Set the alfaa property."""
        self._cards[3].set_value("alfaa", value)

    @property
    def alfac(self) -> float:
        """Get or set the Butler�Volmer equation alpha parameters ?_a and ?_c.
        """ # nopep8
        return self._cards[3].get_value("alfac")

    @alfac.setter
    def alfac(self, value: float) -> None:
        """Set the alfac property."""
        self._cards[3].set_value("alfac", value)

    @property
    def ecrk(self) -> typing.Optional[float]:
        """Get or set the Activation energies for Arrhenius correction factors (?Ea?_k/R,?Ea?_(D_s )/R, ?Ea?_(R_f )/R) on Diffusivity, conductivity, film resistance (K). See IARRH in *BATTERY_ECHEM_CONTROL_THERMAL.
        """ # nopep8
        return self._cards[4].get_value("ecrk")

    @ecrk.setter
    def ecrk(self, value: float) -> None:
        """Set the ecrk property."""
        self._cards[4].set_value("ecrk", value)

    @property
    def ecrds(self) -> typing.Optional[float]:
        """Get or set the Activation energies for Arrhenius correction factors (?Ea?_k/R,?Ea?_(D_s )/R, ?Ea?_(R_f )/R) on Diffusivity, conductivity, film resistance (K). See IARRH in *BATTERY_ECHEM_CONTROL_THERMAL.
        """ # nopep8
        return self._cards[4].get_value("ecrds")

    @ecrds.setter
    def ecrds(self, value: float) -> None:
        """Set the ecrds property."""
        self._cards[4].set_value("ecrds", value)

    @property
    def ecrf(self) -> typing.Optional[float]:
        """Get or set the Activation energies for Arrhenius correction factors (?Ea?_k/R,?Ea?_(D_s )/R, ?Ea?_(R_f )/R) on Diffusivity, conductivity, film resistance (K). See IARRH in *BATTERY_ECHEM_CONTROL_THERMAL.
        """ # nopep8
        return self._cards[4].get_value("ecrf")

    @ecrf.setter
    def ecrf(self, value: float) -> None:
        """Set the ecrf property."""
        self._cards[4].set_value("ecrf", value)

    @property
    def s0c(self) -> typing.Optional[float]:
        """Get or set the Stoichiometric coefficient at 0% and 100% state of charge. Not used by the solve but useful to track SOC evolution.
        """ # nopep8
        return self._cards[5].get_value("s0c")

    @s0c.setter
    def s0c(self, value: float) -> None:
        """Set the s0c property."""
        self._cards[5].set_value("s0c", value)

    @property
    def s100c(self) -> typing.Optional[float]:
        """Get or set the Stoichiometric coefficient at 0% and 100% state of charge. Not used by the solve but useful to track SOC evolution.
        """ # nopep8
        return self._cards[5].get_value("s100c")

    @s100c.setter
    def s100c(self, value: float) -> None:
        """Set the s100c property."""
        self._cards[5].set_value("s100c", value)

    @property
    def pmv(self) -> typing.Optional[float]:
        """Get or set the Partial molar volume (m^3 ?.mol?^(-1))  (only used for swelling, see ISWELL on *EM_CONTROL_COUPLING_P2D).
        """ # nopep8
        return self._cards[5].get_value("pmv")

    @pmv.setter
    def pmv(self, value: float) -> None:
        """Set the pmv property."""
        self._cards[5].set_value("pmv", value)

