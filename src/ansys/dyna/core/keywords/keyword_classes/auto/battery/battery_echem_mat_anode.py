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
    FieldSchema("cellid", int, 0, 10, None),
    FieldSchema("iocpa", int, 10, 10, None),
    FieldSchema("capta", float, 20, 10, None),
    FieldSchema("soca", float, 30, 10, None),
    FieldSchema("rada", float, 40, 10, None),
    FieldSchema("ratea", float, 50, 10, None),
    FieldSchema("ranode", float, 60, 10, None),
    FieldSchema("ianode", float, 70, 10, None),
)

_BATTERYECHEMMATANODE_CARD1 = (
    FieldSchema("rhoea", float, 0, 10, None),
    FieldSchema("rhofa", float, 10, 10, None),
    FieldSchema("rhocca", float, 20, 10, None),
    FieldSchema("dfsa", float, 30, 10, None),
    FieldSchema("conda", float, 40, 10, None),
    FieldSchema("mwa", float, 50, 10, None),
)

_BATTERYECHEMMATANODE_CARD2 = (
    FieldSchema("vfea", float, 0, 10, None),
    FieldSchema("vfpa", float, 10, 10, None),
    FieldSchema("vffa", float, 20, 10, None),
    FieldSchema("vfga", float, 30, 10, None),
    FieldSchema("bruga", float, 40, 10, 1.5),
    FieldSchema("b1star", float, 50, 10, None),
    FieldSchema("lpore", float, 60, 10, None),
)

_BATTERYECHEMMATANODE_CARD3 = (
    FieldSchema("ibvtype", int, 0, 10, 0),
    FieldSchema("alfaa", float, 10, 10, 0.5),
    FieldSchema("alfac", float, 20, 10, 0.5),
)

_BATTERYECHEMMATANODE_CARD4 = (
    FieldSchema("eark", float, 0, 10, None),
    FieldSchema("eards", float, 10, 10, None),
    FieldSchema("earf", float, 20, 10, None),
)

_BATTERYECHEMMATANODE_CARD5 = (
    FieldSchema("s0a", float, 0, 10, None),
    FieldSchema("s100a", float, 10, 10, None),
    FieldSchema("pmv", float, 20, 10, None),
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
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMMATANODE_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMMATANODE_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMMATANODE_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMMATANODE_CARD4,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMMATANODE_CARD5,
                **kwargs,
            ),
        ]
    @property
    def cellid(self) -> typing.Optional[int]:
        """Get or set the Part ID. Not used with EM coupling
        """ # nopep8
        return self._cards[0].get_value("cellid")

    @cellid.setter
    def cellid(self, value: int) -> None:
        """Set the cellid property."""
        self._cards[0].set_value("cellid", value)

    @property
    def iocpa(self) -> typing.Optional[int]:
        """Get or set the Material type for the OCP in the anode electrode.
        LT.0:	Absolute value points to *DEFINE_FUNCTION ID. See Remark 2.
        EQ.1:Sony carbon (petroleum coke)
        EQ.2:MCMB 2510
        EQ.3:MCMB 2528
        EQ.4:KS6 graphite
        EQ.5:LiC6 graphite
        EQ.7:Metal hybrid
        EQ.10: Nernst Ideal Intercalation
        """ # nopep8
        return self._cards[0].get_value("iocpa")

    @iocpa.setter
    def iocpa(self, value: int) -> None:
        """Set the iocpa property."""
        self._cards[0].set_value("iocpa", value)

    @property
    def capta(self) -> typing.Optional[float]:
        """Get or set the Coulombic capacity of anode material(units: mAh/g).
        """ # nopep8
        return self._cards[0].get_value("capta")

    @capta.setter
    def capta(self, value: float) -> None:
        """Set the capta property."""
        self._cards[0].set_value("capta", value)

    @property
    def soca(self) -> typing.Optional[float]:
        """Get or set the Initial Lithium stoichiometric coefficient of the anode side active material. For example LixWO3 (0<x<0.7).
        """ # nopep8
        return self._cards[0].get_value("soca")

    @soca.setter
    def soca(self, value: float) -> None:
        """Set the soca property."""
        self._cards[0].set_value("soca", value)

    @property
    def rada(self) -> typing.Optional[float]:
        """Get or set the Radius of spherical particles in the anode side active material.
        """ # nopep8
        return self._cards[0].get_value("rada")

    @rada.setter
    def rada(self, value: float) -> None:
        """Set the rada property."""
        self._cards[0].set_value("rada", value)

    @property
    def ratea(self) -> typing.Optional[float]:
        """Get or set the Reaction rate constant for the anode electrode.
        """ # nopep8
        return self._cards[0].get_value("ratea")

    @ratea.setter
    def ratea(self, value: float) -> None:
        """Set the ratea property."""
        self._cards[0].set_value("ratea", value)

    @property
    def ranode(self) -> typing.Optional[float]:
        """Get or set the Film resistance for the anode electrode. Can be left to 0
        """ # nopep8
        return self._cards[0].get_value("ranode")

    @ranode.setter
    def ranode(self, value: float) -> None:
        """Set the ranode property."""
        self._cards[0].set_value("ranode", value)

    @property
    def ianode(self) -> typing.Optional[float]:
        """Get or set the Anode type:
        EQ.0:	Porous intercalation anode
        EQ.1:	Lithium metal foil (See Remark 3)
        """ # nopep8
        return self._cards[0].get_value("ianode")

    @ianode.setter
    def ianode(self, value: float) -> None:
        """Set the ianode property."""
        self._cards[0].set_value("ianode", value)

    @property
    def rhoea(self) -> typing.Optional[float]:
        """Get or set the Density of anode insertion material (electrode particles)(Kg/m3).
        """ # nopep8
        return self._cards[1].get_value("rhoea")

    @rhoea.setter
    def rhoea(self, value: float) -> None:
        """Set the rhoea property."""
        self._cards[1].set_value("rhoea", value)

    @property
    def rhofa(self) -> typing.Optional[float]:
        """Get or set the Density of the inert filler ?_(f,a) in the anode (kg.m^(-3)). Optional, used for cell mass.
        """ # nopep8
        return self._cards[1].get_value("rhofa")

    @rhofa.setter
    def rhofa(self, value: float) -> None:
        """Set the rhofa property."""
        self._cards[1].set_value("rhofa", value)

    @property
    def rhocca(self) -> typing.Optional[float]:
        """Get or set the Density of the current collector ?_(c,a) in the anode (kg.m^(-3)). Optional, used for cell mass
        """ # nopep8
        return self._cards[1].get_value("rhocca")

    @rhocca.setter
    def rhocca(self, value: float) -> None:
        """Set the rhocca property."""
        self._cards[1].set_value("rhocca", value)

    @property
    def dfsa(self) -> typing.Optional[float]:
        """Get or set the Diffusion coefficient D_(s,a) of lithium ions in the anode electrode material (m^2 s^(-1)).
        """ # nopep8
        return self._cards[1].get_value("dfsa")

    @dfsa.setter
    def dfsa(self, value: float) -> None:
        """Set the dfsa property."""
        self._cards[1].set_value("dfsa", value)

    @property
    def conda(self) -> typing.Optional[float]:
        """Get or set the Effective electronic conductivity of the anode porous electrode(units: m*m/s).
        """ # nopep8
        return self._cards[1].get_value("conda")

    @conda.setter
    def conda(self, value: float) -> None:
        """Set the conda property."""
        self._cards[1].set_value("conda", value)

    @property
    def mwa(self) -> typing.Optional[float]:
        """Get or set the Molecular weight of the anode electrode ( kg/mol).Used in 10-Equation or 15-Equation models only.
        """ # nopep8
        return self._cards[1].get_value("mwa")

    @mwa.setter
    def mwa(self, value: float) -> None:
        """Set the mwa property."""
        self._cards[1].set_value("mwa", value)

    @property
    def vfea(self) -> typing.Optional[float]:
        """Get or set the Volume fraction of electrolyte in the anode electrode.
        """ # nopep8
        return self._cards[2].get_value("vfea")

    @vfea.setter
    def vfea(self, value: float) -> None:
        """Set the vfea property."""
        self._cards[2].set_value("vfea", value)

    @property
    def vfpa(self) -> typing.Optional[float]:
        """Get or set the Volume fraction of the polymer phase in the anode electrode. Can be left to 0
        """ # nopep8
        return self._cards[2].get_value("vfpa")

    @vfpa.setter
    def vfpa(self, value: float) -> None:
        """Set the vfpa property."""
        self._cards[2].set_value("vfpa", value)

    @property
    def vffa(self) -> typing.Optional[float]:
        """Get or set the Volume fraction of the inert filler in the anode electrode. Can be left to 0
        """ # nopep8
        return self._cards[2].get_value("vffa")

    @vffa.setter
    def vffa(self, value: float) -> None:
        """Set the vffa property."""
        self._cards[2].set_value("vffa", value)

    @property
    def vfga(self) -> typing.Optional[float]:
        """Get or set the Volume fraction of the gas in the anode electrode. Unsed in 14-Equation model only
        """ # nopep8
        return self._cards[2].get_value("vfga")

    @vfga.setter
    def vfga(self, value: float) -> None:
        """Set the vfga property."""
        self._cards[2].set_value("vfga", value)

    @property
    def bruga(self) -> float:
        """Get or set the Bruggeman coefficient in the anode
        """ # nopep8
        return self._cards[2].get_value("bruga")

    @bruga.setter
    def bruga(self, value: float) -> None:
        """Set the bruga property."""
        self._cards[2].set_value("bruga", value)

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
    def ibvtype(self) -> int:
        """Get or set the Butler�Volmer equation formulation (see Remark 1):
        EQ.0 : Symmetric
        EQ.1 : Asymmetric
        EQ.2 : Linearized
        EQ.3 : Inverse formulation
        """ # nopep8
        return self._cards[3].get_value("ibvtype")

    @ibvtype.setter
    def ibvtype(self, value: int) -> None:
        """Set the ibvtype property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""ibvtype must be `None` or one of {0,1,2,3}.""")
        self._cards[3].set_value("ibvtype", value)

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
    def eark(self) -> typing.Optional[float]:
        """Get or set the Activation energies for Arrhenius correction factors on Diffusivity, conductivity, film resistance. See IARRH in *BATTERY_ECHEM_CONTROL_THERMAL.
        """ # nopep8
        return self._cards[4].get_value("eark")

    @eark.setter
    def eark(self, value: float) -> None:
        """Set the eark property."""
        self._cards[4].set_value("eark", value)

    @property
    def eards(self) -> typing.Optional[float]:
        """Get or set the Activation energies for Arrhenius correction factors on Diffusivity, conductivity, film resistance. See IARRH in *BATTERY_ECHEM_CONTROL_THERMAL.
        """ # nopep8
        return self._cards[4].get_value("eards")

    @eards.setter
    def eards(self, value: float) -> None:
        """Set the eards property."""
        self._cards[4].set_value("eards", value)

    @property
    def earf(self) -> typing.Optional[float]:
        """Get or set the Activation energies for Arrhenius correction factors on Diffusivity, conductivity, film resistance. See IARRH in *BATTERY_ECHEM_CONTROL_THERMAL.
        """ # nopep8
        return self._cards[4].get_value("earf")

    @earf.setter
    def earf(self, value: float) -> None:
        """Set the earf property."""
        self._cards[4].set_value("earf", value)

    @property
    def s0a(self) -> typing.Optional[float]:
        """Get or set the Stoichiometric coefficient at 0% and 100% state of charge. Not used by the solve but useful to track SOC evolution.
        """ # nopep8
        return self._cards[5].get_value("s0a")

    @s0a.setter
    def s0a(self, value: float) -> None:
        """Set the s0a property."""
        self._cards[5].set_value("s0a", value)

    @property
    def s100a(self) -> typing.Optional[float]:
        """Get or set the Stoichiometric coefficient at 0% and 100% state of charge. Not used by the solve but useful to track SOC evolution.
        """ # nopep8
        return self._cards[5].get_value("s100a")

    @s100a.setter
    def s100a(self, value: float) -> None:
        """Set the s100a property."""
        self._cards[5].set_value("s100a", value)

    @property
    def pmv(self) -> typing.Optional[float]:
        """Get or set the Partial molar volume (only used for swelling, see ISWELL on *EM_CONTROL_COUPLING_P2D).
        """ # nopep8
        return self._cards[5].get_value("pmv")

    @pmv.setter
    def pmv(self, value: float) -> None:
        """Set the pmv property."""
        self._cards[5].set_value("pmv", value)

