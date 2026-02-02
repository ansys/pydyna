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

"""Module providing the DefineParticleBlast class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_DEFINEPARTICLEBLAST_CARD0 = (
    FieldSchema("lagsid", int, 0, 10, None),
    FieldSchema("lagstype", int, 10, 10, 0),
    FieldSchema("nodid", int, 20, 10, None),
    FieldSchema("nodtype", int, 30, 10, 0),
    FieldSchema("hecid", int, 40, 10, None),
    FieldSchema("hectype", int, 50, 10, 0),
    FieldSchema("aircid", int, 60, 10, None),
)

_DEFINEPARTICLEBLAST_CARD1 = (
    FieldSchema("nphe", int, 0, 10, None),
    FieldSchema("npair", int, 10, 10, None),
    FieldSchema("iunit", int, 20, 10, 0),
)

_DEFINEPARTICLEBLAST_CARD2 = (
    FieldSchema("ihetype", int, 0, 10, 0),
    FieldSchema("density", float, 10, 10, None),
    FieldSchema("energy", float, 20, 10, None),
    FieldSchema("gamma", float, 30, 10, None),
    FieldSchema("covol", float, 40, 10, None),
    FieldSchema("deto_v", float, 50, 10, None),
)

_DEFINEPARTICLEBLAST_CARD3 = (
    FieldSchema("detx", float, 0, 10, None),
    FieldSchema("dety", float, 10, 10, None),
    FieldSchema("detz", float, 20, 10, None),
    FieldSchema("tdet", float, 30, 10, None),
    FieldSchema("btend", float, 40, 10, None),
    FieldSchema("nid", int, 50, 10, None),
)

_DEFINEPARTICLEBLAST_CARD4 = (
    FieldSchema("bcx0", float, 0, 10, None),
    FieldSchema("bcx1", float, 10, 10, None),
    FieldSchema("bcy0", float, 20, 10, None),
    FieldSchema("bcy1", float, 30, 10, None),
    FieldSchema("bcz0", float, 40, 10, None),
    FieldSchema("bcz1", float, 50, 10, None),
)

_DEFINEPARTICLEBLAST_CARD5 = (
    FieldSchema("ibcx0", int, 0, 10, None),
    FieldSchema("ibcx1", int, 10, 10, None),
    FieldSchema("ibcy0", int, 20, 10, None),
    FieldSchema("ibcy1", int, 30, 10, None),
    FieldSchema("ibcz0", int, 40, 10, None),
    FieldSchema("ibcz1", int, 50, 10, None),
    FieldSchema("bc_p", int, 60, 10, 0),
)

_DEFINEPARTICLEBLAST_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineParticleBlast(KeywordBase):
    """DYNA DEFINE_PARTICLE_BLAST keyword"""

    keyword = "DEFINE"
    subkeyword = "PARTICLE_BLAST"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "nid": LinkType.NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineParticleBlast class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEPARTICLEBLAST_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEPARTICLEBLAST_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEPARTICLEBLAST_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEPARTICLEBLAST_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEPARTICLEBLAST_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEPARTICLEBLAST_CARD5,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineParticleBlast.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEPARTICLEBLAST_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def lagsid(self) -> typing.Optional[int]:
        """Get or set the Structure ID for particle structure interaction
        """ # nopep8
        return self._cards[0].get_value("lagsid")

    @lagsid.setter
    def lagsid(self, value: int) -> None:
        """Set the lagsid property."""
        self._cards[0].set_value("lagsid", value)

    @property
    def lagstype(self) -> int:
        """Get or set the Structure type:
        EQ.0:	Part Set
        EQ.1 : Part
        """ # nopep8
        return self._cards[0].get_value("lagstype")

    @lagstype.setter
    def lagstype(self, value: int) -> None:
        """Set the lagstype property."""
        if value not in [0, 1, None]:
            raise Exception("""lagstype must be `None` or one of {0,1}.""")
        self._cards[0].set_value("lagstype", value)

    @property
    def nodid(self) -> typing.Optional[int]:
        """Get or set the Discrete element sphere (DES) or Smooth particle hydrodynamics (SPH) ID for the interaction between particles and nodes
        """ # nopep8
        return self._cards[0].get_value("nodid")

    @nodid.setter
    def nodid(self, value: int) -> None:
        """Set the nodid property."""
        self._cards[0].set_value("nodid", value)

    @property
    def nodtype(self) -> int:
        """Get or set the Nodal type:
        EQ.0:	Node Set
        EQ.1 : Node
        EQ.2 : Part Set
        EQ.3 : Part
        """ # nopep8
        return self._cards[0].get_value("nodtype")

    @nodtype.setter
    def nodtype(self, value: int) -> None:
        """Set the nodtype property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""nodtype must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("nodtype", value)

    @property
    def hecid(self) -> typing.Optional[int]:
        """Get or set the Initial container for high explosive particle
        """ # nopep8
        return self._cards[0].get_value("hecid")

    @hecid.setter
    def hecid(self, value: int) -> None:
        """Set the hecid property."""
        self._cards[0].set_value("hecid", value)

    @property
    def hectype(self) -> int:
        """Get or set the Structure type:
        EQ.0:	Part Set
        EQ.1 : Part
        EQ.2 : Geometry, see* DEFINE_‌PBLAST_‌GEOMETRY
        """ # nopep8
        return self._cards[0].get_value("hectype")

    @hectype.setter
    def hectype(self, value: int) -> None:
        """Set the hectype property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""hectype must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("hectype", value)

    @property
    def aircid(self) -> typing.Optional[int]:
        """Get or set the Initial geometry for air particles:
        EQ.0:	Filled air particles to entire domain defined by Card 5
        GT.0 : Reference to * DEFINE_‌PBLAST_‌AIRGEO ID
        """ # nopep8
        return self._cards[0].get_value("aircid")

    @aircid.setter
    def aircid(self, value: int) -> None:
        """Set the aircid property."""
        self._cards[0].set_value("aircid", value)

    @property
    def nphe(self) -> typing.Optional[int]:
        """Get or set the Number of high explosive particles
        """ # nopep8
        return self._cards[1].get_value("nphe")

    @nphe.setter
    def nphe(self, value: int) -> None:
        """Set the nphe property."""
        self._cards[1].set_value("nphe", value)

    @property
    def npair(self) -> typing.Optional[int]:
        """Get or set the Number of air particles
        """ # nopep8
        return self._cards[1].get_value("npair")

    @npair.setter
    def npair(self, value: int) -> None:
        """Set the npair property."""
        self._cards[1].set_value("npair", value)

    @property
    def iunit(self) -> int:
        """Get or set the Unit System
        EQ.0:	Kg - mm - ms - K
        EQ.1 : SI Units
        EQ.2 : Ton - mm - s - K
        EQ.3 : g - cm - us - K
        EQ.4 : blob - in - s - K
        """ # nopep8
        return self._cards[1].get_value("iunit")

    @iunit.setter
    def iunit(self, value: int) -> None:
        """Set the iunit property."""
        if value not in [0, 1, 2, 3, 4, None]:
            raise Exception("""iunit must be `None` or one of {0,1,2,3,4}.""")
        self._cards[1].set_value("iunit", value)

    @property
    def ihetype(self) -> int:
        """Get or set the High Explosive type (see Remark 1):
        EQ.0:	User defined
        EQ.1 : TNT
        EQ.2 : C4
        """ # nopep8
        return self._cards[2].get_value("ihetype")

    @ihetype.setter
    def ihetype(self, value: int) -> None:
        """Set the ihetype property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""ihetype must be `None` or one of {0,1,2}.""")
        self._cards[2].set_value("ihetype", value)

    @property
    def density(self) -> typing.Optional[float]:
        """Get or set the High Explosive density for user defined explosive
        """ # nopep8
        return self._cards[2].get_value("density")

    @density.setter
    def density(self, value: float) -> None:
        """Set the density property."""
        self._cards[2].set_value("density", value)

    @property
    def energy(self) -> typing.Optional[float]:
        """Get or set the High Explosive energy per unit volume for user defined explosive
        """ # nopep8
        return self._cards[2].get_value("energy")

    @energy.setter
    def energy(self, value: float) -> None:
        """Set the energy property."""
        self._cards[2].set_value("energy", value)

    @property
    def gamma(self) -> typing.Optional[float]:
        """Get or set the High Explosive fraction between C_p and C_v for user defined explosive
        """ # nopep8
        return self._cards[2].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        """Set the gamma property."""
        self._cards[2].set_value("gamma", value)

    @property
    def covol(self) -> typing.Optional[float]:
        """Get or set the High Explosive co-volume for user defined explosive
        """ # nopep8
        return self._cards[2].get_value("covol")

    @covol.setter
    def covol(self, value: float) -> None:
        """Set the covol property."""
        self._cards[2].set_value("covol", value)

    @property
    def deto_v(self) -> typing.Optional[float]:
        """Get or set the High Explosive detonation velocity for user define explosive
        """ # nopep8
        return self._cards[2].get_value("deto_v")

    @deto_v.setter
    def deto_v(self, value: float) -> None:
        """Set the deto_v property."""
        self._cards[2].set_value("deto_v", value)

    @property
    def detx(self) -> typing.Optional[float]:
        """Get or set the Detonation point x
        """ # nopep8
        return self._cards[3].get_value("detx")

    @detx.setter
    def detx(self, value: float) -> None:
        """Set the detx property."""
        self._cards[3].set_value("detx", value)

    @property
    def dety(self) -> typing.Optional[float]:
        """Get or set the Detonation point y
        """ # nopep8
        return self._cards[3].get_value("dety")

    @dety.setter
    def dety(self, value: float) -> None:
        """Set the dety property."""
        self._cards[3].set_value("dety", value)

    @property
    def detz(self) -> typing.Optional[float]:
        """Get or set the Detonation point z
        """ # nopep8
        return self._cards[3].get_value("detz")

    @detz.setter
    def detz(self, value: float) -> None:
        """Set the detz property."""
        self._cards[3].set_value("detz", value)

    @property
    def tdet(self) -> typing.Optional[float]:
        """Get or set the Detonation time
        """ # nopep8
        return self._cards[3].get_value("tdet")

    @tdet.setter
    def tdet(self, value: float) -> None:
        """Set the tdet property."""
        self._cards[3].set_value("tdet", value)

    @property
    def btend(self) -> typing.Optional[float]:
        """Get or set the Blast end time
        """ # nopep8
        return self._cards[3].get_value("btend")

    @btend.setter
    def btend(self, value: float) -> None:
        """Set the btend property."""
        self._cards[3].set_value("btend", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the An optional node ID defining the position of the detonation point. If defined, its coordinates will overwrite the DETX, DETY, and DETZ defined above.
        """ # nopep8
        return self._cards[3].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[3].set_value("nid", value)

    @property
    def bcx0(self) -> typing.Optional[float]:
        """Get or set the Global domain x-min
        """ # nopep8
        return self._cards[4].get_value("bcx0")

    @bcx0.setter
    def bcx0(self, value: float) -> None:
        """Set the bcx0 property."""
        self._cards[4].set_value("bcx0", value)

    @property
    def bcx1(self) -> typing.Optional[float]:
        """Get or set the Global domain x-max
        """ # nopep8
        return self._cards[4].get_value("bcx1")

    @bcx1.setter
    def bcx1(self, value: float) -> None:
        """Set the bcx1 property."""
        self._cards[4].set_value("bcx1", value)

    @property
    def bcy0(self) -> typing.Optional[float]:
        """Get or set the Global domain y-min
        """ # nopep8
        return self._cards[4].get_value("bcy0")

    @bcy0.setter
    def bcy0(self, value: float) -> None:
        """Set the bcy0 property."""
        self._cards[4].set_value("bcy0", value)

    @property
    def bcy1(self) -> typing.Optional[float]:
        """Get or set the Global domain y-max
        """ # nopep8
        return self._cards[4].get_value("bcy1")

    @bcy1.setter
    def bcy1(self, value: float) -> None:
        """Set the bcy1 property."""
        self._cards[4].set_value("bcy1", value)

    @property
    def bcz0(self) -> typing.Optional[float]:
        """Get or set the Global domain z-min
        """ # nopep8
        return self._cards[4].get_value("bcz0")

    @bcz0.setter
    def bcz0(self, value: float) -> None:
        """Set the bcz0 property."""
        self._cards[4].set_value("bcz0", value)

    @property
    def bcz1(self) -> typing.Optional[float]:
        """Get or set the Global domain y-max
        """ # nopep8
        return self._cards[4].get_value("bcz1")

    @bcz1.setter
    def bcz1(self, value: float) -> None:
        """Set the bcz1 property."""
        self._cards[4].set_value("bcz1", value)

    @property
    def ibcx0(self) -> typing.Optional[int]:
        """Get or set the Boundary conditions for global domain x-min:
        EQ.0:	Free
        EQ.1 : Rigid reflecting boundary
        """ # nopep8
        return self._cards[5].get_value("ibcx0")

    @ibcx0.setter
    def ibcx0(self, value: int) -> None:
        """Set the ibcx0 property."""
        self._cards[5].set_value("ibcx0", value)

    @property
    def ibcx1(self) -> typing.Optional[int]:
        """Get or set the Boundary conditions for global domain x-max:
        EQ.0:	Free
        EQ.1 : Rigid reflecting boundary
        """ # nopep8
        return self._cards[5].get_value("ibcx1")

    @ibcx1.setter
    def ibcx1(self, value: int) -> None:
        """Set the ibcx1 property."""
        self._cards[5].set_value("ibcx1", value)

    @property
    def ibcy0(self) -> typing.Optional[int]:
        """Get or set the Boundary conditions for global domain y-min:
        EQ.0:	Free
        EQ.1 : Rigid reflecting boundary
        """ # nopep8
        return self._cards[5].get_value("ibcy0")

    @ibcy0.setter
    def ibcy0(self, value: int) -> None:
        """Set the ibcy0 property."""
        self._cards[5].set_value("ibcy0", value)

    @property
    def ibcy1(self) -> typing.Optional[int]:
        """Get or set the Boundary conditions for global domain y-max:
        EQ.0:	Free
        EQ.1 : Rigid reflecting boundary
        """ # nopep8
        return self._cards[5].get_value("ibcy1")

    @ibcy1.setter
    def ibcy1(self, value: int) -> None:
        """Set the ibcy1 property."""
        self._cards[5].set_value("ibcy1", value)

    @property
    def ibcz0(self) -> typing.Optional[int]:
        """Get or set the Boundary conditions for global domain z-min:
        EQ.0:	Free
        EQ.1 : Rigid reflecting boundary
        """ # nopep8
        return self._cards[5].get_value("ibcz0")

    @ibcz0.setter
    def ibcz0(self, value: int) -> None:
        """Set the ibcz0 property."""
        self._cards[5].set_value("ibcz0", value)

    @property
    def ibcz1(self) -> typing.Optional[int]:
        """Get or set the Boundary conditions for global domain z-max:
        EQ.0:	Free
        EQ.1 : Rigid reflecting boundary
        """ # nopep8
        return self._cards[5].get_value("ibcz1")

    @ibcz1.setter
    def ibcz1(self, value: int) -> None:
        """Set the ibcz1 property."""
        self._cards[5].set_value("ibcz1", value)

    @property
    def bc_p(self) -> int:
        """Get or set the Pressure ambient boundary condition for global domain:
        EQ.0:	Off(Default)
        EQ.1 : On
        """ # nopep8
        return self._cards[5].get_value("bc_p")

    @bc_p.setter
    def bc_p(self, value: int) -> None:
        """Set the bc_p property."""
        self._cards[5].set_value("bc_p", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[6].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[6].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def nid_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given nid."""
        return self._get_link_by_attr("NODE", "nid", self.nid, "parts")

