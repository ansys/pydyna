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

import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class ParticleBlast(KeywordBase):
    """DYNA PARTICLE_BLAST keyword"""

    keyword = "PARTICLE"
    subkeyword = "BLAST"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "lagsid",
                        int,
                        0,
                        10,
                        kwargs.get("lagsid", 0)
                    ),
                    Field(
                        "lagstype",
                        int,
                        10,
                        10,
                        kwargs.get("lagstype", 0)
                    ),
                    Field(
                        "dodid",
                        int,
                        20,
                        10,
                        kwargs.get("dodid", 0)
                    ),
                    Field(
                        "dodtype",
                        int,
                        30,
                        10,
                        kwargs.get("dodtype", 0)
                    ),
                    Field(
                        "hecid",
                        int,
                        40,
                        10,
                        kwargs.get("hecid", 0)
                    ),
                    Field(
                        "hectype",
                        int,
                        50,
                        10,
                        kwargs.get("hectype", 0)
                    ),
                    Field(
                        "aircid",
                        int,
                        60,
                        10,
                        kwargs.get("aircid", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nphe",
                        int,
                        0,
                        10,
                        kwargs.get("nphe", 0)
                    ),
                    Field(
                        "npair",
                        int,
                        10,
                        10,
                        kwargs.get("npair", 0)
                    ),
                    Field(
                        "iunit",
                        int,
                        20,
                        10,
                        kwargs.get("iunit", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ihetype",
                        int,
                        0,
                        10,
                        kwargs.get("ihetype", 0)
                    ),
                    Field(
                        "densit",
                        float,
                        10,
                        10,
                        kwargs.get("densit", 0.0)
                    ),
                    Field(
                        "energy",
                        float,
                        20,
                        10,
                        kwargs.get("energy", 0.0)
                    ),
                    Field(
                        "gamma",
                        float,
                        30,
                        10,
                        kwargs.get("gamma", 0.0)
                    ),
                    Field(
                        "covol",
                        float,
                        40,
                        10,
                        kwargs.get("covol", 0.0)
                    ),
                    Field(
                        "deto_v",
                        float,
                        50,
                        10,
                        kwargs.get("deto_v", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "detx",
                        float,
                        0,
                        10,
                        kwargs.get("detx", 0.0)
                    ),
                    Field(
                        "dety",
                        float,
                        10,
                        10,
                        kwargs.get("dety", 0.0)
                    ),
                    Field(
                        "detz",
                        float,
                        20,
                        10,
                        kwargs.get("detz", 0.0)
                    ),
                    Field(
                        "tdet",
                        float,
                        30,
                        10,
                        kwargs.get("tdet", 0.0)
                    ),
                    Field(
                        "btend",
                        float,
                        40,
                        10,
                        kwargs.get("btend", 0.0)
                    ),
                    Field(
                        "nid",
                        int,
                        50,
                        10,
                        kwargs.get("nid", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "bcxo",
                        float,
                        0,
                        10,
                        kwargs.get("bcxo", 0.0)
                    ),
                    Field(
                        "bcx1",
                        float,
                        10,
                        10,
                        kwargs.get("bcx1", 0.0)
                    ),
                    Field(
                        "bcy0",
                        float,
                        20,
                        10,
                        kwargs.get("bcy0", 0.0)
                    ),
                    Field(
                        "bcy1",
                        float,
                        30,
                        10,
                        kwargs.get("bcy1", 0.0)
                    ),
                    Field(
                        "bcz0",
                        float,
                        40,
                        10,
                        kwargs.get("bcz0", 0.0)
                    ),
                    Field(
                        "bcz1",
                        float,
                        50,
                        10,
                        kwargs.get("bcz1", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ibcx0",
                        int,
                        0,
                        10,
                        kwargs.get("ibcx0", 0)
                    ),
                    Field(
                        "ibcx1",
                        int,
                        10,
                        10,
                        kwargs.get("ibcx1", 0)
                    ),
                    Field(
                        "ibcy0",
                        int,
                        20,
                        10,
                        kwargs.get("ibcy0", 0)
                    ),
                    Field(
                        "ibcy1",
                        int,
                        30,
                        10,
                        kwargs.get("ibcy1", 0)
                    ),
                    Field(
                        "ibcz0",
                        int,
                        40,
                        10,
                        kwargs.get("ibcz0", 0)
                    ),
                    Field(
                        "ibcz1",
                        int,
                        50,
                        10,
                        kwargs.get("ibcz1", 0)
                    ),
                    Field(
                        "bc_p",
                        int,
                        60,
                        10,
                        kwargs.get("bc_p", 0)
                    ),
                ],
            ),
        ]

    @property
    def lagsid(self) -> int:
        """Get or set the Structure id for particle structure interaction
        """ # nopep8
        return self._cards[0].get_value("lagsid")

    @lagsid.setter
    def lagsid(self, value: int) -> None:
        self._cards[0].set_value("lagsid", value)

    @property
    def lagstype(self) -> int:
        """Get or set the Structure type
        EQ.0: Part Set
        EQ.1: Part
        """ # nopep8
        return self._cards[0].get_value("lagstype")

    @lagstype.setter
    def lagstype(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""lagstype must be one of {0,1}""")
        self._cards[0].set_value("lagstype", value)

    @property
    def dodid(self) -> int:
        """Get or set the Discrete element sphere (DES) or Smooth particle hydrodynamics (SPH) id for the interaction between particles and nodes.
        """ # nopep8
        return self._cards[0].get_value("dodid")

    @dodid.setter
    def dodid(self, value: int) -> None:
        self._cards[0].set_value("dodid", value)

    @property
    def dodtype(self) -> int:
        """Get or set the Nodal type
        EQ.0: Node Set
        EQ.1: Node
        """ # nopep8
        return self._cards[0].get_value("dodtype")

    @dodtype.setter
    def dodtype(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""dodtype must be one of {0,1}""")
        self._cards[0].set_value("dodtype", value)

    @property
    def hecid(self) -> int:
        """Get or set the Initial container for high explosive particle
        """ # nopep8
        return self._cards[0].get_value("hecid")

    @hecid.setter
    def hecid(self, value: int) -> None:
        self._cards[0].set_value("hecid", value)

    @property
    def hectype(self) -> int:
        """Get or set the Structure type
        EQ.0: Part Set
        EQ.1: Part
        EQ.2: Geometry, see *DEFINE_PBLAST_GEOMETRY
        """ # nopep8
        return self._cards[0].get_value("hectype")

    @hectype.setter
    def hectype(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""hectype must be one of {0,1,2}""")
        self._cards[0].set_value("hectype", value)

    @property
    def aircid(self) -> int:
        """Get or set the Initial geometry for air particles
        EQ.0: filled air particles to entire domain defined by Card 5
        GT.0: Reference to *DEFINE_PBLAST_AIRGEO ID
        """ # nopep8
        return self._cards[0].get_value("aircid")

    @aircid.setter
    def aircid(self, value: int) -> None:
        self._cards[0].set_value("aircid", value)

    @property
    def nphe(self) -> int:
        """Get or set the Number of high explosive particles
        """ # nopep8
        return self._cards[1].get_value("nphe")

    @nphe.setter
    def nphe(self, value: int) -> None:
        self._cards[1].set_value("nphe", value)

    @property
    def npair(self) -> int:
        """Get or set the Number of air particles
        """ # nopep8
        return self._cards[1].get_value("npair")

    @npair.setter
    def npair(self, value: int) -> None:
        self._cards[1].set_value("npair", value)

    @property
    def iunit(self) -> int:
        """Get or set the Unit System
        EQ.0: Kg-mm-ms-K
        EQ.1: SI Units
        EQ.2: Ton-mm-s-K
        EQ.3: g-cm-us-K
        """ # nopep8
        return self._cards[1].get_value("iunit")

    @iunit.setter
    def iunit(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""iunit must be one of {0,1,2,3}""")
        self._cards[1].set_value("iunit", value)

    @property
    def ihetype(self) -> int:
        """Get or set the High Explosive type
        EQ.1: TNT
        EQ.2: C4
        Others: Self Define
        """ # nopep8
        return self._cards[2].get_value("ihetype")

    @ihetype.setter
    def ihetype(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""ihetype must be one of {0,1,2}""")
        self._cards[2].set_value("ihetype", value)

    @property
    def densit(self) -> float:
        """Get or set the High Explosive density
        """ # nopep8
        return self._cards[2].get_value("densit")

    @densit.setter
    def densit(self, value: float) -> None:
        self._cards[2].set_value("densit", value)

    @property
    def energy(self) -> float:
        """Get or set the High Explosive energy per unit volume
        """ # nopep8
        return self._cards[2].get_value("energy")

    @energy.setter
    def energy(self, value: float) -> None:
        self._cards[2].set_value("energy", value)

    @property
    def gamma(self) -> float:
        """Get or set the High Explosive fraction between Cp and Cv
        """ # nopep8
        return self._cards[2].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        self._cards[2].set_value("gamma", value)

    @property
    def covol(self) -> float:
        """Get or set the High Explosive co-volume
        """ # nopep8
        return self._cards[2].get_value("covol")

    @covol.setter
    def covol(self, value: float) -> None:
        self._cards[2].set_value("covol", value)

    @property
    def deto_v(self) -> float:
        """Get or set the High Explosive detonation velocity
        """ # nopep8
        return self._cards[2].get_value("deto_v")

    @deto_v.setter
    def deto_v(self, value: float) -> None:
        self._cards[2].set_value("deto_v", value)

    @property
    def detx(self) -> float:
        """Get or set the Detonation point x
        """ # nopep8
        return self._cards[3].get_value("detx")

    @detx.setter
    def detx(self, value: float) -> None:
        self._cards[3].set_value("detx", value)

    @property
    def dety(self) -> float:
        """Get or set the Detonation point y
        """ # nopep8
        return self._cards[3].get_value("dety")

    @dety.setter
    def dety(self, value: float) -> None:
        self._cards[3].set_value("dety", value)

    @property
    def detz(self) -> float:
        """Get or set the Detonation point z
        """ # nopep8
        return self._cards[3].get_value("detz")

    @detz.setter
    def detz(self, value: float) -> None:
        self._cards[3].set_value("detz", value)

    @property
    def tdet(self) -> float:
        """Get or set the Detonation time
        """ # nopep8
        return self._cards[3].get_value("tdet")

    @tdet.setter
    def tdet(self, value: float) -> None:
        self._cards[3].set_value("tdet", value)

    @property
    def btend(self) -> float:
        """Get or set the Blast end time
        """ # nopep8
        return self._cards[3].get_value("btend")

    @btend.setter
    def btend(self, value: float) -> None:
        self._cards[3].set_value("btend", value)

    @property
    def nid(self) -> int:
        """Get or set the An optional node ID defining the position of the detonation point. If defined, its coordinates will overwrite the DETX, DETY, and DETZ defined above.
        """ # nopep8
        return self._cards[3].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[3].set_value("nid", value)

    @property
    def bcxo(self) -> float:
        """Get or set the Global domain x-min
        """ # nopep8
        return self._cards[4].get_value("bcxo")

    @bcxo.setter
    def bcxo(self, value: float) -> None:
        self._cards[4].set_value("bcxo", value)

    @property
    def bcx1(self) -> float:
        """Get or set the Global domain x-max
        """ # nopep8
        return self._cards[4].get_value("bcx1")

    @bcx1.setter
    def bcx1(self, value: float) -> None:
        self._cards[4].set_value("bcx1", value)

    @property
    def bcy0(self) -> float:
        """Get or set the Global domain y-min
        """ # nopep8
        return self._cards[4].get_value("bcy0")

    @bcy0.setter
    def bcy0(self, value: float) -> None:
        self._cards[4].set_value("bcy0", value)

    @property
    def bcy1(self) -> float:
        """Get or set the Global domain x-max
        """ # nopep8
        return self._cards[4].get_value("bcy1")

    @bcy1.setter
    def bcy1(self, value: float) -> None:
        self._cards[4].set_value("bcy1", value)

    @property
    def bcz0(self) -> float:
        """Get or set the Global domain z-min
        """ # nopep8
        return self._cards[4].get_value("bcz0")

    @bcz0.setter
    def bcz0(self, value: float) -> None:
        self._cards[4].set_value("bcz0", value)

    @property
    def bcz1(self) -> float:
        """Get or set the Global domain x-max
        """ # nopep8
        return self._cards[4].get_value("bcz1")

    @bcz1.setter
    def bcz1(self, value: float) -> None:
        self._cards[4].set_value("bcz1", value)

    @property
    def ibcx0(self) -> int:
        """Get or set the Boundary conditions for global domain x-min
        EQ.0: Free
        EQ.1: Rigid reflecting boundary
        """ # nopep8
        return self._cards[5].get_value("ibcx0")

    @ibcx0.setter
    def ibcx0(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ibcx0 must be one of {0,1}""")
        self._cards[5].set_value("ibcx0", value)

    @property
    def ibcx1(self) -> int:
        """Get or set the Boundary conditions for global domain x-max
        EQ.0: Free
        EQ.1: Rigid reflecting boundary
        """ # nopep8
        return self._cards[5].get_value("ibcx1")

    @ibcx1.setter
    def ibcx1(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ibcx1 must be one of {0,1}""")
        self._cards[5].set_value("ibcx1", value)

    @property
    def ibcy0(self) -> int:
        """Get or set the Boundary conditions for global domain y-min
        EQ.0: Free
        EQ.1: Rigid reflecting boundary
        """ # nopep8
        return self._cards[5].get_value("ibcy0")

    @ibcy0.setter
    def ibcy0(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ibcy0 must be one of {0,1}""")
        self._cards[5].set_value("ibcy0", value)

    @property
    def ibcy1(self) -> int:
        """Get or set the Boundary conditions for global domain y-max
        EQ.0: Free
        EQ.1: Rigid reflecting boundary
        """ # nopep8
        return self._cards[5].get_value("ibcy1")

    @ibcy1.setter
    def ibcy1(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ibcy1 must be one of {0,1}""")
        self._cards[5].set_value("ibcy1", value)

    @property
    def ibcz0(self) -> int:
        """Get or set the Boundary conditions for global domain z-min
        EQ.0: Free
        EQ.1: Rigid reflecting boundary
        """ # nopep8
        return self._cards[5].get_value("ibcz0")

    @ibcz0.setter
    def ibcz0(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ibcz0 must be one of {0,1}""")
        self._cards[5].set_value("ibcz0", value)

    @property
    def ibcz1(self) -> int:
        """Get or set the Boundary conditions for global domain z-max
        EQ.0: Free
        EQ.1: Rigid reflecting boundary
        """ # nopep8
        return self._cards[5].get_value("ibcz1")

    @ibcz1.setter
    def ibcz1(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ibcz1 must be one of {0,1}""")
        self._cards[5].set_value("ibcz1", value)

    @property
    def bc_p(self) -> int:
        """Get or set the Pressure ambient boundary condition for global domain
        EQ.0: Off (Default)
        EQ.1: On
        """ # nopep8
        return self._cards[5].get_value("bc_p")

    @bc_p.setter
    def bc_p(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""bc_p must be one of {0,1}""")
        self._cards[5].set_value("bc_p", value)

