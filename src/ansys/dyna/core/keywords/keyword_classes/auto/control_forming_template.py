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

class ControlFormingTemplate(KeywordBase):
    """DYNA CONTROL_FORMING_TEMPLATE keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_TEMPLATE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "idtemp",
                        int,
                        0,
                        10,
                        kwargs.get("idtemp", 0)
                    ),
                    Field(
                        "blkid",
                        int,
                        10,
                        10,
                        kwargs.get("blkid", 0)
                    ),
                    Field(
                        "dieid",
                        int,
                        20,
                        10,
                        kwargs.get("dieid")
                    ),
                    Field(
                        "pnch",
                        int,
                        30,
                        10,
                        kwargs.get("pnch")
                    ),
                    Field(
                        "bndu",
                        int,
                        40,
                        10,
                        kwargs.get("bndu")
                    ),
                    Field(
                        "bndl",
                        int,
                        50,
                        10,
                        kwargs.get("bndl")
                    ),
                    Field(
                        "type",
                        int,
                        60,
                        10,
                        kwargs.get("type", 0)
                    ),
                    Field(
                        "prebd",
                        float,
                        70,
                        10,
                        kwargs.get("prebd")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcss",
                        int,
                        0,
                        10,
                        kwargs.get("lcss")
                    ),
                    Field(
                        "al/fe",
                        str,
                        10,
                        10,
                        kwargs.get("al/fe", "F")
                    ),
                    Field(
                        "r00",
                        float,
                        20,
                        10,
                        kwargs.get("r00", 1.0)
                    ),
                    Field(
                        "r45",
                        float,
                        30,
                        10,
                        kwargs.get("r45", 1.0)
                    ),
                    Field(
                        "r90",
                        float,
                        40,
                        10,
                        kwargs.get("r90", 1.0)
                    ),
                    Field(
                        "e",
                        float,
                        50,
                        10,
                        kwargs.get("e")
                    ),
                    Field(
                        "density",
                        float,
                        60,
                        10,
                        kwargs.get("density")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "k",
                        float,
                        0,
                        10,
                        kwargs.get("k")
                    ),
                    Field(
                        "n",
                        float,
                        10,
                        10,
                        kwargs.get("n")
                    ),
                    Field(
                        "mtype",
                        int,
                        20,
                        10,
                        kwargs.get("mtype", 37)
                    ),
                    Field(
                        "unit",
                        int,
                        30,
                        10,
                        kwargs.get("unit", 1)
                    ),
                    Field(
                        "thick",
                        float,
                        40,
                        10,
                        kwargs.get("thick")
                    ),
                    Field(
                        "gap",
                        float,
                        50,
                        10,
                        kwargs.get("gap", 1.1)
                    ),
                    Field(
                        "fs",
                        float,
                        60,
                        10,
                        kwargs.get("fs", 0.1)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "patern",
                        int,
                        0,
                        10,
                        kwargs.get("patern", 1)
                    ),
                    Field(
                        "vmax",
                        float,
                        10,
                        10,
                        kwargs.get("vmax", 1000)
                    ),
                    Field(
                        "vx",
                        float,
                        20,
                        10,
                        kwargs.get("vx", 0)
                    ),
                    Field(
                        "vy",
                        float,
                        30,
                        10,
                        kwargs.get("vy", 0)
                    ),
                    Field(
                        "vz",
                        float,
                        40,
                        10,
                        kwargs.get("vz", -1)
                    ),
                    Field(
                        "vid",
                        int,
                        50,
                        10,
                        kwargs.get("vid")
                    ),
                    Field(
                        "amax",
                        float,
                        60,
                        10,
                        kwargs.get("amax", 1.0E6)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lvlada",
                        int,
                        0,
                        10,
                        kwargs.get("lvlada")
                    ),
                    Field(
                        "sizeada",
                        float,
                        10,
                        10,
                        kwargs.get("sizeada")
                    ),
                    Field(
                        "timsada",
                        int,
                        20,
                        10,
                        kwargs.get("timsada", 20)
                    ),
                    Field(
                        "d3plt",
                        int,
                        30,
                        10,
                        kwargs.get("d3plt", 10)
                    ),
                ],
            ),
        ]

    @property
    def idtemp(self) -> int:
        """Get or set the Type of forming process (Detailed descriptions can be found in the Remark section)
        EQ. 1: 3-piece air-draw
        EQ. 2: 3-piece Toggle-draw
        EQ. 3: 4-piece draw
        EQ. 4: Springback
        EQ. 5: Trimming
        """ # nopep8
        return self._cards[0].get_value("idtemp")

    @idtemp.setter
    def idtemp(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5]:
            raise Exception("""idtemp must be one of {0,1,2,3,4,5}""")
        self._cards[0].set_value("idtemp", value)

    @property
    def blkid(self) -> int:
        """Get or set the Part/Part Set  Id for the blank. If Type equals to 0, this ID is part ID, otherwise, it is part set id.
        """ # nopep8
        return self._cards[0].get_value("blkid")

    @blkid.setter
    def blkid(self, value: int) -> None:
        self._cards[0].set_value("blkid", value)

    @property
    def dieid(self) -> typing.Optional[int]:
        """Get or set the Rigid Body 1 ID, See Figures 8.2a, 8.2b and 8.2c for more information
        """ # nopep8
        return self._cards[0].get_value("dieid")

    @dieid.setter
    def dieid(self, value: int) -> None:
        self._cards[0].set_value("dieid", value)

    @property
    def pnch(self) -> typing.Optional[int]:
        """Get or set the Rigid Body 2 ID, See Figures 8.2a, 8.2b and 8.2c for more information
        """ # nopep8
        return self._cards[0].get_value("pnch")

    @pnch.setter
    def pnch(self, value: int) -> None:
        self._cards[0].set_value("pnch", value)

    @property
    def bndu(self) -> typing.Optional[int]:
        """Get or set the Rigid Body 2 ID, See Figures 8.2a, 8.2b and 8.2c for more information
        """ # nopep8
        return self._cards[0].get_value("bndu")

    @bndu.setter
    def bndu(self, value: int) -> None:
        self._cards[0].set_value("bndu", value)

    @property
    def bndl(self) -> typing.Optional[int]:
        """Get or set the Rigid Body 3 ID, See Figures 8.2a, 8.2b and 8.2c for more information
        """ # nopep8
        return self._cards[0].get_value("bndl")

    @bndl.setter
    def bndl(self, value: int) -> None:
        self._cards[0].set_value("bndl", value)

    @property
    def type(self) -> int:
        """Get or set the 0:  REST1|REST4 are part IDs
        1:   REST1|REST4 are PART SET IDs
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""type must be one of {0,1}""")
        self._cards[0].set_value("type", value)

    @property
    def prebd(self) -> typing.Optional[float]:
        """Get or set the “Pull-over” distance, for 4 piece stretch draw only.  This is the travel distance of both upper and lower binder together after they are fully closed.  Typically, this distance is below 50 mm.  See Figure 0-3 for more information.
        """ # nopep8
        return self._cards[0].get_value("prebd")

    @prebd.setter
    def prebd(self, value: float) -> None:
        self._cards[0].set_value("prebd", value)

    @property
    def lcss(self) -> typing.Optional[int]:
        """Get or set the If the material for the blank has not been defined, this curve will be used to define the stress-strain relation. Otherwise, this curve is ignored
        """ # nopep8
        return self._cards[1].get_value("lcss")

    @lcss.setter
    def lcss(self, value: int) -> None:
        self._cards[1].set_value("lcss", value)

    @property
    def al_fe(self) -> str:
        """Get or set the This parameter is used to define blank Young's Modulus and density. If this parameter is defined, E and Density will be found by using the proper unit, which is specified below.
        EQ. A:  the blank is aluminum
        EQ. F:   the blank is steel (default)
        """ # nopep8
        return self._cards[1].get_value("al/fe")

    @al_fe.setter
    def al_fe(self, value: str) -> None:
        if value not in ["F", "A"]:
            raise Exception("""al_fe must be one of {"F","A"}""")
        self._cards[1].set_value("al/fe", value)

    @property
    def r00(self) -> float:
        """Get or set the anisotropic parameters
        """ # nopep8
        return self._cards[1].get_value("r00")

    @r00.setter
    def r00(self, value: float) -> None:
        self._cards[1].set_value("r00", value)

    @property
    def r45(self) -> float:
        """Get or set the anisotropic parameters
        """ # nopep8
        return self._cards[1].get_value("r45")

    @r45.setter
    def r45(self, value: float) -> None:
        self._cards[1].set_value("r45", value)

    @property
    def r90(self) -> float:
        """Get or set the anisotropic parameters
        """ # nopep8
        return self._cards[1].get_value("r90")

    @r90.setter
    def r90(self, value: float) -> None:
        self._cards[1].set_value("r90", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Material Young's Modulus. If AL/FE is defined, E is not necessary
        """ # nopep8
        return self._cards[1].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        self._cards[1].set_value("e", value)

    @property
    def density(self) -> typing.Optional[float]:
        """Get or set the Blank density. If AL/FE is defined, this parameter is not necessary
        """ # nopep8
        return self._cards[1].get_value("density")

    @density.setter
    def density(self, value: float) -> None:
        self._cards[1].set_value("density", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the strength coefficient for exponential hardening. If LCSS is defined, or blank materials has already been defined, this parameter is ignored
        """ # nopep8
        return self._cards[2].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        self._cards[2].set_value("k", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Exponent for exponential hardening. If LCSS is defined, or blank materials has already been defined, this parameter is ignored
        """ # nopep8
        return self._cards[2].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        self._cards[2].set_value("n", value)

    @property
    def mtype(self) -> int:
        """Get or set the Material model (Only M37 is supported)
        """ # nopep8
        return self._cards[2].get_value("mtype")

    @mtype.setter
    def mtype(self, value: int) -> None:
        self._cards[2].set_value("mtype", value)

    @property
    def unit(self) -> int:
        """Get or set the Unit adopted in this simulation. This unit is used to obtain proper punch velocity, acceleration, time step, and material properties
        """ # nopep8
        return self._cards[2].get_value("unit")

    @unit.setter
    def unit(self, value: int) -> None:
        self._cards[2].set_value("unit", value)

    @property
    def thick(self) -> typing.Optional[float]:
        """Get or set the Blank thickness. If the blank thickness is already defined, this parameter is ignored
        """ # nopep8
        return self._cards[2].get_value("thick")

    @thick.setter
    def thick(self, value: float) -> None:
        self._cards[2].set_value("thick", value)

    @property
    def gap(self) -> float:
        """Get or set the The home gap between rigid tools (see notes below). If *BOUNDARY_PRESCRIBED_RIGID_BODY is defined, this parameter is ignored
        """ # nopep8
        return self._cards[2].get_value("gap")

    @gap.setter
    def gap(self, value: float) -> None:
        self._cards[2].set_value("gap", value)

    @property
    def fs(self) -> float:
        """Get or set the Friction coefficient. If contact is defined, this parameter is ignored
        """ # nopep8
        return self._cards[2].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        self._cards[2].set_value("fs", value)

    @property
    def patern(self) -> int:
        """Get or set the Velocity profile. If rigid body is already defined, this parameter is ignored.
        1.  Ramped velocity profile
        2.  Smooth velocity curve
        """ # nopep8
        return self._cards[3].get_value("patern")

    @patern.setter
    def patern(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""patern must be one of {1,2}""")
        self._cards[3].set_value("patern", value)

    @property
    def vmax(self) -> float:
        """Get or set the vector components of the described punch moving direction. The default direction is defined by VID
        """ # nopep8
        return self._cards[3].get_value("vmax")

    @vmax.setter
    def vmax(self, value: float) -> None:
        self._cards[3].set_value("vmax", value)

    @property
    def vx(self) -> float:
        """Get or set the vector components of the described punch moving direction. The default direction is defined by VID
        """ # nopep8
        return self._cards[3].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        self._cards[3].set_value("vx", value)

    @property
    def vy(self) -> float:
        """Get or set the vector components of the described punch moving direction. The default direction is defined by VID
        """ # nopep8
        return self._cards[3].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        self._cards[3].set_value("vy", value)

    @property
    def vz(self) -> float:
        """Get or set the vector components of the described punch moving direction. The default direction is defined by VID
        """ # nopep8
        return self._cards[3].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        self._cards[3].set_value("vz", value)

    @property
    def vid(self) -> typing.Optional[int]:
        """Get or set the VID is the vector ID describing the punch moving direction.  The vector defined in VID overrides the vector defined in (VX,VY,VZ). If neither VID nor (VX,VY,VZ) is not defined, the punch is assumed to move in the negative z direction
        """ # nopep8
        return self._cards[3].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        self._cards[3].set_value("vid", value)

    @property
    def amax(self) -> float:
        """Get or set the The maximum allowable acceleration.
        """ # nopep8
        return self._cards[3].get_value("amax")

    @amax.setter
    def amax(self, value: float) -> None:
        self._cards[3].set_value("amax", value)

    @property
    def lvlada(self) -> typing.Optional[int]:
        """Get or set the Maximum levels of adaptivity for the blank
        """ # nopep8
        return self._cards[4].get_value("lvlada")

    @lvlada.setter
    def lvlada(self, value: int) -> None:
        self._cards[4].set_value("lvlada", value)

    @property
    def sizeada(self) -> typing.Optional[float]:
        """Get or set the Minimum element size for adaptivity
        """ # nopep8
        return self._cards[4].get_value("sizeada")

    @sizeada.setter
    def sizeada(self, value: float) -> None:
        self._cards[4].set_value("sizeada", value)

    @property
    def timsada(self) -> int:
        """Get or set the Total number of adaptivities in this forming simulation
        """ # nopep8
        return self._cards[4].get_value("timsada")

    @timsada.setter
    def timsada(self, value: int) -> None:
        self._cards[4].set_value("timsada", value)

    @property
    def d3plt(self) -> int:
        """Get or set the The total number of d3plot output
        """ # nopep8
        return self._cards[4].get_value("d3plt")

    @d3plt.setter
    def d3plt(self, value: int) -> None:
        self._cards[4].set_value("d3plt", value)

