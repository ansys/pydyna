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

class ControlFormingUser(KeywordBase):
    """DYNA CONTROL_FORMING_USER keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_USER"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "blank",
                        int,
                        0,
                        10,
                        kwargs.get("blank")
                    ),
                    Field(
                        "type",
                        int,
                        10,
                        10,
                        kwargs.get("type", 0)
                    ),
                    Field(
                        "thick",
                        float,
                        20,
                        10,
                        kwargs.get("thick")
                    ),
                    Field(
                        "r00",
                        float,
                        30,
                        10,
                        kwargs.get("r00", 1.0)
                    ),
                    Field(
                        "r45",
                        float,
                        40,
                        10,
                        kwargs.get("r45", 1.0)
                    ),
                    Field(
                        "r90",
                        float,
                        50,
                        10,
                        kwargs.get("r90", 1.0)
                    ),
                    Field(
                        "al/fe",
                        str,
                        60,
                        10,
                        kwargs.get("al/fe", "F")
                    ),
                    Field(
                        "unit",
                        int,
                        70,
                        10,
                        kwargs.get("unit", 1)
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
                        "k",
                        float,
                        10,
                        10,
                        kwargs.get("k", 2)
                    ),
                    Field(
                        "n",
                        float,
                        20,
                        10,
                        kwargs.get("n")
                    ),
                    Field(
                        "e",
                        float,
                        30,
                        10,
                        kwargs.get("e")
                    ),
                    Field(
                        "density",
                        float,
                        40,
                        10,
                        kwargs.get("density")
                    ),
                    Field(
                        "pr",
                        float,
                        50,
                        10,
                        kwargs.get("pr")
                    ),
                    Field(
                        "fs",
                        float,
                        60,
                        10,
                        kwargs.get("fs", 0.1)
                    ),
                    Field(
                        "mtype",
                        int,
                        70,
                        10,
                        kwargs.get("mtype", 37)
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
                        kwargs.get("vmax", 1000.0)
                    ),
                    Field(
                        "amax",
                        float,
                        20,
                        10,
                        kwargs.get("amax", 500000)
                    ),
                    Field(
                        "lvlada",
                        int,
                        30,
                        10,
                        kwargs.get("lvlada", 0)
                    ),
                    Field(
                        "sizeada",
                        float,
                        40,
                        10,
                        kwargs.get("sizeada", 0)
                    ),
                    Field(
                        "adatims",
                        int,
                        50,
                        10,
                        kwargs.get("adatims", 0)
                    ),
                    Field(
                        "d3plot",
                        int,
                        60,
                        10,
                        kwargs.get("d3plot", 10)
                    ),
                    Field(
                        "gap",
                        float,
                        70,
                        10,
                        kwargs.get("gap", 1.1)
                    ),
                ],
            ),
        ]

    @property
    def blank(self) -> typing.Optional[int]:
        """Get or set the Blank ID
        """ # nopep8
        return self._cards[0].get_value("blank")

    @blank.setter
    def blank(self, value: int) -> None:
        self._cards[0].set_value("blank", value)

    @property
    def type(self) -> int:
        """Get or set the 0: Part ID (blank)
        1:   PART SET Ids (blank).
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""type must be one of {0,1}""")
        self._cards[0].set_value("type", value)

    @property
    def thick(self) -> typing.Optional[float]:
        """Get or set the Blank thickness. If the blank thickness is already defined, this parameter is ignored
        """ # nopep8
        return self._cards[0].get_value("thick")

    @thick.setter
    def thick(self, value: float) -> None:
        self._cards[0].set_value("thick", value)

    @property
    def r00(self) -> float:
        """Get or set the Material anisotropic parameters
        """ # nopep8
        return self._cards[0].get_value("r00")

    @r00.setter
    def r00(self, value: float) -> None:
        self._cards[0].set_value("r00", value)

    @property
    def r45(self) -> float:
        """Get or set the Material anisotropic parameters
        """ # nopep8
        return self._cards[0].get_value("r45")

    @r45.setter
    def r45(self, value: float) -> None:
        self._cards[0].set_value("r45", value)

    @property
    def r90(self) -> float:
        """Get or set the Material anisotropic parameters
        """ # nopep8
        return self._cards[0].get_value("r90")

    @r90.setter
    def r90(self, value: float) -> None:
        self._cards[0].set_value("r90", value)

    @property
    def al_fe(self) -> str:
        """Get or set the This parameter is used to define blank Young's Modulus and density. If this parameter is defined, E and Density will be found by using the proper unit, which is specified below.
        EQ. A:  the blank is aluminum
        EQ. F:   the blank is steel (default)
        """ # nopep8
        return self._cards[0].get_value("al/fe")

    @al_fe.setter
    def al_fe(self, value: str) -> None:
        if value not in ["F", "A"]:
            raise Exception("""al_fe must be one of {"F","A"}""")
        self._cards[0].set_value("al/fe", value)

    @property
    def unit(self) -> int:
        """Get or set the Unit adopted in this simulation. This unit is used to obtain proper punch velocity, acceleration, time step, and material properties
        """ # nopep8
        return self._cards[0].get_value("unit")

    @unit.setter
    def unit(self, value: int) -> None:
        self._cards[0].set_value("unit", value)

    @property
    def lcss(self) -> typing.Optional[int]:
        """Get or set the If the material for the blank has not been defined, this curve will be used to define the stress-strain relation. Otherwise, this curve is ignored.
        """ # nopep8
        return self._cards[1].get_value("lcss")

    @lcss.setter
    def lcss(self, value: int) -> None:
        self._cards[1].set_value("lcss", value)

    @property
    def k(self) -> float:
        """Get or set the strength coefficient for exponential hardening. If LCSS is defined, or blank materials has already been defined, this parameter is ignored
        """ # nopep8
        return self._cards[1].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        self._cards[1].set_value("k", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the exponent for exponential hardening. If LCSS is defined, or blank materials has already been defined, this parameter is ignored
        """ # nopep8
        return self._cards[1].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        self._cards[1].set_value("n", value)

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
    def pr(self) -> typing.Optional[float]:
        """Get or set the Possion Ratio
        """ # nopep8
        return self._cards[1].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[1].set_value("pr", value)

    @property
    def fs(self) -> float:
        """Get or set the Friction coefficient. If contact is defined, this parameter is ignored
        """ # nopep8
        return self._cards[1].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        self._cards[1].set_value("fs", value)

    @property
    def mtype(self) -> int:
        """Get or set the Material model (Only M37 is supported)
        """ # nopep8
        return self._cards[1].get_value("mtype")

    @mtype.setter
    def mtype(self, value: int) -> None:
        self._cards[1].set_value("mtype", value)

    @property
    def patern(self) -> int:
        """Get or set the Velocity profile.
        EQ.: 1.Ramped velocity profile
        EQ.:2. Smooth velocity curve
        """ # nopep8
        return self._cards[2].get_value("patern")

    @patern.setter
    def patern(self, value: int) -> None:
        self._cards[2].set_value("patern", value)

    @property
    def vmax(self) -> float:
        """Get or set the The maximum allowable tool velocity.
        """ # nopep8
        return self._cards[2].get_value("vmax")

    @vmax.setter
    def vmax(self, value: float) -> None:
        self._cards[2].set_value("vmax", value)

    @property
    def amax(self) -> float:
        """Get or set the The maximum allowable acceleration
        """ # nopep8
        return self._cards[2].get_value("amax")

    @amax.setter
    def amax(self, value: float) -> None:
        self._cards[2].set_value("amax", value)

    @property
    def lvlada(self) -> int:
        """Get or set the Level of adaptivity
        """ # nopep8
        return self._cards[2].get_value("lvlada")

    @lvlada.setter
    def lvlada(self, value: int) -> None:
        self._cards[2].set_value("lvlada", value)

    @property
    def sizeada(self) -> float:
        """Get or set the Minimize for adaptivity
        """ # nopep8
        return self._cards[2].get_value("sizeada")

    @sizeada.setter
    def sizeada(self, value: float) -> None:
        self._cards[2].set_value("sizeada", value)

    @property
    def adatims(self) -> int:
        """Get or set the Total number of adaptivity cycles in this process
        """ # nopep8
        return self._cards[2].get_value("adatims")

    @adatims.setter
    def adatims(self, value: int) -> None:
        self._cards[2].set_value("adatims", value)

    @property
    def d3plot(self) -> int:
        """Get or set the Number of state output for d3plot file
        """ # nopep8
        return self._cards[2].get_value("d3plot")

    @d3plot.setter
    def d3plot(self, value: int) -> None:
        self._cards[2].set_value("d3plot", value)

    @property
    def gap(self) -> float:
        """Get or set the minimum gap between tools
        """ # nopep8
        return self._cards[2].get_value("gap")

    @gap.setter
    def gap(self, value: float) -> None:
        self._cards[2].set_value("gap", value)

