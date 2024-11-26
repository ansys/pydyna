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

class LoadBlastEnhanced(KeywordBase):
    """DYNA LOAD_BLAST_ENHANCED keyword"""

    keyword = "LOAD"
    subkeyword = "BLAST_ENHANCED"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "bid",
                        int,
                        0,
                        10,
                        kwargs.get("bid")
                    ),
                    Field(
                        "m",
                        float,
                        10,
                        10,
                        kwargs.get("m", 0.0)
                    ),
                    Field(
                        "xbo",
                        float,
                        20,
                        10,
                        kwargs.get("xbo", 0.0)
                    ),
                    Field(
                        "ybo",
                        float,
                        30,
                        10,
                        kwargs.get("ybo", 0.0)
                    ),
                    Field(
                        "zbo",
                        float,
                        40,
                        10,
                        kwargs.get("zbo", 0.0)
                    ),
                    Field(
                        "tbo",
                        float,
                        50,
                        10,
                        kwargs.get("tbo", 0.0)
                    ),
                    Field(
                        "unit",
                        int,
                        60,
                        10,
                        kwargs.get("unit", 2)
                    ),
                    Field(
                        "blast",
                        int,
                        70,
                        10,
                        kwargs.get("blast", 2)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cfm",
                        float,
                        0,
                        10,
                        kwargs.get("cfm", 0.0)
                    ),
                    Field(
                        "cfl",
                        float,
                        10,
                        10,
                        kwargs.get("cfl", 0.0)
                    ),
                    Field(
                        "cft",
                        float,
                        20,
                        10,
                        kwargs.get("cft", 0.0)
                    ),
                    Field(
                        "cfp",
                        float,
                        30,
                        10,
                        kwargs.get("cfp", 0.0)
                    ),
                    Field(
                        "nidbo",
                        int,
                        40,
                        10,
                        kwargs.get("nidbo")
                    ),
                    Field(
                        "death",
                        float,
                        50,
                        10,
                        kwargs.get("death", 1.e+20)
                    ),
                    Field(
                        "negphs",
                        int,
                        60,
                        10,
                        kwargs.get("negphs", 0)
                    ),
                ],
            ),
        ]

    @property
    def bid(self) -> typing.Optional[int]:
        """Get or set the Blast ID.  A unique number must be defined for each blast source (charge).  Multiple charges may be defined, however, interaction of the waves in air is not considered.
        """ # nopep8
        return self._cards[0].get_value("bid")

    @bid.setter
    def bid(self, value: int) -> None:
        self._cards[0].set_value("bid", value)

    @property
    def m(self) -> float:
        """Get or set the Equivalent mass of TNT.
        """ # nopep8
        return self._cards[0].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        self._cards[0].set_value("m", value)

    @property
    def xbo(self) -> float:
        """Get or set the x-coordinate of charge center.
        """ # nopep8
        return self._cards[0].get_value("xbo")

    @xbo.setter
    def xbo(self, value: float) -> None:
        self._cards[0].set_value("xbo", value)

    @property
    def ybo(self) -> float:
        """Get or set the y-coordinate of charge center.
        """ # nopep8
        return self._cards[0].get_value("ybo")

    @ybo.setter
    def ybo(self, value: float) -> None:
        self._cards[0].set_value("ybo", value)

    @property
    def zbo(self) -> float:
        """Get or set the z-coordinate of charge center.
        """ # nopep8
        return self._cards[0].get_value("zbo")

    @zbo.setter
    def zbo(self, value: float) -> None:
        self._cards[0].set_value("zbo", value)

    @property
    def tbo(self) -> float:
        """Get or set the Time of detonation.
        """ # nopep8
        return self._cards[0].get_value("tbo")

    @tbo.setter
    def tbo(self, value: float) -> None:
        self._cards[0].set_value("tbo", value)

    @property
    def unit(self) -> int:
        """Get or set the Unit conversion flag.    EQ.1:  feet, pound-mass, seconds, psi    EQ.2:  meters, kilograms, seconds, Pascals (default)    EQ.3:  inch, dozens of slugs, seconds, psi    EQ.4:  centimeters, grams, microseconds, Megabars    EQ.5:  user conversions will be supplied(see Card 2).    EQ.6: kilogram, millimeter, millisecond, GPa    EQ.7: metric ton, millimeter, second, Mpa    EQ.8: gram, millimeter, millisecond, MPa
        """ # nopep8
        return self._cards[0].get_value("unit")

    @unit.setter
    def unit(self, value: int) -> None:
        if value not in [2, 1, 3, 4, 5, 6, 7, 8]:
            raise Exception("""unit must be one of {2,1,3,4,5,6,7,8}""")
        self._cards[0].set_value("unit", value)

    @property
    def blast(self) -> int:
        """Get or set the Type of blast source    EQ.1:  hemispherical surface burst - charge is located on or very near the ground surface, initial shock wave is reflected and reinforced by the ground    EQ.2:  spherical free-air burst (default) - no amplification of the initial shock wave due to interaction with the ground surface    EQ.3:  air burst - moving non-sperhical warhead    EQ.4:  air burst with ground reflection - initial shock wave impinges on the ground surface and is reinforced by the reflected wave to produce a Mach stem.
        """ # nopep8
        return self._cards[0].get_value("blast")

    @blast.setter
    def blast(self, value: int) -> None:
        if value not in [2, 1, 3, 4]:
            raise Exception("""blast must be one of {2,1,3,4}""")
        self._cards[0].set_value("blast", value)

    @property
    def cfm(self) -> float:
        """Get or set the Conversion factor - pounds per LS-DYNA mass unit.
        """ # nopep8
        return self._cards[1].get_value("cfm")

    @cfm.setter
    def cfm(self, value: float) -> None:
        self._cards[1].set_value("cfm", value)

    @property
    def cfl(self) -> float:
        """Get or set the Conversion factor - feet per LS-DYNA length units.
        """ # nopep8
        return self._cards[1].get_value("cfl")

    @cfl.setter
    def cfl(self, value: float) -> None:
        self._cards[1].set_value("cfl", value)

    @property
    def cft(self) -> float:
        """Get or set the Conversion factor - milliseconds per LS-DYNA time unit.
        """ # nopep8
        return self._cards[1].get_value("cft")

    @cft.setter
    def cft(self, value: float) -> None:
        self._cards[1].set_value("cft", value)

    @property
    def cfp(self) -> float:
        """Get or set the Conversion factor - psi per LS-DYNA pressure unit.
        """ # nopep8
        return self._cards[1].get_value("cfp")

    @cfp.setter
    def cfp(self, value: float) -> None:
        self._cards[1].set_value("cfp", value)

    @property
    def nidbo(self) -> typing.Optional[int]:
        """Get or set the Optional node ID representing the charge center.  If defined then XBO, YBO and XBO are ignored.
        """ # nopep8
        return self._cards[1].get_value("nidbo")

    @nidbo.setter
    def nidbo(self, value: int) -> None:
        self._cards[1].set_value("nidbo", value)

    @property
    def death(self) -> float:
        """Get or set the Death time.  Blast pressures are deactivated at this time.
        """ # nopep8
        return self._cards[1].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        self._cards[1].set_value("death", value)

    @property
    def negphs(self) -> int:
        """Get or set the Treament of negative phase.
        EQ.0:  negative dictated by the Friedlander equation.
        EQ.1:  negative phase ignored as in ConWep.
        """ # nopep8
        return self._cards[1].get_value("negphs")

    @negphs.setter
    def negphs(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""negphs must be one of {0,1}""")
        self._cards[1].set_value("negphs", value)

