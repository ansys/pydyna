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

class LoadBlast(KeywordBase):
    """DYNA LOAD_BLAST keyword"""

    keyword = "LOAD"
    subkeyword = "BLAST"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "wgt",
                        float,
                        0,
                        10,
                        kwargs.get("wgt")
                    ),
                    Field(
                        "xbo",
                        float,
                        10,
                        10,
                        kwargs.get("xbo", 0.0)
                    ),
                    Field(
                        "ybo",
                        float,
                        20,
                        10,
                        kwargs.get("ybo", 0.0)
                    ),
                    Field(
                        "zbo",
                        float,
                        30,
                        10,
                        kwargs.get("zbo", 0.0)
                    ),
                    Field(
                        "tbo",
                        float,
                        40,
                        10,
                        kwargs.get("tbo", 0.0)
                    ),
                    Field(
                        "iunit",
                        int,
                        50,
                        10,
                        kwargs.get("iunit", 2)
                    ),
                    Field(
                        "isurf",
                        int,
                        60,
                        10,
                        kwargs.get("isurf", 2)
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
                        "death",
                        float,
                        40,
                        10,
                        kwargs.get("death", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def wgt(self) -> typing.Optional[float]:
        """Get or set the Equivalent mass of TNT.
        """ # nopep8
        return self._cards[0].get_value("wgt")

    @wgt.setter
    def wgt(self, value: float) -> None:
        self._cards[0].set_value("wgt", value)

    @property
    def xbo(self) -> float:
        """Get or set the x-coordinate of point of explosion.
        """ # nopep8
        return self._cards[0].get_value("xbo")

    @xbo.setter
    def xbo(self, value: float) -> None:
        self._cards[0].set_value("xbo", value)

    @property
    def ybo(self) -> float:
        """Get or set the y-coordinate of point of explosion.
        """ # nopep8
        return self._cards[0].get_value("ybo")

    @ybo.setter
    def ybo(self, value: float) -> None:
        self._cards[0].set_value("ybo", value)

    @property
    def zbo(self) -> float:
        """Get or set the z-coordinate of point of explosion.
        """ # nopep8
        return self._cards[0].get_value("zbo")

    @zbo.setter
    def zbo(self, value: float) -> None:
        self._cards[0].set_value("zbo", value)

    @property
    def tbo(self) -> float:
        """Get or set the Time-zero of explosion.
        """ # nopep8
        return self._cards[0].get_value("tbo")

    @tbo.setter
    def tbo(self, value: float) -> None:
        self._cards[0].set_value("tbo", value)

    @property
    def iunit(self) -> int:
        """Get or set the Unit conversion flag:
        EQ.1: feet, pounds, seconds, psi,
        EQ.2: meters, kilograms, seconds, Pascals (default),
        EQ.3: inch, dozens of slugs, seconds, psi,
        EQ.4: centimeters, grams, microseconds, Megabars,
        EQ.5: user conversions will be supplied (see Card 2).
        """ # nopep8
        return self._cards[0].get_value("iunit")

    @iunit.setter
    def iunit(self, value: int) -> None:
        if value not in [2, 1, 3, 4, 5]:
            raise Exception("""iunit must be one of {2,1,3,4,5}""")
        self._cards[0].set_value("iunit", value)

    @property
    def isurf(self) -> int:
        """Get or set the Type of burst:,
        EQ.1: surface burst - hemispherical charge situated on the surface,
        EQ.2: air burst - spherical charge at least one charge diameter away from the surface (default).
        """ # nopep8
        return self._cards[0].get_value("isurf")

    @isurf.setter
    def isurf(self, value: int) -> None:
        if value not in [2, 1]:
            raise Exception("""isurf must be one of {2,1}""")
        self._cards[0].set_value("isurf", value)

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
    def death(self) -> float:
        """Get or set the Death time. Blast pressures are deactivated at this time.
        """ # nopep8
        return self._cards[1].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        self._cards[1].set_value("death", value)

