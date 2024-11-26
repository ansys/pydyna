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
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class MatFhwaSoil(KeywordBase):
    """DYNA MAT_FHWA_SOIL keyword"""

    keyword = "MAT"
    subkeyword = "FHWA_SOIL"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "mid",
                        int,
                        0,
                        10,
                        kwargs.get("mid")
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        kwargs.get("ro")
                    ),
                    Field(
                        "nplot",
                        int,
                        20,
                        10,
                        kwargs.get("nplot", 1)
                    ),
                    Field(
                        "spgrav",
                        float,
                        30,
                        10,
                        kwargs.get("spgrav")
                    ),
                    Field(
                        "rhowat",
                        float,
                        40,
                        10,
                        kwargs.get("rhowat", 1.0)
                    ),
                    Field(
                        "vn",
                        float,
                        50,
                        10,
                        kwargs.get("vn")
                    ),
                    Field(
                        "gammar",
                        float,
                        60,
                        10,
                        kwargs.get("gammar")
                    ),
                    Field(
                        "intrmx",
                        int,
                        70,
                        10,
                        kwargs.get("intrmx", 1)
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
                        "g",
                        float,
                        10,
                        10,
                        kwargs.get("g")
                    ),
                    Field(
                        "phimax",
                        float,
                        20,
                        10,
                        kwargs.get("phimax")
                    ),
                    Field(
                        "ahyp",
                        float,
                        30,
                        10,
                        kwargs.get("ahyp")
                    ),
                    Field(
                        "coh",
                        float,
                        40,
                        10,
                        kwargs.get("coh")
                    ),
                    Field(
                        "eccen",
                        float,
                        50,
                        10,
                        kwargs.get("eccen")
                    ),
                    Field(
                        "an",
                        float,
                        60,
                        10,
                        kwargs.get("an")
                    ),
                    Field(
                        "et",
                        float,
                        70,
                        10,
                        kwargs.get("et")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "mcont",
                        float,
                        0,
                        10,
                        kwargs.get("mcont")
                    ),
                    Field(
                        "pwd1",
                        float,
                        10,
                        10,
                        kwargs.get("pwd1")
                    ),
                    Field(
                        "pwksk",
                        float,
                        20,
                        10,
                        kwargs.get("pwksk")
                    ),
                    Field(
                        "pwd2",
                        float,
                        30,
                        10,
                        kwargs.get("pwd2")
                    ),
                    Field(
                        "phires",
                        float,
                        40,
                        10,
                        kwargs.get("phires")
                    ),
                    Field(
                        "dint",
                        float,
                        50,
                        10,
                        kwargs.get("dint")
                    ),
                    Field(
                        "vdfm",
                        float,
                        60,
                        10,
                        kwargs.get("vdfm")
                    ),
                    Field(
                        "damlev",
                        float,
                        70,
                        10,
                        kwargs.get("damlev")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "epsmax",
                        float,
                        0,
                        10,
                        kwargs.get("epsmax")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatFhwaSoil.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique unmber has to be chosen.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def nplot(self) -> int:
        """Get or set the Plotting options
        EQ.1: Effective Strain.
        EQ.2: Damage Criterion Threshold.
        EQ.3: Damage (diso).
        EQ.4: Current Damage Criterion.
        EQ.5: Not used.
        EQ.6: Current Friction Angle (phi).
        """ # nopep8
        return self._cards[0].get_value("nplot")

    @nplot.setter
    def nplot(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5, 6]:
            raise Exception("""nplot must be one of {1,2,3,4,5,6}""")
        self._cards[0].set_value("nplot", value)

    @property
    def spgrav(self) -> typing.Optional[float]:
        """Get or set the Specific Gravity of Soil used to get porosity.
        """ # nopep8
        return self._cards[0].get_value("spgrav")

    @spgrav.setter
    def spgrav(self, value: float) -> None:
        self._cards[0].set_value("spgrav", value)

    @property
    def rhowat(self) -> float:
        """Get or set the Density of water in model units - used to determine air void strain (saturation).
        """ # nopep8
        return self._cards[0].get_value("rhowat")

    @rhowat.setter
    def rhowat(self, value: float) -> None:
        self._cards[0].set_value("rhowat", value)

    @property
    def vn(self) -> typing.Optional[float]:
        """Get or set the Viscoplasticity parameter (strain-rate enhanced strength).
        """ # nopep8
        return self._cards[0].get_value("vn")

    @vn.setter
    def vn(self, value: float) -> None:
        self._cards[0].set_value("vn", value)

    @property
    def gammar(self) -> typing.Optional[float]:
        """Get or set the Viscoplasticity parameter (strain-rate enhanced strength).
        """ # nopep8
        return self._cards[0].get_value("gammar")

    @gammar.setter
    def gammar(self, value: float) -> None:
        self._cards[0].set_value("gammar", value)

    @property
    def intrmx(self) -> int:
        """Get or set the Maximum number of plasticity iterations (default 1).
        """ # nopep8
        return self._cards[0].get_value("intrmx")

    @intrmx.setter
    def intrmx(self, value: int) -> None:
        self._cards[0].set_value("intrmx", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the Bulk Modulus (non-zero).
        """ # nopep8
        return self._cards[1].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        self._cards[1].set_value("k", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the Shear modulus (non-zero).
        """ # nopep8
        return self._cards[1].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        self._cards[1].set_value("g", value)

    @property
    def phimax(self) -> typing.Optional[float]:
        """Get or set the Peak Shear Strength Angle (friction angle) (radians).
        """ # nopep8
        return self._cards[1].get_value("phimax")

    @phimax.setter
    def phimax(self, value: float) -> None:
        self._cards[1].set_value("phimax", value)

    @property
    def ahyp(self) -> typing.Optional[float]:
        """Get or set the Coefficient A for modified Drucker-Prager Surface.
        """ # nopep8
        return self._cards[1].get_value("ahyp")

    @ahyp.setter
    def ahyp(self, value: float) -> None:
        self._cards[1].set_value("ahyp", value)

    @property
    def coh(self) -> typing.Optional[float]:
        """Get or set the Cohesion n Shear Strength at zero confinement (overburden).
        """ # nopep8
        return self._cards[1].get_value("coh")

    @coh.setter
    def coh(self, value: float) -> None:
        self._cards[1].set_value("coh", value)

    @property
    def eccen(self) -> typing.Optional[float]:
        """Get or set the Eccentricity parameter for third invariant effects.
        """ # nopep8
        return self._cards[1].get_value("eccen")

    @eccen.setter
    def eccen(self, value: float) -> None:
        self._cards[1].set_value("eccen", value)

    @property
    def an(self) -> typing.Optional[float]:
        """Get or set the Strain hardening percent of phimax where non-linear effects start.
        """ # nopep8
        return self._cards[1].get_value("an")

    @an.setter
    def an(self, value: float) -> None:
        self._cards[1].set_value("an", value)

    @property
    def et(self) -> typing.Optional[float]:
        """Get or set the Strain Hardening Amount of non-linear effects.
        """ # nopep8
        return self._cards[1].get_value("et")

    @et.setter
    def et(self, value: float) -> None:
        self._cards[1].set_value("et", value)

    @property
    def mcont(self) -> typing.Optional[float]:
        """Get or set the Moisture Content of Soil (Determines amount of air voids) (0-1.00)
        """ # nopep8
        return self._cards[2].get_value("mcont")

    @mcont.setter
    def mcont(self, value: float) -> None:
        self._cards[2].set_value("mcont", value)

    @property
    def pwd1(self) -> typing.Optional[float]:
        """Get or set the Parameter for pore water effects on bulk modulus.
        """ # nopep8
        return self._cards[2].get_value("pwd1")

    @pwd1.setter
    def pwd1(self, value: float) -> None:
        self._cards[2].set_value("pwd1", value)

    @property
    def pwksk(self) -> typing.Optional[float]:
        """Get or set the Skeleton bulk modulus- Pore water parameter n set to zero to eliminate effects.
        """ # nopep8
        return self._cards[2].get_value("pwksk")

    @pwksk.setter
    def pwksk(self, value: float) -> None:
        self._cards[2].set_value("pwksk", value)

    @property
    def pwd2(self) -> typing.Optional[float]:
        """Get or set the Parameter for pore waterr effects on the effective pressure (confinement).
        """ # nopep8
        return self._cards[2].get_value("pwd2")

    @pwd2.setter
    def pwd2(self, value: float) -> None:
        self._cards[2].set_value("pwd2", value)

    @property
    def phires(self) -> typing.Optional[float]:
        """Get or set the The minimum internal friction angle, radians (residual shear strength).
        """ # nopep8
        return self._cards[2].get_value("phires")

    @phires.setter
    def phires(self, value: float) -> None:
        self._cards[2].set_value("phires", value)

    @property
    def dint(self) -> typing.Optional[float]:
        """Get or set the Volumetric Strain at Initial damage threshold, EMBED Equation.3.
        """ # nopep8
        return self._cards[2].get_value("dint")

    @dint.setter
    def dint(self, value: float) -> None:
        self._cards[2].set_value("dint", value)

    @property
    def vdfm(self) -> typing.Optional[float]:
        """Get or set the Void formation energy (like fracture energy).
        """ # nopep8
        return self._cards[2].get_value("vdfm")

    @vdfm.setter
    def vdfm(self, value: float) -> None:
        self._cards[2].set_value("vdfm", value)

    @property
    def damlev(self) -> typing.Optional[float]:
        """Get or set the Level of damage that will cause element deletion (0.0-1.0).
        """ # nopep8
        return self._cards[2].get_value("damlev")

    @damlev.setter
    def damlev(self, value: float) -> None:
        self._cards[2].set_value("damlev", value)

    @property
    def epsmax(self) -> typing.Optional[float]:
        """Get or set the Maximum principle failure strain.
        """ # nopep8
        return self._cards[3].get_value("epsmax")

    @epsmax.setter
    def epsmax(self, value: float) -> None:
        self._cards[3].set_value("epsmax", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[4].cards[0].set_value("title", value)

