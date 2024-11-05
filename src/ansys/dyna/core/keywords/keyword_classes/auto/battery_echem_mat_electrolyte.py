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

class BatteryEchemMatElectrolyte(KeywordBase):
    """DYNA BATTERY_ECHEM_MAT_ELECTROLYTE keyword"""

    keyword = "BATTERY"
    subkeyword = "ECHEM_MAT_ELECTROLYTE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "ielyte",
                        int,
                        10,
                        10,
                        kwargs.get("ielyte")
                    ),
                    Field(
                        "etype",
                        int,
                        20,
                        10,
                        kwargs.get("etype")
                    ),
                    Field(
                        "rhoe",
                        float,
                        30,
                        10,
                        kwargs.get("rhoe")
                    ),
                    Field(
                        "rhop",
                        float,
                        40,
                        10,
                        kwargs.get("rhop")
                    ),
                    Field(
                        "rhos",
                        float,
                        50,
                        10,
                        kwargs.get("rhos")
                    ),
                    Field(
                        "calmax",
                        float,
                        60,
                        10,
                        kwargs.get("calmax")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "vfes",
                        float,
                        0,
                        10,
                        kwargs.get("vfes")
                    ),
                    Field(
                        "vfps",
                        float,
                        10,
                        10,
                        kwargs.get("vfps")
                    ),
                    Field(
                        "vfgs",
                        float,
                        20,
                        10,
                        kwargs.get("vfgs")
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part number identifier
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def ielyte(self) -> typing.Optional[int]:
        """Get or set the Material identifier for the open-circuit potential.
        EQ.1:	Lithium Hexafluoroarsenate in Methyl acetate, LiAsF6.
        EQ.2 : Perchlorate in polyethylene oxide(PEO).
        EQ.3 : Sodium Triflate, CF3NaO3S in PEO.
        EQ.4 : Lithium Hexafluoroarsenate in propylene carbonate(PC).
        EQ.5 : Perchlorate in PC.
        EQ.6 : Triflate in PEO.
        EQ.7 : LiPF6 in ethylene carbonate(EC) / dimethyl carbonates(DMC) and p(VdF - HFP).
        """ # nopep8
        return self._cards[0].get_value("ielyte")

    @ielyte.setter
    def ielyte(self, value: int) -> None:
        self._cards[0].set_value("ielyte", value)

    @property
    def etype(self) -> typing.Optional[int]:
        """Get or set the Type of electrolyte:  (Kg/m3):
        EQ.0:	Liquid electrolyte.
        EQ.1 : Solid electrolyte
        """ # nopep8
        return self._cards[0].get_value("etype")

    @etype.setter
    def etype(self, value: int) -> None:
        self._cards[0].set_value("etype", value)

    @property
    def rhoe(self) -> typing.Optional[float]:
        """Get or set the Density of the electrolyte. (Kg/m3)
        """ # nopep8
        return self._cards[0].get_value("rhoe")

    @rhoe.setter
    def rhoe(self, value: float) -> None:
        self._cards[0].set_value("rhoe", value)

    @property
    def rhop(self) -> typing.Optional[float]:
        """Get or set the Density of the polymer phase. (Kg/m3)
        """ # nopep8
        return self._cards[0].get_value("rhop")

    @rhop.setter
    def rhop(self, value: float) -> None:
        self._cards[0].set_value("rhop", value)

    @property
    def rhos(self) -> typing.Optional[float]:
        """Get or set the Density of the separator material. (Kg/m3)
        """ # nopep8
        return self._cards[0].get_value("rhos")

    @rhos.setter
    def rhos(self, value: float) -> None:
        self._cards[0].set_value("rhos", value)

    @property
    def calmax(self) -> typing.Optional[float]:
        """Get or set the Maximum concentration of the electrolyte
        """ # nopep8
        return self._cards[0].get_value("calmax")

    @calmax.setter
    def calmax(self, value: float) -> None:
        self._cards[0].set_value("calmax", value)

    @property
    def vfes(self) -> typing.Optional[float]:
        """Get or set the Volume fraction of electrolyte in the separator
        """ # nopep8
        return self._cards[1].get_value("vfes")

    @vfes.setter
    def vfes(self, value: float) -> None:
        self._cards[1].set_value("vfes", value)

    @property
    def vfps(self) -> typing.Optional[float]:
        """Get or set the Volume fraction of the polymer phase in the separator
        """ # nopep8
        return self._cards[1].get_value("vfps")

    @vfps.setter
    def vfps(self, value: float) -> None:
        self._cards[1].set_value("vfps", value)

    @property
    def vfgs(self) -> typing.Optional[float]:
        """Get or set the Volume fraction of the gas in the separator
        """ # nopep8
        return self._cards[1].get_value("vfgs")

    @vfgs.setter
    def vfgs(self, value: float) -> None:
        self._cards[1].set_value("vfgs", value)

