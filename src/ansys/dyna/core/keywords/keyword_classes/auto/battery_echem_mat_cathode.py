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

class BatteryEchemMatCathode(KeywordBase):
    """DYNA BATTERY_ECHEM_MAT_CATHODE keyword"""

    keyword = "BATTERY"
    subkeyword = "ECHEM_MAT_CATHODE"

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
                        "iocpa",
                        int,
                        10,
                        10,
                        kwargs.get("iocpa")
                    ),
                    Field(
                        "capta",
                        float,
                        20,
                        10,
                        kwargs.get("capta")
                    ),
                    Field(
                        "s_yc",
                        float,
                        30,
                        10,
                        kwargs.get("s_yc")
                    ),
                    Field(
                        "s_rad",
                        float,
                        40,
                        10,
                        kwargs.get("s_rad")
                    ),
                    Field(
                        "ratec",
                        float,
                        50,
                        10,
                        kwargs.get("ratec")
                    ),
                    Field(
                        "rcath",
                        float,
                        60,
                        10,
                        kwargs.get("rcath")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "rhoec",
                        float,
                        0,
                        10,
                        kwargs.get("rhoec")
                    ),
                    Field(
                        "rhofc",
                        float,
                        10,
                        10,
                        kwargs.get("rhofc")
                    ),
                    Field(
                        "rhoccc",
                        float,
                        20,
                        10,
                        kwargs.get("rhoccc")
                    ),
                    Field(
                        "diffc",
                        float,
                        30,
                        10,
                        kwargs.get("diffc")
                    ),
                    Field(
                        "condc",
                        float,
                        40,
                        10,
                        kwargs.get("condc")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "vfec",
                        float,
                        0,
                        10,
                        kwargs.get("vfec")
                    ),
                    Field(
                        "vfpc",
                        float,
                        10,
                        10,
                        kwargs.get("vfpc")
                    ),
                    Field(
                        "vffc",
                        float,
                        20,
                        10,
                        kwargs.get("vffc")
                    ),
                    Field(
                        "vfgc",
                        float,
                        30,
                        10,
                        kwargs.get("vfgc")
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
    def iocpa(self) -> typing.Optional[int]:
        """Get or set the Material identifier for the open-circuit potential.
        EQ.1: Titanium disulfide, LiyTiS2(0 < y < 1).
        EQ.2 : Spinel Mn2O4(lower plateau) (1.1 < y < 1.99).
        EQ.3 : Cobalt dioxide, LiyCoO2(0.0 < y < 0.99).
        EQ.4 : Spinel Mn2O4(upper plateau) (0.17 < y < 0.99).
        EQ.5 : NMC - 111 (not working).
        EQ.6 : NMC - 811 (not working).
        EQ.7 : LFP(not working).
        """ # nopep8
        return self._cards[0].get_value("iocpa")

    @iocpa.setter
    def iocpa(self, value: int) -> None:
        self._cards[0].set_value("iocpa", value)

    @property
    def capta(self) -> typing.Optional[float]:
        """Get or set the Coulombic capacity of anode material.
        """ # nopep8
        return self._cards[0].get_value("capta")

    @capta.setter
    def capta(self, value: float) -> None:
        self._cards[0].set_value("capta", value)

    @property
    def s_yc(self) -> typing.Optional[float]:
        """Get or set the Initial Lithium stoichiometric coefficient of the anode side active material. For example LixWO3 (0<x<0.67).
        """ # nopep8
        return self._cards[0].get_value("s_yc")

    @s_yc.setter
    def s_yc(self, value: float) -> None:
        self._cards[0].set_value("s_yc", value)

    @property
    def s_rad(self) -> typing.Optional[float]:
        """Get or set the Radius of spherical particle in the cathode side active material. (m)
        """ # nopep8
        return self._cards[0].get_value("s_rad")

    @s_rad.setter
    def s_rad(self, value: float) -> None:
        self._cards[0].set_value("s_rad", value)

    @property
    def ratec(self) -> typing.Optional[float]:
        """Get or set the Reaction rate constant for the cathode electrode
        """ # nopep8
        return self._cards[0].get_value("ratec")

    @ratec.setter
    def ratec(self, value: float) -> None:
        self._cards[0].set_value("ratec", value)

    @property
    def rcath(self) -> typing.Optional[float]:
        """Get or set the Film resistance for the cathode electrode
        """ # nopep8
        return self._cards[0].get_value("rcath")

    @rcath.setter
    def rcath(self, value: float) -> None:
        self._cards[0].set_value("rcath", value)

    @property
    def rhoec(self) -> typing.Optional[float]:
        """Get or set the Density of the cathode insertion material (electrode particles). (Kg/m3)
        """ # nopep8
        return self._cards[1].get_value("rhoec")

    @rhoec.setter
    def rhoec(self, value: float) -> None:
        self._cards[1].set_value("rhoec", value)

    @property
    def rhofc(self) -> typing.Optional[float]:
        """Get or set the Density of the cathode side inert filler. (Kg/m3)
        """ # nopep8
        return self._cards[1].get_value("rhofc")

    @rhofc.setter
    def rhofc(self, value: float) -> None:
        self._cards[1].set_value("rhofc", value)

    @property
    def rhoccc(self) -> typing.Optional[float]:
        """Get or set the Density of the cathode side current collector. (Kg/m3)
        """ # nopep8
        return self._cards[1].get_value("rhoccc")

    @rhoccc.setter
    def rhoccc(self, value: float) -> None:
        self._cards[1].set_value("rhoccc", value)

    @property
    def diffc(self) -> typing.Optional[float]:
        """Get or set the Diffusion coefficient of Lithium ions in the cathode insertion material. (m2/s)
        """ # nopep8
        return self._cards[1].get_value("diffc")

    @diffc.setter
    def diffc(self, value: float) -> None:
        self._cards[1].set_value("diffc", value)

    @property
    def condc(self) -> typing.Optional[float]:
        """Get or set the Effective electronic conductivity of the cathode porous electrode
        """ # nopep8
        return self._cards[1].get_value("condc")

    @condc.setter
    def condc(self, value: float) -> None:
        self._cards[1].set_value("condc", value)

    @property
    def vfec(self) -> typing.Optional[float]:
        """Get or set the Volume fraction of electrolyte in the anode electrode
        """ # nopep8
        return self._cards[2].get_value("vfec")

    @vfec.setter
    def vfec(self, value: float) -> None:
        self._cards[2].set_value("vfec", value)

    @property
    def vfpc(self) -> typing.Optional[float]:
        """Get or set the Volume fraction of the polymer phase in the anode electrode
        """ # nopep8
        return self._cards[2].get_value("vfpc")

    @vfpc.setter
    def vfpc(self, value: float) -> None:
        self._cards[2].set_value("vfpc", value)

    @property
    def vffc(self) -> typing.Optional[float]:
        """Get or set the Volume fraction of the inert filler in the anode electrode
        """ # nopep8
        return self._cards[2].get_value("vffc")

    @vffc.setter
    def vffc(self, value: float) -> None:
        self._cards[2].set_value("vffc", value)

    @property
    def vfgc(self) -> typing.Optional[float]:
        """Get or set the Volume fraction of the gas in the anode electrode
        """ # nopep8
        return self._cards[2].get_value("vfgc")

    @vfgc.setter
    def vfgc(self, value: float) -> None:
        self._cards[2].set_value("vfgc", value)

