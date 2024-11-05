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

class BatteryEchemControlSolver(KeywordBase):
    """DYNA BATTERY_ECHEM_CONTROL_SOLVER keyword"""

    keyword = "BATTERY"
    subkeyword = "ECHEM_CONTROL_SOLVER"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "imodel",
                        int,
                        0,
                        10,
                        kwargs.get("imodel")
                    ),
                    Field(
                        "igeom",
                        int,
                        10,
                        10,
                        kwargs.get("igeom")
                    ),
                    Field(
                        "ncycle",
                        int,
                        20,
                        10,
                        kwargs.get("ncycle", 1)
                    ),
                    Field(
                        "aging",
                        int,
                        30,
                        10,
                        kwargs.get("aging", 1)
                    ),
                    Field(
                        "tra",
                        int,
                        40,
                        10,
                        kwargs.get("tra", 0)
                    ),
                    Field(
                        "gas",
                        int,
                        50,
                        10,
                        kwargs.get("gas", 0)
                    ),
                    Field(
                        "esolid",
                        int,
                        60,
                        10,
                        kwargs.get("esolid", 0)
                    ),
                    Field(
                        "unused",
                        int,
                        70,
                        10,
                        kwargs.get("unused")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "irun",
                        int,
                        0,
                        10,
                        kwargs.get("irun")
                    ),
                    Field(
                        "lcur",
                        int,
                        10,
                        10,
                        kwargs.get("lcur")
                    ),
                    Field(
                        "curv",
                        float,
                        20,
                        10,
                        kwargs.get("curv")
                    ),
                    Field(
                        "ctime",
                        float,
                        30,
                        10,
                        kwargs.get("ctime", 0.0)
                    ),
                    Field(
                        "vcut",
                        float,
                        40,
                        10,
                        kwargs.get("vcut", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "mws",
                        float,
                        0,
                        10,
                        kwargs.get("mws")
                    ),
                    Field(
                        "dens",
                        float,
                        10,
                        10,
                        kwargs.get("dens")
                    ),
                    Field(
                        "brugs",
                        float,
                        20,
                        10,
                        kwargs.get("brugs")
                    ),
                    Field(
                        "epss",
                        float,
                        30,
                        10,
                        kwargs.get("epss")
                    ),
                    Field(
                        "cseio",
                        float,
                        40,
                        10,
                        kwargs.get("cseio")
                    ),
                    Field(
                        "tseio",
                        float,
                        50,
                        10,
                        kwargs.get("tseio")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ecdo",
                        float,
                        0,
                        10,
                        kwargs.get("ecdo")
                    ),
                    Field(
                        "kfs",
                        float,
                        10,
                        10,
                        kwargs.get("kfs")
                    ),
                    Field(
                        "ceco",
                        float,
                        20,
                        10,
                        kwargs.get("ceco")
                    ),
                    Field(
                        "ecdf",
                        float,
                        30,
                        10,
                        kwargs.get("ecdf")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "hofeln",
                        int,
                        0,
                        10,
                        kwargs.get("hofeln")
                    ),
                    Field(
                        "hofli",
                        int,
                        10,
                        10,
                        kwargs.get("hofli")
                    ),
                    Field(
                        "hofsei",
                        float,
                        20,
                        10,
                        kwargs.get("hofsei")
                    ),
                    Field(
                        "hofc2h4",
                        float,
                        30,
                        10,
                        kwargs.get("hofc2h4")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "afi",
                        float,
                        0,
                        10,
                        kwargs.get("afi")
                    ),
                    Field(
                        "eat",
                        float,
                        10,
                        10,
                        kwargs.get("eat")
                    ),
                    Field(
                        "hoflc",
                        float,
                        20,
                        10,
                        kwargs.get("hoflc")
                    ),
                    Field(
                        "hofco2",
                        float,
                        30,
                        10,
                        kwargs.get("hofco2")
                    ),
                    Field(
                        "hofo2",
                        float,
                        40,
                        10,
                        kwargs.get("hofo2")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "file1",
                        str,
                        0,
                        80,
                        kwargs.get("file1")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "file2",
                        str,
                        0,
                        80,
                        kwargs.get("file2")
                    ),
                ],
            ),
        ]

    @property
    def imodel(self) -> typing.Optional[int]:
        """Get or set the Sets the battery model.
        EQ.1:	A single insertion model
        EQ.2 : Dual insertion model
        """ # nopep8
        return self._cards[0].get_value("imodel")

    @imodel.setter
    def imodel(self, value: int) -> None:
        self._cards[0].set_value("imodel", value)

    @property
    def igeom(self) -> typing.Optional[int]:
        """Get or set the Sets the geometric dimension:
        EQ.1:	1D Electrochemical model
        EQ.11 : 1D Aging and Thermal Runaway model
        EQ.101 : 1D ECTM model
        EQ.111 : 1D ECTM with A & T model
        """ # nopep8
        return self._cards[0].get_value("igeom")

    @igeom.setter
    def igeom(self, value: int) -> None:
        self._cards[0].set_value("igeom", value)

    @property
    def ncycle(self) -> int:
        """Get or set the The number of cycles to run. Default is 1 cycle
        """ # nopep8
        return self._cards[0].get_value("ncycle")

    @ncycle.setter
    def ncycle(self, value: int) -> None:
        self._cards[0].set_value("ncycle", value)

    @property
    def aging(self) -> int:
        """Get or set the Aging model. 1 for ON, and 0 for OFF
        """ # nopep8
        return self._cards[0].get_value("aging")

    @aging.setter
    def aging(self, value: int) -> None:
        self._cards[0].set_value("aging", value)

    @property
    def tra(self) -> int:
        """Get or set the Thermal runaway model. 1for ON, and 0 for OFF.
        """ # nopep8
        return self._cards[0].get_value("tra")

    @tra.setter
    def tra(self, value: int) -> None:
        self._cards[0].set_value("tra", value)

    @property
    def gas(self) -> int:
        """Get or set the Gas generation model (scheduled)
        """ # nopep8
        return self._cards[0].get_value("gas")

    @gas.setter
    def gas(self, value: int) -> None:
        self._cards[0].set_value("gas", value)

    @property
    def esolid(self) -> int:
        """Get or set the The concentration of the electrode particle model
        EQ.0: Superposition method.
        EQ.1 : Full equation method.
        """ # nopep8
        return self._cards[0].get_value("esolid")

    @esolid.setter
    def esolid(self, value: int) -> None:
        self._cards[0].set_value("esolid", value)

    @property
    def irun(self) -> typing.Optional[int]:
        """Get or set the Battery simulation cycle termination criterion
        EQ.1:	The current cycle runs for a given time
        EQ.2 : The current cycle runs until the cell voltage reaches VCUT
        """ # nopep8
        return self._cards[1].get_value("irun")

    @irun.setter
    def irun(self, value: int) -> None:
        self._cards[1].set_value("irun", value)

    @property
    def lcur(self) -> typing.Optional[int]:
        """Get or set the Running current.
        EQ.0:	Constant current.
        EQ.1 : Variable current
        """ # nopep8
        return self._cards[1].get_value("lcur")

    @lcur.setter
    def lcur(self, value: int) -> None:
        self._cards[1].set_value("lcur", value)

    @property
    def curv(self) -> typing.Optional[float]:
        """Get or set the Current value to run
        """ # nopep8
        return self._cards[1].get_value("curv")

    @curv.setter
    def curv(self, value: float) -> None:
        self._cards[1].set_value("curv", value)

    @property
    def ctime(self) -> float:
        """Get or set the Running time for the cycle
        """ # nopep8
        return self._cards[1].get_value("ctime")

    @ctime.setter
    def ctime(self, value: float) -> None:
        self._cards[1].set_value("ctime", value)

    @property
    def vcut(self) -> float:
        """Get or set the A voltage to terminate
        """ # nopep8
        return self._cards[1].get_value("vcut")

    @vcut.setter
    def vcut(self, value: float) -> None:
        self._cards[1].set_value("vcut", value)

    @property
    def mws(self) -> typing.Optional[float]:
        """Get or set the Molecular weight of the SEI
        """ # nopep8
        return self._cards[2].get_value("mws")

    @mws.setter
    def mws(self, value: float) -> None:
        self._cards[2].set_value("mws", value)

    @property
    def dens(self) -> typing.Optional[float]:
        """Get or set the Density of the SEI
        """ # nopep8
        return self._cards[2].get_value("dens")

    @dens.setter
    def dens(self, value: float) -> None:
        self._cards[2].set_value("dens", value)

    @property
    def brugs(self) -> typing.Optional[float]:
        """Get or set the The Brugmann constant of the SEI
        """ # nopep8
        return self._cards[2].get_value("brugs")

    @brugs.setter
    def brugs(self, value: float) -> None:
        self._cards[2].set_value("brugs", value)

    @property
    def epss(self) -> typing.Optional[float]:
        """Get or set the Initial SEI porosity
        """ # nopep8
        return self._cards[2].get_value("epss")

    @epss.setter
    def epss(self, value: float) -> None:
        self._cards[2].set_value("epss", value)

    @property
    def cseio(self) -> typing.Optional[float]:
        """Get or set the Initial SEI concentration, [mol/m3]
        """ # nopep8
        return self._cards[2].get_value("cseio")

    @cseio.setter
    def cseio(self, value: float) -> None:
        self._cards[2].set_value("cseio", value)

    @property
    def tseio(self) -> typing.Optional[float]:
        """Get or set the Initial thickness of the SEI layer
        """ # nopep8
        return self._cards[2].get_value("tseio")

    @tseio.setter
    def tseio(self, value: float) -> None:
        self._cards[2].set_value("tseio", value)

    @property
    def ecdo(self) -> typing.Optional[float]:
        """Get or set the The exchange current density for the SEI reaction
        """ # nopep8
        return self._cards[3].get_value("ecdo")

    @ecdo.setter
    def ecdo(self, value: float) -> None:
        self._cards[3].set_value("ecdo", value)

    @property
    def kfs(self) -> typing.Optional[float]:
        """Get or set the The reaction rate constant for the SEI reaction
        """ # nopep8
        return self._cards[3].get_value("kfs")

    @kfs.setter
    def kfs(self, value: float) -> None:
        self._cards[3].set_value("kfs", value)

    @property
    def ceco(self) -> typing.Optional[float]:
        """Get or set the Initial concentration of EC (Ethylene Carbonate)
        """ # nopep8
        return self._cards[3].get_value("ceco")

    @ceco.setter
    def ceco(self, value: float) -> None:
        self._cards[3].set_value("ceco", value)

    @property
    def ecdf(self) -> typing.Optional[float]:
        """Get or set the Diffusion coefficient of EC
        """ # nopep8
        return self._cards[3].get_value("ecdf")

    @ecdf.setter
    def ecdf(self, value: float) -> None:
        self._cards[3].set_value("ecdf", value)

    @property
    def hofeln(self) -> typing.Optional[int]:
        """Get or set the Formation enthalpy of electrolyte, [KJ/mol]
        """ # nopep8
        return self._cards[4].get_value("hofeln")

    @hofeln.setter
    def hofeln(self, value: int) -> None:
        self._cards[4].set_value("hofeln", value)

    @property
    def hofli(self) -> typing.Optional[int]:
        """Get or set the Formation enthalpy of Li+, [KJ/mol]
        """ # nopep8
        return self._cards[4].get_value("hofli")

    @hofli.setter
    def hofli(self, value: int) -> None:
        self._cards[4].set_value("hofli", value)

    @property
    def hofsei(self) -> typing.Optional[float]:
        """Get or set the Formation enthalpy of the SEI layer, [KJ/mol]
        """ # nopep8
        return self._cards[4].get_value("hofsei")

    @hofsei.setter
    def hofsei(self, value: float) -> None:
        self._cards[4].set_value("hofsei", value)

    @property
    def hofc2h4(self) -> typing.Optional[float]:
        """Get or set the Formation enthalpy of ethylene, [KJ/mol]
        """ # nopep8
        return self._cards[4].get_value("hofc2h4")

    @hofc2h4.setter
    def hofc2h4(self, value: float) -> None:
        self._cards[4].set_value("hofc2h4", value)

    @property
    def afi(self) -> typing.Optional[float]:
        """Get or set the Frequency factor for the reaction
        """ # nopep8
        return self._cards[5].get_value("afi")

    @afi.setter
    def afi(self, value: float) -> None:
        self._cards[5].set_value("afi", value)

    @property
    def eat(self) -> typing.Optional[float]:
        """Get or set the Activation energy for the reaction
        """ # nopep8
        return self._cards[5].get_value("eat")

    @eat.setter
    def eat(self, value: float) -> None:
        self._cards[5].set_value("eat", value)

    @property
    def hoflc(self) -> typing.Optional[float]:
        """Get or set the Formation enthalpy of LC (Li2CO3)
        """ # nopep8
        return self._cards[5].get_value("hoflc")

    @hoflc.setter
    def hoflc(self, value: float) -> None:
        self._cards[5].set_value("hoflc", value)

    @property
    def hofco2(self) -> typing.Optional[float]:
        """Get or set the Formation enthalpy of CO2
        """ # nopep8
        return self._cards[5].get_value("hofco2")

    @hofco2.setter
    def hofco2(self, value: float) -> None:
        self._cards[5].set_value("hofco2", value)

    @property
    def hofo2(self) -> typing.Optional[float]:
        """Get or set the Formation enthalpy of O2
        """ # nopep8
        return self._cards[5].get_value("hofo2")

    @hofo2.setter
    def hofo2(self, value: float) -> None:
        self._cards[5].set_value("hofo2", value)

    @property
    def file1(self) -> typing.Optional[str]:
        """Get or set the 
        """ # nopep8
        return self._cards[6].get_value("file1")

    @file1.setter
    def file1(self, value: str) -> None:
        self._cards[6].set_value("file1", value)

    @property
    def file2(self) -> typing.Optional[str]:
        """Get or set the 
        """ # nopep8
        return self._cards[7].get_value("file2")

    @file2.setter
    def file2(self, value: str) -> None:
        self._cards[7].set_value("file2", value)

