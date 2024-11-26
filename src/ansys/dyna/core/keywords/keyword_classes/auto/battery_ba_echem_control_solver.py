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

class BatteryBaEchemControlSolver(KeywordBase):
    """DYNA BATTERY_BA_ECHEM_CONTROL_SOLVER keyword"""

    keyword = "BATTERY"
    subkeyword = "BA_ECHEM_CONTROL_SOLVER"

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

