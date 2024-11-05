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

class ChangeThermalParameters(KeywordBase):
    """DYNA CHANGE_THERMAL_PARAMETERS keyword"""

    keyword = "CHANGE"
    subkeyword = "THERMAL_PARAMETERS"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ts",
                        int,
                        0,
                        10,
                        kwargs.get("ts", 0)
                    ),
                    Field(
                        "dt",
                        float,
                        10,
                        10,
                        kwargs.get("dt", 0.0)
                    ),
                    Field(
                        "tmin",
                        float,
                        20,
                        10,
                        kwargs.get("tmin", 0.0)
                    ),
                    Field(
                        "tmax",
                        float,
                        30,
                        10,
                        kwargs.get("tmax", 0.0)
                    ),
                    Field(
                        "dtemp",
                        float,
                        40,
                        10,
                        kwargs.get("dtemp", 0.0)
                    ),
                    Field(
                        "tscp",
                        float,
                        50,
                        10,
                        kwargs.get("tscp", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "refmax",
                        int,
                        0,
                        10,
                        kwargs.get("refmax", 0)
                    ),
                    Field(
                        "tol",
                        float,
                        10,
                        10,
                        kwargs.get("tol", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def ts(self) -> int:
        """Get or set the Thermal time step code:
        EQ.0: No change (default),
        EQ.1: Fixed timestep,
        EQ.2: variable timestep.
        """ # nopep8
        return self._cards[0].get_value("ts")

    @ts.setter
    def ts(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""ts must be one of {0,1,2}""")
        self._cards[0].set_value("ts", value)

    @property
    def dt(self) -> float:
        """Get or set the Thermal time step on restart:
        EQ.0.0: No change (default).
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[0].set_value("dt", value)

    @property
    def tmin(self) -> float:
        """Get or set the Minimum thermal timestep:
        EQ.0.0: No change (default).
        """ # nopep8
        return self._cards[0].get_value("tmin")

    @tmin.setter
    def tmin(self, value: float) -> None:
        self._cards[0].set_value("tmin", value)

    @property
    def tmax(self) -> float:
        """Get or set the Maximum thermal timestep:
        EQ.0.0: No change (default).
        """ # nopep8
        return self._cards[0].get_value("tmax")

    @tmax.setter
    def tmax(self, value: float) -> None:
        self._cards[0].set_value("tmax", value)

    @property
    def dtemp(self) -> float:
        """Get or set the Maximum temperature change in a thermal timestep:
        EQ.0.0: No change (default).
        """ # nopep8
        return self._cards[0].get_value("dtemp")

    @dtemp.setter
    def dtemp(self, value: float) -> None:
        self._cards[0].set_value("dtemp", value)

    @property
    def tscp(self) -> float:
        """Get or set the Time step control parameter (0.0 < TSCP < 1.0 ):
        EQ.0.0: No change (default).
        """ # nopep8
        return self._cards[0].get_value("tscp")

    @tscp.setter
    def tscp(self, value: float) -> None:
        self._cards[0].set_value("tscp", value)

    @property
    def refmax(self) -> int:
        """Get or set the Maximum number of reformations per thermal time step:
        EQ.0: No change (default).
        """ # nopep8
        return self._cards[1].get_value("refmax")

    @refmax.setter
    def refmax(self, value: int) -> None:
        self._cards[1].set_value("refmax", value)

    @property
    def tol(self) -> float:
        """Get or set the Non-linear convergence tolerance:
        EQ.0.0: No change (default).
        """ # nopep8
        return self._cards[1].get_value("tol")

    @tol.setter
    def tol(self, value: float) -> None:
        self._cards[1].set_value("tol", value)

