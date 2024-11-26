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

class IcfdControlTime(KeywordBase):
    """DYNA ICFD_CONTROL_TIME keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_TIME"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ttm",
                        float,
                        0,
                        10,
                        kwargs.get("ttm", 1.E28)
                    ),
                    Field(
                        "dt",
                        float,
                        10,
                        10,
                        kwargs.get("dt", 0.0)
                    ),
                    Field(
                        "cfl",
                        float,
                        20,
                        10,
                        kwargs.get("cfl", 1.0)
                    ),
                    Field(
                        "lcidsf",
                        int,
                        30,
                        10,
                        kwargs.get("lcidsf")
                    ),
                    Field(
                        "dtmin",
                        float,
                        40,
                        10,
                        kwargs.get("dtmin")
                    ),
                    Field(
                        "dtmax",
                        float,
                        50,
                        10,
                        kwargs.get("dtmax")
                    ),
                    Field(
                        "dtinit",
                        float,
                        60,
                        10,
                        kwargs.get("dtinit")
                    ),
                    Field(
                        "tdeath",
                        float,
                        70,
                        10,
                        kwargs.get("tdeath", 1e28)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dtt",
                        float,
                        0,
                        10,
                        kwargs.get("dtt")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "btbl",
                        int,
                        0,
                        10,
                        kwargs.get("btbl", 0)
                    ),
                ],
            ),
        ]

    @property
    def ttm(self) -> float:
        """Get or set the Total time of simulation for the fluid problem.
        """ # nopep8
        return self._cards[0].get_value("ttm")

    @ttm.setter
    def ttm(self, value: float) -> None:
        self._cards[0].set_value("ttm", value)

    @property
    def dt(self) -> float:
        """Get or set the Time step for the fluid problem. If different than zero the time step will be set constant and equal to this value. If DT = 0 then the time step is automatically computed.
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[0].set_value("dt", value)

    @property
    def cfl(self) -> float:
        """Get or set the Scale factor that multplies DT.
        """ # nopep8
        return self._cards[0].get_value("cfl")

    @cfl.setter
    def cfl(self, value: float) -> None:
        self._cards[0].set_value("cfl", value)

    @property
    def lcidsf(self) -> typing.Optional[int]:
        """Get or set the Load Curve ID specifying the CFL number when DT = 0 as a function of time, and more generally LCIDSF specifies the time step scale factor as the function of time.
        """ # nopep8
        return self._cards[0].get_value("lcidsf")

    @lcidsf.setter
    def lcidsf(self, value: int) -> None:
        self._cards[0].set_value("lcidsf", value)

    @property
    def dtmin(self) -> typing.Optional[float]:
        """Get or set the Minimum time step. When an automatic time step is used and DTMIN is defined, the time step cannot adopt a smaller value than DTMIN.A negative value will refer to a time dependent load curve.
        """ # nopep8
        return self._cards[0].get_value("dtmin")

    @dtmin.setter
    def dtmin(self, value: float) -> None:
        self._cards[0].set_value("dtmin", value)

    @property
    def dtmax(self) -> typing.Optional[float]:
        """Get or set the Maximum time step. When an automatic time step is used and DTMAX is defined, the time step cannot adopt a higher value than DTMAX.. A negative value will refer to a time dependent load curve.
        """ # nopep8
        return self._cards[0].get_value("dtmax")

    @dtmax.setter
    def dtmax(self, value: float) -> None:
        self._cards[0].set_value("dtmax", value)

    @property
    def dtinit(self) -> typing.Optional[float]:
        """Get or set the Initial time step. If not defined, the solver will automatically determine an initial timestep based on the flow velocity or dimensions of the problem in cases where there is no inflow.
        """ # nopep8
        return self._cards[0].get_value("dtinit")

    @dtinit.setter
    def dtinit(self, value: float) -> None:
        self._cards[0].set_value("dtinit", value)

    @property
    def tdeath(self) -> float:
        """Get or set the Death time for the Navier Stokes solve. After TDEATH, the velocity and pressure will no longer be updated. But the temperature and other similar quantities still can.
        """ # nopep8
        return self._cards[0].get_value("tdeath")

    @tdeath.setter
    def tdeath(self, value: float) -> None:
        self._cards[0].set_value("tdeath", value)

    @property
    def dtt(self) -> typing.Optional[float]:
        """Get or set the Thermal timestep
        """ # nopep8
        return self._cards[1].get_value("dtt")

    @dtt.setter
    def dtt(self, value: float) -> None:
        self._cards[1].set_value("dtt", value)

    @property
    def btbl(self) -> int:
        """Get or set the Flag to include boundary layer elements in the automatic timestep calculation.
        EQ.0:	Default.The boundary layer elements are excluded.
        EQ.1 : The boundary layer elements are included.
        """ # nopep8
        return self._cards[2].get_value("btbl")

    @btbl.setter
    def btbl(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""btbl must be one of {0,1}""")
        self._cards[2].set_value("btbl", value)

