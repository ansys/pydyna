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

class IcfdControlDemCoupling(KeywordBase):
    """DYNA ICFD_CONTROL_DEM_COUPLING keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_DEM_COUPLING"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ctype",
                        int,
                        0,
                        10,
                        kwargs.get("ctype", 0)
                    ),
                    Field(
                        "bt",
                        float,
                        10,
                        10,
                        kwargs.get("bt", 0.0)
                    ),
                    Field(
                        "dt",
                        float,
                        20,
                        10,
                        kwargs.get("dt", 1E+28)
                    ),
                    Field(
                        "sf",
                        float,
                        30,
                        10,
                        kwargs.get("sf", 1.)
                    ),
                    Field(
                        "maxvel",
                        float,
                        40,
                        10,
                        kwargs.get("maxvel")
                    ),
                    Field(
                        "dtype",
                        int,
                        50,
                        10,
                        kwargs.get("dtype")
                    ),
                ],
            ),
        ]

    @property
    def ctype(self) -> int:
        """Get or set the Indicates the coupling direction to the solver.
        EQ.0:two-way coupling between the fluid and the solidEQ.
        EQ.1:one-way coupling:The DEM particles transfer their location to the fluid solver.
        EQ.2:one-way coupling. The fluid solver transfers forces to the DEM particles.
        """ # nopep8
        return self._cards[0].get_value("ctype")

    @ctype.setter
    def ctype(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""ctype must be one of {0,1,2}""")
        self._cards[0].set_value("ctype", value)

    @property
    def bt(self) -> float:
        """Get or set the Birth time for the DEM coupling.
        """ # nopep8
        return self._cards[0].get_value("bt")

    @bt.setter
    def bt(self, value: float) -> None:
        self._cards[0].set_value("bt", value)

    @property
    def dt(self) -> float:
        """Get or set the Death time for the DEM coupling.
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[0].set_value("dt", value)

    @property
    def sf(self) -> float:
        """Get or set the Scale factor applied to the force transmitted by the fluid to the structure.
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        self._cards[0].set_value("sf", value)

    @property
    def maxvel(self) -> typing.Optional[float]:
        """Get or set the Maximal fluid velocity that can be used for the calculation of the fluid force passed on to the DEM particle. This is to avoid having spurious velocities in the fluid causing very high and unrealistic forces on the DEM particles which may lead to a crash.
        """ # nopep8
        return self._cards[0].get_value("maxvel")

    @maxvel.setter
    def maxvel(self, value: float) -> None:
        self._cards[0].set_value("maxvel", value)

    @property
    def dtype(self) -> typing.Optional[int]:
        """Get or set the Drag calculation type :
        EQ.0:	Constant C_d value 0.5 scaled by SF.
        EQ.1 : Morrison formula for C_d calculation based on local Reynolds number value scaled by SF.
        """ # nopep8
        return self._cards[0].get_value("dtype")

    @dtype.setter
    def dtype(self, value: int) -> None:
        self._cards[0].set_value("dtype", value)

