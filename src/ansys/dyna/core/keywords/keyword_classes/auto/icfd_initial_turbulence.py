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

class IcfdInitialTurbulence(KeywordBase):
    """DYNA ICFD_INITIAL_TURBULENCE keyword"""

    keyword = "ICFD"
    subkeyword = "INITIAL_TURBULENCE"

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
                        "i",
                        float,
                        10,
                        10,
                        kwargs.get("i")
                    ),
                    Field(
                        "r",
                        float,
                        20,
                        10,
                        kwargs.get("r")
                    ),
                    Field(
                        "k",
                        float,
                        30,
                        10,
                        kwargs.get("k")
                    ),
                    Field(
                        "ew",
                        float,
                        40,
                        10,
                        kwargs.get("ew")
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID for the volume elements or the surface elements where the values are initialized (see *ICFD_PART_VOL and *ICFD_PART).PID = 0 to assign the initial condition to all nodes at once.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def i(self) -> typing.Optional[float]:
        """Get or set the Initial turbulent intensity.
        """ # nopep8
        return self._cards[0].get_value("i")

    @i.setter
    def i(self, value: float) -> None:
        self._cards[0].set_value("i", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Initial turbulent viscosity to laminar viscosity ratio.
        """ # nopep8
        return self._cards[0].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        self._cards[0].set_value("r", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the Initial kinetic energy. When defined, it replaces the choice of I. A negative integer will point to a *DEFINE_FUNCTION ID. The following parameters are allowed : f(x,y,z), allowing to define initial profiles function of coordinates
        """ # nopep8
        return self._cards[0].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        self._cards[0].set_value("k", value)

    @property
    def ew(self) -> typing.Optional[float]:
        """Get or set the Initial turbulent specific dissipation rate or dissipation rate depending on the choice of turbulence model. When defined, it replaces the choice of R. A negative integer will point to a *DEFINE_FUNCTION ID. The following parameters are allowed : f(x,y,z), allowing to define initial profiles function of coordinates
        """ # nopep8
        return self._cards[0].get_value("ew")

    @ew.setter
    def ew(self, value: float) -> None:
        self._cards[0].set_value("ew", value)

