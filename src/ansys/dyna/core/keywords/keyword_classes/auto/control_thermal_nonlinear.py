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

class ControlThermalNonlinear(KeywordBase):
    """DYNA CONTROL_THERMAL_NONLINEAR keyword"""

    keyword = "CONTROL"
    subkeyword = "THERMAL_NONLINEAR"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "refmax",
                        int,
                        0,
                        10,
                        kwargs.get("refmax", 10)
                    ),
                    Field(
                        "tol",
                        float,
                        10,
                        10,
                        kwargs.get("tol", 1.0E-4)
                    ),
                    Field(
                        "dcp",
                        float,
                        20,
                        10,
                        kwargs.get("dcp", 1.0)
                    ),
                    Field(
                        "lumpbc",
                        int,
                        30,
                        10,
                        kwargs.get("lumpbc", 0)
                    ),
                    Field(
                        "thlstl",
                        float,
                        40,
                        10,
                        kwargs.get("thlstl", 0.0)
                    ),
                    Field(
                        "nlthpr",
                        int,
                        50,
                        10,
                        kwargs.get("nlthpr", 0)
                    ),
                    Field(
                        "phchpn",
                        float,
                        60,
                        10,
                        kwargs.get("phchpn", 100.0)
                    ),
                ],
            ),
        ]

    @property
    def refmax(self) -> int:
        """Get or set the Maximum number of matrix reformations per time step (default = 10)
        """ # nopep8
        return self._cards[0].get_value("refmax")

    @refmax.setter
    def refmax(self, value: int) -> None:
        self._cards[0].set_value("refmax", value)

    @property
    def tol(self) -> float:
        """Get or set the Convergence tolerance for temperature: EQ.0.0: set to 1000 * machine roundoff.
        """ # nopep8
        return self._cards[0].get_value("tol")

    @tol.setter
    def tol(self, value: float) -> None:
        self._cards[0].set_value("tol", value)

    @property
    def dcp(self) -> float:
        """Get or set the Divergence control parameter:
        steady state problems 0.3 <= DCP<=1.0.   default is 1.0
        transient problems 0.0 < DCP <=1.0.    default is 0.5.
        """ # nopep8
        return self._cards[0].get_value("dcp")

    @dcp.setter
    def dcp(self, value: float) -> None:
        self._cards[0].set_value("dcp", value)

    @property
    def lumpbc(self) -> int:
        """Get or set the Lump enclosure radiation boundary condition:
        EQ.0: off (default)
        EQ.1: on
        """ # nopep8
        return self._cards[0].get_value("lumpbc")

    @lumpbc.setter
    def lumpbc(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""lumpbc must be one of {0,1}""")
        self._cards[0].set_value("lumpbc", value)

    @property
    def thlstl(self) -> float:
        """Get or set the Line search convergence tolerance:
        EQ.0.0: No line search
        GT.0.0: Line search convergence tolerance
        """ # nopep8
        return self._cards[0].get_value("thlstl")

    @thlstl.setter
    def thlstl(self, value: float) -> None:
        self._cards[0].set_value("thlstl", value)

    @property
    def nlthpr(self) -> int:
        """Get or set the Thermal nonlinear print out level:
        EQ.0: No print out
        EQ.1: Print convergence parameters during solution of nonlinear system
        """ # nopep8
        return self._cards[0].get_value("nlthpr")

    @nlthpr.setter
    def nlthpr(self, value: int) -> None:
        self._cards[0].set_value("nlthpr", value)

    @property
    def phchpn(self) -> float:
        """Get or set the Phase change penalty parameter:
        EQ.0.0: Penalty formulation not activated
        GT.0.0: Penalty to enforce constant phase change temperature
        """ # nopep8
        return self._cards[0].get_value("phchpn")

    @phchpn.setter
    def phchpn(self, value: float) -> None:
        self._cards[0].set_value("phchpn", value)

