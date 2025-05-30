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

"""Module providing the CeseControlTimestep class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class CeseControlTimestep(KeywordBase):
    """DYNA CESE_CONTROL_TIMESTEP keyword"""

    keyword = "CESE"
    subkeyword = "CONTROL_TIMESTEP"

    def __init__(self, **kwargs):
        """Initialize the CeseControlTimestep class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "iddt",
                        int,
                        0,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "cfl",
                        float,
                        10,
                        10,
                        0.9,
                        **kwargs,
                    ),
                    Field(
                        "dtint",
                        float,
                        20,
                        10,
                        1.0e-3,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def iddt(self) -> int:
        """Get or set the Set the time step option:
        EQ.0: Fixed time step size ( DTINT, i.e., given initial time step size)
        NE.0: the time step size will be calculated based on the given CFL-number and the flow solution at the previous time step.
        """ # nopep8
        return self._cards[0].get_value("iddt")

    @iddt.setter
    def iddt(self, value: int) -> None:
        """Set the iddt property."""
        self._cards[0].set_value("iddt", value)

    @property
    def cfl(self) -> float:
        """Get or set the CFL number (Courant�CFriedrichs�CLewy condition)
        ( 0.0 < CFL <= 1.0 )
        .
        """ # nopep8
        return self._cards[0].get_value("cfl")

    @cfl.setter
    def cfl(self, value: float) -> None:
        """Set the cfl property."""
        self._cards[0].set_value("cfl", value)

    @property
    def dtint(self) -> float:
        """Get or set the Initial time step size.
        """ # nopep8
        return self._cards[0].get_value("dtint")

    @dtint.setter
    def dtint(self, value: float) -> None:
        """Set the dtint property."""
        self._cards[0].set_value("dtint", value)

