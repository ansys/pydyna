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

class ControlTermination(KeywordBase):
    """DYNA CONTROL_TERMINATION keyword"""

    keyword = "CONTROL"
    subkeyword = "TERMINATION"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "endtim",
                        float,
                        0,
                        10,
                        kwargs.get("endtim", 0.0)
                    ),
                    Field(
                        "endcyc",
                        int,
                        10,
                        10,
                        kwargs.get("endcyc", 0)
                    ),
                    Field(
                        "dtmin",
                        float,
                        20,
                        10,
                        kwargs.get("dtmin", 0.0)
                    ),
                    Field(
                        "endeng",
                        float,
                        30,
                        10,
                        kwargs.get("endeng", 0.0)
                    ),
                    Field(
                        "endmas",
                        float,
                        40,
                        10,
                        kwargs.get("endmas", 100000000.0)
                    ),
                    Field(
                        "nosol",
                        int,
                        50,
                        10,
                        kwargs.get("nosol", 0)
                    ),
                ],
            ),
        ]

    @property
    def endtim(self) -> float:
        """Get or set the Termination time. Mandatory.
        """ # nopep8
        return self._cards[0].get_value("endtim")

    @endtim.setter
    def endtim(self, value: float) -> None:
        self._cards[0].set_value("endtim", value)

    @property
    def endcyc(self) -> int:
        """Get or set the Termination cycle.
        """ # nopep8
        return self._cards[0].get_value("endcyc")

    @endcyc.setter
    def endcyc(self, value: int) -> None:
        self._cards[0].set_value("endcyc", value)

    @property
    def dtmin(self) -> float:
        """Get or set the Reduction (or scale) factor for initial time step size to determine minimum time step.
        """ # nopep8
        return self._cards[0].get_value("dtmin")

    @dtmin.setter
    def dtmin(self, value: float) -> None:
        self._cards[0].set_value("dtmin", value)

    @property
    def endeng(self) -> float:
        """Get or set the Percent change in energy ratio for termination of calculation. If undefined, this option is inactive.
        """ # nopep8
        return self._cards[0].get_value("endeng")

    @endeng.setter
    def endeng(self, value: float) -> None:
        self._cards[0].set_value("endeng", value)

    @property
    def endmas(self) -> float:
        """Get or set the Percent change in the total mass for termination of calculation.  This option is relevant if and only if mass scaling is used to limit the minimum time step size; see *CONTROL_TIMESTEP field DT2MS.
        LT.0.0: |ENDMAS| is the load curve ID defining the percent change in the total mass as a function of the total mass.
        """ # nopep8
        return self._cards[0].get_value("endmas")

    @endmas.setter
    def endmas(self, value: float) -> None:
        self._cards[0].set_value("endmas", value)

    @property
    def nosol(self) -> int:
        """Get or set the Flag for a non-solution run, i.e. normal termination directly after initialization.
        EQ.0: off (default),
        EQ.1: on.
        """ # nopep8
        return self._cards[0].get_value("nosol")

    @nosol.setter
    def nosol(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""nosol must be one of {0,1}""")
        self._cards[0].set_value("nosol", value)

