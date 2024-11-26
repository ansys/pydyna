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

class ControlStagedConstruction(KeywordBase):
    """DYNA CONTROL_STAGED_CONSTRUCTION keyword"""

    keyword = "CONTROL"
    subkeyword = "STAGED_CONSTRUCTION"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "tstart",
                        float,
                        0,
                        10,
                        kwargs.get("tstart", 0.0)
                    ),
                    Field(
                        "stgs",
                        int,
                        10,
                        10,
                        kwargs.get("stgs", 0)
                    ),
                    Field(
                        "stge",
                        int,
                        20,
                        10,
                        kwargs.get("stge", 0)
                    ),
                    Field(
                        "accel",
                        float,
                        30,
                        10,
                        kwargs.get("accel", 0.0)
                    ),
                    Field(
                        "fact",
                        float,
                        40,
                        10,
                        kwargs.get("fact", 1e-6)
                    ),
                    Field(
                        "unused",
                        int,
                        50,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "dordel",
                        int,
                        60,
                        10,
                        kwargs.get("dordel", 0)
                    ),
                    Field(
                        "nopdel",
                        int,
                        70,
                        10,
                        kwargs.get("nopdel", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "itime",
                        int,
                        0,
                        10,
                        kwargs.get("itime", 0)
                    ),
                    Field(
                        "unused",
                        int,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "idynain",
                        int,
                        20,
                        10,
                        kwargs.get("idynain", 0)
                    ),
                ],
            ),
        ]

    @property
    def tstart(self) -> float:
        """Get or set the Time at start of analysis (normally leave blank)
        """ # nopep8
        return self._cards[0].get_value("tstart")

    @tstart.setter
    def tstart(self, value: float) -> None:
        self._cards[0].set_value("tstart", value)

    @property
    def stgs(self) -> int:
        """Get or set the Construction stage at start of analysis.
        """ # nopep8
        return self._cards[0].get_value("stgs")

    @stgs.setter
    def stgs(self, value: int) -> None:
        self._cards[0].set_value("stgs", value)

    @property
    def stge(self) -> int:
        """Get or set the Construction stage at end of analysis.
        """ # nopep8
        return self._cards[0].get_value("stge")

    @stge.setter
    def stge(self, value: int) -> None:
        self._cards[0].set_value("stge", value)

    @property
    def accel(self) -> float:
        """Get or set the Default acceleration for gravity loading.
        """ # nopep8
        return self._cards[0].get_value("accel")

    @accel.setter
    def accel(self, value: float) -> None:
        self._cards[0].set_value("accel", value)

    @property
    def fact(self) -> float:
        """Get or set the Default stiffness and gravity factor for parts before they are added.
        """ # nopep8
        return self._cards[0].get_value("fact")

    @fact.setter
    def fact(self, value: float) -> None:
        self._cards[0].set_value("fact", value)

    @property
    def dordel(self) -> int:
        """Get or set the Dormant part treatment in d3plot file.
        EQ 0: Parts not shown when dormant (flagged as  deleted ),
        EQ 1: Parts shown normally when dormant..
        """ # nopep8
        return self._cards[0].get_value("dordel")

    @dordel.setter
    def dordel(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""dordel must be one of {0,1}""")
        self._cards[0].set_value("dordel", value)

    @property
    def nopdel(self) -> int:
        """Get or set the Treatment of pressure loads on deleted elements.
        EQ 0: Pressure loads automatically deleted,
        EQ 1: No automatic deletion.
        """ # nopep8
        return self._cards[0].get_value("nopdel")

    @nopdel.setter
    def nopdel(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""nopdel must be one of {0,1}""")
        self._cards[0].set_value("nopdel", value)

    @property
    def itime(self) -> int:
        """Get or set the Treatment of “Real Time” on *DEFINE_‌CONSTRUCTION_‌STAGES (see Remark 9):
        EQ.0:	Real Time is ignored.
        EQ.1:	Time in output files (d3plot, d3thdt, binout…) is converted to Real Time
        """ # nopep8
        return self._cards[1].get_value("itime")

    @itime.setter
    def itime(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""itime must be one of {0,1}""")
        self._cards[1].set_value("itime", value)

    @property
    def idynain(self) -> int:
        """Get or set the Flag to control output of dynain file at the end of every stage
        EQ.0:	write dynain file
        EQ.1:	do not write dynain file .
        """ # nopep8
        return self._cards[1].get_value("idynain")

    @idynain.setter
    def idynain(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""idynain must be one of {0,1}""")
        self._cards[1].set_value("idynain", value)

