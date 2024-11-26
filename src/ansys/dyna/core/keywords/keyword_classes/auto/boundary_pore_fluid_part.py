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

class BoundaryPoreFluidPart(KeywordBase):
    """DYNA BOUNDARY_PORE_FLUID_PART keyword"""

    keyword = "BOUNDARY"
    subkeyword = "PORE_FLUID_PART"

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
                        "wtable",
                        float,
                        10,
                        10,
                        kwargs.get("wtable")
                    ),
                    Field(
                        "pf_rho",
                        float,
                        20,
                        10,
                        kwargs.get("pf_rho")
                    ),
                    Field(
                        "atype",
                        int,
                        30,
                        10,
                        kwargs.get("atype", 0)
                    ),
                    Field(
                        "pf_bulk",
                        float,
                        40,
                        10,
                        kwargs.get("pf_bulk")
                    ),
                    Field(
                        "acurve",
                        int,
                        50,
                        10,
                        kwargs.get("acurve")
                    ),
                    Field(
                        "wtcur",
                        int,
                        60,
                        10,
                        kwargs.get("wtcur")
                    ),
                    Field(
                        "suclim",
                        float,
                        70,
                        10,
                        kwargs.get("suclim")
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID (PID),see *PART.  All elements within the part must lie below the water table..
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def wtable(self) -> typing.Optional[float]:
        """Get or set the Z-coordinate at which pore pressure = 0 (water table)
        """ # nopep8
        return self._cards[0].get_value("wtable")

    @wtable.setter
    def wtable(self, value: float) -> None:
        self._cards[0].set_value("wtable", value)

    @property
    def pf_rho(self) -> typing.Optional[float]:
        """Get or set the Density of pore water in soil skeleton:	EQ.0:  Default density specified on *CONTROL_PORE_FLUID card is used.
        """ # nopep8
        return self._cards[0].get_value("pf_rho")

    @pf_rho.setter
    def pf_rho(self, value: float) -> None:
        self._cards[0].set_value("pf_rho", value)

    @property
    def atype(self) -> int:
        """Get or set the Analysis type for Parts:
        EQ.0: Default to value specified on *CONTROL_PORE_FLUID
        EQ 1: Undrained analysis
        EQ 2: Drained analysis
        EQ 3:Time dependent consolidation (coupled)
        EQ 4:Consolidate to steady state (uncoupled)
        EQ 5:Drained in dynamic relaxation, undrained in transient
        """ # nopep8
        return self._cards[0].get_value("atype")

    @atype.setter
    def atype(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5]:
            raise Exception("""atype must be one of {0,1,2,3,4,5}""")
        self._cards[0].set_value("atype", value)

    @property
    def pf_bulk(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus of pore fluid:EQ.0: Default to value specified on *CONTROL_PORE_FLUID
        """ # nopep8
        return self._cards[0].get_value("pf_bulk")

    @pf_bulk.setter
    def pf_bulk(self, value: float) -> None:
        self._cards[0].set_value("pf_bulk", value)

    @property
    def acurve(self) -> typing.Optional[int]:
        """Get or set the Curve of analysis type vs time (see notes below)
        """ # nopep8
        return self._cards[0].get_value("acurve")

    @acurve.setter
    def acurve(self, value: int) -> None:
        self._cards[0].set_value("acurve", value)

    @property
    def wtcur(self) -> typing.Optional[int]:
        """Get or set the Curve of water table (z-coordinate) vs time
        """ # nopep8
        return self._cards[0].get_value("wtcur")

    @wtcur.setter
    def wtcur(self, value: int) -> None:
        self._cards[0].set_value("wtcur", value)

    @property
    def suclim(self) -> typing.Optional[float]:
        """Get or set the Suction limit (defined in head, i.e. length units). Must not be negative. See notes
        """ # nopep8
        return self._cards[0].get_value("suclim")

    @suclim.setter
    def suclim(self, value: float) -> None:
        self._cards[0].set_value("suclim", value)

