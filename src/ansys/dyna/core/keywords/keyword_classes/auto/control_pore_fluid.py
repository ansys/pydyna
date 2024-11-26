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

class ControlPoreFluid(KeywordBase):
    """DYNA CONTROL_PORE_FLUID keyword"""

    keyword = "CONTROL"
    subkeyword = "PORE_FLUID"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "atype",
                        int,
                        0,
                        10,
                        kwargs.get("atype", 0)
                    ),
                    Field(
                        "unused",
                        float,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "wtable",
                        float,
                        20,
                        10,
                        kwargs.get("wtable", 0.0)
                    ),
                    Field(
                        "pf_rho",
                        float,
                        30,
                        10,
                        kwargs.get("pf_rho")
                    ),
                    Field(
                        "grav",
                        float,
                        40,
                        10,
                        kwargs.get("grav")
                    ),
                    Field(
                        "pf_bulk",
                        float,
                        50,
                        10,
                        kwargs.get("pf_bulk")
                    ),
                    Field(
                        "output",
                        int,
                        60,
                        10,
                        kwargs.get("output", 0)
                    ),
                    Field(
                        "tmf",
                        float,
                        70,
                        10,
                        kwargs.get("tmf", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "targ",
                        float,
                        0,
                        10,
                        kwargs.get("targ", 0.0)
                    ),
                    Field(
                        "fmin",
                        float,
                        10,
                        10,
                        kwargs.get("fmin", 0.0)
                    ),
                    Field(
                        "fmax",
                        float,
                        20,
                        10,
                        kwargs.get("fmax", 0.0)
                    ),
                    Field(
                        "ftied ",
                        float,
                        30,
                        10,
                        kwargs.get("ftied ", 0.0)
                    ),
                    Field(
                        "conv",
                        float,
                        40,
                        10,
                        kwargs.get("conv", 1.0E-4)
                    ),
                    Field(
                        "conmax",
                        float,
                        50,
                        10,
                        kwargs.get("conmax", 1.0E20)
                    ),
                    Field(
                        "eterm",
                        float,
                        60,
                        10,
                        kwargs.get("eterm", 0.0)
                    ),
                    Field(
                        "therm",
                        float,
                        70,
                        10,
                        kwargs.get("therm", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "etfag",
                        int,
                        0,
                        10,
                        kwargs.get("etfag", 0)
                    ),
                ],
            ),
        ]

    @property
    def atype(self) -> int:
        """Get or set the Analysis type for pore water pressure calculations:
        EQ.0:  No pore water pressure calculation.
        EQ.1:  Undrained analysis,
        EQ.2:  Drained analysis,
        EQ.3:  Time dependent consolidation (coupled)
        EQ.4:  Consolidate to steady state (uncoupled)
        EQ.5:  Drained in dynamic relaxation, undrained in transient.
        EQ.6:  As 4 but do not check convergence, continue to end time .
        """ # nopep8
        return self._cards[0].get_value("atype")

    @atype.setter
    def atype(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6]:
            raise Exception("""atype must be one of {0,1,2,3,4,5,6}""")
        self._cards[0].set_value("atype", value)

    @property
    def wtable(self) -> float:
        """Get or set the Default z-coordinate of water table (where pore pressure is zero).
        """ # nopep8
        return self._cards[0].get_value("wtable")

    @wtable.setter
    def wtable(self, value: float) -> None:
        self._cards[0].set_value("wtable", value)

    @property
    def pf_rho(self) -> typing.Optional[float]:
        """Get or set the Default density for pore water.
        """ # nopep8
        return self._cards[0].get_value("pf_rho")

    @pf_rho.setter
    def pf_rho(self, value: float) -> None:
        self._cards[0].set_value("pf_rho", value)

    @property
    def grav(self) -> typing.Optional[float]:
        """Get or set the Gravitational acceleration used to calculate hydrostatic pore water pressure.
        """ # nopep8
        return self._cards[0].get_value("grav")

    @grav.setter
    def grav(self, value: float) -> None:
        self._cards[0].set_value("grav", value)

    @property
    def pf_bulk(self) -> typing.Optional[float]:
        """Get or set the Default bulk modulus of pore fluid (stress units).
        """ # nopep8
        return self._cards[0].get_value("pf_bulk")

    @pf_bulk.setter
    def pf_bulk(self, value: float) -> None:
        self._cards[0].set_value("pf_bulk", value)

    @property
    def output(self) -> int:
        """Get or set the Output flag controlling stresses to D3PLOT and D3THDT binary files:
        EQ.0:  total stresses are output
        EQ.1:  effective stresses are output, see notes
        """ # nopep8
        return self._cards[0].get_value("output")

    @output.setter
    def output(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""output must be one of {0,1}""")
        self._cards[0].set_value("output", value)

    @property
    def tmf(self) -> float:
        """Get or set the Initial Time Magnification factor on seepage (ATYPE=3,4 only).
        GT.0:   Factor (can be used with automatic control, see TARG, FMIN, FMAX).
        LT.0:  Load Curve ID (see *DEFINE_CURVE) giving Time Magnification Factor versus analysis time.
        """ # nopep8
        return self._cards[0].get_value("tmf")

    @tmf.setter
    def tmf(self, value: float) -> None:
        self._cards[0].set_value("tmf", value)

    @property
    def targ(self) -> float:
        """Get or set the Target for maximum change of excess pore pressure at any node, per timestep. If the actual change falls below the target, the time factor on the seepage calculation will be increased (see notes). If zero, the constant value of TMF is used. If non-zero, TMF is taken as the initial factor. .
        """ # nopep8
        return self._cards[1].get_value("targ")

    @targ.setter
    def targ(self, value: float) -> None:
        self._cards[1].set_value("targ", value)

    @property
    def fmin(self) -> float:
        """Get or set the Minimum time factor on seepage calculation
        """ # nopep8
        return self._cards[1].get_value("fmin")

    @fmin.setter
    def fmin(self, value: float) -> None:
        self._cards[1].set_value("fmin", value)

    @property
    def fmax(self) -> float:
        """Get or set the Maximum time factor on seepage calculation
        """ # nopep8
        return self._cards[1].get_value("fmax")

    @fmax.setter
    def fmax(self, value: float) -> None:
        self._cards[1].set_value("fmax", value)

    @property
    def ftied_(self) -> float:
        """Get or set the Analysis type for pore water pressure calculations (see Remark 1):
        EQ.0.0:	Tied contacts act as impermeable membranes,
        EQ.1.0 : Fluid may flow freely through tied contacts.
        """ # nopep8
        return self._cards[1].get_value("ftied ")

    @ftied_.setter
    def ftied_(self, value: float) -> None:
        self._cards[1].set_value("ftied ", value)

    @property
    def conv(self) -> float:
        """Get or set the Convergence tolerance for ATYPE=4 - maximum head change per timestep at any node (length units)
        """ # nopep8
        return self._cards[1].get_value("conv")

    @conv.setter
    def conv(self, value: float) -> None:
        self._cards[1].set_value("conv", value)

    @property
    def conmax(self) -> float:
        """Get or set the Maximum factor on permeability with ATYPE=-4
        """ # nopep8
        return self._cards[1].get_value("conmax")

    @conmax.setter
    def conmax(self, value: float) -> None:
        self._cards[1].set_value("conmax", value)

    @property
    def eterm(self) -> float:
        """Get or set the Event time termination (ATYPE=3)
        """ # nopep8
        return self._cards[1].get_value("eterm")

    @eterm.setter
    def eterm(self, value: float) -> None:
        self._cards[1].set_value("eterm", value)

    @property
    def therm(self) -> float:
        """Get or set the Thermal expansion:  Volumetric strain per degree increase for undrained soil
        """ # nopep8
        return self._cards[1].get_value("therm")

    @therm.setter
    def therm(self, value: float) -> None:
        self._cards[1].set_value("therm", value)

    @property
    def etfag(self) -> int:
        """Get or set the Flag for interpretation of time (see Time Factoring):
        EQ.0:	Time means analysis time,
        EQ.1 : Time means event time..
        """ # nopep8
        return self._cards[2].get_value("etfag")

    @etfag.setter
    def etfag(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""etfag must be one of {0,1}""")
        self._cards[2].set_value("etfag", value)

