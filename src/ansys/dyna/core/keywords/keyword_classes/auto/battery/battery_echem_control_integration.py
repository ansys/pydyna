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

"""Module providing the BatteryEchemControlIntegration class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BATTERYECHEMCONTROLINTEGRATION_CARD0 = (
    FieldSchema("isolv", int, 0, 10, None),
    FieldSchema("rctol", float, 10, 10, 1e-07),
    FieldSchema("actol", float, 20, 10, 1e-10),
    FieldSchema("niter", float, 30, 10, 100.0),
    FieldSchema("omi", float, 40, 10, 0.9),
    FieldSchema("ommax", float, 50, 10, 1.0),
    FieldSchema("ommin", float, 60, 10, 0.1),
)

_BATTERYECHEMCONTROLINTEGRATION_CARD1 = (
    FieldSchema("eletip", int, 0, 10, 0),
    FieldSchema("radtip", int, 10, 10, 0),
    FieldSchema("ipor", int, 20, 10, 0),
    FieldSchema("vclamp", float, 30, 10, 0.02),
    FieldSchema("cclamp", float, 40, 10, 1e-06),
    FieldSchema("declamp", float, 50, 10, 2.0),
)

_BATTERYECHEMCONTROLINTEGRATION_CARD2 = (
    FieldSchema("maxdiv", int, 0, 10, 10),
    FieldSchema("dtgr", float, 10, 10, 1.1),
    FieldSchema("dtaut", int, 20, 10, 0),
    FieldSchema("dtmax", float, 30, 10, None),
    FieldSchema("dtemp", float, 40, 10, None),
)

class BatteryEchemControlIntegration(KeywordBase):
    """DYNA BATTERY_ECHEM_CONTROL_INTEGRATION keyword"""

    keyword = "BATTERY"
    subkeyword = "ECHEM_CONTROL_INTEGRATION"

    def __init__(self, **kwargs):
        """Initialize the BatteryEchemControlIntegration class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMCONTROLINTEGRATION_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMCONTROLINTEGRATION_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMCONTROLINTEGRATION_CARD2,
                **kwargs,
            ),
        ]
    @property
    def isolv(self) -> typing.Optional[int]:
        """Get or set the Band solver selection.
        EQ.0 : Custom block -tridiagonal algorithm with block pivoting(forward elimination + back - substitution, called per - node during assembly).
        EQ.1 : General banded LU factorization with full partial pivoting across the entire band,followed by triangular solve.Assembly stores all blocks first, then solves the global system in one call.More robust, small overhead.
        EQ.2 : Same LU factorization as EQ.1 with row / column equilibration(rescales badly conditioned systems) and iterative refinement of the solution.
        """ # nopep8
        return self._cards[0].get_value("isolv")

    @isolv.setter
    def isolv(self, value: int) -> None:
        """Set the isolv property."""
        self._cards[0].set_value("isolv", value)

    @property
    def rctol(self) -> float:
        """Get or set the Relative convergence criteria for Newton scheme.
        """ # nopep8
        return self._cards[0].get_value("rctol")

    @rctol.setter
    def rctol(self, value: float) -> None:
        """Set the rctol property."""
        self._cards[0].set_value("rctol", value)

    @property
    def actol(self) -> float:
        """Get or set the Absolute convergence tolerance for Newton scheme
        """ # nopep8
        return self._cards[0].get_value("actol")

    @actol.setter
    def actol(self, value: float) -> None:
        """Set the actol property."""
        self._cards[0].set_value("actol", value)

    @property
    def niter(self) -> float:
        """Get or set the Maximum number of Newton iterations
        """ # nopep8
        return self._cards[0].get_value("niter")

    @niter.setter
    def niter(self, value: float) -> None:
        """Set the niter property."""
        self._cards[0].set_value("niter", value)

    @property
    def omi(self) -> float:
        """Get or set the Initial Newton damping factor ?_init.
        """ # nopep8
        return self._cards[0].get_value("omi")

    @omi.setter
    def omi(self, value: float) -> None:
        """Set the omi property."""
        self._cards[0].set_value("omi", value)

    @property
    def ommax(self) -> float:
        """Get or set the Minimum Newton damping factor ?_min
        """ # nopep8
        return self._cards[0].get_value("ommax")

    @ommax.setter
    def ommax(self, value: float) -> None:
        """Set the ommax property."""
        self._cards[0].set_value("ommax", value)

    @property
    def ommin(self) -> float:
        """Get or set the Maximum Newton damping factor ?_max
        """ # nopep8
        return self._cards[0].get_value("ommin")

    @ommin.setter
    def ommin(self, value: float) -> None:
        """Set the ommin property."""
        self._cards[0].set_value("ommin", value)

    @property
    def eletip(self) -> int:
        """Get or set the Time Integration Parameter for Electrolyte Material Balance equation
        EQ.0 : Backward Euler
        EQ.1 : Crank - Nicholson
        """ # nopep8
        return self._cards[1].get_value("eletip")

    @eletip.setter
    def eletip(self, value: int) -> None:
        """Set the eletip property."""
        if value not in [0, 1, None]:
            raise Exception("""eletip must be `None` or one of {0,1}.""")
        self._cards[1].set_value("eletip", value)

    @property
    def radtip(self) -> int:
        """Get or set the Radial time integration for the Solid Phase diffusion equation.
        EQ.0:	TR-BDF2:	Two-stage scheme: a trapezoidal rule half, followed by a BDF2 step from with fixed sub-step ratio. L-stable (important for very stiff systems), second order, self-starting but adds some computational cost
        EQ.1:	Standard BDF2:	Falls back to BDF1 (backward Euler) for the first step. A-stable, second order integration.
        """ # nopep8
        return self._cards[1].get_value("radtip")

    @radtip.setter
    def radtip(self, value: int) -> None:
        """Set the radtip property."""
        if value not in [0, 1, None]:
            raise Exception("""radtip must be `None` or one of {0,1}.""")
        self._cards[1].set_value("radtip", value)

    @property
    def ipor(self) -> int:
        """Get or set the Porosity Feedback when ten equation SEI Aging model selected
        EQ.0 : Off
        EQ.1 : On
        """ # nopep8
        return self._cards[1].get_value("ipor")

    @ipor.setter
    def ipor(self, value: int) -> None:
        """Set the ipor property."""
        if value not in [0, 1, None]:
            raise Exception("""ipor must be `None` or one of {0,1}.""")
        self._cards[1].set_value("ipor", value)

    @property
    def vclamp(self) -> float:
        """Get or set the Voltage clamping.
        """ # nopep8
        return self._cards[1].get_value("vclamp")

    @vclamp.setter
    def vclamp(self, value: float) -> None:
        """Set the vclamp property."""
        self._cards[1].set_value("vclamp", value)

    @property
    def cclamp(self) -> float:
        """Get or set the stoichiometry clamp ? c?_clamp
        """ # nopep8
        return self._cards[1].get_value("cclamp")

    @cclamp.setter
    def cclamp(self, value: float) -> None:
        """Set the cclamp property."""
        self._cards[1].set_value("cclamp", value)

    @property
    def declamp(self) -> float:
        """Get or set the overpotential clamp ? ??_clamp
        """ # nopep8
        return self._cards[1].get_value("declamp")

    @declamp.setter
    def declamp(self, value: float) -> None:
        """Set the declamp property."""
        self._cards[1].set_value("declamp", value)

    @property
    def maxdiv(self) -> int:
        """Get or set the Maximum number of time step divisions allowed before giving up when subcycling is active.
        """ # nopep8
        return self._cards[2].get_value("maxdiv")

    @maxdiv.setter
    def maxdiv(self, value: int) -> None:
        """Set the maxdiv property."""
        self._cards[2].set_value("maxdiv", value)

    @property
    def dtgr(self) -> float:
        """Get or set the Timestep growth factor after halving has occurred and convergence has been achieved when subcycling is active.
        """ # nopep8
        return self._cards[2].get_value("dtgr")

    @dtgr.setter
    def dtgr(self, value: float) -> None:
        """Set the dtgr property."""
        self._cards[2].set_value("dtgr", value)

    @property
    def dtaut(self) -> int:
        """Get or set the Subcyling timestep adaptation.
        EQ.0:	Timestep for the P2D solve is allowed to grow (using DTGR) or be reduced in case of non-convergence.
        EQ.1:	Fixed timestep.
        """ # nopep8
        return self._cards[2].get_value("dtaut")

    @dtaut.setter
    def dtaut(self, value: int) -> None:
        """Set the dtaut property."""
        if value not in [0, 1, None]:
            raise Exception("""dtaut must be `None` or one of {0,1}.""")
        self._cards[2].set_value("dtaut", value)

    @property
    def dtmax(self) -> typing.Optional[float]:
        """Get or set the Maximum timestep.
        """ # nopep8
        return self._cards[2].get_value("dtmax")

    @dtmax.setter
    def dtmax(self, value: float) -> None:
        """Set the dtmax property."""
        self._cards[2].set_value("dtmax", value)

    @property
    def dtemp(self) -> typing.Optional[float]:
        """Get or set the Maximum temperature change that can occur between two P2D timesteps (See ITYPE=1 in *BATTERY_ECHEM_THERMAL). Mostly useful when thermal runaway laws defined (See *BATTERY_ECHEM_MAT_THERMAL_ABUSE).
        """ # nopep8
        return self._cards[2].get_value("dtemp")

    @dtemp.setter
    def dtemp(self, value: float) -> None:
        """Set the dtemp property."""
        self._cards[2].set_value("dtemp", value)

