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

"""Module providing the DualceseBoundaryPrescribedSegmentSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DUALCESEBOUNDARYPRESCRIBEDSEGMENTSET_CARD0 = (
    FieldSchema("ssid", int, 0, 10, None),
    FieldSchema("idcomp", int, 10, 10, None),
)

_DUALCESEBOUNDARYPRESCRIBEDSEGMENTSET_CARD1 = (
    FieldSchema("lc_u", int, 0, 10, None),
    FieldSchema("lc_v", int, 10, 10, None),
    FieldSchema("lc_w", int, 20, 10, None),
    FieldSchema("lc_rho", int, 30, 10, None),
    FieldSchema("lc_p", int, 40, 10, None),
    FieldSchema("lc_t", int, 50, 10, None),
)

_DUALCESEBOUNDARYPRESCRIBEDSEGMENTSET_CARD2 = (
    FieldSchema("sf_u", float, 0, 10, 1.0),
    FieldSchema("sf_v", float, 10, 10, 1.0),
    FieldSchema("sf_w", float, 20, 10, 1.0),
    FieldSchema("sf_rho", float, 30, 10, 1.0),
    FieldSchema("sf_p", float, 40, 10, 1.0),
    FieldSchema("sf_t", float, 50, 10, 1.0),
)

class DualceseBoundaryPrescribedSegmentSet(KeywordBase):
    """DYNA DUALCESE_BOUNDARY_PRESCRIBED_SEGMENT_SET keyword"""

    keyword = "DUALCESE"
    subkeyword = "BOUNDARY_PRESCRIBED_SEGMENT_SET"

    def __init__(self, **kwargs):
        """Initialize the DualceseBoundaryPrescribedSegmentSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEBOUNDARYPRESCRIBEDSEGMENTSET_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DUALCESEBOUNDARYPRESCRIBEDSEGMENTSET_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DUALCESEBOUNDARYPRESCRIBEDSEGMENTSET_CARD2,
                **kwargs,
            ),        ]
    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID created with *DUALCESE_SEGMENTSET
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[0].set_value("ssid", value)

    @property
    def idcomp(self) -> typing.Optional[int]:
        """Get or set the For inflow boundaries in problems involving chemical reacting flows, the chemical mixture of the fluid entering the domain is defined with a *CHEMISTRY_?COMPOSITION card with this ID
        """ # nopep8
        return self._cards[0].get_value("idcomp")

    @idcomp.setter
    def idcomp(self, value: int) -> None:
        """Set the idcomp property."""
        self._cards[0].set_value("idcomp", value)

    @property
    def lc_u(self) -> typing.Optional[int]:
        """Get or set the Load curve ID (see *DEFINE_CURVE) to describe the  -component of the velocity as a function of time or function ID (see *DEFINE_FUNCTION) to give the  -component of the velocity as a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time).
        EQ.0:	 -component of velocity is a constant with value SF_U.
        EQ.-1:	 -component of velocity is computed by the solver
        """ # nopep8
        return self._cards[1].get_value("lc_u")

    @lc_u.setter
    def lc_u(self, value: int) -> None:
        """Set the lc_u property."""
        self._cards[1].set_value("lc_u", value)

    @property
    def lc_v(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the  -component of the velocity as a function of time or function ID to give the  -component of the velocity as a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time).
        EQ.0:	 -component of velocity is a constant with value SF_V.
        EQ.-1:	 -component of velocity is computed by the solver.
        """ # nopep8
        return self._cards[1].get_value("lc_v")

    @lc_v.setter
    def lc_v(self, value: int) -> None:
        """Set the lc_v property."""
        self._cards[1].set_value("lc_v", value)

    @property
    def lc_w(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the  -component of the velocity as a function of time or function ID to give the  -component of the velocity as a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time).
        EQ.0:	 -component of velocity is a constant with value SF_W.
        EQ.-1:	 -component of velocity is computed by the solver
        """ # nopep8
        return self._cards[1].get_value("lc_w")

    @lc_w.setter
    def lc_w(self, value: int) -> None:
        """Set the lc_w property."""
        self._cards[1].set_value("lc_w", value)

    @property
    def lc_rho(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the density as a function of time or function ID to give the density as a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time).
        EQ.0:	Density is a constant with value SF_RHO.
        EQ.-1:	Density is computed by the solver
        """ # nopep8
        return self._cards[1].get_value("lc_rho")

    @lc_rho.setter
    def lc_rho(self, value: int) -> None:
        """Set the lc_rho property."""
        self._cards[1].set_value("lc_rho", value)

    @property
    def lc_p(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the pressure as a function of time or function ID to give the pressure as a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time).
        EQ.0:	Pressure is a constant with value SF_P.
        EQ.-1:	Pressure is computed by the solver
        """ # nopep8
        return self._cards[1].get_value("lc_p")

    @lc_p.setter
    def lc_p(self, value: int) -> None:
        """Set the lc_p property."""
        self._cards[1].set_value("lc_p", value)

    @property
    def lc_t(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the temperature as a function of time or function ID to give the temperature as a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time).
        EQ.0:	Temperature is a constant with value SF_T.
        EQ.-1:	Temperature is computed by the solver
        """ # nopep8
        return self._cards[1].get_value("lc_t")

    @lc_t.setter
    def lc_t(self, value: int) -> None:
        """Set the lc_t property."""
        self._cards[1].set_value("lc_t", value)

    @property
    def sf_u(self) -> float:
        """Get or set the Scale factor for LC_U (default = 1.0).
        """ # nopep8
        return self._cards[2].get_value("sf_u")

    @sf_u.setter
    def sf_u(self, value: float) -> None:
        """Set the sf_u property."""
        self._cards[2].set_value("sf_u", value)

    @property
    def sf_v(self) -> float:
        """Get or set the Scale factor for LC_V (default = 1.0).
        """ # nopep8
        return self._cards[2].get_value("sf_v")

    @sf_v.setter
    def sf_v(self, value: float) -> None:
        """Set the sf_v property."""
        self._cards[2].set_value("sf_v", value)

    @property
    def sf_w(self) -> float:
        """Get or set the Scale factor for LC_W (default = 1.0).
        """ # nopep8
        return self._cards[2].get_value("sf_w")

    @sf_w.setter
    def sf_w(self, value: float) -> None:
        """Set the sf_w property."""
        self._cards[2].set_value("sf_w", value)

    @property
    def sf_rho(self) -> float:
        """Get or set the Scale factor for LC_RHO (default = 1.0).
        """ # nopep8
        return self._cards[2].get_value("sf_rho")

    @sf_rho.setter
    def sf_rho(self, value: float) -> None:
        """Set the sf_rho property."""
        self._cards[2].set_value("sf_rho", value)

    @property
    def sf_p(self) -> float:
        """Get or set the Scale factor for LC_P (default = 1.0).
        """ # nopep8
        return self._cards[2].get_value("sf_p")

    @sf_p.setter
    def sf_p(self, value: float) -> None:
        """Set the sf_p property."""
        self._cards[2].set_value("sf_p", value)

    @property
    def sf_t(self) -> float:
        """Get or set the Scale factor for LC_T (default = 1.0).
        """ # nopep8
        return self._cards[2].get_value("sf_t")

    @sf_t.setter
    def sf_t(self, value: float) -> None:
        """Set the sf_t property."""
        self._cards[2].set_value("sf_t", value)

