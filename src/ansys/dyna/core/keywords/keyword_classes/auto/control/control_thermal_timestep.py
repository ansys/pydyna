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

"""Module providing the ControlThermalTimestep class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_CONTROLTHERMALTIMESTEP_CARD0 = (
    FieldSchema("ts", int, 0, 10, 0),
    FieldSchema("tip", float, 10, 10, 0.5),
    FieldSchema("its", float, 20, 10, None),
    FieldSchema("tmin", float, 30, 10, None),
    FieldSchema("tmax", float, 40, 10, None),
    FieldSchema("dtemp", float, 50, 10, 1.0),
    FieldSchema("tscp", float, 60, 10, 0.5),
    FieldSchema("lcts", int, 70, 10, None),
)

class ControlThermalTimestep(KeywordBase):
    """DYNA CONTROL_THERMAL_TIMESTEP keyword"""

    keyword = "CONTROL"
    subkeyword = "THERMAL_TIMESTEP"
    _link_fields = {
        "lcts": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the ControlThermalTimestep class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLTHERMALTIMESTEP_CARD0,
                **kwargs,
            ),        ]
    @property
    def ts(self) -> int:
        """Get or set the Time step control:
        EQ.0: fixed time step (default),
        EQ.1: variable time step (may increase or decrease).
        """ # nopep8
        return self._cards[0].get_value("ts")

    @ts.setter
    def ts(self, value: int) -> None:
        """Set the ts property."""
        self._cards[0].set_value("ts", value)

    @property
    def tip(self) -> float:
        """Get or set the Time integration parameter:
        Default is 0.5 - Crank-Nicholson scheme (default),
        EQ 1.0: fully implicit.
        """ # nopep8
        return self._cards[0].get_value("tip")

    @tip.setter
    def tip(self, value: float) -> None:
        """Set the tip property."""
        self._cards[0].set_value("tip", value)

    @property
    def its(self) -> typing.Optional[float]:
        """Get or set the Initial thermal time step
        """ # nopep8
        return self._cards[0].get_value("its")

    @its.setter
    def its(self, value: float) -> None:
        """Set the its property."""
        self._cards[0].set_value("its", value)

    @property
    def tmin(self) -> typing.Optional[float]:
        """Get or set the Minimum thermal time step:
        EQ.0.0. Set to structural explicit timestep.
        LT.0.0: curve ID=(-TMIN) gives minimum thermal time step size as function of time.The load curve defines pairs(thermal time breakpoint, new minimum time step).
        """ # nopep8
        return self._cards[0].get_value("tmin")

    @tmin.setter
    def tmin(self, value: float) -> None:
        """Set the tmin property."""
        self._cards[0].set_value("tmin", value)

    @property
    def tmax(self) -> typing.Optional[float]:
        """Get or set the Maximum thermal time step:
        EQ.0.0: Set to 100 * structural explicit timestep.
        LT.0.0: curve ID=(-TMAX) gives maximum thermal time step size as function of time.The load curve defines pairs(thermal time breakpoint,new maximum time step).The time step will be adjusted to hit the time breakpoints exactly.
        """ # nopep8
        return self._cards[0].get_value("tmax")

    @tmax.setter
    def tmax(self, value: float) -> None:
        """Set the tmax property."""
        self._cards[0].set_value("tmax", value)

    @property
    def dtemp(self) -> float:
        """Get or set the Maximum temperature change in each time step above which the thermal timestep will be decreased
        EQ.0.0: set to a temperature change of 1.0.
        LT.0.0: curve ID=(-DTEMP) gives maximum temperature change as function of time.The load curve defines pairs(thermal time breakpoint,new temperature change).
        """ # nopep8
        return self._cards[0].get_value("dtemp")

    @dtemp.setter
    def dtemp(self, value: float) -> None:
        """Set the dtemp property."""
        self._cards[0].set_value("dtemp", value)

    @property
    def tscp(self) -> float:
        """Get or set the Time step control parameter. The thermal time step is decreased by this factor if convergence is not obtained. 0.0 < TSCP < 1.0:
        Default value is 0.5.
        """ # nopep8
        return self._cards[0].get_value("tscp")

    @tscp.setter
    def tscp(self, value: float) -> None:
        """Set the tscp property."""
        self._cards[0].set_value("tscp", value)

    @property
    def lcts(self) -> typing.Optional[int]:
        """Get or set the LCTS designates a load curve number which defines the thermal time step as a function of time. If LCTS is defined, then the other time step control parameters on this keyword are ignored.
        """ # nopep8
        return self._cards[0].get_value("lcts")

    @lcts.setter
    def lcts(self, value: int) -> None:
        """Set the lcts property."""
        self._cards[0].set_value("lcts", value)

    @property
    def lcts_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcts."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcts:
                return kwd
        return None

    @lcts_link.setter
    def lcts_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcts."""
        self.lcts = value.lcid

