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

"""Module providing the IcfdControlTime class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_ICFDCONTROLTIME_CARD0 = (
    FieldSchema("ttm", float, 0, 10, 1e+28),
    FieldSchema("dt", float, 10, 10, 0.0),
    FieldSchema("cfl", float, 20, 10, 1.0),
    FieldSchema("lcidsf", int, 30, 10, None),
    FieldSchema("dtmin", float, 40, 10, None),
    FieldSchema("dtmax", float, 50, 10, None),
    FieldSchema("dtinit", float, 60, 10, None),
    FieldSchema("tdeath", float, 70, 10, 1e+28),
)

_ICFDCONTROLTIME_CARD1 = (
    FieldSchema("dtt", float, 0, 10, None),
)

_ICFDCONTROLTIME_CARD2 = (
    FieldSchema("btbl", int, 0, 10, 0),
)

class IcfdControlTime(KeywordBase):
    """DYNA ICFD_CONTROL_TIME keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_TIME"
    _link_fields = {
        "lcidsf": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the IcfdControlTime class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLTIME_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLTIME_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLTIME_CARD2,
                **kwargs,
            ),        ]
    @property
    def ttm(self) -> float:
        """Get or set the Total time of simulation for the fluid problem.
        """ # nopep8
        return self._cards[0].get_value("ttm")

    @ttm.setter
    def ttm(self, value: float) -> None:
        """Set the ttm property."""
        self._cards[0].set_value("ttm", value)

    @property
    def dt(self) -> float:
        """Get or set the Time step for the fluid problem. If different than zero the time step will be set constant and equal to this value. If DT = 0 then the time step is automatically computed.
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        """Set the dt property."""
        self._cards[0].set_value("dt", value)

    @property
    def cfl(self) -> float:
        """Get or set the Scale factor that multplies DT.
        """ # nopep8
        return self._cards[0].get_value("cfl")

    @cfl.setter
    def cfl(self, value: float) -> None:
        """Set the cfl property."""
        self._cards[0].set_value("cfl", value)

    @property
    def lcidsf(self) -> typing.Optional[int]:
        """Get or set the Load Curve ID specifying the CFL number when DT = 0 as a function of time, and more generally LCIDSF specifies the time step scale factor as the function of time.
        """ # nopep8
        return self._cards[0].get_value("lcidsf")

    @lcidsf.setter
    def lcidsf(self, value: int) -> None:
        """Set the lcidsf property."""
        self._cards[0].set_value("lcidsf", value)

    @property
    def dtmin(self) -> typing.Optional[float]:
        """Get or set the Minimum time step. When an automatic time step is used and DTMIN is defined, the time step cannot adopt a smaller value than DTMIN.A negative value will refer to a time dependent load curve.
        """ # nopep8
        return self._cards[0].get_value("dtmin")

    @dtmin.setter
    def dtmin(self, value: float) -> None:
        """Set the dtmin property."""
        self._cards[0].set_value("dtmin", value)

    @property
    def dtmax(self) -> typing.Optional[float]:
        """Get or set the Maximum time step. When an automatic time step is used and DTMAX is defined, the time step cannot adopt a higher value than DTMAX.. A negative value will refer to a time dependent load curve.
        """ # nopep8
        return self._cards[0].get_value("dtmax")

    @dtmax.setter
    def dtmax(self, value: float) -> None:
        """Set the dtmax property."""
        self._cards[0].set_value("dtmax", value)

    @property
    def dtinit(self) -> typing.Optional[float]:
        """Get or set the Initial time step. If not defined, the solver will automatically determine an initial timestep based on the flow velocity or dimensions of the problem in cases where there is no inflow.
        """ # nopep8
        return self._cards[0].get_value("dtinit")

    @dtinit.setter
    def dtinit(self, value: float) -> None:
        """Set the dtinit property."""
        self._cards[0].set_value("dtinit", value)

    @property
    def tdeath(self) -> float:
        """Get or set the Death time for the Navier Stokes solve. After TDEATH, the velocity and pressure will no longer be updated. But the temperature and other similar quantities still can.
        """ # nopep8
        return self._cards[0].get_value("tdeath")

    @tdeath.setter
    def tdeath(self, value: float) -> None:
        """Set the tdeath property."""
        self._cards[0].set_value("tdeath", value)

    @property
    def dtt(self) -> typing.Optional[float]:
        """Get or set the Thermal timestep
        """ # nopep8
        return self._cards[1].get_value("dtt")

    @dtt.setter
    def dtt(self, value: float) -> None:
        """Set the dtt property."""
        self._cards[1].set_value("dtt", value)

    @property
    def btbl(self) -> int:
        """Get or set the Flag to include boundary layer elements in the automatic timestep calculation.
        EQ.0:	Default.The boundary layer elements are excluded.
        EQ.1 : The boundary layer elements are included.
        """ # nopep8
        return self._cards[2].get_value("btbl")

    @btbl.setter
    def btbl(self, value: int) -> None:
        """Set the btbl property."""
        if value not in [0, 1, None]:
            raise Exception("""btbl must be `None` or one of {0,1}.""")
        self._cards[2].set_value("btbl", value)

    @property
    def lcidsf_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidsf."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidsf:
                return kwd
        return None

    @lcidsf_link.setter
    def lcidsf_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidsf."""
        self.lcidsf = value.lcid

