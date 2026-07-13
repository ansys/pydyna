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

"""Module providing the AirbagFluidAndGas class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_AIRBAGFLUIDANDGAS_CARD0 = (
    FieldSchema("xwini", float, 0, 10, None),
    FieldSchema("xwadd", float, 10, 10, None),
    FieldSchema("xw", float, 20, 10, None),
    FieldSchema("p", float, 30, 10, None),
    FieldSchema("tend", float, 40, 10, None),
    FieldSchema("rho", float, 50, 10, None),
    FieldSchema("lcxw", int, 60, 10, None),
    FieldSchema("lcp", int, 70, 10, None),
)

_AIRBAGFLUIDANDGAS_CARD1 = (
    FieldSchema("gdir", float, 0, 10, None),
    FieldSchema("nproj", int, 10, 10, 3),
    FieldSchema("idir", int, 20, 10, None),
    FieldSchema("iidir", int, 30, 10, None),
    FieldSchema("kappa", float, 40, 10, 1.0),
    FieldSchema("kbm", float, 50, 10, None),
    FieldSchema("grav", float, 60, 10, 10.0),
)

class AirbagFluidAndGas(KeywordBase):
    """DYNA AIRBAG_FLUID_AND_GAS keyword"""

    keyword = "AIRBAG"
    subkeyword = "FLUID_AND_GAS"
    _link_fields = {
        "lcxw": LinkType.DEFINE_CURVE,
        "lcp": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the AirbagFluidAndGas class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _AIRBAGFLUIDANDGAS_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _AIRBAGFLUIDANDGAS_CARD1,
                **kwargs,
            ),
        ]
    @property
    def xwini(self) -> typing.Optional[float]:
        """Get or set the Fluid level at time t = 0 in |GDIR| direction.
        """ # nopep8
        return self._cards[0].get_value("xwini")

    @xwini.setter
    def xwini(self, value: float) -> None:
        """Set the xwini property."""
        self._cards[0].set_value("xwini", value)

    @property
    def xwadd(self) -> typing.Optional[float]:
        """Get or set the Fluid level filling increment per time step.
        """ # nopep8
        return self._cards[0].get_value("xwadd")

    @xwadd.setter
    def xwadd(self, value: float) -> None:
        """Set the xwadd property."""
        self._cards[0].set_value("xwadd", value)

    @property
    def xw(self) -> typing.Optional[float]:
        """Get or set the Final fluid level in filling process.
        """ # nopep8
        return self._cards[0].get_value("xw")

    @xw.setter
    def xw(self, value: float) -> None:
        """Set the xw property."""
        self._cards[0].set_value("xw", value)

    @property
    def p(self) -> typing.Optional[float]:
        """Get or set the Gas pressure at time t = TEND.
        """ # nopep8
        return self._cards[0].get_value("p")

    @p.setter
    def p(self, value: float) -> None:
        """Set the p property."""
        self._cards[0].set_value("p", value)

    @property
    def tend(self) -> typing.Optional[float]:
        """Get or set the Time when gas pressure P is reached.
        """ # nopep8
        return self._cards[0].get_value("tend")

    @tend.setter
    def tend(self, value: float) -> None:
        """Set the tend property."""
        self._cards[0].set_value("tend", value)

    @property
    def rho(self) -> typing.Optional[float]:
        """Get or set the Density of the fluid (e.g. for water, RHO is about 1000.0 kg/m3).
        """ # nopep8
        return self._cards[0].get_value("rho")

    @rho.setter
    def rho(self, value: float) -> None:
        """Set the rho property."""
        self._cards[0].set_value("rho", value)

    @property
    def lcxw(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for fluid level vs. time. XW, XWADD, and XWINI are with this option.
        """ # nopep8
        return self._cards[0].get_value("lcxw")

    @lcxw.setter
    def lcxw(self, value: int) -> None:
        """Set the lcxw property."""
        self._cards[0].set_value("lcxw", value)

    @property
    def lcp(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for gas pressure vs. time. P and TEND are ignored with this option.
        """ # nopep8
        return self._cards[0].get_value("lcp")

    @lcp.setter
    def lcp(self, value: int) -> None:
        """Set the lcp property."""
        self._cards[0].set_value("lcp", value)

    @property
    def gdir(self) -> typing.Optional[float]:
        """Get or set the Global direction of gravity (e.g. -3.0 for negative global z-axis).
        EQ.1.0: global x-direction,
        EQ.2.0: global y-direction,
        EQ.3.0: global z-direction.
        """ # nopep8
        return self._cards[1].get_value("gdir")

    @gdir.setter
    def gdir(self, value: float) -> None:
        """Set the gdir property."""
        self._cards[1].set_value("gdir", value)

    @property
    def nproj(self) -> int:
        """Get or set the Number of projection directions (only global axis) for volume calculation.
        """ # nopep8
        return self._cards[1].get_value("nproj")

    @nproj.setter
    def nproj(self, value: int) -> None:
        """Set the nproj property."""
        self._cards[1].set_value("nproj", value)

    @property
    def idir(self) -> typing.Optional[int]:
        """Get or set the First direction of projection (if |NPROJ| != 3), only global axis.
        """ # nopep8
        return self._cards[1].get_value("idir")

    @idir.setter
    def idir(self, value: int) -> None:
        """Set the idir property."""
        self._cards[1].set_value("idir", value)

    @property
    def iidir(self) -> typing.Optional[int]:
        """Get or set the Second direction of projection (if |NPROJ| = 2), only global axis.
        """ # nopep8
        return self._cards[1].get_value("iidir")

    @iidir.setter
    def iidir(self, value: int) -> None:
        """Set the iidir property."""
        self._cards[1].set_value("iidir", value)

    @property
    def kappa(self) -> float:
        """Get or set the Adiabatic exponent.
        """ # nopep8
        return self._cards[1].get_value("kappa")

    @kappa.setter
    def kappa(self, value: float) -> None:
        """Set the kappa property."""
        self._cards[1].set_value("kappa", value)

    @property
    def kbm(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus of the fluid (e.g. for water, BKM is about 2080 N/mm2).
        """ # nopep8
        return self._cards[1].get_value("kbm")

    @kbm.setter
    def kbm(self, value: float) -> None:
        """Set the kbm property."""
        self._cards[1].set_value("kbm", value)

    @property
    def grav(self) -> float:
        """Get or set the Gravity (default is 10.0 which is only correct when using m/s2)
        """ # nopep8
        return self._cards[1].get_value("grav")

    @grav.setter
    def grav(self, value: float) -> None:
        """Set the grav property."""
        self._cards[1].set_value("grav", value)

    @property
    def lcxw_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcxw."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcxw:
                return kwd
        return None

    @lcxw_link.setter
    def lcxw_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcxw."""
        self.lcxw = value.lcid

    @property
    def lcp_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcp."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcp:
                return kwd
        return None

    @lcp_link.setter
    def lcp_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcp."""
        self.lcp = value.lcid

