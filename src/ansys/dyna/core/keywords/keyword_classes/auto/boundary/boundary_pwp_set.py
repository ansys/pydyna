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

"""Module providing the BoundaryPwpSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_BOUNDARYPWPSET_CARD0 = (
    FieldSchema("sid", int, 0, 10, None),
    FieldSchema("lc", int, 10, 10, None),
    FieldSchema("cmult", float, 20, 10, 0.0),
    FieldSchema("lcdr", int, 30, 10, None),
    FieldSchema("tbirth", float, 40, 10, 0.0),
    FieldSchema("tdeath", float, 50, 10, 1e+20),
)

_BOUNDARYPWPSET_CARD1 = (
    FieldSchema("iphre", int, 0, 10, 0),
    FieldSchema("itotex", int, 10, 10, 0),
    FieldSchema("idrflag", int, 20, 10, 0),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("lcleak", int, 40, 10, None),
    FieldSchema("cleak", float, 50, 10, None),
    FieldSchema("lcpum", int, 60, 10, None),
)

class BoundaryPwpSet(KeywordBase):
    """DYNA BOUNDARY_PWP_SET keyword"""

    keyword = "BOUNDARY"
    subkeyword = "PWP_SET"
    _link_fields = {
        "lcdr": LinkType.DEFINE_CURVE,
        "lcleak": LinkType.DEFINE_CURVE,
        "lcpum": LinkType.DEFINE_CURVE,
        "sid": LinkType.SET_NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the BoundaryPwpSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYPWPSET_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _BOUNDARYPWPSET_CARD1,
                **kwargs,
            ),        ]
    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Node SET ID.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def lc(self) -> typing.Optional[int]:
        """Get or set the Load curve giving pore water pressure head (length units) vs time. =0: constant pressure head assumed equal to CMULT(leave blank for TABLE option)
        """ # nopep8
        return self._cards[0].get_value("lc")

    @lc.setter
    def lc(self, value: int) -> None:
        """Set the lc property."""
        self._cards[0].set_value("lc", value)

    @property
    def cmult(self) -> float:
        """Get or set the Factor on curve or constant pressure head if LC=0
        """ # nopep8
        return self._cards[0].get_value("cmult")

    @cmult.setter
    def cmult(self, value: float) -> None:
        """Set the cmult property."""
        self._cards[0].set_value("cmult", value)

    @property
    def lcdr(self) -> typing.Optional[int]:
        """Get or set the Load curve giving pore water pressure head during dynamic relaxation.
        """ # nopep8
        return self._cards[0].get_value("lcdr")

    @lcdr.setter
    def lcdr(self, value: int) -> None:
        """Set the lcdr property."""
        self._cards[0].set_value("lcdr", value)

    @property
    def tbirth(self) -> float:
        """Get or set the Time at which boundary condition becomes active
        """ # nopep8
        return self._cards[0].get_value("tbirth")

    @tbirth.setter
    def tbirth(self, value: float) -> None:
        """Set the tbirth property."""
        self._cards[0].set_value("tbirth", value)

    @property
    def tdeath(self) -> float:
        """Get or set the Time at which boundary condition becomes inactive
        """ # nopep8
        return self._cards[0].get_value("tdeath")

    @tdeath.setter
    def tdeath(self, value: float) -> None:
        """Set the tdeath property."""
        self._cards[0].set_value("tdeath", value)

    @property
    def iphre(self) -> int:
        """Get or set the Flag =1 for phreatic behaviour (water can be removed by the boundary condition but not added, e.g. at a sloping free surface). Not applicable to TABLE option.
        """ # nopep8
        return self._cards[1].get_value("iphre")

    @iphre.setter
    def iphre(self, value: int) -> None:
        """Set the iphre property."""
        self._cards[1].set_value("iphre", value)

    @property
    def itotex(self) -> int:
        """Get or set the Flag for type of pressure boundary condition: (see notes)
        =0: 	Total head
        =1: 	Excess head
        =2:	Hydraulic head
        =4:	Z-coord where head=0 (piezometric level)
        """ # nopep8
        return self._cards[1].get_value("itotex")

    @itotex.setter
    def itotex(self, value: int) -> None:
        """Set the itotex property."""
        if value not in [0, 1, 2, 4, None]:
            raise Exception("""itotex must be `None` or one of {0,1,2,4}.""")
        self._cards[1].set_value("itotex", value)

    @property
    def idrflag(self) -> int:
        """Get or set the Active flag:
        =0:	Active only in transient analysis
        =1:	Active only in dynamic relaxation
        =2:	Active in all analysis phases(leave blank for TABLE option)
        """ # nopep8
        return self._cards[1].get_value("idrflag")

    @idrflag.setter
    def idrflag(self, value: int) -> None:
        """Set the idrflag property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""idrflag must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("idrflag", value)

    @property
    def lcleak(self) -> typing.Optional[int]:
        """Get or set the Optional load curve ID (see *DEFINE_CURVE) applicable to IPHRE = 1 only, giving area of the hole through which pore fluid leaks to the zero pressure boundary condition. See Remark 9.
        """ # nopep8
        return self._cards[1].get_value("lcleak")

    @lcleak.setter
    def lcleak(self, value: int) -> None:
        """Set the lcleak property."""
        self._cards[1].set_value("lcleak", value)

    @property
    def cleak(self) -> typing.Optional[float]:
        """Get or set the Discharge coefficient, applicable when LCLEAK is nonzero
        """ # nopep8
        return self._cards[1].get_value("cleak")

    @cleak.setter
    def cleak(self, value: float) -> None:
        """Set the cleak property."""
        self._cards[1].set_value("cleak", value)

    @property
    def lcpum(self) -> typing.Optional[int]:
        """Get or set the Optional load curve ID (see *DEFINE_CURVE) giving volumetric outflow rate per node. The curve x-axis is time while the y-axis is in units of volume per unit time. If defined, LCPUMP overrides all other input fields on Card 2.  See Remark 11
        """ # nopep8
        return self._cards[1].get_value("lcpum")

    @lcpum.setter
    def lcpum(self, value: int) -> None:
        """Set the lcpum property."""
        self._cards[1].set_value("lcpum", value)

    @property
    def lcdr_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcdr."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcdr:
                return kwd
        return None

    @lcdr_link.setter
    def lcdr_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcdr."""
        self.lcdr = value.lcid

    @property
    def lcleak_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcleak."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcleak:
                return kwd
        return None

    @lcleak_link.setter
    def lcleak_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcleak."""
        self.lcleak = value.lcid

    @property
    def lcpum_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcpum."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcpum:
                return kwd
        return None

    @lcpum_link.setter
    def lcpum_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcpum."""
        self.lcpum = value.lcid

    @property
    def sid_link(self) -> KeywordBase:
        """Get the SET_NODE_* keyword for sid."""
        return self._get_set_link("NODE", self.sid)

    @sid_link.setter
    def sid_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for sid."""
        self.sid = value.sid

