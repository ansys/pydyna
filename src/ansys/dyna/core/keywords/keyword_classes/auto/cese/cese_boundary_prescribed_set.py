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

"""Module providing the CeseBoundaryPrescribedSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_CESEBOUNDARYPRESCRIBEDSET_CARD0 = (
    FieldSchema("ssid", int, 0, 10, None),
    FieldSchema("idcomp", int, 10, 10, None),
)

_CESEBOUNDARYPRESCRIBEDSET_CARD1 = (
    FieldSchema("lc_u", int, 0, 10, None),
    FieldSchema("lc_v_", int, 10, 10, None, "lc_v "),
    FieldSchema("lc_w", int, 20, 10, None),
    FieldSchema("lc_rho", int, 30, 10, None),
    FieldSchema("lc_p_", int, 40, 10, None, "lc_p "),
    FieldSchema("lc_t", int, 50, 10, None),
)

_CESEBOUNDARYPRESCRIBEDSET_CARD2 = (
    FieldSchema("sf_u", float, 0, 10, 1.0),
    FieldSchema("sf_v_", float, 10, 10, 1.0, "sf_v "),
    FieldSchema("sf_w", float, 20, 10, 1.0),
    FieldSchema("sf_rho", float, 30, 10, 1.0),
    FieldSchema("sf_p_", float, 40, 10, 1.0, "sf_p "),
    FieldSchema("sf_t", float, 50, 10, 1.0),
)

class CeseBoundaryPrescribedSet(KeywordBase):
    """DYNA CESE_BOUNDARY_PRESCRIBED_SET keyword"""

    keyword = "CESE"
    subkeyword = "BOUNDARY_PRESCRIBED_SET"
    _link_fields = {
        "lc_u": LinkType.DEFINE_CURVE,
        "lc_v_": LinkType.DEFINE_CURVE,
        "lc_w": LinkType.DEFINE_CURVE,
        "lc_rho": LinkType.DEFINE_CURVE,
        "lc_p_": LinkType.DEFINE_CURVE,
        "lc_t": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the CeseBoundaryPrescribedSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CESEBOUNDARYPRESCRIBEDSET_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CESEBOUNDARYPRESCRIBEDSET_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CESEBOUNDARYPRESCRIBEDSET_CARD2,
                **kwargs,
            ),        ]
    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Segment set  ID.
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[0].set_value("ssid", value)

    @property
    def idcomp(self) -> typing.Optional[int]:
        """Get or set the For inflow boundaries in problems involving chemical reacting flows, the chemical mixture of the fluid entering the domain as defined with a *CHEMISTRY_COMPOSITION card.
        """ # nopep8
        return self._cards[0].get_value("idcomp")

    @idcomp.setter
    def idcomp(self, value: int) -> None:
        """Set the idcomp property."""
        self._cards[0].set_value("idcomp", value)

    @property
    def lc_u(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the x-component of the velocity versus time
        """ # nopep8
        return self._cards[1].get_value("lc_u")

    @lc_u.setter
    def lc_u(self, value: int) -> None:
        """Set the lc_u property."""
        self._cards[1].set_value("lc_u", value)

    @property
    def lc_v_(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the y-component of the velocity versus time.
        """ # nopep8
        return self._cards[1].get_value("lc_v_")

    @lc_v_.setter
    def lc_v_(self, value: int) -> None:
        """Set the lc_v_ property."""
        self._cards[1].set_value("lc_v_", value)

    @property
    def lc_w(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the z-component of the velocity versus time.
        """ # nopep8
        return self._cards[1].get_value("lc_w")

    @lc_w.setter
    def lc_w(self, value: int) -> None:
        """Set the lc_w property."""
        self._cards[1].set_value("lc_w", value)

    @property
    def lc_rho(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the density versus time.
        """ # nopep8
        return self._cards[1].get_value("lc_rho")

    @lc_rho.setter
    def lc_rho(self, value: int) -> None:
        """Set the lc_rho property."""
        self._cards[1].set_value("lc_rho", value)

    @property
    def lc_p_(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the pressure versus time.
        """ # nopep8
        return self._cards[1].get_value("lc_p_")

    @lc_p_.setter
    def lc_p_(self, value: int) -> None:
        """Set the lc_p_ property."""
        self._cards[1].set_value("lc_p_", value)

    @property
    def lc_t(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the temperature versus time.
        """ # nopep8
        return self._cards[1].get_value("lc_t")

    @lc_t.setter
    def lc_t(self, value: int) -> None:
        """Set the lc_t property."""
        self._cards[1].set_value("lc_t", value)

    @property
    def sf_u(self) -> float:
        """Get or set the Scale factor for LC_U
        """ # nopep8
        return self._cards[2].get_value("sf_u")

    @sf_u.setter
    def sf_u(self, value: float) -> None:
        """Set the sf_u property."""
        self._cards[2].set_value("sf_u", value)

    @property
    def sf_v_(self) -> float:
        """Get or set the Scale factor for LC_V
        """ # nopep8
        return self._cards[2].get_value("sf_v_")

    @sf_v_.setter
    def sf_v_(self, value: float) -> None:
        """Set the sf_v_ property."""
        self._cards[2].set_value("sf_v_", value)

    @property
    def sf_w(self) -> float:
        """Get or set the Scale factor for LC_W
        """ # nopep8
        return self._cards[2].get_value("sf_w")

    @sf_w.setter
    def sf_w(self, value: float) -> None:
        """Set the sf_w property."""
        self._cards[2].set_value("sf_w", value)

    @property
    def sf_rho(self) -> float:
        """Get or set the Scale factor for LC_RHO
        """ # nopep8
        return self._cards[2].get_value("sf_rho")

    @sf_rho.setter
    def sf_rho(self, value: float) -> None:
        """Set the sf_rho property."""
        self._cards[2].set_value("sf_rho", value)

    @property
    def sf_p_(self) -> float:
        """Get or set the Scale factor for LC_P
        """ # nopep8
        return self._cards[2].get_value("sf_p_")

    @sf_p_.setter
    def sf_p_(self, value: float) -> None:
        """Set the sf_p_ property."""
        self._cards[2].set_value("sf_p_", value)

    @property
    def sf_t(self) -> float:
        """Get or set the Scale factor for LC_T
        """ # nopep8
        return self._cards[2].get_value("sf_t")

    @sf_t.setter
    def sf_t(self, value: float) -> None:
        """Set the sf_t property."""
        self._cards[2].set_value("sf_t", value)

    @property
    def lc_u_link(self) -> DefineCurve:
        """Get the DefineCurve object for lc_u."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc_u:
                return kwd
        return None

    @lc_u_link.setter
    def lc_u_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc_u."""
        self.lc_u = value.lcid

    @property
    def lc_v__link(self) -> DefineCurve:
        """Get the DefineCurve object for lc_v_."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc_v_:
                return kwd
        return None

    @lc_v__link.setter
    def lc_v__link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc_v_."""
        self.lc_v_ = value.lcid

    @property
    def lc_w_link(self) -> DefineCurve:
        """Get the DefineCurve object for lc_w."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc_w:
                return kwd
        return None

    @lc_w_link.setter
    def lc_w_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc_w."""
        self.lc_w = value.lcid

    @property
    def lc_rho_link(self) -> DefineCurve:
        """Get the DefineCurve object for lc_rho."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc_rho:
                return kwd
        return None

    @lc_rho_link.setter
    def lc_rho_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc_rho."""
        self.lc_rho = value.lcid

    @property
    def lc_p__link(self) -> DefineCurve:
        """Get the DefineCurve object for lc_p_."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc_p_:
                return kwd
        return None

    @lc_p__link.setter
    def lc_p__link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc_p_."""
        self.lc_p_ = value.lcid

    @property
    def lc_t_link(self) -> DefineCurve:
        """Get the DefineCurve object for lc_t."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc_t:
                return kwd
        return None

    @lc_t_link.setter
    def lc_t_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc_t."""
        self.lc_t = value.lcid

