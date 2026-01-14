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

"""Module providing the MatInvHyperbolicSinThermal class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MATINVHYPERBOLICSINTHERMAL_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("alpha", float, 20, 10, None),
    FieldSchema("n", float, 30, 10, None),
    FieldSchema("a", float, 40, 10, None),
    FieldSchema("q", float, 50, 10, None),
    FieldSchema("g", float, 60, 10, None),
    FieldSchema("epso", float, 70, 10, None),
)

_MATINVHYPERBOLICSINTHERMAL_CARD1 = (
    FieldSchema("lce", float, 0, 10, None),
    FieldSchema("lcpr", float, 10, 10, None),
    FieldSchema("lccte", float, 20, 10, None),
)

_MATINVHYPERBOLICSINTHERMAL_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatInvHyperbolicSinThermal(KeywordBase):
    """DYNA MAT_INV_HYPERBOLIC_SIN_THERMAL keyword"""

    keyword = "MAT"
    subkeyword = "INV_HYPERBOLIC_SIN_THERMAL"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lce": LinkType.DEFINE_CURVE,
        "lcpr": LinkType.DEFINE_CURVE,
        "lccte": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatInvHyperbolicSinThermal class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATINVHYPERBOLICSINTHERMAL_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATINVHYPERBOLICSINTHERMAL_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatInvHyperbolicSinThermal.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATINVHYPERBOLICSINTHERMAL_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Not to be confused with coefficient of thermal expansion.
        """ # nopep8
        return self._cards[0].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[0].set_value("alpha", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the See Remarks for detail.
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[0].set_value("n", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the See Remarks for detail.
        """ # nopep8
        return self._cards[0].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[0].set_value("a", value)

    @property
    def q(self) -> typing.Optional[float]:
        """Get or set the See Remarks for detail.
        """ # nopep8
        return self._cards[0].get_value("q")

    @q.setter
    def q(self, value: float) -> None:
        """Set the q property."""
        self._cards[0].set_value("q", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the See Remarks for detail
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        """Set the g property."""
        self._cards[0].set_value("g", value)

    @property
    def epso(self) -> typing.Optional[float]:
        """Get or set the Minimum strain rate considered in calculating Z
        """ # nopep8
        return self._cards[0].get_value("epso")

    @epso.setter
    def epso(self, value: float) -> None:
        """Set the epso property."""
        self._cards[0].set_value("epso", value)

    @property
    def lce(self) -> typing.Optional[float]:
        """Get or set the ID of curve defining Young’s Modulus as a function of temperature
        """ # nopep8
        return self._cards[1].get_value("lce")

    @lce.setter
    def lce(self, value: float) -> None:
        """Set the lce property."""
        self._cards[1].set_value("lce", value)

    @property
    def lcpr(self) -> typing.Optional[float]:
        """Get or set the ID of curve defining Poisson’s ratio as a function of temperature
        """ # nopep8
        return self._cards[1].get_value("lcpr")

    @lcpr.setter
    def lcpr(self, value: float) -> None:
        """Set the lcpr property."""
        self._cards[1].set_value("lcpr", value)

    @property
    def lccte(self) -> typing.Optional[float]:
        """Get or set the ID of curve defining coefficient of thermal expansion as a function of temperature
        """ # nopep8
        return self._cards[1].get_value("lccte")

    @lccte.setter
    def lccte(self, value: float) -> None:
        """Set the lccte property."""
        self._cards[1].set_value("lccte", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[2].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def lce_link(self) -> DefineCurve:
        """Get the DefineCurve object for lce."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lce:
                return kwd
        return None

    @lce_link.setter
    def lce_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lce."""
        self.lce = value.lcid

    @property
    def lcpr_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcpr."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcpr:
                return kwd
        return None

    @lcpr_link.setter
    def lcpr_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcpr."""
        self.lcpr = value.lcid

    @property
    def lccte_link(self) -> DefineCurve:
        """Get the DefineCurve object for lccte."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lccte:
                return kwd
        return None

    @lccte_link.setter
    def lccte_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lccte."""
        self.lccte = value.lcid

