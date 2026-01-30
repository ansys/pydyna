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

"""Module providing the MatAnandViscoplasticity class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MATANANDVISCOPLASTICITY_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("ym", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("alpha", float, 40, 10, None),
    FieldSchema("a1", float, 50, 10, None),
    FieldSchema("ratioqr", float, 60, 10, None),
    FieldSchema("xi", float, 70, 10, None),
)

_MATANANDVISCOPLASTICITY_CARD1 = (
    FieldSchema("m", float, 0, 10, None),
    FieldSchema("s0", float, 10, 10, None),
    FieldSchema("h0", float, 20, 10, None),
    FieldSchema("a2", float, 30, 10, None),
    FieldSchema("sbar", float, 40, 10, None),
    FieldSchema("n", float, 50, 10, None),
    FieldSchema("lcym", int, 60, 10, None),
    FieldSchema("tref", float, 70, 10, None),
)

_MATANANDVISCOPLASTICITY_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatAnandViscoplasticity(KeywordBase):
    """DYNA MAT_ANAND_VISCOPLASTICITY keyword"""

    keyword = "MAT"
    subkeyword = "ANAND_VISCOPLASTICITY"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lcym": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatAnandViscoplasticity class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATANANDVISCOPLASTICITY_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATANANDVISCOPLASTICITY_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatAnandViscoplasticity.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATANANDVISCOPLASTICITY_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number or label must be specified.
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
    def ym(self) -> typing.Optional[float]:
        """Get or set the Young's Modulus.
        """ # nopep8
        return self._cards[0].get_value("ym")

    @ym.setter
    def ym(self, value: float) -> None:
        """Set the ym property."""
        self._cards[0].set_value("ym", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Coefficient of thermal expansion.
        """ # nopep8
        return self._cards[0].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[0].set_value("alpha", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Pre-exponential factor.
        """ # nopep8
        return self._cards[0].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[0].set_value("a1", value)

    @property
    def ratioqr(self) -> typing.Optional[float]:
        """Get or set the Ratio of the activation energy, to the universal gas constant.
        """ # nopep8
        return self._cards[0].get_value("ratioqr")

    @ratioqr.setter
    def ratioqr(self, value: float) -> None:
        """Set the ratioqr property."""
        self._cards[0].set_value("ratioqr", value)

    @property
    def xi(self) -> typing.Optional[float]:
        """Get or set the Multiplier of stress.
        """ # nopep8
        return self._cards[0].get_value("xi")

    @xi.setter
    def xi(self, value: float) -> None:
        """Set the xi property."""
        self._cards[0].set_value("xi", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Strain rate sensitivity.
        """ # nopep8
        return self._cards[1].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        """Set the m property."""
        self._cards[1].set_value("m", value)

    @property
    def s0(self) -> typing.Optional[float]:
        """Get or set the Initial value of deformation resistance.
        """ # nopep8
        return self._cards[1].get_value("s0")

    @s0.setter
    def s0(self, value: float) -> None:
        """Set the s0 property."""
        self._cards[1].set_value("s0", value)

    @property
    def h0(self) -> typing.Optional[float]:
        """Get or set the Hardening/softening constant.
        """ # nopep8
        return self._cards[1].get_value("h0")

    @h0.setter
    def h0(self, value: float) -> None:
        """Set the h0 property."""
        self._cards[1].set_value("h0", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Strain rate sensitivity of hardening or softening.
        """ # nopep8
        return self._cards[1].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[1].set_value("a2", value)

    @property
    def sbar(self) -> typing.Optional[float]:
        """Get or set the Coefficient of deformation resistance saturation value.
        """ # nopep8
        return self._cards[1].get_value("sbar")

    @sbar.setter
    def sbar(self, value: float) -> None:
        """Set the sbar property."""
        self._cards[1].set_value("sbar", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Strain rate sensitivity of deformation resistance saturation value.
        """ # nopep8
        return self._cards[1].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[1].set_value("n", value)

    @property
    def lcym(self) -> typing.Optional[int]:
        """Get or set the Load curve ID when Young's Modulus is temperature dependent.
        """ # nopep8
        return self._cards[1].get_value("lcym")

    @lcym.setter
    def lcym(self, value: int) -> None:
        """Set the lcym property."""
        self._cards[1].set_value("lcym", value)

    @property
    def tref(self) -> typing.Optional[float]:
        """Get or set the Reference temperature, Tref.
        """ # nopep8
        return self._cards[1].get_value("tref")

    @tref.setter
    def tref(self, value: float) -> None:
        """Set the tref property."""
        self._cards[1].set_value("tref", value)

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
    def lcym_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcym."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcym:
                return kwd
        return None

    @lcym_link.setter
    def lcym_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcym."""
        self.lcym = value.lcid

