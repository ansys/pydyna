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

"""Module providing the Mat016 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MAT016_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("g", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
)

_MAT016_CARD1 = (
    FieldSchema("sigf", float, 0, 10, None),
    FieldSchema("a0", float, 10, 10, None),
    FieldSchema("a1", float, 20, 10, None),
    FieldSchema("a2", float, 30, 10, None),
    FieldSchema("a0f", float, 40, 10, None),
    FieldSchema("a1f", float, 50, 10, None),
    FieldSchema("b1", float, 60, 10, None),
    FieldSchema("per", float, 70, 10, None),
)

_MAT016_CARD2 = (
    FieldSchema("er", float, 0, 10, None),
    FieldSchema("prr", float, 10, 10, None),
    FieldSchema("sigy", float, 20, 10, None),
    FieldSchema("etan", float, 30, 10, None),
    FieldSchema("lcp", int, 40, 10, 0),
    FieldSchema("lcr", int, 50, 10, 0),
)

_MAT016_CARD3 = (
    FieldSchema("x1", float, 0, 10, None),
    FieldSchema("x2", float, 10, 10, None),
    FieldSchema("x3", float, 20, 10, None),
    FieldSchema("x4", float, 30, 10, None),
    FieldSchema("x5", float, 40, 10, None),
    FieldSchema("x6", float, 50, 10, None),
    FieldSchema("x7", float, 60, 10, None),
    FieldSchema("x8", float, 70, 10, None),
)

_MAT016_CARD4 = (
    FieldSchema("x9", float, 0, 10, None),
    FieldSchema("x10", float, 10, 10, None),
    FieldSchema("x11", float, 20, 10, None),
    FieldSchema("x12", float, 30, 10, None),
    FieldSchema("x13", float, 40, 10, None),
    FieldSchema("x14", float, 50, 10, None),
    FieldSchema("x15", float, 60, 10, None),
    FieldSchema("x16", float, 70, 10, None),
)

_MAT016_CARD5 = (
    FieldSchema("ys1", float, 0, 10, None),
    FieldSchema("ys2", float, 10, 10, None),
    FieldSchema("ys3", float, 20, 10, None),
    FieldSchema("ys4", float, 30, 10, None),
    FieldSchema("ys5", float, 40, 10, None),
    FieldSchema("ys6", float, 50, 10, None),
    FieldSchema("ys7", float, 60, 10, None),
    FieldSchema("ys8", float, 70, 10, None),
)

_MAT016_CARD6 = (
    FieldSchema("ys9", float, 0, 10, None),
    FieldSchema("ys10", float, 10, 10, None),
    FieldSchema("ys11", float, 20, 10, None),
    FieldSchema("ys12", float, 30, 10, None),
    FieldSchema("ys13", float, 40, 10, None),
    FieldSchema("ys14", float, 50, 10, None),
    FieldSchema("ys15", float, 60, 10, None),
    FieldSchema("ys16", float, 70, 10, None),
)

_MAT016_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat016(KeywordBase):
    """DYNA MAT_016 keyword"""

    keyword = "MAT"
    subkeyword = "016"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lcp": LinkType.DEFINE_CURVE,
        "lcr": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the Mat016 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT016_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT016_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT016_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT016_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT016_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT016_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT016_CARD6,
                **kwargs,
            ),            OptionCardSet(
                option_spec = Mat016.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT016_OPTION0_CARD0,
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
    def g(self) -> typing.Optional[float]:
        """Get or set the Shear modulus.
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        """Set the g property."""
        self._cards[0].set_value("g", value)

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
    def sigf(self) -> typing.Optional[float]:
        """Get or set the Tensile cutoff (maximum principal stress for failure).
        """ # nopep8
        return self._cards[1].get_value("sigf")

    @sigf.setter
    def sigf(self, value: float) -> None:
        """Set the sigf property."""
        self._cards[1].set_value("sigf", value)

    @property
    def a0(self) -> typing.Optional[float]:
        """Get or set the Cohesion.
        """ # nopep8
        return self._cards[1].get_value("a0")

    @a0.setter
    def a0(self, value: float) -> None:
        """Set the a0 property."""
        self._cards[1].set_value("a0", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Pressure hardening coefficient.
        """ # nopep8
        return self._cards[1].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[1].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Pressure hardening coefficient.
        """ # nopep8
        return self._cards[1].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[1].set_value("a2", value)

    @property
    def a0f(self) -> typing.Optional[float]:
        """Get or set the Cohesion for failed material.
        """ # nopep8
        return self._cards[1].get_value("a0f")

    @a0f.setter
    def a0f(self, value: float) -> None:
        """Set the a0f property."""
        self._cards[1].set_value("a0f", value)

    @property
    def a1f(self) -> typing.Optional[float]:
        """Get or set the Pressure hardening coefficient for failed material.
        """ # nopep8
        return self._cards[1].get_value("a1f")

    @a1f.setter
    def a1f(self, value: float) -> None:
        """Set the a1f property."""
        self._cards[1].set_value("a1f", value)

    @property
    def b1(self) -> typing.Optional[float]:
        """Get or set the Damage scaling factor.
        """ # nopep8
        return self._cards[1].get_value("b1")

    @b1.setter
    def b1(self, value: float) -> None:
        """Set the b1 property."""
        self._cards[1].set_value("b1", value)

    @property
    def per(self) -> typing.Optional[float]:
        """Get or set the Percent reinforcement.
        """ # nopep8
        return self._cards[1].get_value("per")

    @per.setter
    def per(self, value: float) -> None:
        """Set the per property."""
        self._cards[1].set_value("per", value)

    @property
    def er(self) -> typing.Optional[float]:
        """Get or set the Elastic modulus for reinforcement.
        """ # nopep8
        return self._cards[2].get_value("er")

    @er.setter
    def er(self, value: float) -> None:
        """Set the er property."""
        self._cards[2].set_value("er", value)

    @property
    def prr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio for reinforcement.
        """ # nopep8
        return self._cards[2].get_value("prr")

    @prr.setter
    def prr(self, value: float) -> None:
        """Set the prr property."""
        self._cards[2].set_value("prr", value)

    @property
    def sigy(self) -> typing.Optional[float]:
        """Get or set the Initial yield stress.
        """ # nopep8
        return self._cards[2].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        """Set the sigy property."""
        self._cards[2].set_value("sigy", value)

    @property
    def etan(self) -> typing.Optional[float]:
        """Get or set the Tangent modulus/plastic hardening modulus.
        """ # nopep8
        return self._cards[2].get_value("etan")

    @etan.setter
    def etan(self, value: float) -> None:
        """Set the etan property."""
        self._cards[2].set_value("etan", value)

    @property
    def lcp(self) -> int:
        """Get or set the Load curve ID giving rate sensitivity for principal material, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[2].get_value("lcp")

    @lcp.setter
    def lcp(self, value: int) -> None:
        """Set the lcp property."""
        self._cards[2].set_value("lcp", value)

    @property
    def lcr(self) -> int:
        """Get or set the Load curve ID giving rate sensitivity for reinforcement, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[2].get_value("lcr")

    @lcr.setter
    def lcr(self, value: int) -> None:
        """Set the lcr property."""
        self._cards[2].set_value("lcr", value)

    @property
    def x1(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[3].get_value("x1")

    @x1.setter
    def x1(self, value: float) -> None:
        """Set the x1 property."""
        self._cards[3].set_value("x1", value)

    @property
    def x2(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[3].get_value("x2")

    @x2.setter
    def x2(self, value: float) -> None:
        """Set the x2 property."""
        self._cards[3].set_value("x2", value)

    @property
    def x3(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[3].get_value("x3")

    @x3.setter
    def x3(self, value: float) -> None:
        """Set the x3 property."""
        self._cards[3].set_value("x3", value)

    @property
    def x4(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[3].get_value("x4")

    @x4.setter
    def x4(self, value: float) -> None:
        """Set the x4 property."""
        self._cards[3].set_value("x4", value)

    @property
    def x5(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[3].get_value("x5")

    @x5.setter
    def x5(self, value: float) -> None:
        """Set the x5 property."""
        self._cards[3].set_value("x5", value)

    @property
    def x6(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[3].get_value("x6")

    @x6.setter
    def x6(self, value: float) -> None:
        """Set the x6 property."""
        self._cards[3].set_value("x6", value)

    @property
    def x7(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[3].get_value("x7")

    @x7.setter
    def x7(self, value: float) -> None:
        """Set the x7 property."""
        self._cards[3].set_value("x7", value)

    @property
    def x8(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[3].get_value("x8")

    @x8.setter
    def x8(self, value: float) -> None:
        """Set the x8 property."""
        self._cards[3].set_value("x8", value)

    @property
    def x9(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[4].get_value("x9")

    @x9.setter
    def x9(self, value: float) -> None:
        """Set the x9 property."""
        self._cards[4].set_value("x9", value)

    @property
    def x10(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[4].get_value("x10")

    @x10.setter
    def x10(self, value: float) -> None:
        """Set the x10 property."""
        self._cards[4].set_value("x10", value)

    @property
    def x11(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[4].get_value("x11")

    @x11.setter
    def x11(self, value: float) -> None:
        """Set the x11 property."""
        self._cards[4].set_value("x11", value)

    @property
    def x12(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[4].get_value("x12")

    @x12.setter
    def x12(self, value: float) -> None:
        """Set the x12 property."""
        self._cards[4].set_value("x12", value)

    @property
    def x13(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[4].get_value("x13")

    @x13.setter
    def x13(self, value: float) -> None:
        """Set the x13 property."""
        self._cards[4].set_value("x13", value)

    @property
    def x14(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[4].get_value("x14")

    @x14.setter
    def x14(self, value: float) -> None:
        """Set the x14 property."""
        self._cards[4].set_value("x14", value)

    @property
    def x15(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[4].get_value("x15")

    @x15.setter
    def x15(self, value: float) -> None:
        """Set the x15 property."""
        self._cards[4].set_value("x15", value)

    @property
    def x16(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain, damage, or pressure.
        """ # nopep8
        return self._cards[4].get_value("x16")

    @x16.setter
    def x16(self, value: float) -> None:
        """Set the x16 property."""
        self._cards[4].set_value("x16", value)

    @property
    def ys1(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[5].get_value("ys1")

    @ys1.setter
    def ys1(self, value: float) -> None:
        """Set the ys1 property."""
        self._cards[5].set_value("ys1", value)

    @property
    def ys2(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[5].get_value("ys2")

    @ys2.setter
    def ys2(self, value: float) -> None:
        """Set the ys2 property."""
        self._cards[5].set_value("ys2", value)

    @property
    def ys3(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[5].get_value("ys3")

    @ys3.setter
    def ys3(self, value: float) -> None:
        """Set the ys3 property."""
        self._cards[5].set_value("ys3", value)

    @property
    def ys4(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[5].get_value("ys4")

    @ys4.setter
    def ys4(self, value: float) -> None:
        """Set the ys4 property."""
        self._cards[5].set_value("ys4", value)

    @property
    def ys5(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[5].get_value("ys5")

    @ys5.setter
    def ys5(self, value: float) -> None:
        """Set the ys5 property."""
        self._cards[5].set_value("ys5", value)

    @property
    def ys6(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[5].get_value("ys6")

    @ys6.setter
    def ys6(self, value: float) -> None:
        """Set the ys6 property."""
        self._cards[5].set_value("ys6", value)

    @property
    def ys7(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[5].get_value("ys7")

    @ys7.setter
    def ys7(self, value: float) -> None:
        """Set the ys7 property."""
        self._cards[5].set_value("ys7", value)

    @property
    def ys8(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[5].get_value("ys8")

    @ys8.setter
    def ys8(self, value: float) -> None:
        """Set the ys8 property."""
        self._cards[5].set_value("ys8", value)

    @property
    def ys9(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[6].get_value("ys9")

    @ys9.setter
    def ys9(self, value: float) -> None:
        """Set the ys9 property."""
        self._cards[6].set_value("ys9", value)

    @property
    def ys10(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[6].get_value("ys10")

    @ys10.setter
    def ys10(self, value: float) -> None:
        """Set the ys10 property."""
        self._cards[6].set_value("ys10", value)

    @property
    def ys11(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[6].get_value("ys11")

    @ys11.setter
    def ys11(self, value: float) -> None:
        """Set the ys11 property."""
        self._cards[6].set_value("ys11", value)

    @property
    def ys12(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[6].get_value("ys12")

    @ys12.setter
    def ys12(self, value: float) -> None:
        """Set the ys12 property."""
        self._cards[6].set_value("ys12", value)

    @property
    def ys13(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[6].get_value("ys13")

    @ys13.setter
    def ys13(self, value: float) -> None:
        """Set the ys13 property."""
        self._cards[6].set_value("ys13", value)

    @property
    def ys14(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[6].get_value("ys14")

    @ys14.setter
    def ys14(self, value: float) -> None:
        """Set the ys14 property."""
        self._cards[6].set_value("ys14", value)

    @property
    def ys15(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[6].get_value("ys15")

    @ys15.setter
    def ys15(self, value: float) -> None:
        """Set the ys15 property."""
        self._cards[6].set_value("ys15", value)

    @property
    def ys16(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[6].get_value("ys16")

    @ys16.setter
    def ys16(self, value: float) -> None:
        """Set the ys16 property."""
        self._cards[6].set_value("ys16", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[7].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[7].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

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

    @property
    def lcr_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcr."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcr:
                return kwd
        return None

    @lcr_link.setter
    def lcr_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcr."""
        self.lcr = value.lcid

