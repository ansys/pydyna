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

"""Module providing the MatGurson class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MATGURSON_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("sigy", float, 40, 10, None),
    FieldSchema("n", float, 50, 10, None),
    FieldSchema("q1", float, 60, 10, None),
    FieldSchema("q2", float, 70, 10, None),
)

_MATGURSON_CARD1 = (
    FieldSchema("fc", float, 0, 10, None),
    FieldSchema("f0", float, 10, 10, None),
    FieldSchema("en", float, 20, 10, None),
    FieldSchema("sn", float, 30, 10, None),
    FieldSchema("fn", float, 40, 10, None),
    FieldSchema("etan", float, 50, 10, None),
    FieldSchema("atyp", float, 60, 10, 1.0),
    FieldSchema("ff0", float, 70, 10, None),
)

_MATGURSON_CARD2 = (
    FieldSchema("eps1", float, 0, 10, None),
    FieldSchema("eps2", float, 10, 10, None),
    FieldSchema("eps3", float, 20, 10, None),
    FieldSchema("eps4", float, 30, 10, None),
    FieldSchema("eps5", float, 40, 10, None),
    FieldSchema("eps6", float, 50, 10, None),
    FieldSchema("eps7", float, 60, 10, None),
    FieldSchema("eps8", float, 70, 10, None),
)

_MATGURSON_CARD3 = (
    FieldSchema("es1", float, 0, 10, None),
    FieldSchema("es2", float, 10, 10, None),
    FieldSchema("es3", float, 20, 10, None),
    FieldSchema("es4", float, 30, 10, None),
    FieldSchema("es5", float, 40, 10, None),
    FieldSchema("es6", float, 50, 10, None),
    FieldSchema("es7", float, 60, 10, None),
    FieldSchema("es8", float, 70, 10, None),
)

_MATGURSON_CARD4 = (
    FieldSchema("l1", float, 0, 10, None),
    FieldSchema("l2", float, 10, 10, None),
    FieldSchema("l3", float, 20, 10, None),
    FieldSchema("l4", float, 30, 10, None),
    FieldSchema("ff1", float, 40, 10, None),
    FieldSchema("ff2", float, 50, 10, None),
    FieldSchema("ff3", float, 60, 10, None),
    FieldSchema("ff4", float, 70, 10, None),
)

_MATGURSON_CARD5 = (
    FieldSchema("lcss", int, 0, 10, 0),
    FieldSchema("lclf", int, 10, 10, 0),
    FieldSchema("numint", float, 20, 10, 1.0),
    FieldSchema("lcf0", int, 30, 10, 0),
    FieldSchema("lcfc", int, 40, 10, 0),
    FieldSchema("lcfn", int, 50, 10, 0),
    FieldSchema("vgtyp", float, 60, 10, None),
)

_MATGURSON_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatGurson(KeywordBase):
    """DYNA MAT_GURSON keyword"""

    keyword = "MAT"
    subkeyword = "GURSON"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lcss": LinkType.DEFINE_CURVE,
        "lclf": LinkType.DEFINE_CURVE,
        "lcf0": LinkType.DEFINE_CURVE,
        "lcfc": LinkType.DEFINE_CURVE,
        "lcfn": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatGurson class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATGURSON_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATGURSON_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATGURSON_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATGURSON_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATGURSON_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATGURSON_CARD5,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatGurson.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATGURSON_OPTION0_CARD0,
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
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[0].set_value("e", value)

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
    def sigy(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[0].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        """Set the sigy property."""
        self._cards[0].set_value("sigy", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Exponent for Power law.This value is only used if ATYP=1 and LCSS=0.
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[0].set_value("n", value)

    @property
    def q1(self) -> typing.Optional[float]:
        """Get or set the Parameter q1.
        """ # nopep8
        return self._cards[0].get_value("q1")

    @q1.setter
    def q1(self, value: float) -> None:
        """Set the q1 property."""
        self._cards[0].set_value("q1", value)

    @property
    def q2(self) -> typing.Optional[float]:
        """Get or set the Parameter q2.
        """ # nopep8
        return self._cards[0].get_value("q2")

    @q2.setter
    def q2(self, value: float) -> None:
        """Set the q2 property."""
        self._cards[0].set_value("q2", value)

    @property
    def fc(self) -> typing.Optional[float]:
        """Get or set the Critical void volume fraction fc.
        """ # nopep8
        return self._cards[1].get_value("fc")

    @fc.setter
    def fc(self, value: float) -> None:
        """Set the fc property."""
        self._cards[1].set_value("fc", value)

    @property
    def f0(self) -> typing.Optional[float]:
        """Get or set the Initial void volume fraction f0.
        """ # nopep8
        return self._cards[1].get_value("f0")

    @f0.setter
    def f0(self, value: float) -> None:
        """Set the f0 property."""
        self._cards[1].set_value("f0", value)

    @property
    def en(self) -> typing.Optional[float]:
        """Get or set the Mean nucleation strain En .
        GT.0.0:	Constant value,
        LT.0.0:	Load curve ID = (-EN) which defines mean nucleation strain ε_N  as a function of element length.
        """ # nopep8
        return self._cards[1].get_value("en")

    @en.setter
    def en(self, value: float) -> None:
        """Set the en property."""
        self._cards[1].set_value("en", value)

    @property
    def sn(self) -> typing.Optional[float]:
        """Get or set the Standard deviation Sn of the normal distribution of En.
        GT.0.0:	Constant value,
        LT.0.0:	Load curve ID = (-SN) which defines standard deviation s_N of the normal distribution of ε_N as a function of element length.
        """ # nopep8
        return self._cards[1].get_value("sn")

    @sn.setter
    def sn(self, value: float) -> None:
        """Set the sn property."""
        self._cards[1].set_value("sn", value)

    @property
    def fn(self) -> typing.Optional[float]:
        """Get or set the Void volume fraction of nucleating particles.
        """ # nopep8
        return self._cards[1].get_value("fn")

    @fn.setter
    def fn(self, value: float) -> None:
        """Set the fn property."""
        self._cards[1].set_value("fn", value)

    @property
    def etan(self) -> typing.Optional[float]:
        """Get or set the Hardening modulus. This value is only used if ATYP=2 and LCSS=0.
        """ # nopep8
        return self._cards[1].get_value("etan")

    @etan.setter
    def etan(self, value: float) -> None:
        """Set the etan property."""
        self._cards[1].set_value("etan", value)

    @property
    def atyp(self) -> float:
        """Get or set the Type of hardening.
        EQ.1.0 Power law.
        EQ.2.0: Linear hardening.
        EQ.3.0: 8 points curve.
        """ # nopep8
        return self._cards[1].get_value("atyp")

    @atyp.setter
    def atyp(self, value: float) -> None:
        """Set the atyp property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""atyp must be `None` or one of {1,2,3}.""")
        self._cards[1].set_value("atyp", value)

    @property
    def ff0(self) -> typing.Optional[float]:
        """Get or set the Failure void volume fraction. This value is used if no curve is given by the points L1,FF1 - L4,FF4 and LCLF=0.
        """ # nopep8
        return self._cards[1].get_value("ff0")

    @ff0.setter
    def ff0(self, value: float) -> None:
        """Set the ff0 property."""
        self._cards[1].set_value("ff0", value)

    @property
    def eps1(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values.The first point must be zero corresponding to the initial yield stress. This option is only used if ATYP equal to 3. At least 2 points should be defined.These values are used if ATYP=3 and LCSS=0.
        """ # nopep8
        return self._cards[2].get_value("eps1")

    @eps1.setter
    def eps1(self, value: float) -> None:
        """Set the eps1 property."""
        self._cards[2].set_value("eps1", value)

    @property
    def eps2(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values at point 2
        """ # nopep8
        return self._cards[2].get_value("eps2")

    @eps2.setter
    def eps2(self, value: float) -> None:
        """Set the eps2 property."""
        self._cards[2].set_value("eps2", value)

    @property
    def eps3(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values at point 3
        """ # nopep8
        return self._cards[2].get_value("eps3")

    @eps3.setter
    def eps3(self, value: float) -> None:
        """Set the eps3 property."""
        self._cards[2].set_value("eps3", value)

    @property
    def eps4(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values at point 4
        """ # nopep8
        return self._cards[2].get_value("eps4")

    @eps4.setter
    def eps4(self, value: float) -> None:
        """Set the eps4 property."""
        self._cards[2].set_value("eps4", value)

    @property
    def eps5(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values at point 5
        """ # nopep8
        return self._cards[2].get_value("eps5")

    @eps5.setter
    def eps5(self, value: float) -> None:
        """Set the eps5 property."""
        self._cards[2].set_value("eps5", value)

    @property
    def eps6(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values at point 6
        """ # nopep8
        return self._cards[2].get_value("eps6")

    @eps6.setter
    def eps6(self, value: float) -> None:
        """Set the eps6 property."""
        self._cards[2].set_value("eps6", value)

    @property
    def eps7(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values at point 7
        """ # nopep8
        return self._cards[2].get_value("eps7")

    @eps7.setter
    def eps7(self, value: float) -> None:
        """Set the eps7 property."""
        self._cards[2].set_value("eps7", value)

    @property
    def eps8(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values at point 8
        """ # nopep8
        return self._cards[2].get_value("eps8")

    @eps8.setter
    def eps8(self, value: float) -> None:
        """Set the eps8 property."""
        self._cards[2].set_value("eps8", value)

    @property
    def es1(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress values to EPS1 - EPS8. These values are used if ATYP=3 and LCSS=0.
        """ # nopep8
        return self._cards[3].get_value("es1")

    @es1.setter
    def es1(self, value: float) -> None:
        """Set the es1 property."""
        self._cards[3].set_value("es1", value)

    @property
    def es2(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress values to EPS2
        """ # nopep8
        return self._cards[3].get_value("es2")

    @es2.setter
    def es2(self, value: float) -> None:
        """Set the es2 property."""
        self._cards[3].set_value("es2", value)

    @property
    def es3(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress values to EPS3
        """ # nopep8
        return self._cards[3].get_value("es3")

    @es3.setter
    def es3(self, value: float) -> None:
        """Set the es3 property."""
        self._cards[3].set_value("es3", value)

    @property
    def es4(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress values to EPS4
        """ # nopep8
        return self._cards[3].get_value("es4")

    @es4.setter
    def es4(self, value: float) -> None:
        """Set the es4 property."""
        self._cards[3].set_value("es4", value)

    @property
    def es5(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress values to EPS5
        """ # nopep8
        return self._cards[3].get_value("es5")

    @es5.setter
    def es5(self, value: float) -> None:
        """Set the es5 property."""
        self._cards[3].set_value("es5", value)

    @property
    def es6(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress values to EPS6
        """ # nopep8
        return self._cards[3].get_value("es6")

    @es6.setter
    def es6(self, value: float) -> None:
        """Set the es6 property."""
        self._cards[3].set_value("es6", value)

    @property
    def es7(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress values to EPS7
        """ # nopep8
        return self._cards[3].get_value("es7")

    @es7.setter
    def es7(self, value: float) -> None:
        """Set the es7 property."""
        self._cards[3].set_value("es7", value)

    @property
    def es8(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress values to EPS8
        """ # nopep8
        return self._cards[3].get_value("es8")

    @es8.setter
    def es8(self, value: float) -> None:
        """Set the es8 property."""
        self._cards[3].set_value("es8", value)

    @property
    def l1(self) -> typing.Optional[float]:
        """Get or set the Element length values.These values are only used if LCLF=0.
        """ # nopep8
        return self._cards[4].get_value("l1")

    @l1.setter
    def l1(self, value: float) -> None:
        """Set the l1 property."""
        self._cards[4].set_value("l1", value)

    @property
    def l2(self) -> typing.Optional[float]:
        """Get or set the Element length values.
        """ # nopep8
        return self._cards[4].get_value("l2")

    @l2.setter
    def l2(self, value: float) -> None:
        """Set the l2 property."""
        self._cards[4].set_value("l2", value)

    @property
    def l3(self) -> typing.Optional[float]:
        """Get or set the Element length values.
        """ # nopep8
        return self._cards[4].get_value("l3")

    @l3.setter
    def l3(self, value: float) -> None:
        """Set the l3 property."""
        self._cards[4].set_value("l3", value)

    @property
    def l4(self) -> typing.Optional[float]:
        """Get or set the Element length values.
        """ # nopep8
        return self._cards[4].get_value("l4")

    @l4.setter
    def l4(self, value: float) -> None:
        """Set the l4 property."""
        self._cards[4].set_value("l4", value)

    @property
    def ff1(self) -> typing.Optional[float]:
        """Get or set the Corresponding failure void volume fraction. These values are only used if LCLF=0.
        """ # nopep8
        return self._cards[4].get_value("ff1")

    @ff1.setter
    def ff1(self, value: float) -> None:
        """Set the ff1 property."""
        self._cards[4].set_value("ff1", value)

    @property
    def ff2(self) -> typing.Optional[float]:
        """Get or set the Corresponding failure void volume fraction.
        """ # nopep8
        return self._cards[4].get_value("ff2")

    @ff2.setter
    def ff2(self, value: float) -> None:
        """Set the ff2 property."""
        self._cards[4].set_value("ff2", value)

    @property
    def ff3(self) -> typing.Optional[float]:
        """Get or set the Corresponding failure void volume fraction.
        """ # nopep8
        return self._cards[4].get_value("ff3")

    @ff3.setter
    def ff3(self, value: float) -> None:
        """Set the ff3 property."""
        self._cards[4].set_value("ff3", value)

    @property
    def ff4(self) -> typing.Optional[float]:
        """Get or set the Corresponding failure void volume fraction.
        """ # nopep8
        return self._cards[4].get_value("ff4")

    @ff4.setter
    def ff4(self, value: float) -> None:
        """Set the ff4 property."""
        self._cards[4].set_value("ff4", value)

    @property
    def lcss(self) -> int:
        """Get or set the Load curve ID defining effective stress versus effective plastic strain. ATYP is ignored with this option.
        """ # nopep8
        return self._cards[5].get_value("lcss")

    @lcss.setter
    def lcss(self, value: int) -> None:
        """Set the lcss property."""
        self._cards[5].set_value("lcss", value)

    @property
    def lclf(self) -> int:
        """Get or set the Load curve ID defining failure void volume fraction versus element length. The values L1-L4 and FF1-FF4 are ignored with this option.
        """ # nopep8
        return self._cards[5].get_value("lclf")

    @lclf.setter
    def lclf(self, value: int) -> None:
        """Set the lclf property."""
        self._cards[5].set_value("lclf", value)

    @property
    def numint(self) -> float:
        """Get or set the Number of through thickness integration points which must fail before the element is deleted.
        """ # nopep8
        return self._cards[5].get_value("numint")

    @numint.setter
    def numint(self, value: float) -> None:
        """Set the numint property."""
        self._cards[5].set_value("numint", value)

    @property
    def lcf0(self) -> int:
        """Get or set the Load curve ID defining initial void volume fraction   versus element length.  This option is available starting with the second formal release of version 971..
        """ # nopep8
        return self._cards[5].get_value("lcf0")

    @lcf0.setter
    def lcf0(self, value: int) -> None:
        """Set the lcf0 property."""
        self._cards[5].set_value("lcf0", value)

    @property
    def lcfc(self) -> int:
        """Get or set the Load curve ID defining critical void volume fraction   versus element length.  This option is available starting with the second formal release of version 971.
        """ # nopep8
        return self._cards[5].get_value("lcfc")

    @lcfc.setter
    def lcfc(self, value: int) -> None:
        """Set the lcfc property."""
        self._cards[5].set_value("lcfc", value)

    @property
    def lcfn(self) -> int:
        """Get or set the Load curve ID defining void volume fraction of nucleating particles   versus element length.  This option is available starting with the second formal release of version 971..
        """ # nopep8
        return self._cards[5].get_value("lcfn")

    @lcfn.setter
    def lcfn(self, value: int) -> None:
        """Set the lcfn property."""
        self._cards[5].set_value("lcfn", value)

    @property
    def vgtyp(self) -> typing.Optional[float]:
        """Get or set the Type of void growth behavior.
        EQ.0.0: Void growth in case of tension and void contraction in case of compression, but never below   (default).
        EQ.1.0: Void growth only in case of tension.
        EQ.2.0: Void growth in case of tension and void contraction in case of compression
        """ # nopep8
        return self._cards[5].get_value("vgtyp")

    @vgtyp.setter
    def vgtyp(self, value: float) -> None:
        """Set the vgtyp property."""
        self._cards[5].set_value("vgtyp", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[6].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[6].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def lcss_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcss."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcss:
                return kwd
        return None

    @lcss_link.setter
    def lcss_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcss."""
        self.lcss = value.lcid

    @property
    def lclf_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lclf."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lclf:
                return kwd
        return None

    @lclf_link.setter
    def lclf_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lclf."""
        self.lclf = value.lcid

    @property
    def lcf0_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcf0."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcf0:
                return kwd
        return None

    @lcf0_link.setter
    def lcf0_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcf0."""
        self.lcf0 = value.lcid

    @property
    def lcfc_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcfc."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcfc:
                return kwd
        return None

    @lcfc_link.setter
    def lcfc_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcfc."""
        self.lcfc = value.lcid

    @property
    def lcfn_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcfn."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcfn:
                return kwd
        return None

    @lcfn_link.setter
    def lcfn_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcfn."""
        self.lcfn = value.lcid

