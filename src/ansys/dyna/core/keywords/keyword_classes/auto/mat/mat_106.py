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

"""Module providing the Mat106 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MAT106_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("sigy", float, 40, 10, None),
    FieldSchema("alpha", float, 50, 10, None),
    FieldSchema("lcss", int, 60, 10, None),
    FieldSchema("fail", float, 70, 10, None),
)

_MAT106_CARD1 = (
    FieldSchema("qr1", float, 0, 10, None),
    FieldSchema("cr1", float, 10, 10, None),
    FieldSchema("qr2", float, 20, 10, None),
    FieldSchema("cr2", float, 30, 10, None),
    FieldSchema("qx1", float, 40, 10, None),
    FieldSchema("cx1", float, 50, 10, None),
    FieldSchema("qx2", float, 60, 10, None),
    FieldSchema("cx2", float, 70, 10, None),
)

_MAT106_CARD2 = (
    FieldSchema("c", float, 0, 10, None),
    FieldSchema("p", float, 10, 10, None),
    FieldSchema("lce", float, 20, 10, None),
    FieldSchema("lcpr", float, 30, 10, None),
    FieldSchema("lcsigy", float, 40, 10, None),
    FieldSchema("lcr", float, 50, 10, None),
    FieldSchema("lcx", float, 60, 10, None),
    FieldSchema("lcalph", float, 70, 10, None),
)

_MAT106_CARD3 = (
    FieldSchema("lcc", int, 0, 10, None),
    FieldSchema("lcp", int, 10, 10, None),
    FieldSchema("tref", float, 20, 10, None),
    FieldSchema("lcfail", float, 30, 10, None),
    FieldSchema("nuhis", int, 40, 10, None),
    FieldSchema("t1phas", float, 50, 10, None),
    FieldSchema("t2phas", float, 60, 10, None),
    FieldSchema("tol", float, 70, 10, None),
)

_MAT106_CARD4 = (
    FieldSchema("fushi1", int, 0, 10, None),
    FieldSchema("fushi2", int, 10, 10, None),
    FieldSchema("fushi3", int, 20, 10, None),
    FieldSchema("fushi4", int, 30, 10, None),
    FieldSchema("fushi5", int, 40, 10, None),
    FieldSchema("fushi6", int, 50, 10, None),
    FieldSchema("fushi7", int, 60, 10, None),
    FieldSchema("fushi8", int, 70, 10, None),
)

_MAT106_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat106(KeywordBase):
    """DYNA MAT_106 keyword"""

    keyword = "MAT"
    subkeyword = "106"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lcss": LinkType.DEFINE_CURVE,
        "lce": LinkType.DEFINE_CURVE,
        "lcpr": LinkType.DEFINE_CURVE,
        "lcsigy": LinkType.DEFINE_CURVE,
        "lcr": LinkType.DEFINE_CURVE,
        "lcx": LinkType.DEFINE_CURVE,
        "lcalph": LinkType.DEFINE_CURVE,
        "lcc": LinkType.DEFINE_CURVE,
        "lcp": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the Mat106 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT106_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT106_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT106_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT106_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT106_CARD4,
                **kwargs,
            ),            OptionCardSet(
                option_spec = Mat106.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT106_OPTION0_CARD0,
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
        """Get or set the Initial yield stress.
        """ # nopep8
        return self._cards[0].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        """Set the sigy property."""
        self._cards[0].set_value("sigy", value)

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
    def lcss(self) -> typing.Optional[int]:
        """Get or set the Load curve ID. The load curve ID defines effective stress versus effective plastic strain.
        Card 2 is ignored with this option.
        """ # nopep8
        return self._cards[0].get_value("lcss")

    @lcss.setter
    def lcss(self, value: int) -> None:
        """Set the lcss property."""
        self._cards[0].set_value("lcss", value)

    @property
    def fail(self) -> typing.Optional[float]:
        """Get or set the Effective plastic failure strain for erosion.
        """ # nopep8
        return self._cards[0].get_value("fail")

    @fail.setter
    def fail(self, value: float) -> None:
        """Set the fail property."""
        self._cards[0].set_value("fail", value)

    @property
    def qr1(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter QR1.
        """ # nopep8
        return self._cards[1].get_value("qr1")

    @qr1.setter
    def qr1(self, value: float) -> None:
        """Set the qr1 property."""
        self._cards[1].set_value("qr1", value)

    @property
    def cr1(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter CR1.
        """ # nopep8
        return self._cards[1].get_value("cr1")

    @cr1.setter
    def cr1(self, value: float) -> None:
        """Set the cr1 property."""
        self._cards[1].set_value("cr1", value)

    @property
    def qr2(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter QR2.
        """ # nopep8
        return self._cards[1].get_value("qr2")

    @qr2.setter
    def qr2(self, value: float) -> None:
        """Set the qr2 property."""
        self._cards[1].set_value("qr2", value)

    @property
    def cr2(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter CR2.
        """ # nopep8
        return self._cards[1].get_value("cr2")

    @cr2.setter
    def cr2(self, value: float) -> None:
        """Set the cr2 property."""
        self._cards[1].set_value("cr2", value)

    @property
    def qx1(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening parameter QX1.
        """ # nopep8
        return self._cards[1].get_value("qx1")

    @qx1.setter
    def qx1(self, value: float) -> None:
        """Set the qx1 property."""
        self._cards[1].set_value("qx1", value)

    @property
    def cx1(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening parameter CX1.
        """ # nopep8
        return self._cards[1].get_value("cx1")

    @cx1.setter
    def cx1(self, value: float) -> None:
        """Set the cx1 property."""
        self._cards[1].set_value("cx1", value)

    @property
    def qx2(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening parameter QX2.
        """ # nopep8
        return self._cards[1].get_value("qx2")

    @qx2.setter
    def qx2(self, value: float) -> None:
        """Set the qx2 property."""
        self._cards[1].set_value("qx2", value)

    @property
    def cx2(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening parameter CX2.
        """ # nopep8
        return self._cards[1].get_value("cx2")

    @cx2.setter
    def cx2(self, value: float) -> None:
        """Set the cx2 property."""
        self._cards[1].set_value("cx2", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Viscous material parameter C.
        """ # nopep8
        return self._cards[2].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[2].set_value("c", value)

    @property
    def p(self) -> typing.Optional[float]:
        """Get or set the Viscous material parameter P.
        """ # nopep8
        return self._cards[2].get_value("p")

    @p.setter
    def p(self, value: float) -> None:
        """Set the p property."""
        self._cards[2].set_value("p", value)

    @property
    def lce(self) -> typing.Optional[float]:
        """Get or set the Load curve defining Young's modulus as a function of temperature.
        E on card 1 is ignored with this option.
        """ # nopep8
        return self._cards[2].get_value("lce")

    @lce.setter
    def lce(self, value: float) -> None:
        """Set the lce property."""
        self._cards[2].set_value("lce", value)

    @property
    def lcpr(self) -> typing.Optional[float]:
        """Get or set the Load curve defining Poisson's ratio as a function of temperature.
        PR on card 1 is ignored with this option.
        """ # nopep8
        return self._cards[2].get_value("lcpr")

    @lcpr.setter
    def lcpr(self, value: float) -> None:
        """Set the lcpr property."""
        self._cards[2].set_value("lcpr", value)

    @property
    def lcsigy(self) -> typing.Optional[float]:
        """Get or set the Load curve defining the initial yield stress as a function of temperature.
        SIGY on card 1 is ignored with this option.
        """ # nopep8
        return self._cards[2].get_value("lcsigy")

    @lcsigy.setter
    def lcsigy(self, value: float) -> None:
        """Set the lcsigy property."""
        self._cards[2].set_value("lcsigy", value)

    @property
    def lcr(self) -> typing.Optional[float]:
        """Get or set the Load curve for scaling the isotropic hardening parameters QR1 and QR2 or the stress given by the load curve LCSS as a function of temperature.
        """ # nopep8
        return self._cards[2].get_value("lcr")

    @lcr.setter
    def lcr(self, value: float) -> None:
        """Set the lcr property."""
        self._cards[2].set_value("lcr", value)

    @property
    def lcx(self) -> typing.Optional[float]:
        """Get or set the Load curve for scaling the isotropic hardening parameters QX1 and QX2 as a function of temperature.
        """ # nopep8
        return self._cards[2].get_value("lcx")

    @lcx.setter
    def lcx(self, value: float) -> None:
        """Set the lcx property."""
        self._cards[2].set_value("lcx", value)

    @property
    def lcalph(self) -> typing.Optional[float]:
        """Get or set the Load curve defining the coefficient of thermal expansion as a function of temperature.
        ALPHA on card 1 is ignored with this option.
        """ # nopep8
        return self._cards[2].get_value("lcalph")

    @lcalph.setter
    def lcalph(self, value: float) -> None:
        """Set the lcalph property."""
        self._cards[2].set_value("lcalph", value)

    @property
    def lcc(self) -> typing.Optional[int]:
        """Get or set the Load curve for scaling the viscous materal parameter C as a function of temperature.
        """ # nopep8
        return self._cards[3].get_value("lcc")

    @lcc.setter
    def lcc(self, value: int) -> None:
        """Set the lcc property."""
        self._cards[3].set_value("lcc", value)

    @property
    def lcp(self) -> typing.Optional[int]:
        """Get or set the Load curve for scaling the viscous material parameter P as a function of temperature.
        """ # nopep8
        return self._cards[3].get_value("lcp")

    @lcp.setter
    def lcp(self, value: int) -> None:
        """Set the lcp property."""
        self._cards[3].set_value("lcp", value)

    @property
    def tref(self) -> typing.Optional[float]:
        """Get or set the Reference temperature required if and only if LCALPH is given with a negative curve ID.
        """ # nopep8
        return self._cards[3].get_value("tref")

    @tref.setter
    def tref(self, value: float) -> None:
        """Set the tref property."""
        self._cards[3].set_value("tref", value)

    @property
    def lcfail(self) -> typing.Optional[float]:
        """Get or set the Load curve defining the plastic failure strain as a function of temperature. FAIL on card 1 is ignored with this option.
        """ # nopep8
        return self._cards[3].get_value("lcfail")

    @lcfail.setter
    def lcfail(self, value: float) -> None:
        """Set the lcfail property."""
        self._cards[3].set_value("lcfail", value)

    @property
    def nuhis(self) -> typing.Optional[int]:
        """Get or set the Number of additional user defined history variables
        """ # nopep8
        return self._cards[3].get_value("nuhis")

    @nuhis.setter
    def nuhis(self, value: int) -> None:
        """Set the nuhis property."""
        self._cards[3].set_value("nuhis", value)

    @property
    def t1phas(self) -> typing.Optional[float]:
        """Get or set the Lower temperature limit for cooling rate evaluation.  Cooling rate can be used as input for user defined variables
        """ # nopep8
        return self._cards[3].get_value("t1phas")

    @t1phas.setter
    def t1phas(self, value: float) -> None:
        """Set the t1phas property."""
        self._cards[3].set_value("t1phas", value)

    @property
    def t2phas(self) -> typing.Optional[float]:
        """Get or set the Upper temperature limit for cooling rate evaluation.  Cooling rate can be used as input for user defined variables
        """ # nopep8
        return self._cards[3].get_value("t2phas")

    @t2phas.setter
    def t2phas(self, value: float) -> None:
        """Set the t2phas property."""
        self._cards[3].set_value("t2phas", value)

    @property
    def tol(self) -> typing.Optional[float]:
        """Get or set the Optional tolerance for plasticity update. The default is 10-6 for solid elements and 10-3 for shells. This parameter overrides the default tolerance for all element types.
        """ # nopep8
        return self._cards[3].get_value("tol")

    @tol.setter
    def tol(self, value: float) -> None:
        """Set the tol property."""
        self._cards[3].set_value("tol", value)

    @property
    def fushi1(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables
        """ # nopep8
        return self._cards[4].get_value("fushi1")

    @fushi1.setter
    def fushi1(self, value: int) -> None:
        """Set the fushi1 property."""
        self._cards[4].set_value("fushi1", value)

    @property
    def fushi2(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables
        """ # nopep8
        return self._cards[4].get_value("fushi2")

    @fushi2.setter
    def fushi2(self, value: int) -> None:
        """Set the fushi2 property."""
        self._cards[4].set_value("fushi2", value)

    @property
    def fushi3(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables
        """ # nopep8
        return self._cards[4].get_value("fushi3")

    @fushi3.setter
    def fushi3(self, value: int) -> None:
        """Set the fushi3 property."""
        self._cards[4].set_value("fushi3", value)

    @property
    def fushi4(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables
        """ # nopep8
        return self._cards[4].get_value("fushi4")

    @fushi4.setter
    def fushi4(self, value: int) -> None:
        """Set the fushi4 property."""
        self._cards[4].set_value("fushi4", value)

    @property
    def fushi5(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables
        """ # nopep8
        return self._cards[4].get_value("fushi5")

    @fushi5.setter
    def fushi5(self, value: int) -> None:
        """Set the fushi5 property."""
        self._cards[4].set_value("fushi5", value)

    @property
    def fushi6(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables
        """ # nopep8
        return self._cards[4].get_value("fushi6")

    @fushi6.setter
    def fushi6(self, value: int) -> None:
        """Set the fushi6 property."""
        self._cards[4].set_value("fushi6", value)

    @property
    def fushi7(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables
        """ # nopep8
        return self._cards[4].get_value("fushi7")

    @fushi7.setter
    def fushi7(self, value: int) -> None:
        """Set the fushi7 property."""
        self._cards[4].set_value("fushi7", value)

    @property
    def fushi8(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables
        """ # nopep8
        return self._cards[4].get_value("fushi8")

    @fushi8.setter
    def fushi8(self, value: int) -> None:
        """Set the fushi8 property."""
        self._cards[4].set_value("fushi8", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[5].cards[0].set_value("title", value)

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
    def lce_link(self) -> typing.Optional[DefineCurve]:
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
    def lcpr_link(self) -> typing.Optional[DefineCurve]:
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
    def lcsigy_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcsigy."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcsigy:
                return kwd
        return None

    @lcsigy_link.setter
    def lcsigy_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcsigy."""
        self.lcsigy = value.lcid

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

    @property
    def lcx_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcx."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcx:
                return kwd
        return None

    @lcx_link.setter
    def lcx_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcx."""
        self.lcx = value.lcid

    @property
    def lcalph_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcalph."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcalph:
                return kwd
        return None

    @lcalph_link.setter
    def lcalph_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcalph."""
        self.lcalph = value.lcid

    @property
    def lcc_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcc."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcc:
                return kwd
        return None

    @lcc_link.setter
    def lcc_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcc."""
        self.lcc = value.lcid

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

