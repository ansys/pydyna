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

"""Module providing the Mat252 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_MAT252_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("flg", int, 40, 10, 0),
    FieldSchema("jcfl", int, 50, 10, 0),
    FieldSchema("dopt", int, 60, 10, None),
)

_MAT252_CARD1 = (
    FieldSchema("lcss", int, 0, 10, None),
    FieldSchema("tau0", float, 10, 10, None),
    FieldSchema("q", float, 20, 10, None),
    FieldSchema("b", float, 30, 10, None),
    FieldSchema("h", float, 40, 10, None),
    FieldSchema("c", float, 50, 10, None),
    FieldSchema("gam0", float, 60, 10, None),
    FieldSchema("gamm", float, 70, 10, None),
)

_MAT252_CARD2 = (
    FieldSchema("a10", float, 0, 10, None),
    FieldSchema("a20", float, 10, 10, None),
    FieldSchema("a1h", float, 20, 10, None),
    FieldSchema("a2h", float, 30, 10, None),
    FieldSchema("a2s", float, 40, 10, None),
    FieldSchema("pow", float, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("srfilt", float, 70, 10, None),
)

_MAT252_CARD3 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("d1", float, 20, 10, None),
    FieldSchema("d2", float, 30, 10, None),
    FieldSchema("d3", float, 40, 10, None),
    FieldSchema("d4", float, 50, 10, None),
    FieldSchema("d1c", float, 60, 10, None),
    FieldSchema("d2c", float, 70, 10, None),
)

_MAT252_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat252(KeywordBase):
    """DYNA MAT_252 keyword"""

    keyword = "MAT"
    subkeyword = "252"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lcss": LinkType.DEFINE_CURVE_OR_TABLE,
    }

    def __init__(self, **kwargs):
        """Initialize the Mat252 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT252_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT252_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT252_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT252_CARD3,
                **kwargs,
            ),            OptionCardSet(
                option_spec = Mat252.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT252_OPTION0_CARD0,
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
    def flg(self) -> int:
        """Get or set the Flag to choose between yield functions f and f^, see Remarks.
        EQ.0.0: Cap in tension and nonlinear Drucker & Prager in compression,
        EQ.2.0: Cap in tension. and von Mises in compression
        """ # nopep8
        return self._cards[0].get_value("flg")

    @flg.setter
    def flg(self, value: int) -> None:
        """Set the flg property."""
        if value not in [0.0, 2.0, None]:
            raise Exception("""flg must be `None` or one of {0.0,2.0}.""")
        self._cards[0].set_value("flg", value)

    @property
    def jcfl(self) -> int:
        """Get or set the Johnson & Cook constitutive failure criterion flag, see Remarks.
        EQ.0.0: use triaxiality factor only in tension,
        EQ.1.0: use triaxiality factor in tension and compression
        """ # nopep8
        return self._cards[0].get_value("jcfl")

    @jcfl.setter
    def jcfl(self, value: int) -> None:
        """Set the jcfl property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""jcfl must be `None` or one of {0.0,1.0}.""")
        self._cards[0].set_value("jcfl", value)

    @property
    def dopt(self) -> typing.Optional[int]:
        """Get or set the Damage criterion flag D or D^, see Remarks.
        EQ.0.0: damage model uses damage plastic strain r.
        damage model uses plastic arc length rv
        """ # nopep8
        return self._cards[0].get_value("dopt")

    @dopt.setter
    def dopt(self, value: int) -> None:
        """Set the dopt property."""
        self._cards[0].set_value("dopt", value)

    @property
    def lcss(self) -> typing.Optional[int]:
        """Get or set the Curve ID or Table ID.
        """ # nopep8
        return self._cards[1].get_value("lcss")

    @lcss.setter
    def lcss(self, value: int) -> None:
        """Set the lcss property."""
        self._cards[1].set_value("lcss", value)

    @property
    def tau0(self) -> typing.Optional[float]:
        """Get or set the Initial shear yield stress.
        """ # nopep8
        return self._cards[1].get_value("tau0")

    @tau0.setter
    def tau0(self, value: float) -> None:
        """Set the tau0 property."""
        self._cards[1].set_value("tau0", value)

    @property
    def q(self) -> typing.Optional[float]:
        """Get or set the Isotropic nonlinear hardening modulus q.
        """ # nopep8
        return self._cards[1].get_value("q")

    @q.setter
    def q(self, value: float) -> None:
        """Set the q property."""
        self._cards[1].set_value("q", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Isotropic exponential decay parameter b.
        """ # nopep8
        return self._cards[1].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[1].set_value("b", value)

    @property
    def h(self) -> typing.Optional[float]:
        """Get or set the Isotropic linear hardening modulus H
        """ # nopep8
        return self._cards[1].get_value("h")

    @h.setter
    def h(self, value: float) -> None:
        """Set the h property."""
        self._cards[1].set_value("h", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Strain rate coefficient C
        """ # nopep8
        return self._cards[1].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[1].set_value("c", value)

    @property
    def gam0(self) -> typing.Optional[float]:
        """Get or set the Quasi-static threshold strain rate
        """ # nopep8
        return self._cards[1].get_value("gam0")

    @gam0.setter
    def gam0(self, value: float) -> None:
        """Set the gam0 property."""
        self._cards[1].set_value("gam0", value)

    @property
    def gamm(self) -> typing.Optional[float]:
        """Get or set the Maximum threshold strain rate
        """ # nopep8
        return self._cards[1].get_value("gamm")

    @gamm.setter
    def gamm(self, value: float) -> None:
        """Set the gamm property."""
        self._cards[1].set_value("gamm", value)

    @property
    def a10(self) -> typing.Optional[float]:
        """Get or set the Yield function parameter: initial value.
        """ # nopep8
        return self._cards[2].get_value("a10")

    @a10.setter
    def a10(self, value: float) -> None:
        """Set the a10 property."""
        self._cards[2].set_value("a10", value)

    @property
    def a20(self) -> typing.Optional[float]:
        """Get or set the Yield function parameter: initial value.
        """ # nopep8
        return self._cards[2].get_value("a20")

    @a20.setter
    def a20(self, value: float) -> None:
        """Set the a20 property."""
        self._cards[2].set_value("a20", value)

    @property
    def a1h(self) -> typing.Optional[float]:
        """Get or set the Yield function parameter for formative hardening.
        """ # nopep8
        return self._cards[2].get_value("a1h")

    @a1h.setter
    def a1h(self, value: float) -> None:
        """Set the a1h property."""
        self._cards[2].set_value("a1h", value)

    @property
    def a2h(self) -> typing.Optional[float]:
        """Get or set the Yield function parameter for formative hardening.
        """ # nopep8
        return self._cards[2].get_value("a2h")

    @a2h.setter
    def a2h(self, value: float) -> None:
        """Set the a2h property."""
        self._cards[2].set_value("a2h", value)

    @property
    def a2s(self) -> typing.Optional[float]:
        """Get or set the Plastic potential parameter for hydrostatic stress term
        """ # nopep8
        return self._cards[2].get_value("a2s")

    @a2s.setter
    def a2s(self, value: float) -> None:
        """Set the a2s property."""
        self._cards[2].set_value("a2s", value)

    @property
    def pow(self) -> typing.Optional[float]:
        """Get or set the Exponent of the phenomenological damage model
        """ # nopep8
        return self._cards[2].get_value("pow")

    @pow.setter
    def pow(self, value: float) -> None:
        """Set the pow property."""
        self._cards[2].set_value("pow", value)

    @property
    def srfilt(self) -> typing.Optional[float]:
        """Get or set the Strain rate filtering parameter in exponential moving average with admissible values ranging from 0 to 1
        """ # nopep8
        return self._cards[2].get_value("srfilt")

    @srfilt.setter
    def srfilt(self, value: float) -> None:
        """Set the srfilt property."""
        self._cards[2].set_value("srfilt", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Johnson & Cook failure parameter d1.
        """ # nopep8
        return self._cards[3].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[3].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Johnson & Cook failure parameter d2.
        """ # nopep8
        return self._cards[3].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[3].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Johnson & Cook failure parameter d3
        """ # nopep8
        return self._cards[3].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[3].set_value("d3", value)

    @property
    def d4(self) -> typing.Optional[float]:
        """Get or set the Johnson & Cook rate dependent failure parameter d4.
        """ # nopep8
        return self._cards[3].get_value("d4")

    @d4.setter
    def d4(self, value: float) -> None:
        """Set the d4 property."""
        self._cards[3].set_value("d4", value)

    @property
    def d1c(self) -> typing.Optional[float]:
        """Get or set the Johnson & Cook damage threshold parameter d1c
        """ # nopep8
        return self._cards[3].get_value("d1c")

    @d1c.setter
    def d1c(self, value: float) -> None:
        """Set the d1c property."""
        self._cards[3].set_value("d1c", value)

    @property
    def d2c(self) -> typing.Optional[float]:
        """Get or set the Johnson & Cook damage threshold parameter d2c
        """ # nopep8
        return self._cards[3].get_value("d2c")

    @d2c.setter
    def d2c(self, value: float) -> None:
        """Set the d2c property."""
        self._cards[3].set_value("d2c", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[4].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def lcss_link(self) -> typing.Optional[KeywordBase]:
        """Get the linked DEFINE_CURVE or DEFINE_TABLE for lcss."""
        if self.deck is None:
            return None
        field_value = self.lcss
        if field_value is None or field_value == 0:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == field_value:
                return kwd
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "TABLE"):
            if kwd.tbid == field_value:
                return kwd
        return None

    @lcss_link.setter
    def lcss_link(self, value: KeywordBase) -> None:
        """Set the linked keyword for lcss."""
        if hasattr(value, "lcid"):
            self.lcss = value.lcid
        elif hasattr(value, "tbid"):
            self.lcss = value.tbid

