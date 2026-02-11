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

"""Module providing the Mat019 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MAT019_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("vp", float, 40, 10, 0.0),
)

_MAT019_CARD1 = (
    FieldSchema("lc1", int, 0, 10, 0),
    FieldSchema("etan", float, 10, 10, None),
    FieldSchema("lc2", int, 20, 10, 0),
    FieldSchema("lc3", int, 30, 10, 0),
    FieldSchema("lc4", int, 40, 10, 0),
    FieldSchema("tdel", float, 50, 10, None),
    FieldSchema("rdef", float, 60, 10, 1.0),
)

_MAT019_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat019(KeywordBase):
    """DYNA MAT_019 keyword"""

    keyword = "MAT"
    subkeyword = "019"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lc1": LinkType.DEFINE_CURVE,
        "lc2": LinkType.DEFINE_CURVE,
        "lc3": LinkType.DEFINE_CURVE,
        "lc4": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the Mat019 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT019_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT019_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = Mat019.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT019_OPTION0_CARD0,
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
    def vp(self) -> float:
        """Get or set the Formulation for rate effects:
        EQ.0.0: Scale yield stress (default),
        EQ.1.0: Viscoplastic formulation.
        """ # nopep8
        return self._cards[0].get_value("vp")

    @vp.setter
    def vp(self, value: float) -> None:
        """Set the vp property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""vp must be `None` or one of {0.0,1.0}.""")
        self._cards[0].set_value("vp", value)

    @property
    def lc1(self) -> int:
        """Get or set the Load curve ID defining the yield stress as a function of the effective strain rate.
        """ # nopep8
        return self._cards[1].get_value("lc1")

    @lc1.setter
    def lc1(self, value: int) -> None:
        """Set the lc1 property."""
        self._cards[1].set_value("lc1", value)

    @property
    def etan(self) -> typing.Optional[float]:
        """Get or set the Plastic hardening modulus.
        """ # nopep8
        return self._cards[1].get_value("etan")

    @etan.setter
    def etan(self, value: float) -> None:
        """Set the etan property."""
        self._cards[1].set_value("etan", value)

    @property
    def lc2(self) -> int:
        """Get or set the Load curve ID defining Young's modulus as a function of the effective strain rate (optional).
        """ # nopep8
        return self._cards[1].get_value("lc2")

    @lc2.setter
    def lc2(self, value: int) -> None:
        """Set the lc2 property."""
        self._cards[1].set_value("lc2", value)

    @property
    def lc3(self) -> int:
        """Get or set the Load curve ID defining tangent modulus as a function of the effective strain rate (optional).
        """ # nopep8
        return self._cards[1].get_value("lc3")

    @lc3.setter
    def lc3(self, value: int) -> None:
        """Set the lc3 property."""
        self._cards[1].set_value("lc3", value)

    @property
    def lc4(self) -> int:
        """Get or set the Load curve ID defining von Mises stress at failure as a function of the effective strain rate (optional).
        """ # nopep8
        return self._cards[1].get_value("lc4")

    @lc4.setter
    def lc4(self, value: int) -> None:
        """Set the lc4 property."""
        self._cards[1].set_value("lc4", value)

    @property
    def tdel(self) -> typing.Optional[float]:
        """Get or set the Minimum time step size for automatic element deletion. Use for shells only.
        """ # nopep8
        return self._cards[1].get_value("tdel")

    @tdel.setter
    def tdel(self, value: float) -> None:
        """Set the tdel property."""
        self._cards[1].set_value("tdel", value)

    @property
    def rdef(self) -> float:
        """Get or set the Redefinition of failure curve:
        EQ.1.0: Effective plastic strain (default),
        EQ.2.0: Maximum principal stress.
        EQ.3.0:  Maximum principal stress
        """ # nopep8
        return self._cards[1].get_value("rdef")

    @rdef.setter
    def rdef(self, value: float) -> None:
        """Set the rdef property."""
        if value not in [1.0, 2.0, 3.0, None]:
            raise Exception("""rdef must be `None` or one of {1.0,2.0,3.0}.""")
        self._cards[1].set_value("rdef", value)

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
    def lc1_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lc1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc1:
                return kwd
        return None

    @lc1_link.setter
    def lc1_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc1."""
        self.lc1 = value.lcid

    @property
    def lc2_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lc2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc2:
                return kwd
        return None

    @lc2_link.setter
    def lc2_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc2."""
        self.lc2 = value.lcid

    @property
    def lc3_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lc3."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc3:
                return kwd
        return None

    @lc3_link.setter
    def lc3_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc3."""
        self.lc3 = value.lcid

    @property
    def lc4_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lc4."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc4:
                return kwd
        return None

    @lc4_link.setter
    def lc4_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc4."""
        self.lc4 = value.lcid

