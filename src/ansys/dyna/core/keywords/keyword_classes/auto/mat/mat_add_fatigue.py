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

"""Module providing the MatAddFatigue class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MATADDFATIGUE_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("lcid", int, 10, 10, -1),
    FieldSchema("ltype", int, 20, 10, 0),
    FieldSchema("a", float, 30, 10, None),
    FieldSchema("b", float, 40, 10, None),
    FieldSchema("sthres", float, 50, 10, None),
    FieldSchema("snlimt", int, 60, 10, 0),
    FieldSchema("sntype", int, 70, 10, 0),
)

_MATADDFATIGUE_CARD1 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("ai", float, 30, 10, None),
    FieldSchema("bi", float, 40, 10, None),
    FieldSchema("sthresi", float, 50, 10, None),
)

_MATADDFATIGUE_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatAddFatigue(KeywordBase):
    """DYNA MAT_ADD_FATIGUE keyword"""

    keyword = "MAT"
    subkeyword = "ADD_FATIGUE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatAddFatigue class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATADDFATIGUE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATADDFATIGUE_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatAddFatigue.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATADDFATIGUE_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification for which the fatigue property applies.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def lcid(self) -> int:
        """Get or set the S-N fatigue curve ID.
        GT.0: S-N fatigue curve ID.
        EQ.-1: S-N fatigue curve uses equation N*S**b=a.
        EQ.-2: S-N fatigue curve uses equation log(S)=a-b*log(N).
        EQ.-3: S-N fatigue curve uses equation S=a*N**b
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def ltype(self) -> int:
        """Get or set the Type of S-N curve.
        EQ.0: Semi-log interpolation (default).
        EQ.1: Log-Log interpolation.
        EQ.2: Linear-Linear interpolation.
        """ # nopep8
        return self._cards[0].get_value("ltype")

    @ltype.setter
    def ltype(self, value: int) -> None:
        """Set the ltype property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""ltype must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("ltype", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Material parameter a in S-N fatigue equation.
        """ # nopep8
        return self._cards[0].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[0].set_value("a", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Material parameter a in S-N fatigue equation.
        """ # nopep8
        return self._cards[0].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[0].set_value("b", value)

    @property
    def sthres(self) -> typing.Optional[float]:
        """Get or set the Fatigue threshold if the S-N curve is defined by equation (LCID<0).
        """ # nopep8
        return self._cards[0].get_value("sthres")

    @sthres.setter
    def sthres(self, value: float) -> None:
        """Set the sthres property."""
        self._cards[0].set_value("sthres", value)

    @property
    def snlimt(self) -> int:
        """Get or set the If LCID > 0
        Flag setting algorithm used when stress is lower than the lowest stress on S-N curve.
        EQ.0: use the life at the last point on S-N curve.
        EQ.1: extrapolation from the last two points on S-N curve.
        EQ.2: infinity.
        If LCID < 0
        Flag setting algorithm used when stress is lower than STHRES.
        EQ.0: use the life at STHRES.
        EQ.1: Ignored. only applicable for LCID > 0.
        EQ.2: infinity.
        """ # nopep8
        return self._cards[0].get_value("snlimt")

    @snlimt.setter
    def snlimt(self, value: int) -> None:
        """Set the snlimt property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""snlimt must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("snlimt", value)

    @property
    def sntype(self) -> int:
        """Get or set the Stress type of S-N curve.
        EQ.0: stress range (default)
        EQ.1: stress amplitude.
        """ # nopep8
        return self._cards[0].get_value("sntype")

    @sntype.setter
    def sntype(self, value: int) -> None:
        """Set the sntype property."""
        if value not in [0, 1, None]:
            raise Exception("""sntype must be `None` or one of {0,1}.""")
        self._cards[0].set_value("sntype", value)

    @property
    def ai(self) -> typing.Optional[float]:
        """Get or set the Material parameter a in S-N fatigue equation for the i-th segment.
        """ # nopep8
        return self._cards[1].get_value("ai")

    @ai.setter
    def ai(self, value: float) -> None:
        """Set the ai property."""
        self._cards[1].set_value("ai", value)

    @property
    def bi(self) -> typing.Optional[float]:
        """Get or set the Material parameter b in S-N fatigue equation for the i-th segment.
        """ # nopep8
        return self._cards[1].get_value("bi")

    @bi.setter
    def bi(self, value: float) -> None:
        """Set the bi property."""
        self._cards[1].set_value("bi", value)

    @property
    def sthresi(self) -> typing.Optional[float]:
        """Get or set the Fatigue threshold stress for the i-th segment.
        """ # nopep8
        return self._cards[1].get_value("sthresi")

    @sthresi.setter
    def sthresi(self, value: float) -> None:
        """Set the sthresi property."""
        self._cards[1].set_value("sthresi", value)

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
    def lcid_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid:
                return kwd
        return None

    @lcid_link.setter
    def lcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid."""
        self.lcid = value.lcid

