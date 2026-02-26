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

"""Module providing the MatSteelEc3 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MATSTEELEC3_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("sgy", float, 40, 10, None),
)

_MATSTEELEC3_CARD1 = (
    FieldSchema("lc_e", int, 0, 10, None),
    FieldSchema("lc_pr", int, 10, 10, None),
    FieldSchema("lc_al", int, 20, 10, None),
    FieldSchema("tbl_ss", int, 30, 10, None),
    FieldSchema("lc_fs", int, 40, 10, None),
)

_MATSTEELEC3_CARD2 = (
)

_MATSTEELEC3_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatSteelEc3(KeywordBase):
    """DYNA MAT_STEEL_EC3 keyword"""

    keyword = "MAT"
    subkeyword = "STEEL_EC3"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lc_e": LinkType.DEFINE_CURVE,
        "lc_pr": LinkType.DEFINE_CURVE,
        "lc_al": LinkType.DEFINE_CURVE,
        "tbl_ss": LinkType.DEFINE_CURVE,
        "lc_fs": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatSteelEc3 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATSTEELEC3_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATSTEELEC3_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATSTEELEC3_CARD2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatSteelEc3.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATSTEELEC3_OPTION0_CARD0,
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
        """Get or set the Young's modulus - a reasonable value must be provided even if LC_E is also input.
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
    def sgy(self) -> typing.Optional[float]:
        """Get or set the Initial yield stress.
        """ # nopep8
        return self._cards[0].get_value("sgy")

    @sgy.setter
    def sgy(self, value: float) -> None:
        """Set the sgy property."""
        self._cards[0].set_value("sgy", value)

    @property
    def lc_e(self) -> typing.Optional[int]:
        """Get or set the Optional Load curve ID: Young's Modulus vs Temperature (overrides E	and factors from EC3).
        """ # nopep8
        return self._cards[1].get_value("lc_e")

    @lc_e.setter
    def lc_e(self, value: int) -> None:
        """Set the lc_e property."""
        self._cards[1].set_value("lc_e", value)

    @property
    def lc_pr(self) -> typing.Optional[int]:
        """Get or set the Optional Load curve ID: Poisson's Ratio vs Temperature (overrides PR).
        """ # nopep8
        return self._cards[1].get_value("lc_pr")

    @lc_pr.setter
    def lc_pr(self, value: int) -> None:
        """Set the lc_pr property."""
        self._cards[1].set_value("lc_pr", value)

    @property
    def lc_al(self) -> typing.Optional[int]:
        """Get or set the Optional Load curve ID: alpha vs temperature (over-rides thermal expansion data from EC3).
        """ # nopep8
        return self._cards[1].get_value("lc_al")

    @lc_al.setter
    def lc_al(self, value: int) -> None:
        """Set the lc_al property."""
        self._cards[1].set_value("lc_al", value)

    @property
    def tbl_ss(self) -> typing.Optional[int]:
        """Get or set the Optional Table ID containing stress-strain curves at different temperatures (overrides curves from EC3).
        """ # nopep8
        return self._cards[1].get_value("tbl_ss")

    @tbl_ss.setter
    def tbl_ss(self, value: int) -> None:
        """Set the tbl_ss property."""
        self._cards[1].set_value("tbl_ss", value)

    @property
    def lc_fs(self) -> typing.Optional[int]:
        """Get or set the Optional Load curve ID: failure strain vs temperature.
        """ # nopep8
        return self._cards[1].get_value("lc_fs")

    @lc_fs.setter
    def lc_fs(self, value: int) -> None:
        """Set the lc_fs property."""
        self._cards[1].set_value("lc_fs", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[3].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def lc_e_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lc_e."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc_e:
                return kwd
        return None

    @lc_e_link.setter
    def lc_e_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc_e."""
        self.lc_e = value.lcid

    @property
    def lc_pr_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lc_pr."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc_pr:
                return kwd
        return None

    @lc_pr_link.setter
    def lc_pr_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc_pr."""
        self.lc_pr = value.lcid

    @property
    def lc_al_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lc_al."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc_al:
                return kwd
        return None

    @lc_al_link.setter
    def lc_al_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc_al."""
        self.lc_al = value.lcid

    @property
    def tbl_ss_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for tbl_ss."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.tbl_ss:
                return kwd
        return None

    @tbl_ss_link.setter
    def tbl_ss_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for tbl_ss."""
        self.tbl_ss = value.lcid

    @property
    def lc_fs_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lc_fs."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc_fs:
                return kwd
        return None

    @lc_fs_link.setter
    def lc_fs_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc_fs."""
        self.lc_fs = value.lcid

