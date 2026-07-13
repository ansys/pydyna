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

"""Module providing the LoadLoSeismicFreeField class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_LOADLOSEISMICFREEFIELD_CARD0 = (
    FieldSchema("lssid", int, 0, 10, None),
    FieldSchema("rssid", int, 10, 10, None),
    FieldSchema("lcd", int, 20, 10, None),
    FieldSchema("lcs", int, 30, 10, None),
)

class LoadLoSeismicFreeField(KeywordBase):
    """DYNA LOAD_LO_SEISMIC_FREE_FIELD keyword"""

    keyword = "LOAD"
    subkeyword = "LO_SEISMIC_FREE_FIELD"
    _link_fields = {
        "lcd": LinkType.DEFINE_CURVE,
        "lcs": LinkType.DEFINE_CURVE,
        "lssid": LinkType.SET_SEGMENT,
        "rssid": LinkType.SET_SEGMENT,
    }

    def __init__(self, **kwargs):
        """Initialize the LoadLoSeismicFreeField class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADLOSEISMICFREEFIELD_CARD0,
                **kwargs,
            ),
        ]
    @property
    def lssid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID for application of consistent seismic load.
        """ # nopep8
        return self._cards[0].get_value("lssid")

    @lssid.setter
    def lssid(self, value: int) -> None:
        """Set the lssid property."""
        self._cards[0].set_value("lssid", value)

    @property
    def rssid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID attached to reference elements (soil column) representing free-field site response.
        """ # nopep8
        return self._cards[0].get_value("rssid")

    @rssid.setter
    def rssid(self, value: int) -> None:
        """Set the rssid property."""
        self._cards[0].set_value("rssid", value)

    @property
    def lcd(self) -> typing.Optional[int]:
        """Get or set the Load curve defining pVp (y-axis) as a function of global Z coordinate (x-axis).
        """ # nopep8
        return self._cards[0].get_value("lcd")

    @lcd.setter
    def lcd(self, value: int) -> None:
        """Set the lcd property."""
        self._cards[0].set_value("lcd", value)

    @property
    def lcs(self) -> typing.Optional[int]:
        """Get or set the Load curve defining pVs (y-axis) as a function of global Z coordinate (x-axis).
        """ # nopep8
        return self._cards[0].get_value("lcs")

    @lcs.setter
    def lcs(self, value: int) -> None:
        """Set the lcs property."""
        self._cards[0].set_value("lcs", value)

    @property
    def lcd_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcd."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcd:
                return kwd
        return None

    @lcd_link.setter
    def lcd_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcd."""
        self.lcd = value.lcid

    @property
    def lcs_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcs."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcs:
                return kwd
        return None

    @lcs_link.setter
    def lcs_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcs."""
        self.lcs = value.lcid

    @property
    def lssid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SEGMENT_* keyword for lssid."""
        return self._get_set_link("SEGMENT", self.lssid)

    @lssid_link.setter
    def lssid_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for lssid."""
        self.lssid = value.sid

    @property
    def rssid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SEGMENT_* keyword for rssid."""
        return self._get_set_link("SEGMENT", self.rssid)

    @rssid_link.setter
    def rssid_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for rssid."""
        self.rssid = value.sid

