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

"""Module providing the SetSegmentAdd class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.series_card import SeriesCard
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_SETSEGMENTADD_CARD0 = (
    FieldSchema("sid", int, 0, 10, None),
)

_SETSEGMENTADD_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SetSegmentAdd(KeywordBase):
    """DYNA SET_SEGMENT_ADD keyword"""

    keyword = "SET"
    subkeyword = "SEGMENT_ADD"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "ssid1": LinkType.SET_SEGMENT,
        "ssid2": LinkType.SET_SEGMENT,
        "ssid3": LinkType.SET_SEGMENT,
        "ssid4": LinkType.SET_SEGMENT,
        "ssid5": LinkType.SET_SEGMENT,
        "ssid6": LinkType.SET_SEGMENT,
        "ssid7": LinkType.SET_SEGMENT,
        "ssid8": LinkType.SET_SEGMENT,
    }

    def __init__(self, **kwargs):
        """Initialize the SetSegmentAdd class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SETSEGMENTADD_CARD0,
                **kwargs,
            ),            SeriesCard(
                "sets",
                8,
                10,
                int,
                None,
                data = kwargs.get("sets")),            OptionCardSet(
                option_spec = SetSegmentAdd.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SETSEGMENTADD_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID. All segment sets should have a unique set ID.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def sets(self) -> SeriesCard:
        """dynamic array of set segment ids.."""
        return self._cards[1]

    @sets.setter
    def sets(self, value: typing.List) -> None:
        self._cards[1].data = value

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
    def ssid1_link(self) -> KeywordBase:
        """Get the SET_SEGMENT_* keyword for ssid1."""
        return self._get_set_link("SEGMENT", self.ssid1)

    @ssid1_link.setter
    def ssid1_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for ssid1."""
        self.ssid1 = value.sid

    @property
    def ssid2_link(self) -> KeywordBase:
        """Get the SET_SEGMENT_* keyword for ssid2."""
        return self._get_set_link("SEGMENT", self.ssid2)

    @ssid2_link.setter
    def ssid2_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for ssid2."""
        self.ssid2 = value.sid

    @property
    def ssid3_link(self) -> KeywordBase:
        """Get the SET_SEGMENT_* keyword for ssid3."""
        return self._get_set_link("SEGMENT", self.ssid3)

    @ssid3_link.setter
    def ssid3_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for ssid3."""
        self.ssid3 = value.sid

    @property
    def ssid4_link(self) -> KeywordBase:
        """Get the SET_SEGMENT_* keyword for ssid4."""
        return self._get_set_link("SEGMENT", self.ssid4)

    @ssid4_link.setter
    def ssid4_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for ssid4."""
        self.ssid4 = value.sid

    @property
    def ssid5_link(self) -> KeywordBase:
        """Get the SET_SEGMENT_* keyword for ssid5."""
        return self._get_set_link("SEGMENT", self.ssid5)

    @ssid5_link.setter
    def ssid5_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for ssid5."""
        self.ssid5 = value.sid

    @property
    def ssid6_link(self) -> KeywordBase:
        """Get the SET_SEGMENT_* keyword for ssid6."""
        return self._get_set_link("SEGMENT", self.ssid6)

    @ssid6_link.setter
    def ssid6_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for ssid6."""
        self.ssid6 = value.sid

    @property
    def ssid7_link(self) -> KeywordBase:
        """Get the SET_SEGMENT_* keyword for ssid7."""
        return self._get_set_link("SEGMENT", self.ssid7)

    @ssid7_link.setter
    def ssid7_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for ssid7."""
        self.ssid7 = value.sid

    @property
    def ssid8_link(self) -> KeywordBase:
        """Get the SET_SEGMENT_* keyword for ssid8."""
        return self._get_set_link("SEGMENT", self.ssid8)

    @ssid8_link.setter
    def ssid8_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for ssid8."""
        self.ssid8 = value.sid

