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

"""Module providing the SetSegmentIntersect class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_SETSEGMENTINTERSECT_CARD0 = (
    FieldSchema("sid", int, 0, 10, None),
)

_SETSEGMENTINTERSECT_CARD1 = (
    FieldSchema("ssid1", int, 0, 10, None),
    FieldSchema("ssid2", int, 10, 10, None),
    FieldSchema("ssid3", int, 20, 10, None),
    FieldSchema("ssid4", int, 30, 10, None),
    FieldSchema("ssid5", int, 40, 10, None),
    FieldSchema("ssid6", int, 50, 10, None),
    FieldSchema("ssid7", int, 60, 10, None),
    FieldSchema("ssid8", int, 70, 10, None),
)

_SETSEGMENTINTERSECT_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SetSegmentIntersect(KeywordBase):
    """DYNA SET_SEGMENT_INTERSECT keyword"""

    keyword = "SET"
    subkeyword = "SEGMENT_INTERSECT"
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
        """Initialize the SetSegmentIntersect class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SETSEGMENTINTERSECT_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SETSEGMENTINTERSECT_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = SetSegmentIntersect.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SETSEGMENTINTERSECT_OPTION0_CARD0,
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
    def ssid1(self) -> typing.Optional[int]:
        """Get or set the The nth shell set ID.
        """ # nopep8
        return self._cards[1].get_value("ssid1")

    @ssid1.setter
    def ssid1(self, value: int) -> None:
        """Set the ssid1 property."""
        self._cards[1].set_value("ssid1", value)

    @property
    def ssid2(self) -> typing.Optional[int]:
        """Get or set the The nth shell set ID.
        """ # nopep8
        return self._cards[1].get_value("ssid2")

    @ssid2.setter
    def ssid2(self, value: int) -> None:
        """Set the ssid2 property."""
        self._cards[1].set_value("ssid2", value)

    @property
    def ssid3(self) -> typing.Optional[int]:
        """Get or set the The nth shell set ID.
        """ # nopep8
        return self._cards[1].get_value("ssid3")

    @ssid3.setter
    def ssid3(self, value: int) -> None:
        """Set the ssid3 property."""
        self._cards[1].set_value("ssid3", value)

    @property
    def ssid4(self) -> typing.Optional[int]:
        """Get or set the The nth shell set ID.
        """ # nopep8
        return self._cards[1].get_value("ssid4")

    @ssid4.setter
    def ssid4(self, value: int) -> None:
        """Set the ssid4 property."""
        self._cards[1].set_value("ssid4", value)

    @property
    def ssid5(self) -> typing.Optional[int]:
        """Get or set the The nth shell set ID.
        """ # nopep8
        return self._cards[1].get_value("ssid5")

    @ssid5.setter
    def ssid5(self, value: int) -> None:
        """Set the ssid5 property."""
        self._cards[1].set_value("ssid5", value)

    @property
    def ssid6(self) -> typing.Optional[int]:
        """Get or set the The nth shell set ID.
        """ # nopep8
        return self._cards[1].get_value("ssid6")

    @ssid6.setter
    def ssid6(self, value: int) -> None:
        """Set the ssid6 property."""
        self._cards[1].set_value("ssid6", value)

    @property
    def ssid7(self) -> typing.Optional[int]:
        """Get or set the The nth shell set ID.
        """ # nopep8
        return self._cards[1].get_value("ssid7")

    @ssid7.setter
    def ssid7(self, value: int) -> None:
        """Set the ssid7 property."""
        self._cards[1].set_value("ssid7", value)

    @property
    def ssid8(self) -> typing.Optional[int]:
        """Get or set the The nth shell set ID.
        """ # nopep8
        return self._cards[1].get_value("ssid8")

    @ssid8.setter
    def ssid8(self, value: int) -> None:
        """Set the ssid8 property."""
        self._cards[1].set_value("ssid8", value)

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
    def ssid1_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SEGMENT_* keyword for ssid1."""
        return self._get_set_link("SEGMENT", self.ssid1)

    @ssid1_link.setter
    def ssid1_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for ssid1."""
        self.ssid1 = value.sid

    @property
    def ssid2_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SEGMENT_* keyword for ssid2."""
        return self._get_set_link("SEGMENT", self.ssid2)

    @ssid2_link.setter
    def ssid2_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for ssid2."""
        self.ssid2 = value.sid

    @property
    def ssid3_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SEGMENT_* keyword for ssid3."""
        return self._get_set_link("SEGMENT", self.ssid3)

    @ssid3_link.setter
    def ssid3_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for ssid3."""
        self.ssid3 = value.sid

    @property
    def ssid4_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SEGMENT_* keyword for ssid4."""
        return self._get_set_link("SEGMENT", self.ssid4)

    @ssid4_link.setter
    def ssid4_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for ssid4."""
        self.ssid4 = value.sid

    @property
    def ssid5_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SEGMENT_* keyword for ssid5."""
        return self._get_set_link("SEGMENT", self.ssid5)

    @ssid5_link.setter
    def ssid5_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for ssid5."""
        self.ssid5 = value.sid

    @property
    def ssid6_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SEGMENT_* keyword for ssid6."""
        return self._get_set_link("SEGMENT", self.ssid6)

    @ssid6_link.setter
    def ssid6_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for ssid6."""
        self.ssid6 = value.sid

    @property
    def ssid7_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SEGMENT_* keyword for ssid7."""
        return self._get_set_link("SEGMENT", self.ssid7)

    @ssid7_link.setter
    def ssid7_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for ssid7."""
        self.ssid7 = value.sid

    @property
    def ssid8_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SEGMENT_* keyword for ssid8."""
        return self._get_set_link("SEGMENT", self.ssid8)

    @ssid8_link.setter
    def ssid8_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for ssid8."""
        self.ssid8 = value.sid

