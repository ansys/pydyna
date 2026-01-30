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

"""Module providing the SetBox class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.series_card import SeriesCard
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_box import DefineBox

_SETBOX_CARD0 = (
    FieldSchema("sid", int, 0, 10, None),
)

_SETBOX_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SetBox(KeywordBase):
    """DYNA SET_BOX keyword"""

    keyword = "SET"
    subkeyword = "BOX"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "bid1": LinkType.DEFINE_BOX,
        "bid2": LinkType.DEFINE_BOX,
        "bid3": LinkType.DEFINE_BOX,
        "bid4": LinkType.DEFINE_BOX,
        "bid5": LinkType.DEFINE_BOX,
        "bid6": LinkType.DEFINE_BOX,
        "bid7": LinkType.DEFINE_BOX,
        "bid8": LinkType.DEFINE_BOX,
    }

    def __init__(self, **kwargs):
        """Initialize the SetBox class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SETBOX_CARD0,
                **kwargs,
            ),            SeriesCard(
                "box",
                8,
                10,
                int,
                None,
                data = kwargs.get("box")),            OptionCardSet(
                option_spec = SetBox.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SETBOX_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Set ID of new beam set. All beam sets should have a unique set ID.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def box(self) -> SeriesCard:
        """dynamic array of box set ids.."""
        return self._cards[1]

    @box.setter
    def box(self, value: typing.List) -> None:
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
    def bid1_link(self) -> DefineBox:
        """Get the DefineBox object for bid1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "BOX"):
            if kwd.boxid == self.bid1:
                return kwd
        return None

    @bid1_link.setter
    def bid1_link(self, value: DefineBox) -> None:
        """Set the DefineBox object for bid1."""
        self.bid1 = value.boxid

    @property
    def bid2_link(self) -> DefineBox:
        """Get the DefineBox object for bid2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "BOX"):
            if kwd.boxid == self.bid2:
                return kwd
        return None

    @bid2_link.setter
    def bid2_link(self, value: DefineBox) -> None:
        """Set the DefineBox object for bid2."""
        self.bid2 = value.boxid

    @property
    def bid3_link(self) -> DefineBox:
        """Get the DefineBox object for bid3."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "BOX"):
            if kwd.boxid == self.bid3:
                return kwd
        return None

    @bid3_link.setter
    def bid3_link(self, value: DefineBox) -> None:
        """Set the DefineBox object for bid3."""
        self.bid3 = value.boxid

    @property
    def bid4_link(self) -> DefineBox:
        """Get the DefineBox object for bid4."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "BOX"):
            if kwd.boxid == self.bid4:
                return kwd
        return None

    @bid4_link.setter
    def bid4_link(self, value: DefineBox) -> None:
        """Set the DefineBox object for bid4."""
        self.bid4 = value.boxid

    @property
    def bid5_link(self) -> DefineBox:
        """Get the DefineBox object for bid5."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "BOX"):
            if kwd.boxid == self.bid5:
                return kwd
        return None

    @bid5_link.setter
    def bid5_link(self, value: DefineBox) -> None:
        """Set the DefineBox object for bid5."""
        self.bid5 = value.boxid

    @property
    def bid6_link(self) -> DefineBox:
        """Get the DefineBox object for bid6."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "BOX"):
            if kwd.boxid == self.bid6:
                return kwd
        return None

    @bid6_link.setter
    def bid6_link(self, value: DefineBox) -> None:
        """Set the DefineBox object for bid6."""
        self.bid6 = value.boxid

    @property
    def bid7_link(self) -> DefineBox:
        """Get the DefineBox object for bid7."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "BOX"):
            if kwd.boxid == self.bid7:
                return kwd
        return None

    @bid7_link.setter
    def bid7_link(self, value: DefineBox) -> None:
        """Set the DefineBox object for bid7."""
        self.bid7 = value.boxid

    @property
    def bid8_link(self) -> DefineBox:
        """Get the DefineBox object for bid8."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "BOX"):
            if kwd.boxid == self.bid8:
                return kwd
        return None

    @bid8_link.setter
    def bid8_link(self, value: DefineBox) -> None:
        """Set the DefineBox object for bid8."""
        self.bid8 = value.boxid

