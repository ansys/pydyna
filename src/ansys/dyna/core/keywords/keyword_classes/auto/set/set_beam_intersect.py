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

"""Module providing the SetBeamIntersect class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.series_card import SeriesCard
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_SETBEAMINTERSECT_CARD0 = (
    FieldSchema("sid", int, 0, 10, None),
)

_SETBEAMINTERSECT_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SetBeamIntersect(KeywordBase):
    """DYNA SET_BEAM_INTERSECT keyword"""

    keyword = "SET"
    subkeyword = "BEAM_INTERSECT"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "bsid1": LinkType.SET_BEAM,
        "bsid2": LinkType.SET_BEAM,
        "bsid3": LinkType.SET_BEAM,
        "bsid4": LinkType.SET_BEAM,
        "bsid5": LinkType.SET_BEAM,
        "bsid6": LinkType.SET_BEAM,
        "bsid7": LinkType.SET_BEAM,
        "bsid8": LinkType.SET_BEAM,
    }

    def __init__(self, **kwargs):
        """Initialize the SetBeamIntersect class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SETBEAMINTERSECT_CARD0,
                **kwargs,
            ),            SeriesCard(
                "beams",
                8,
                10,
                int,
                None,
                data = kwargs.get("beams")),            OptionCardSet(
                option_spec = SetBeamIntersect.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SETBEAMINTERSECT_OPTION0_CARD0,
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
    def beams(self) -> SeriesCard:
        """dynamic array of beam set ids.."""
        return self._cards[1]

    @beams.setter
    def beams(self, value: typing.List) -> None:
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
    def bsid1_link(self) -> KeywordBase:
        """Get the SET_BEAM_* keyword for bsid1."""
        return self._get_set_link("BEAM", self.bsid1)

    @bsid1_link.setter
    def bsid1_link(self, value: KeywordBase) -> None:
        """Set the SET_BEAM_* keyword for bsid1."""
        self.bsid1 = value.sid

    @property
    def bsid2_link(self) -> KeywordBase:
        """Get the SET_BEAM_* keyword for bsid2."""
        return self._get_set_link("BEAM", self.bsid2)

    @bsid2_link.setter
    def bsid2_link(self, value: KeywordBase) -> None:
        """Set the SET_BEAM_* keyword for bsid2."""
        self.bsid2 = value.sid

    @property
    def bsid3_link(self) -> KeywordBase:
        """Get the SET_BEAM_* keyword for bsid3."""
        return self._get_set_link("BEAM", self.bsid3)

    @bsid3_link.setter
    def bsid3_link(self, value: KeywordBase) -> None:
        """Set the SET_BEAM_* keyword for bsid3."""
        self.bsid3 = value.sid

    @property
    def bsid4_link(self) -> KeywordBase:
        """Get the SET_BEAM_* keyword for bsid4."""
        return self._get_set_link("BEAM", self.bsid4)

    @bsid4_link.setter
    def bsid4_link(self, value: KeywordBase) -> None:
        """Set the SET_BEAM_* keyword for bsid4."""
        self.bsid4 = value.sid

    @property
    def bsid5_link(self) -> KeywordBase:
        """Get the SET_BEAM_* keyword for bsid5."""
        return self._get_set_link("BEAM", self.bsid5)

    @bsid5_link.setter
    def bsid5_link(self, value: KeywordBase) -> None:
        """Set the SET_BEAM_* keyword for bsid5."""
        self.bsid5 = value.sid

    @property
    def bsid6_link(self) -> KeywordBase:
        """Get the SET_BEAM_* keyword for bsid6."""
        return self._get_set_link("BEAM", self.bsid6)

    @bsid6_link.setter
    def bsid6_link(self, value: KeywordBase) -> None:
        """Set the SET_BEAM_* keyword for bsid6."""
        self.bsid6 = value.sid

    @property
    def bsid7_link(self) -> KeywordBase:
        """Get the SET_BEAM_* keyword for bsid7."""
        return self._get_set_link("BEAM", self.bsid7)

    @bsid7_link.setter
    def bsid7_link(self, value: KeywordBase) -> None:
        """Set the SET_BEAM_* keyword for bsid7."""
        self.bsid7 = value.sid

    @property
    def bsid8_link(self) -> KeywordBase:
        """Get the SET_BEAM_* keyword for bsid8."""
        return self._get_set_link("BEAM", self.bsid8)

    @bsid8_link.setter
    def bsid8_link(self, value: KeywordBase) -> None:
        """Set the SET_BEAM_* keyword for bsid8."""
        self.bsid8 = value.sid

