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

"""Module providing the DefineCpgVent class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_DEFINECPGVENT_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("lct", int, 10, 10, None),
    FieldSchema("lcp", int, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("ppop", float, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("c23up", float, 60, 10, None),
    FieldSchema("topen", float, 70, 10, None),
)

_DEFINECPGVENT_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineCpgVent(KeywordBase):
    """DYNA DEFINE_CPG_VENT keyword"""

    keyword = "DEFINE"
    subkeyword = "CPG_VENT"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "lct": LinkType.DEFINE_CURVE,
        "lcp": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineCpgVent class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINECPGVENT_CARD0,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineCpgVent._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINECPGVENT_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Unique ID for this card
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def lct(self) -> typing.Optional[int]:
        """Get or set the Optional load curve to impose temperature as a function of time in cases where backflow is detected. Otherwise, TATM in *AIRBAG_?CPG is used. See Remark 6 of AIRBAG_?CPG
        """ # nopep8
        return self._cards[0].get_value("lct")

    @lct.setter
    def lct(self, value: int) -> None:
        """Set the lct property."""
        self._cards[0].set_value("lct", value)

    @property
    def lcp(self) -> typing.Optional[int]:
        """Get or set the Optional load curve to impose pressure as a function of time. Otherwise, PATM of *AIRBAG_?CPG is used. See Remark 6 of *AIRBAG_?CPG
        """ # nopep8
        return self._cards[0].get_value("lcp")

    @lcp.setter
    def lcp(self, value: int) -> None:
        """Set the lcp property."""
        self._cards[0].set_value("lcp", value)

    @property
    def ppop(self) -> typing.Optional[float]:
        """Get or set the Pressure difference between the interior and ambient pressure (PATM) for opening the vent holes. Once the vents are open, they stay open
        """ # nopep8
        return self._cards[0].get_value("ppop")

    @ppop.setter
    def ppop(self, value: float) -> None:
        """Set the ppop property."""
        self._cards[0].set_value("ppop", value)

    @property
    def c23up(self) -> typing.Optional[float]:
        """Get or set the Scale factor of C23 while switching from CPG to uniform pressure calculation.
        """ # nopep8
        return self._cards[0].get_value("c23up")

    @c23up.setter
    def c23up(self, value: float) -> None:
        """Set the c23up property."""
        self._cards[0].set_value("c23up", value)

    @property
    def topen(self) -> typing.Optional[float]:
        """Get or set the Optional time for opening the vent holes. If PPOP is also defined, both conditions need to be met for the vent to open.
        """ # nopep8
        return self._cards[0].get_value("topen")

    @topen.setter
    def topen(self, value: float) -> None:
        """Set the topen property."""
        self._cards[0].set_value("topen", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[1].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def lct_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lct."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lct:
                return kwd
        return None

    @lct_link.setter
    def lct_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lct."""
        self.lct = value.lcid

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

