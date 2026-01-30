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

"""Module providing the LoadBrode class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_LOADBRODE_CARD0 = (
    FieldSchema("yld", float, 0, 10, 0.0),
    FieldSchema("bht", float, 10, 10, 0.0),
    FieldSchema("xbo", float, 20, 10, 0.0),
    FieldSchema("ybo", float, 30, 10, 0.0),
    FieldSchema("zbo", float, 40, 10, 0.0),
    FieldSchema("tbo", float, 50, 10, 0.0),
    FieldSchema("talc", int, 60, 10, 0),
    FieldSchema("sflc", int, 70, 10, 0),
)

_LOADBRODE_CARD1 = (
    FieldSchema("cfl", float, 0, 10, 0.0),
    FieldSchema("cft", float, 10, 10, 0.0),
    FieldSchema("cfp", float, 20, 10, 0.0),
)

class LoadBrode(KeywordBase):
    """DYNA LOAD_BRODE keyword"""

    keyword = "LOAD"
    subkeyword = "BRODE"
    _link_fields = {
        "talc": LinkType.DEFINE_CURVE,
        "sflc": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the LoadBrode class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADBRODE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADBRODE_CARD1,
                **kwargs,
            ),        ]
    @property
    def yld(self) -> float:
        """Get or set the Yield (Kt, equivalent tons of TNT).
        """ # nopep8
        return self._cards[0].get_value("yld")

    @yld.setter
    def yld(self, value: float) -> None:
        """Set the yld property."""
        self._cards[0].set_value("yld", value)

    @property
    def bht(self) -> float:
        """Get or set the Height of burst.
        """ # nopep8
        return self._cards[0].get_value("bht")

    @bht.setter
    def bht(self, value: float) -> None:
        """Set the bht property."""
        self._cards[0].set_value("bht", value)

    @property
    def xbo(self) -> float:
        """Get or set the x-coordinates of Brode origin.
        """ # nopep8
        return self._cards[0].get_value("xbo")

    @xbo.setter
    def xbo(self, value: float) -> None:
        """Set the xbo property."""
        self._cards[0].set_value("xbo", value)

    @property
    def ybo(self) -> float:
        """Get or set the y-coordinates of Brode origin.
        """ # nopep8
        return self._cards[0].get_value("ybo")

    @ybo.setter
    def ybo(self, value: float) -> None:
        """Set the ybo property."""
        self._cards[0].set_value("ybo", value)

    @property
    def zbo(self) -> float:
        """Get or set the z-coordinates of Brode origin.
        """ # nopep8
        return self._cards[0].get_value("zbo")

    @zbo.setter
    def zbo(self, value: float) -> None:
        """Set the zbo property."""
        self._cards[0].set_value("zbo", value)

    @property
    def tbo(self) -> float:
        """Get or set the Time offset of Brode origin.
        """ # nopep8
        return self._cards[0].get_value("tbo")

    @tbo.setter
    def tbo(self, value: float) -> None:
        """Set the tbo property."""
        self._cards[0].set_value("tbo", value)

    @property
    def talc(self) -> int:
        """Get or set the Load curve number giving time of arrival versus range relative to Brode origin (space, time), see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[0].get_value("talc")

    @talc.setter
    def talc(self, value: int) -> None:
        """Set the talc property."""
        self._cards[0].set_value("talc", value)

    @property
    def sflc(self) -> int:
        """Get or set the Load curve number giving yield scaling versus scaled time , see *DEFINE_ CURVE.
        """ # nopep8
        return self._cards[0].get_value("sflc")

    @sflc.setter
    def sflc(self, value: int) -> None:
        """Set the sflc property."""
        self._cards[0].set_value("sflc", value)

    @property
    def cfl(self) -> float:
        """Get or set the Conversion factor - kft to LS-DYNA length.
        """ # nopep8
        return self._cards[1].get_value("cfl")

    @cfl.setter
    def cfl(self, value: float) -> None:
        """Set the cfl property."""
        self._cards[1].set_value("cfl", value)

    @property
    def cft(self) -> float:
        """Get or set the Conversion factor - milliseconds to LS-DYNA time units.
        """ # nopep8
        return self._cards[1].get_value("cft")

    @cft.setter
    def cft(self, value: float) -> None:
        """Set the cft property."""
        self._cards[1].set_value("cft", value)

    @property
    def cfp(self) -> float:
        """Get or set the Conversion factor - psi to LS-DYNA pressure units.
        """ # nopep8
        return self._cards[1].get_value("cfp")

    @cfp.setter
    def cfp(self, value: float) -> None:
        """Set the cfp property."""
        self._cards[1].set_value("cfp", value)

    @property
    def talc_link(self) -> DefineCurve:
        """Get the DefineCurve object for talc."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.talc:
                return kwd
        return None

    @talc_link.setter
    def talc_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for talc."""
        self.talc = value.lcid

    @property
    def sflc_link(self) -> DefineCurve:
        """Get the DefineCurve object for sflc."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.sflc:
                return kwd
        return None

    @sflc_link.setter
    def sflc_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for sflc."""
        self.sflc = value.lcid

