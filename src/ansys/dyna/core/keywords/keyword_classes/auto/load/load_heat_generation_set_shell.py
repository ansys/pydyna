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

"""Module providing the LoadHeatGenerationSetShell class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_LOADHEATGENERATIONSETSHELL_CARD0 = (
    FieldSchema("sid", int, 0, 10, None),
    FieldSchema("lcid", int, 10, 10, None),
    FieldSchema("cmult", float, 20, 10, 1.0),
    FieldSchema("wblcid", int, 30, 10, None),
    FieldSchema("cblcid", int, 40, 10, None),
    FieldSchema("tblcid", int, 50, 10, None),
)

class LoadHeatGenerationSetShell(KeywordBase):
    """DYNA LOAD_HEAT_GENERATION_SET_SHELL keyword"""

    keyword = "LOAD"
    subkeyword = "HEAT_GENERATION_SET_SHELL"
    _link_fields = {
        "wblcid": LinkType.DEFINE_CURVE,
        "cblcid": LinkType.DEFINE_CURVE,
        "tblcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the LoadHeatGenerationSetShell class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADHEATGENERATIONSETSHELL_CARD0,
                **kwargs,
            ),        ]
    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Shell element set ID, *SET_SHELL.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for volumetric heat generation rate:
        GT.0: function versus time,
        EQ.0: use multiplier value CMULT only,
        LT.0: function versus temperature.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def cmult(self) -> float:
        """Get or set the Curve multiplier for  heat generation rate (LCID). Depending on the definition of LCID this value is either used for scaling or for constant heat generation.
        """ # nopep8
        return self._cards[0].get_value("cmult")

    @cmult.setter
    def cmult(self, value: float) -> None:
        """Set the cmult property."""
        self._cards[0].set_value("cmult", value)

    @property
    def wblcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining the blood persusion rate as a function of time
        """ # nopep8
        return self._cards[0].get_value("wblcid")

    @wblcid.setter
    def wblcid(self, value: int) -> None:
        """Set the wblcid property."""
        self._cards[0].set_value("wblcid", value)

    @property
    def cblcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining the blood heat capacity as a function of the blood temperature
        """ # nopep8
        return self._cards[0].get_value("cblcid")

    @cblcid.setter
    def cblcid(self, value: int) -> None:
        """Set the cblcid property."""
        self._cards[0].set_value("cblcid", value)

    @property
    def tblcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining the blood temperature as a function of time
        """ # nopep8
        return self._cards[0].get_value("tblcid")

    @tblcid.setter
    def tblcid(self, value: int) -> None:
        """Set the tblcid property."""
        self._cards[0].set_value("tblcid", value)

    @property
    def wblcid_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for wblcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.wblcid:
                return kwd
        return None

    @wblcid_link.setter
    def wblcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for wblcid."""
        self.wblcid = value.lcid

    @property
    def cblcid_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for cblcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.cblcid:
                return kwd
        return None

    @cblcid_link.setter
    def cblcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for cblcid."""
        self.cblcid = value.lcid

    @property
    def tblcid_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for tblcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.tblcid:
                return kwd
        return None

    @tblcid_link.setter
    def tblcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for tblcid."""
        self.tblcid = value.lcid

