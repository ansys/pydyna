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

"""Module providing the AleReferenceSystemCurve class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_ALEREFERENCESYSTEMCURVE_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
)

_ALEREFERENCESYSTEMCURVE_CARD1 = (
    FieldSchema("lc1", int, 0, 10, None),
    FieldSchema("lc2", int, 10, 10, None),
    FieldSchema("lc3", int, 20, 10, None),
    FieldSchema("lc4", int, 30, 10, None),
    FieldSchema("lc5", int, 40, 10, None),
    FieldSchema("lc6", int, 50, 10, None),
    FieldSchema("lc7", int, 60, 10, None),
    FieldSchema("lc8", int, 70, 10, None),
)

_ALEREFERENCESYSTEMCURVE_CARD2 = (
    FieldSchema("lc9", int, 0, 10, None),
    FieldSchema("lc10", int, 10, 10, None),
    FieldSchema("lc11", int, 20, 10, None),
    FieldSchema("lc12", int, 30, 10, None),
)

class AleReferenceSystemCurve(KeywordBase):
    """DYNA ALE_REFERENCE_SYSTEM_CURVE keyword"""

    keyword = "ALE"
    subkeyword = "REFERENCE_SYSTEM_CURVE"
    _link_fields = {
        "lc1": LinkType.DEFINE_CURVE,
        "lc2": LinkType.DEFINE_CURVE,
        "lc3": LinkType.DEFINE_CURVE,
        "lc4": LinkType.DEFINE_CURVE,
        "lc5": LinkType.DEFINE_CURVE,
        "lc6": LinkType.DEFINE_CURVE,
        "lc7": LinkType.DEFINE_CURVE,
        "lc8": LinkType.DEFINE_CURVE,
        "lc9": LinkType.DEFINE_CURVE,
        "lc10": LinkType.DEFINE_CURVE,
        "lc11": LinkType.DEFINE_CURVE,
        "lc12": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the AleReferenceSystemCurve class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ALEREFERENCESYSTEMCURVE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ALEREFERENCESYSTEMCURVE_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ALEREFERENCESYSTEMCURVE_CARD2,
                **kwargs,
            ),        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Curve ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def lc1(self) -> typing.Optional[int]:
        """Get or set the First load curve ID.
        """ # nopep8
        return self._cards[1].get_value("lc1")

    @lc1.setter
    def lc1(self, value: int) -> None:
        """Set the lc1 property."""
        self._cards[1].set_value("lc1", value)

    @property
    def lc2(self) -> typing.Optional[int]:
        """Get or set the Second load curve ID.
        """ # nopep8
        return self._cards[1].get_value("lc2")

    @lc2.setter
    def lc2(self, value: int) -> None:
        """Set the lc2 property."""
        self._cards[1].set_value("lc2", value)

    @property
    def lc3(self) -> typing.Optional[int]:
        """Get or set the Third load curve ID.
        """ # nopep8
        return self._cards[1].get_value("lc3")

    @lc3.setter
    def lc3(self, value: int) -> None:
        """Set the lc3 property."""
        self._cards[1].set_value("lc3", value)

    @property
    def lc4(self) -> typing.Optional[int]:
        """Get or set the Fourth load curve ID.
        """ # nopep8
        return self._cards[1].get_value("lc4")

    @lc4.setter
    def lc4(self, value: int) -> None:
        """Set the lc4 property."""
        self._cards[1].set_value("lc4", value)

    @property
    def lc5(self) -> typing.Optional[int]:
        """Get or set the Fifth load curve ID.
        """ # nopep8
        return self._cards[1].get_value("lc5")

    @lc5.setter
    def lc5(self, value: int) -> None:
        """Set the lc5 property."""
        self._cards[1].set_value("lc5", value)

    @property
    def lc6(self) -> typing.Optional[int]:
        """Get or set the Sixth load curve ID.
        """ # nopep8
        return self._cards[1].get_value("lc6")

    @lc6.setter
    def lc6(self, value: int) -> None:
        """Set the lc6 property."""
        self._cards[1].set_value("lc6", value)

    @property
    def lc7(self) -> typing.Optional[int]:
        """Get or set the Seventh load curve ID.
        """ # nopep8
        return self._cards[1].get_value("lc7")

    @lc7.setter
    def lc7(self, value: int) -> None:
        """Set the lc7 property."""
        self._cards[1].set_value("lc7", value)

    @property
    def lc8(self) -> typing.Optional[int]:
        """Get or set the Eighth load curve ID.
        """ # nopep8
        return self._cards[1].get_value("lc8")

    @lc8.setter
    def lc8(self, value: int) -> None:
        """Set the lc8 property."""
        self._cards[1].set_value("lc8", value)

    @property
    def lc9(self) -> typing.Optional[int]:
        """Get or set the Ninth load curve ID.
        """ # nopep8
        return self._cards[2].get_value("lc9")

    @lc9.setter
    def lc9(self, value: int) -> None:
        """Set the lc9 property."""
        self._cards[2].set_value("lc9", value)

    @property
    def lc10(self) -> typing.Optional[int]:
        """Get or set the Tenth load curve ID.
        """ # nopep8
        return self._cards[2].get_value("lc10")

    @lc10.setter
    def lc10(self, value: int) -> None:
        """Set the lc10 property."""
        self._cards[2].set_value("lc10", value)

    @property
    def lc11(self) -> typing.Optional[int]:
        """Get or set the Eleventh load curve ID.
        """ # nopep8
        return self._cards[2].get_value("lc11")

    @lc11.setter
    def lc11(self, value: int) -> None:
        """Set the lc11 property."""
        self._cards[2].set_value("lc11", value)

    @property
    def lc12(self) -> typing.Optional[int]:
        """Get or set the Twelveth load curve ID.
        """ # nopep8
        return self._cards[2].get_value("lc12")

    @lc12.setter
    def lc12(self, value: int) -> None:
        """Set the lc12 property."""
        self._cards[2].set_value("lc12", value)

    @property
    def lc1_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lc1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc1:
                return kwd
        return None

    @lc1_link.setter
    def lc1_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc1."""
        self.lc1 = value.lcid

    @property
    def lc2_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lc2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc2:
                return kwd
        return None

    @lc2_link.setter
    def lc2_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc2."""
        self.lc2 = value.lcid

    @property
    def lc3_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lc3."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc3:
                return kwd
        return None

    @lc3_link.setter
    def lc3_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc3."""
        self.lc3 = value.lcid

    @property
    def lc4_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lc4."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc4:
                return kwd
        return None

    @lc4_link.setter
    def lc4_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc4."""
        self.lc4 = value.lcid

    @property
    def lc5_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lc5."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc5:
                return kwd
        return None

    @lc5_link.setter
    def lc5_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc5."""
        self.lc5 = value.lcid

    @property
    def lc6_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lc6."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc6:
                return kwd
        return None

    @lc6_link.setter
    def lc6_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc6."""
        self.lc6 = value.lcid

    @property
    def lc7_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lc7."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc7:
                return kwd
        return None

    @lc7_link.setter
    def lc7_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc7."""
        self.lc7 = value.lcid

    @property
    def lc8_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lc8."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc8:
                return kwd
        return None

    @lc8_link.setter
    def lc8_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc8."""
        self.lc8 = value.lcid

    @property
    def lc9_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lc9."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc9:
                return kwd
        return None

    @lc9_link.setter
    def lc9_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc9."""
        self.lc9 = value.lcid

    @property
    def lc10_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lc10."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc10:
                return kwd
        return None

    @lc10_link.setter
    def lc10_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc10."""
        self.lc10 = value.lcid

    @property
    def lc11_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lc11."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc11:
                return kwd
        return None

    @lc11_link.setter
    def lc11_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc11."""
        self.lc11 = value.lcid

    @property
    def lc12_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lc12."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc12:
                return kwd
        return None

    @lc12_link.setter
    def lc12_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc12."""
        self.lc12 = value.lcid

