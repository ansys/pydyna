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

"""Module providing the FrequencyDomainAcousticSoundSpeed class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_FREQUENCYDOMAINACOUSTICSOUNDSPEED_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
)

_FREQUENCYDOMAINACOUSTICSOUNDSPEED_CARD1 = (
    FieldSchema("lcid1", int, 0, 10, None),
    FieldSchema("lcid2", int, 10, 10, None),
)

class FrequencyDomainAcousticSoundSpeed(KeywordBase):
    """DYNA FREQUENCY_DOMAIN_ACOUSTIC_SOUND_SPEED keyword"""

    keyword = "FREQUENCY"
    subkeyword = "DOMAIN_ACOUSTIC_SOUND_SPEED"
    _link_fields = {
        "lcid1": LinkType.DEFINE_CURVE,
        "lcid2": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the FrequencyDomainAcousticSoundSpeed class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINACOUSTICSOUNDSPEED_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINACOUSTICSOUNDSPEED_CARD1,
                **kwargs,
            ),        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Complex sound speed ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def lcid1(self) -> typing.Optional[int]:
        """Get or set the Curve ID for real part of frequency dependent complex sound speed
        """ # nopep8
        return self._cards[1].get_value("lcid1")

    @lcid1.setter
    def lcid1(self, value: int) -> None:
        """Set the lcid1 property."""
        self._cards[1].set_value("lcid1", value)

    @property
    def lcid2(self) -> typing.Optional[int]:
        """Get or set the Curve ID for imaginary part of frequency dependent complex sound speed.
        """ # nopep8
        return self._cards[1].get_value("lcid2")

    @lcid2.setter
    def lcid2(self, value: int) -> None:
        """Set the lcid2 property."""
        self._cards[1].set_value("lcid2", value)

    @property
    def lcid1_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcid1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid1:
                return kwd
        return None

    @lcid1_link.setter
    def lcid1_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid1."""
        self.lcid1 = value.lcid

    @property
    def lcid2_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcid2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid2:
                return kwd
        return None

    @lcid2_link.setter
    def lcid2_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid2."""
        self.lcid2 = value.lcid

