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

"""Module providing the BoundarySphPeriodic class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_vector import DefineVector

_BOUNDARYSPHPERIODIC_CARD0 = (
    FieldSchema("vid1", int, 0, 10, None),
    FieldSchema("vid2", int, 10, 10, None),
)

class BoundarySphPeriodic(KeywordBase):
    """DYNA BOUNDARY_SPH_PERIODIC keyword"""

    keyword = "BOUNDARY"
    subkeyword = "SPH_PERIODIC"
    _link_fields = {
        "vid1": LinkType.DEFINE_VECTOR,
        "vid2": LinkType.DEFINE_VECTOR,
    }

    def __init__(self, **kwargs):
        """Initialize the BoundarySphPeriodic class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYSPHPERIODIC_CARD0,
                **kwargs,
            ),
        ]
    @property
    def vid1(self) -> typing.Optional[int]:
        """Get or set the Vector IDs defining the inward pointing normal vectors of the periodic boundary planes. See *DEFINE_VECTOR. The vector's tail is taken as a reference point for the given plane.
        """ # nopep8
        return self._cards[0].get_value("vid1")

    @vid1.setter
    def vid1(self, value: int) -> None:
        """Set the vid1 property."""
        self._cards[0].set_value("vid1", value)

    @property
    def vid2(self) -> typing.Optional[int]:
        """Get or set the Vector IDs defining the inward pointing normal vectors of the periodic boundary planes. See *DEFINE_VECTOR. The vector's tail is taken as a reference point for the given plane.
        """ # nopep8
        return self._cards[0].get_value("vid2")

    @vid2.setter
    def vid2(self, value: int) -> None:
        """Set the vid2 property."""
        self._cards[0].set_value("vid2", value)

    @property
    def vid1_link(self) -> typing.Optional[DefineVector]:
        """Get the DefineVector object for vid1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.vid1:
                return kwd
        return None

    @vid1_link.setter
    def vid1_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for vid1."""
        self.vid1 = value.vid

    @property
    def vid2_link(self) -> typing.Optional[DefineVector]:
        """Get the DefineVector object for vid2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.vid2:
                return kwd
        return None

    @vid2_link.setter
    def vid2_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for vid2."""
        self.vid2 = value.vid

