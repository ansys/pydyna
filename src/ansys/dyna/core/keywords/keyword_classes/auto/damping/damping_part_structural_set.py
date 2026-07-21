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

"""Module providing the DampingPartStructuralSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_DAMPINGPARTSTRUCTURALSET_CARD0 = (
    FieldSchema("psid", int, 0, 10, 0),
    FieldSchema("g", int, 10, 10, 0),
)

class DampingPartStructuralSet(KeywordBase):
    """DYNA DAMPING_PART_STRUCTURAL_SET keyword"""

    keyword = "DAMPING"
    subkeyword = "PART_STRUCTURAL_SET"
    _link_fields = {
        "g": LinkType.DEFINE_CURVE,
        "psid": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the DampingPartStructuralSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DAMPINGPARTSTRUCTURALSET_CARD0,
                **kwargs,
            ),
        ]
    @property
    def psid(self) -> int:
        """Get or set the Part set ID(see *PART SET) with the SET keyword option .
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[0].set_value("psid", value)

    @property
    def g(self) -> int:
        """Get or set the Constant structural damping coefficient
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: int) -> None:
        """Set the g property."""
        self._cards[0].set_value("g", value)

    @property
    def g_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for g."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.g:
                return kwd
        return None

    @g_link.setter
    def g_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for g."""
        self.g = value.lcid

    @property
    def psid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for psid."""
        return self._get_set_link("PART", self.psid)

    @psid_link.setter
    def psid_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psid."""
        self.psid = value.sid

