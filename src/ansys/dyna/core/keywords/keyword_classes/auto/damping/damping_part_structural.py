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

"""Module providing the DampingPartStructural class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_DAMPINGPARTSTRUCTURAL_CARD0 = (
    FieldSchema("pid", int, 0, 10, 0),
    FieldSchema("g", int, 10, 10, 0),
)

class DampingPartStructural(KeywordBase):
    """DYNA DAMPING_PART_STRUCTURAL keyword"""

    keyword = "DAMPING"
    subkeyword = "PART_STRUCTURAL"
    _link_fields = {
        "g": LinkType.DEFINE_CURVE,
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the DampingPartStructural class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DAMPINGPARTSTRUCTURAL_CARD0,
                **kwargs,
            ),
        ]
    @property
    def pid(self) -> int:
        """Get or set the Part ID (see *PART) with no keyword option
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

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
    def pid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

