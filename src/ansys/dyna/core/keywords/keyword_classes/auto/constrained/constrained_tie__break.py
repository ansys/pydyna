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

"""Module providing the ConstrainedTie_Break class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_CONSTRAINEDTIE_BREAK_CARD0 = (
    FieldSchema("nsid1", int, 0, 10, None),
    FieldSchema("nsid2", int, 10, 10, None),
    FieldSchema("eppf", float, 20, 10, 0.0),
)

class ConstrainedTie_Break(KeywordBase):
    """DYNA CONSTRAINED_TIE-BREAK keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "TIE-BREAK"
    _link_fields = {
        "nsid1": LinkType.SET_NODE,
        "nsid2": LinkType.SET_NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the ConstrainedTie_Break class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDTIE_BREAK_CARD0,
                **kwargs,
            ),        ]
    @property
    def nsid1(self) -> typing.Optional[int]:
        """Get or set the Node set ID for nodes on one side of the tied shell edge to shell edge interface; , see *SET_NODE_OPTION.
        """ # nopep8
        return self._cards[0].get_value("nsid1")

    @nsid1.setter
    def nsid1(self, value: int) -> None:
        """Set the nsid1 property."""
        self._cards[0].set_value("nsid1", value)

    @property
    def nsid2(self) -> typing.Optional[int]:
        """Get or set the Node set ID for nodes on the other side of the tied shell edge to shell edge interface, see *SET_NODE.
        """ # nopep8
        return self._cards[0].get_value("nsid2")

    @nsid2.setter
    def nsid2(self, value: int) -> None:
        """Set the nsid2 property."""
        self._cards[0].set_value("nsid2", value)

    @property
    def eppf(self) -> float:
        """Get or set the Plastic strain at failure.
        """ # nopep8
        return self._cards[0].get_value("eppf")

    @eppf.setter
    def eppf(self, value: float) -> None:
        """Set the eppf property."""
        self._cards[0].set_value("eppf", value)

    @property
    def nsid1_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for nsid1."""
        return self._get_set_link("NODE", self.nsid1)

    @nsid1_link.setter
    def nsid1_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for nsid1."""
        self.nsid1 = value.sid

    @property
    def nsid2_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for nsid2."""
        return self._get_set_link("NODE", self.nsid2)

    @nsid2_link.setter
    def nsid2_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for nsid2."""
        self.nsid2 = value.sid

