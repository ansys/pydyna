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

"""Module providing the BoundaryCoupled class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_BOUNDARYCOUPLED_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("title", str, 10, 70, None),
)

_BOUNDARYCOUPLED_CARD1 = (
    FieldSchema("set", int, 0, 10, None),
    FieldSchema("type", int, 10, 10, 1),
    FieldSchema("prog", int, 20, 10, None),
)

class BoundaryCoupled(KeywordBase):
    """DYNA BOUNDARY_COUPLED keyword"""

    keyword = "BOUNDARY"
    subkeyword = "COUPLED"
    _link_fields = {
        "set": LinkType.SET_NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the BoundaryCoupled class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYCOUPLED_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _BOUNDARYCOUPLED_CARD1,
                **kwargs,
            ),        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the ID for this coupled boundary.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Descriptive name for this boundary.
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[0].set_value("title", value)

    @property
    def set(self) -> typing.Optional[int]:
        """Get or set the Node set ID.
        """ # nopep8
        return self._cards[1].get_value("set")

    @set.setter
    def set(self, value: int) -> None:
        """Set the set property."""
        self._cards[1].set_value("set", value)

    @property
    def type(self) -> int:
        """Get or set the Coupling type:
        EQ.1: node set with force feedback
        EQ.2: node set for multiscale spotwelds.
        """ # nopep8
        return self._cards[1].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        """Set the type property."""
        if value not in [1, 2, None]:
            raise Exception("""type must be `None` or one of {1,2}.""")
        self._cards[1].set_value("type", value)

    @property
    def prog(self) -> typing.Optional[int]:
        """Get or set the Program to couple to EQ.1: MPP-DYNA.
        """ # nopep8
        return self._cards[1].get_value("prog")

    @prog.setter
    def prog(self, value: int) -> None:
        """Set the prog property."""
        self._cards[1].set_value("prog", value)

    @property
    def set_link(self) -> KeywordBase:
        """Get the SET_NODE_* keyword for set."""
        return self._get_set_link("NODE", self.set)

    @set_link.setter
    def set_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for set."""
        self.set = value.sid

