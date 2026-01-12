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

"""Module providing the ControlFormingOnestepDrawbead class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLFORMINGONESTEPDRAWBEAD_CARD0 = (
    FieldSchema("ndset", int, 0, 10, None),
    FieldSchema("lcid", int, 10, 10, None),
    FieldSchema("th", float, 20, 10, 0.0),
    FieldSchema("percnt", float, 30, 10, 0.0),
)

class ControlFormingOnestepDrawbead(KeywordBase):
    """DYNA CONTROL_FORMING_ONESTEP_DRAWBEAD keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_ONESTEP_DRAWBEAD"

    def __init__(self, **kwargs):
        """Initialize the ControlFormingOnestepDrawbead class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLFORMINGONESTEPDRAWBEAD_CARD0,
                **kwargs,
            ),        ]
    @property
    def ndset(self) -> typing.Optional[int]:
        """Get or set the Node set ID along the periphery of the part, as defined by keyword *SET_NODE_LIST.
        """ # nopep8
        return self._cards[0].get_value("ndset")

    @ndset.setter
    def ndset(self, value: int) -> None:
        """Set the ndset property."""
        self._cards[0].set_value("ndset", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID that defines the material hardening curve.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def th(self) -> float:
        """Get or set the Thickness of the unformed sheet blank.
        """ # nopep8
        return self._cards[0].get_value("th")

    @th.setter
    def th(self, value: float) -> None:
        """Set the th property."""
        self._cards[0].set_value("th", value)

    @property
    def percnt(self) -> float:
        """Get or set the Draw bead lock force fraction of the fully locked bead force.
        """ # nopep8
        return self._cards[0].get_value("percnt")

    @percnt.setter
    def percnt(self, value: float) -> None:
        """Set the percnt property."""
        self._cards[0].set_value("percnt", value)

