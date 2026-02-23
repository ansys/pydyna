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

"""Module providing the ControlFormingOnestepFriction class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_CONTROLFORMINGONESTEPFRICTION_CARD0 = (
    FieldSchema("ndset", int, 0, 10, None),
    FieldSchema("bdton", float, 10, 10, 0.0),
    FieldSchema("frict", float, 20, 10, 0.12),
)

class ControlFormingOnestepFriction(KeywordBase):
    """DYNA CONTROL_FORMING_ONESTEP_FRICTION keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_ONESTEP_FRICTION"
    _link_fields = {
        "ndset": LinkType.SET_NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the ControlFormingOnestepFriction class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLFORMINGONESTEPFRICTION_CARD0,
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
    def bdton(self) -> float:
        """Get or set the Binder tonnage used to calculate friction force.
        """ # nopep8
        return self._cards[0].get_value("bdton")

    @bdton.setter
    def bdton(self, value: float) -> None:
        """Set the bdton property."""
        self._cards[0].set_value("bdton", value)

    @property
    def frict(self) -> float:
        """Get or set the Coefficient of friction.
        """ # nopep8
        return self._cards[0].get_value("frict")

    @frict.setter
    def frict(self, value: float) -> None:
        """Set the frict property."""
        self._cards[0].set_value("frict", value)

    @property
    def ndset_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for ndset."""
        return self._get_set_link("NODE", self.ndset)

    @ndset_link.setter
    def ndset_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for ndset."""
        self.ndset = value.sid

