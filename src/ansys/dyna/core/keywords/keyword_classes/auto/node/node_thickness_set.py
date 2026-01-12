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

"""Module providing the NodeThicknessSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_NODETHICKNESSSET_CARD0 = (
    FieldSchema("id1", int, 0, 10, None),
    FieldSchema("thk", float, 10, 10, None),
    FieldSchema("id2", int, 20, 10, None),
    FieldSchema("inc", int, 30, 10, None),
)

class NodeThicknessSet(KeywordBase):
    """DYNA NODE_THICKNESS_SET keyword"""

    keyword = "NODE"
    subkeyword = "THICKNESS_SET"

    def __init__(self, **kwargs):
        """Initialize the NodeThicknessSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _NODETHICKNESSSET_CARD0,
                **kwargs,
            ),        ]
    @property
    def id1(self) -> typing.Optional[int]:
        """Get or set the Node set ID. If GENERATE option is active, ID1 serves as the starting node set.
        """ # nopep8
        return self._cards[0].get_value("id1")

    @id1.setter
    def id1(self, value: int) -> None:
        """Set the id1 property."""
        self._cards[0].set_value("id1", value)

    @property
    def thk(self) -> typing.Optional[float]:
        """Get or set the Thickness at node set ID1 (ignored if GENERATE option is active).
        """ # nopep8
        return self._cards[0].get_value("thk")

    @thk.setter
    def thk(self, value: float) -> None:
        """Set the thk property."""
        self._cards[0].set_value("thk", value)

    @property
    def id2(self) -> typing.Optional[int]:
        """Get or set the Ending node set if GENERATE option is active.
        """ # nopep8
        return self._cards[0].get_value("id2")

    @id2.setter
    def id2(self, value: int) -> None:
        """Set the id2 property."""
        self._cards[0].set_value("id2", value)

    @property
    def inc(self) -> typing.Optional[int]:
        """Get or set the Increment in node numbers if GENERATE option is active.
        """ # nopep8
        return self._cards[0].get_value("inc")

    @inc.setter
    def inc(self, value: int) -> None:
        """Set the inc property."""
        self._cards[0].set_value("inc", value)

