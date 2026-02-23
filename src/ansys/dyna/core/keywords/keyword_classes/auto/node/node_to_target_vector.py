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

"""Module providing the NodeToTargetVector class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_NODETOTARGETVECTOR_CARD0 = (
    FieldSchema("nid", int, 0, 8, None),
    FieldSchema("xdelta", float, 8, 16, 0.0),
    FieldSchema("ydelta", float, 24, 16, 0.0),
    FieldSchema("zdelta", float, 40, 16, 0.0),
)

class NodeToTargetVector(KeywordBase):
    """DYNA NODE_TO_TARGET_VECTOR keyword"""

    keyword = "NODE"
    subkeyword = "TO_TARGET_VECTOR"
    _link_fields = {
        "nid": LinkType.NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the NodeToTargetVector class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _NODETOTARGETVECTOR_CARD0,
                **kwargs,
            ),        ]
    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node ID on a part best fitted to the target.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[0].set_value("nid", value)

    @property
    def xdelta(self) -> float:
        """Get or set the Difference in X-coordinates of the normal distance from the target (typically scan data) to a node of a part best fitted to the target.
        """ # nopep8
        return self._cards[0].get_value("xdelta")

    @xdelta.setter
    def xdelta(self, value: float) -> None:
        """Set the xdelta property."""
        self._cards[0].set_value("xdelta", value)

    @property
    def ydelta(self) -> float:
        """Get or set the Difference in Y-coordinates of the normal distance from the target (typically scan data) to a node of a part best fitted to the target.
        """ # nopep8
        return self._cards[0].get_value("ydelta")

    @ydelta.setter
    def ydelta(self, value: float) -> None:
        """Set the ydelta property."""
        self._cards[0].set_value("ydelta", value)

    @property
    def zdelta(self) -> float:
        """Get or set the Difference in Z-coordinates of the normal distance from the target (typically scan data) to a node of a part best fitted to the target.
        """ # nopep8
        return self._cards[0].get_value("zdelta")

    @zdelta.setter
    def zdelta(self, value: float) -> None:
        """Set the zdelta property."""
        self._cards[0].set_value("zdelta", value)

    @property
    def nid_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid."""
        return self._get_link_by_attr("NODE", "nid", self.nid, "parts")

