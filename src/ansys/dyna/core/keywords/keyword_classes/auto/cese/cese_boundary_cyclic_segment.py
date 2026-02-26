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

"""Module providing the CeseBoundaryCyclicSegment class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_CESEBOUNDARYCYCLICSEGMENT_CARD0 = (
    FieldSchema("nd1", int, 0, 10, None),
    FieldSchema("nd2", int, 10, 10, None),
    FieldSchema("nd3", int, 20, 10, None),
    FieldSchema("nd4", int, 30, 10, None),
    FieldSchema("np1", int, 40, 10, None),
    FieldSchema("np2", int, 50, 10, None),
    FieldSchema("np3", int, 60, 10, None),
    FieldSchema("np4", int, 70, 10, None),
)

class CeseBoundaryCyclicSegment(KeywordBase):
    """DYNA CESE_BOUNDARY_CYCLIC_SEGMENT keyword"""

    keyword = "CESE"
    subkeyword = "BOUNDARY_CYCLIC_SEGMENT"
    _link_fields = {
        "nd1": LinkType.NODE,
        "nd2": LinkType.NODE,
        "nd3": LinkType.NODE,
        "nd4": LinkType.NODE,
        "np1": LinkType.NODE,
        "np2": LinkType.NODE,
        "np3": LinkType.NODE,
        "np4": LinkType.NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the CeseBoundaryCyclicSegment class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CESEBOUNDARYCYCLICSEGMENT_CARD0,
                **kwargs,
            ),        ]
    @property
    def nd1(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining segment.
        """ # nopep8
        return self._cards[0].get_value("nd1")

    @nd1.setter
    def nd1(self, value: int) -> None:
        """Set the nd1 property."""
        self._cards[0].set_value("nd1", value)

    @property
    def nd2(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining segment.
        """ # nopep8
        return self._cards[0].get_value("nd2")

    @nd2.setter
    def nd2(self, value: int) -> None:
        """Set the nd2 property."""
        self._cards[0].set_value("nd2", value)

    @property
    def nd3(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining segment.
        """ # nopep8
        return self._cards[0].get_value("nd3")

    @nd3.setter
    def nd3(self, value: int) -> None:
        """Set the nd3 property."""
        self._cards[0].set_value("nd3", value)

    @property
    def nd4(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining segment.
        """ # nopep8
        return self._cards[0].get_value("nd4")

    @nd4.setter
    def nd4(self, value: int) -> None:
        """Set the nd4 property."""
        self._cards[0].set_value("nd4", value)

    @property
    def np1(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining segment.
        """ # nopep8
        return self._cards[0].get_value("np1")

    @np1.setter
    def np1(self, value: int) -> None:
        """Set the np1 property."""
        self._cards[0].set_value("np1", value)

    @property
    def np2(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining segment.
        """ # nopep8
        return self._cards[0].get_value("np2")

    @np2.setter
    def np2(self, value: int) -> None:
        """Set the np2 property."""
        self._cards[0].set_value("np2", value)

    @property
    def np3(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining segment.
        """ # nopep8
        return self._cards[0].get_value("np3")

    @np3.setter
    def np3(self, value: int) -> None:
        """Set the np3 property."""
        self._cards[0].set_value("np3", value)

    @property
    def np4(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining segment.
        """ # nopep8
        return self._cards[0].get_value("np4")

    @np4.setter
    def np4(self, value: int) -> None:
        """Set the np4 property."""
        self._cards[0].set_value("np4", value)

    @property
    def nd1_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nd1."""
        return self._get_link_by_attr("NODE", "nid", self.nd1, "parts")

    @property
    def nd2_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nd2."""
        return self._get_link_by_attr("NODE", "nid", self.nd2, "parts")

    @property
    def nd3_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nd3."""
        return self._get_link_by_attr("NODE", "nid", self.nd3, "parts")

    @property
    def nd4_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nd4."""
        return self._get_link_by_attr("NODE", "nid", self.nd4, "parts")

    @property
    def np1_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given np1."""
        return self._get_link_by_attr("NODE", "nid", self.np1, "parts")

    @property
    def np2_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given np2."""
        return self._get_link_by_attr("NODE", "nid", self.np2, "parts")

    @property
    def np3_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given np3."""
        return self._get_link_by_attr("NODE", "nid", self.np3, "parts")

    @property
    def np4_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given np4."""
        return self._get_link_by_attr("NODE", "nid", self.np4, "parts")

