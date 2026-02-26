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

"""Module providing the CeseBoundarySegment class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_CESEBOUNDARYSEGMENT_CARD0 = (
    FieldSchema("n1", int, 0, 10, None),
    FieldSchema("n2_", int, 10, 10, None, "n2 "),
    FieldSchema("n3", int, 20, 10, None),
    FieldSchema("n4", int, 30, 10, None),
    FieldSchema("dof_", int, 40, 10, None, "dof "),
    FieldSchema("lcid", int, 50, 10, None),
    FieldSchema("sf", float, 60, 10, 1.0),
)

class CeseBoundarySegment(KeywordBase):
    """DYNA CESE_BOUNDARY_SEGMENT keyword"""

    keyword = "CESE"
    subkeyword = "BOUNDARY_SEGMENT"
    _link_fields = {
        "n1": LinkType.NODE,
        "n2_": LinkType.NODE,
        "n3": LinkType.NODE,
        "n4": LinkType.NODE,
        "lcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the CeseBoundarySegment class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CESEBOUNDARYSEGMENT_CARD0,
                **kwargs,
            ),        ]
    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining a segment.
        """ # nopep8
        return self._cards[0].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        """Set the n1 property."""
        self._cards[0].set_value("n1", value)

    @property
    def n2_(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining a segment.
        """ # nopep8
        return self._cards[0].get_value("n2_")

    @n2_.setter
    def n2_(self, value: int) -> None:
        """Set the n2_ property."""
        self._cards[0].set_value("n2_", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining a segment.
        """ # nopep8
        return self._cards[0].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        """Set the n3 property."""
        self._cards[0].set_value("n3", value)

    @property
    def n4(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining a segment.
        """ # nopep8
        return self._cards[0].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        """Set the n4 property."""
        self._cards[0].set_value("n4", value)

    @property
    def dof_(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining a segment.
        """ # nopep8
        return self._cards[0].get_value("dof_")

    @dof_.setter
    def dof_(self, value: int) -> None:
        """Set the dof_ property."""
        self._cards[0].set_value("dof_", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the variable value versus time, see *DEFINE_ CURVE.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def sf(self) -> float:
        """Get or set the Load curve scale factor.  (default=1.0).
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[0].set_value("sf", value)

    @property
    def n1_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n1."""
        return self._get_link_by_attr("NODE", "nid", self.n1, "parts")

    @property
    def n2__link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n2_."""
        return self._get_link_by_attr("NODE", "nid", self.n2_, "parts")

    @property
    def n3_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n3."""
        return self._get_link_by_attr("NODE", "nid", self.n3, "parts")

    @property
    def n4_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n4."""
        return self._get_link_by_attr("NODE", "nid", self.n4, "parts")

    @property
    def lcid_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid:
                return kwd
        return None

    @lcid_link.setter
    def lcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid."""
        self.lcid = value.lcid

