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

"""Module providing the LoadBlastSegment class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_LOADBLASTSEGMENT_CARD0 = (
    FieldSchema("bid", int, 0, 10, None),
    FieldSchema("n1", int, 10, 10, None),
    FieldSchema("n2", int, 20, 10, None),
    FieldSchema("n3", int, 30, 10, None),
    FieldSchema("n4", int, 40, 10, None),
    FieldSchema("alepid", int, 50, 10, None),
    FieldSchema("sfnrb", float, 60, 10, 0.0),
    FieldSchema("scalep", float, 70, 10, 1.0),
)

class LoadBlastSegment(KeywordBase):
    """DYNA LOAD_BLAST_SEGMENT keyword"""

    keyword = "LOAD"
    subkeyword = "BLAST_SEGMENT"
    _link_fields = {
        "n1": LinkType.NODE,
        "n2": LinkType.NODE,
        "n3": LinkType.NODE,
        "n4": LinkType.NODE,
        "alepid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the LoadBlastSegment class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADBLASTSEGMENT_CARD0,
                **kwargs,
            ),        ]
    @property
    def bid(self) -> typing.Optional[int]:
        """Get or set the Blast source ID (see *LOAD_BLAST_ENHANCED).
        """ # nopep8
        return self._cards[0].get_value("bid")

    @bid.setter
    def bid(self, value: int) -> None:
        """Set the bid property."""
        self._cards[0].set_value("bid", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Node ID.
        """ # nopep8
        return self._cards[0].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        """Set the n1 property."""
        self._cards[0].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Node ID.
        """ # nopep8
        return self._cards[0].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        """Set the n2 property."""
        self._cards[0].set_value("n2", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Node ID.  For line segments on two-dimensional geometries set N3 = N2.
        """ # nopep8
        return self._cards[0].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        """Set the n3 property."""
        self._cards[0].set_value("n3", value)

    @property
    def n4(self) -> typing.Optional[int]:
        """Get or set the Node ID.  For line segments on two-dimensional geometries set N4 = N3 = N2 or for triangular segments in three diemensions set N4 = N3.
        """ # nopep8
        return self._cards[0].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        """Set the n4 property."""
        self._cards[0].set_value("n4", value)

    @property
    def alepid(self) -> typing.Optional[int]:
        """Get or set the Part ID of ALE ambient part underlying this segment to be loaded by this blast (see *PART and *SECTION_SOLID, AET=5).This applies only when the blast load is coupled to an ALE air domain.
        """ # nopep8
        return self._cards[0].get_value("alepid")

    @alepid.setter
    def alepid(self, value: int) -> None:
        """Set the alepid property."""
        self._cards[0].set_value("alepid", value)

    @property
    def sfnrb(self) -> float:
        """Get or set the Scale factor for the ambient element non-reflecting boundary condition.
        Shocks waves reflected back to the ambient elements can be attenuated
        with this feature. A value of 1.0 works well for most situations. The
        feature is disabled when a value of zero is specified.
        """ # nopep8
        return self._cards[0].get_value("sfnrb")

    @sfnrb.setter
    def sfnrb(self, value: float) -> None:
        """Set the sfnrb property."""
        self._cards[0].set_value("sfnrb", value)

    @property
    def scalep(self) -> float:
        """Get or set the Pressure scale factor.
        """ # nopep8
        return self._cards[0].get_value("scalep")

    @scalep.setter
    def scalep(self, value: float) -> None:
        """Set the scalep property."""
        self._cards[0].set_value("scalep", value)

    @property
    def n1_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n1."""
        return self._get_link_by_attr("NODE", "nid", self.n1, "parts")

    @property
    def n2_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n2."""
        return self._get_link_by_attr("NODE", "nid", self.n2, "parts")

    @property
    def n3_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n3."""
        return self._get_link_by_attr("NODE", "nid", self.n3, "parts")

    @property
    def n4_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n4."""
        return self._get_link_by_attr("NODE", "nid", self.n4, "parts")

    @property
    def alepid_link(self) -> KeywordBase:
        """Get the PART keyword containing the given alepid."""
        return self._get_link_by_attr("PART", "pid", self.alepid, "parts")

