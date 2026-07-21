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

"""Module providing the InitialTiedContactData class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_INITIALTIEDCONTACTDATA_CARD0 = (
    FieldSchema("ilabel", int, 0, 10, None),
    FieldSchema("snid", int, 10, 10, None),
    FieldSchema("mn1", int, 20, 10, None),
    FieldSchema("mn2", int, 30, 10, None),
    FieldSchema("mn3", int, 40, 10, None),
    FieldSchema("mn4", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

_INITIALTIEDCONTACTDATA_CARD1 = (
    FieldSchema("ss", float, 0, 10, None),
    FieldSchema("tt", float, 10, 10, None),
    FieldSchema("thk", float, 20, 10, None),
)

class InitialTiedContactData(KeywordBase):
    """DYNA INITIAL_TIED_CONTACT_DATA keyword"""

    keyword = "INITIAL"
    subkeyword = "TIED_CONTACT_DATA"
    _link_fields = {
        "snid": LinkType.NODE,
        "mn1": LinkType.NODE,
        "mn2": LinkType.NODE,
        "mn3": LinkType.NODE,
        "mn4": LinkType.NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the InitialTiedContactData class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INITIALTIEDCONTACTDATA_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _INITIALTIEDCONTACTDATA_CARD1,
                **kwargs,
            ),
        ]
    @property
    def ilabel(self) -> typing.Optional[int]:
        """Get or set the ID of *CONTACT
        """ # nopep8
        return self._cards[0].get_value("ilabel")

    @ilabel.setter
    def ilabel(self, value: int) -> None:
        """Set the ilabel property."""
        self._cards[0].set_value("ilabel", value)

    @property
    def snid(self) -> typing.Optional[int]:
        """Get or set the ID of tied node.
        """ # nopep8
        return self._cards[0].get_value("snid")

    @snid.setter
    def snid(self, value: int) -> None:
        """Set the snid property."""
        self._cards[0].set_value("snid", value)

    @property
    def mn1(self) -> typing.Optional[int]:
        """Get or set the ID of node 1 of the segment to which SNID is tied.
        """ # nopep8
        return self._cards[0].get_value("mn1")

    @mn1.setter
    def mn1(self, value: int) -> None:
        """Set the mn1 property."""
        self._cards[0].set_value("mn1", value)

    @property
    def mn2(self) -> typing.Optional[int]:
        """Get or set the ID of node 2 of the segment to which SNID is tied.
        """ # nopep8
        return self._cards[0].get_value("mn2")

    @mn2.setter
    def mn2(self, value: int) -> None:
        """Set the mn2 property."""
        self._cards[0].set_value("mn2", value)

    @property
    def mn3(self) -> typing.Optional[int]:
        """Get or set the ID of node 3 of the segment to which SNID is tied.
        """ # nopep8
        return self._cards[0].get_value("mn3")

    @mn3.setter
    def mn3(self, value: int) -> None:
        """Set the mn3 property."""
        self._cards[0].set_value("mn3", value)

    @property
    def mn4(self) -> typing.Optional[int]:
        """Get or set the ID of node 4 of the segment to which SNID is tied.
        """ # nopep8
        return self._cards[0].get_value("mn4")

    @mn4.setter
    def mn4(self, value: int) -> None:
        """Set the mn4 property."""
        self._cards[0].set_value("mn4", value)

    @property
    def ss(self) -> typing.Optional[float]:
        """Get or set the Initial isoparametric coordinate of the tied node within the segment, in the range -1<=SS<=1
        """ # nopep8
        return self._cards[1].get_value("ss")

    @ss.setter
    def ss(self, value: float) -> None:
        """Set the ss property."""
        self._cards[1].set_value("ss", value)

    @property
    def tt(self) -> typing.Optional[float]:
        """Get or set the Initial isoparametric coordinate of tied node within the segment, in the range -1<=TT<=1
        """ # nopep8
        return self._cards[1].get_value("tt")

    @tt.setter
    def tt(self, value: float) -> None:
        """Set the tt property."""
        self._cards[1].set_value("tt", value)

    @property
    def thk(self) -> typing.Optional[float]:
        """Get or set the Initial offset of the tied node from the plane of the segment, in length units.
        """ # nopep8
        return self._cards[1].get_value("thk")

    @thk.setter
    def thk(self, value: float) -> None:
        """Set the thk property."""
        self._cards[1].set_value("thk", value)

    @property
    def snid_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given snid."""
        return self._get_link_by_attr("NODE", "nid", self.snid, "parts")

    @property
    def mn1_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given mn1."""
        return self._get_link_by_attr("NODE", "nid", self.mn1, "parts")

    @property
    def mn2_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given mn2."""
        return self._get_link_by_attr("NODE", "nid", self.mn2, "parts")

    @property
    def mn3_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given mn3."""
        return self._get_link_by_attr("NODE", "nid", self.mn3, "parts")

    @property
    def mn4_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given mn4."""
        return self._get_link_by_attr("NODE", "nid", self.mn4, "parts")

