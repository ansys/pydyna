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

"""Module providing the AleReferenceSystemNode class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_ALEREFERENCESYSTEMNODE_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
)

_ALEREFERENCESYSTEMNODE_CARD1 = (
    FieldSchema("nid1", int, 0, 10, None),
    FieldSchema("nid2", int, 10, 10, None),
    FieldSchema("nid3", int, 20, 10, None),
    FieldSchema("nid4", int, 30, 10, None),
    FieldSchema("nid5", int, 40, 10, None),
    FieldSchema("nid6", int, 50, 10, None),
    FieldSchema("nid7", int, 60, 10, None),
    FieldSchema("nid8", int, 70, 10, None),
)

_ALEREFERENCESYSTEMNODE_CARD2 = (
    FieldSchema("nid9", int, 0, 10, None),
    FieldSchema("nid10", int, 10, 10, None),
    FieldSchema("nid11", int, 20, 10, None),
    FieldSchema("nid12", int, 30, 10, None),
)

class AleReferenceSystemNode(KeywordBase):
    """DYNA ALE_REFERENCE_SYSTEM_NODE keyword"""

    keyword = "ALE"
    subkeyword = "REFERENCE_SYSTEM_NODE"
    _link_fields = {
        "nid1": LinkType.NODE,
        "nid2": LinkType.NODE,
        "nid3": LinkType.NODE,
        "nid4": LinkType.NODE,
        "nid5": LinkType.NODE,
        "nid6": LinkType.NODE,
        "nid7": LinkType.NODE,
        "nid8": LinkType.NODE,
        "nid9": LinkType.NODE,
        "nid10": LinkType.NODE,
        "nid11": LinkType.NODE,
        "nid12": LinkType.NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the AleReferenceSystemNode class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ALEREFERENCESYSTEMNODE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ALEREFERENCESYSTEMNODE_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ALEREFERENCESYSTEMNODE_CARD2,
                **kwargs,
            ),        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Node group ID for PRTYPE 3 or 7, see *ALE_REFERENCE_SYSTEM_GROUP.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def nid1(self) -> typing.Optional[int]:
        """Get or set the Node one of user specified nodes.
        """ # nopep8
        return self._cards[1].get_value("nid1")

    @nid1.setter
    def nid1(self, value: int) -> None:
        """Set the nid1 property."""
        self._cards[1].set_value("nid1", value)

    @property
    def nid2(self) -> typing.Optional[int]:
        """Get or set the Node two of user specified nodes.
        """ # nopep8
        return self._cards[1].get_value("nid2")

    @nid2.setter
    def nid2(self, value: int) -> None:
        """Set the nid2 property."""
        self._cards[1].set_value("nid2", value)

    @property
    def nid3(self) -> typing.Optional[int]:
        """Get or set the Node three of user specified nodes.
        """ # nopep8
        return self._cards[1].get_value("nid3")

    @nid3.setter
    def nid3(self, value: int) -> None:
        """Set the nid3 property."""
        self._cards[1].set_value("nid3", value)

    @property
    def nid4(self) -> typing.Optional[int]:
        """Get or set the Node four of user specified nodes.
        """ # nopep8
        return self._cards[1].get_value("nid4")

    @nid4.setter
    def nid4(self, value: int) -> None:
        """Set the nid4 property."""
        self._cards[1].set_value("nid4", value)

    @property
    def nid5(self) -> typing.Optional[int]:
        """Get or set the Node five of user specified nodes.
        """ # nopep8
        return self._cards[1].get_value("nid5")

    @nid5.setter
    def nid5(self, value: int) -> None:
        """Set the nid5 property."""
        self._cards[1].set_value("nid5", value)

    @property
    def nid6(self) -> typing.Optional[int]:
        """Get or set the Node six of user specified nodes.
        """ # nopep8
        return self._cards[1].get_value("nid6")

    @nid6.setter
    def nid6(self, value: int) -> None:
        """Set the nid6 property."""
        self._cards[1].set_value("nid6", value)

    @property
    def nid7(self) -> typing.Optional[int]:
        """Get or set the Node seven of user specified nodes.
        """ # nopep8
        return self._cards[1].get_value("nid7")

    @nid7.setter
    def nid7(self, value: int) -> None:
        """Set the nid7 property."""
        self._cards[1].set_value("nid7", value)

    @property
    def nid8(self) -> typing.Optional[int]:
        """Get or set the Node eight of user specified nodes.
        """ # nopep8
        return self._cards[1].get_value("nid8")

    @nid8.setter
    def nid8(self, value: int) -> None:
        """Set the nid8 property."""
        self._cards[1].set_value("nid8", value)

    @property
    def nid9(self) -> typing.Optional[int]:
        """Get or set the Node nine of user specified nodes.
        """ # nopep8
        return self._cards[2].get_value("nid9")

    @nid9.setter
    def nid9(self, value: int) -> None:
        """Set the nid9 property."""
        self._cards[2].set_value("nid9", value)

    @property
    def nid10(self) -> typing.Optional[int]:
        """Get or set the Node ten of user specified nodes.
        """ # nopep8
        return self._cards[2].get_value("nid10")

    @nid10.setter
    def nid10(self, value: int) -> None:
        """Set the nid10 property."""
        self._cards[2].set_value("nid10", value)

    @property
    def nid11(self) -> typing.Optional[int]:
        """Get or set the Node eleven of user specified nodes.
        """ # nopep8
        return self._cards[2].get_value("nid11")

    @nid11.setter
    def nid11(self, value: int) -> None:
        """Set the nid11 property."""
        self._cards[2].set_value("nid11", value)

    @property
    def nid12(self) -> typing.Optional[int]:
        """Get or set the Node twelve of user specified nodes.
        """ # nopep8
        return self._cards[2].get_value("nid12")

    @nid12.setter
    def nid12(self, value: int) -> None:
        """Set the nid12 property."""
        self._cards[2].set_value("nid12", value)

    @property
    def nid1_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid1."""
        return self._get_link_by_attr("NODE", "nid", self.nid1, "parts")

    @property
    def nid2_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid2."""
        return self._get_link_by_attr("NODE", "nid", self.nid2, "parts")

    @property
    def nid3_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid3."""
        return self._get_link_by_attr("NODE", "nid", self.nid3, "parts")

    @property
    def nid4_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid4."""
        return self._get_link_by_attr("NODE", "nid", self.nid4, "parts")

    @property
    def nid5_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid5."""
        return self._get_link_by_attr("NODE", "nid", self.nid5, "parts")

    @property
    def nid6_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid6."""
        return self._get_link_by_attr("NODE", "nid", self.nid6, "parts")

    @property
    def nid7_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid7."""
        return self._get_link_by_attr("NODE", "nid", self.nid7, "parts")

    @property
    def nid8_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid8."""
        return self._get_link_by_attr("NODE", "nid", self.nid8, "parts")

    @property
    def nid9_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid9."""
        return self._get_link_by_attr("NODE", "nid", self.nid9, "parts")

    @property
    def nid10_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid10."""
        return self._get_link_by_attr("NODE", "nid", self.nid10, "parts")

    @property
    def nid11_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid11."""
        return self._get_link_by_attr("NODE", "nid", self.nid11, "parts")

    @property
    def nid12_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid12."""
        return self._get_link_by_attr("NODE", "nid", self.nid12, "parts")

