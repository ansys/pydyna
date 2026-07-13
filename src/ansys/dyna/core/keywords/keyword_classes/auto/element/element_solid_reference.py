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

"""Module providing the ElementSolidReference class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_ELEMENTSOLIDREFERENCE_CARD0 = (
    FieldSchema("eid", int, 0, 8, None),
)

_ELEMENTSOLIDREFERENCE_CARD1 = (
    FieldSchema("n1", int, 0, 8, None),
    FieldSchema("n2", int, 8, 8, None),
    FieldSchema("n3", int, 16, 8, None),
    FieldSchema("n4", int, 24, 8, None),
    FieldSchema("n5", int, 32, 8, None),
    FieldSchema("n6", int, 40, 8, None),
    FieldSchema("n7", int, 48, 8, None),
    FieldSchema("n8", int, 56, 8, None),
    FieldSchema("n9", int, 64, 8, None),
    FieldSchema("n10", int, 72, 8, None),
)

class ElementSolidReference(KeywordBase):
    """DYNA ELEMENT_SOLID_REFERENCE keyword"""

    keyword = "ELEMENT"
    subkeyword = "SOLID_REFERENCE"
    _link_fields = {
        "n10": LinkType.NODE,
        "eid": LinkType.ELEMENT_SOLID,
    }

    def __init__(self, **kwargs):
        """Initialize the ElementSolidReference class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ELEMENTSOLIDREFERENCE_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ELEMENTSOLIDREFERENCE_CARD1,
                **kwargs,
            ),
        ]
    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Element ID.  This ID must correspond to the same ID that is used on the associated *ELEMENT_SOLID card.
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        """Set the eid property."""
        self._cards[0].set_value("eid", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Node number in the connectivity, referring to the corresponding ID in *NODE_REFERENCE.
        """ # nopep8
        return self._cards[1].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        """Set the n1 property."""
        self._cards[1].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Node number in the connectivity, referring to the corresponding ID in *NODE_REFERENCE.
        """ # nopep8
        return self._cards[1].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        """Set the n2 property."""
        self._cards[1].set_value("n2", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Node number in the connectivity, referring to the corresponding ID in *NODE_REFERENCE.
        """ # nopep8
        return self._cards[1].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        """Set the n3 property."""
        self._cards[1].set_value("n3", value)

    @property
    def n4(self) -> typing.Optional[int]:
        """Get or set the Node number in the connectivity, referring to the corresponding ID in *NODE_REFERENCE.
        """ # nopep8
        return self._cards[1].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        """Set the n4 property."""
        self._cards[1].set_value("n4", value)

    @property
    def n5(self) -> typing.Optional[int]:
        """Get or set the Node number in the connectivity, referring to the corresponding ID in *NODE_REFERENCE.
        """ # nopep8
        return self._cards[1].get_value("n5")

    @n5.setter
    def n5(self, value: int) -> None:
        """Set the n5 property."""
        self._cards[1].set_value("n5", value)

    @property
    def n6(self) -> typing.Optional[int]:
        """Get or set the Node number in the connectivity, referring to the corresponding ID in *NODE_REFERENCE.
        """ # nopep8
        return self._cards[1].get_value("n6")

    @n6.setter
    def n6(self, value: int) -> None:
        """Set the n6 property."""
        self._cards[1].set_value("n6", value)

    @property
    def n7(self) -> typing.Optional[int]:
        """Get or set the Node number in the connectivity, referring to the corresponding ID in *NODE_REFERENCE.
        """ # nopep8
        return self._cards[1].get_value("n7")

    @n7.setter
    def n7(self, value: int) -> None:
        """Set the n7 property."""
        self._cards[1].set_value("n7", value)

    @property
    def n8(self) -> typing.Optional[int]:
        """Get or set the Node number in the connectivity, referring to the corresponding ID in *NODE_REFERENCE.
        """ # nopep8
        return self._cards[1].get_value("n8")

    @n8.setter
    def n8(self, value: int) -> None:
        """Set the n8 property."""
        self._cards[1].set_value("n8", value)

    @property
    def n9(self) -> typing.Optional[int]:
        """Get or set the Node number in the connectivity, referring to the corresponding ID in *NODE_REFERENCE.
        """ # nopep8
        return self._cards[1].get_value("n9")

    @n9.setter
    def n9(self, value: int) -> None:
        """Set the n9 property."""
        self._cards[1].set_value("n9", value)

    @property
    def n10(self) -> typing.Optional[int]:
        """Get or set the Node number in the connectivity, referring to the corresponding ID in *NODE_REFERENCE.
        """ # nopep8
        return self._cards[1].get_value("n10")

    @n10.setter
    def n10(self, value: int) -> None:
        """Set the n10 property."""
        self._cards[1].set_value("n10", value)

    @property
    def n10_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n10."""
        return self._get_link_by_attr("NODE", "nid", self.n10, "parts")

    @property
    def eid_link(self) -> typing.Optional[KeywordBase]:
        """Get the ELEMENT keyword containing the given eid."""
        return self._get_link_by_attr("ELEMENT", "eid", self.eid, "parts")

