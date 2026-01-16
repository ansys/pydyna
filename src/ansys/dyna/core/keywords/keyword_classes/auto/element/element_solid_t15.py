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

"""Module providing the ElementSolidT15 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_ELEMENTSOLIDT15_CARD0 = (
    FieldSchema("eid", int, 0, 8, None),
    FieldSchema("pid", int, 8, 8, None),
)

_ELEMENTSOLIDT15_CARD1 = (
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

_ELEMENTSOLIDT15_CARD2 = (
    FieldSchema("n11", int, 0, 8, None),
    FieldSchema("n12", int, 8, 8, None),
    FieldSchema("n13", int, 16, 8, None),
    FieldSchema("n14", int, 24, 8, None),
    FieldSchema("n15", int, 32, 8, None),
)

class ElementSolidT15(KeywordBase):
    """DYNA ELEMENT_SOLID_T15 keyword"""

    keyword = "ELEMENT"
    subkeyword = "SOLID_T15"
    _link_fields = {
        "n1": LinkType.NODE,
        "n2": LinkType.NODE,
        "n3": LinkType.NODE,
        "n4": LinkType.NODE,
        "n5": LinkType.NODE,
        "n6": LinkType.NODE,
        "n7": LinkType.NODE,
        "n8": LinkType.NODE,
        "n9": LinkType.NODE,
        "n10": LinkType.NODE,
        "n11": LinkType.NODE,
        "n12": LinkType.NODE,
        "n13": LinkType.NODE,
        "n14": LinkType.NODE,
        "n15": LinkType.NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the ElementSolidT15 class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ELEMENTSOLIDT15_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSOLIDT15_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSOLIDT15_CARD2,
                **kwargs,
            ),        ]
    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Element ID. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        """Set the eid property."""
        self._cards[0].set_value("eid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID, see *PART.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Nodal point 1.
        """ # nopep8
        return self._cards[1].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        """Set the n1 property."""
        self._cards[1].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Nodal point 2.
        """ # nopep8
        return self._cards[1].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        """Set the n2 property."""
        self._cards[1].set_value("n2", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Nodal point 3.
        """ # nopep8
        return self._cards[1].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        """Set the n3 property."""
        self._cards[1].set_value("n3", value)

    @property
    def n4(self) -> typing.Optional[int]:
        """Get or set the Nodal point 4.
        """ # nopep8
        return self._cards[1].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        """Set the n4 property."""
        self._cards[1].set_value("n4", value)

    @property
    def n5(self) -> typing.Optional[int]:
        """Get or set the Nodal point 5.
        """ # nopep8
        return self._cards[1].get_value("n5")

    @n5.setter
    def n5(self, value: int) -> None:
        """Set the n5 property."""
        self._cards[1].set_value("n5", value)

    @property
    def n6(self) -> typing.Optional[int]:
        """Get or set the Nodal point 6.
        """ # nopep8
        return self._cards[1].get_value("n6")

    @n6.setter
    def n6(self, value: int) -> None:
        """Set the n6 property."""
        self._cards[1].set_value("n6", value)

    @property
    def n7(self) -> typing.Optional[int]:
        """Get or set the Nodal point 7.
        """ # nopep8
        return self._cards[1].get_value("n7")

    @n7.setter
    def n7(self, value: int) -> None:
        """Set the n7 property."""
        self._cards[1].set_value("n7", value)

    @property
    def n8(self) -> typing.Optional[int]:
        """Get or set the Nodal point 8.
        """ # nopep8
        return self._cards[1].get_value("n8")

    @n8.setter
    def n8(self, value: int) -> None:
        """Set the n8 property."""
        self._cards[1].set_value("n8", value)

    @property
    def n9(self) -> typing.Optional[int]:
        """Get or set the Nodal point 9.
        """ # nopep8
        return self._cards[1].get_value("n9")

    @n9.setter
    def n9(self, value: int) -> None:
        """Set the n9 property."""
        self._cards[1].set_value("n9", value)

    @property
    def n10(self) -> typing.Optional[int]:
        """Get or set the Nodal point 10.
        """ # nopep8
        return self._cards[1].get_value("n10")

    @n10.setter
    def n10(self, value: int) -> None:
        """Set the n10 property."""
        self._cards[1].set_value("n10", value)

    @property
    def n11(self) -> typing.Optional[int]:
        """Get or set the Nodal point 11.
        """ # nopep8
        return self._cards[2].get_value("n11")

    @n11.setter
    def n11(self, value: int) -> None:
        """Set the n11 property."""
        self._cards[2].set_value("n11", value)

    @property
    def n12(self) -> typing.Optional[int]:
        """Get or set the Nodal point 12.
        """ # nopep8
        return self._cards[2].get_value("n12")

    @n12.setter
    def n12(self, value: int) -> None:
        """Set the n12 property."""
        self._cards[2].set_value("n12", value)

    @property
    def n13(self) -> typing.Optional[int]:
        """Get or set the Nodal point 13.
        """ # nopep8
        return self._cards[2].get_value("n13")

    @n13.setter
    def n13(self, value: int) -> None:
        """Set the n13 property."""
        self._cards[2].set_value("n13", value)

    @property
    def n14(self) -> typing.Optional[int]:
        """Get or set the Nodal point 14.
        """ # nopep8
        return self._cards[2].get_value("n14")

    @n14.setter
    def n14(self, value: int) -> None:
        """Set the n14 property."""
        self._cards[2].set_value("n14", value)

    @property
    def n15(self) -> typing.Optional[int]:
        """Get or set the Nodal point 15.
        """ # nopep8
        return self._cards[2].get_value("n15")

    @n15.setter
    def n15(self, value: int) -> None:
        """Set the n15 property."""
        self._cards[2].set_value("n15", value)

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
    def n5_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n5."""
        return self._get_link_by_attr("NODE", "nid", self.n5, "parts")

    @property
    def n6_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n6."""
        return self._get_link_by_attr("NODE", "nid", self.n6, "parts")

    @property
    def n7_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n7."""
        return self._get_link_by_attr("NODE", "nid", self.n7, "parts")

    @property
    def n8_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n8."""
        return self._get_link_by_attr("NODE", "nid", self.n8, "parts")

    @property
    def n9_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n9."""
        return self._get_link_by_attr("NODE", "nid", self.n9, "parts")

    @property
    def n10_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n10."""
        return self._get_link_by_attr("NODE", "nid", self.n10, "parts")

    @property
    def n11_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n11."""
        return self._get_link_by_attr("NODE", "nid", self.n11, "parts")

    @property
    def n12_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n12."""
        return self._get_link_by_attr("NODE", "nid", self.n12, "parts")

    @property
    def n13_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n13."""
        return self._get_link_by_attr("NODE", "nid", self.n13, "parts")

    @property
    def n14_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n14."""
        return self._get_link_by_attr("NODE", "nid", self.n14, "parts")

    @property
    def n15_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n15."""
        return self._get_link_by_attr("NODE", "nid", self.n15, "parts")

