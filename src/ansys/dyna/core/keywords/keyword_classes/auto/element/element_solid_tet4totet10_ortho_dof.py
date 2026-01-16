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

"""Module providing the ElementSolidTet4Totet10OrthoDof class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_ELEMENTSOLIDTET4TOTET10ORTHODOF_CARD0 = (
    FieldSchema("eid", int, 0, 8, None),
    FieldSchema("pid", int, 8, 8, None),
    FieldSchema("n1", int, 16, 8, None),
    FieldSchema("n2", int, 24, 8, None),
    FieldSchema("n3", int, 32, 8, None),
    FieldSchema("n4", int, 40, 8, None),
    FieldSchema("n5", int, 48, 8, None),
    FieldSchema("n6", int, 56, 8, None),
    FieldSchema("n7", int, 64, 8, None),
    FieldSchema("n8", int, 72, 8, None),
)

_ELEMENTSOLIDTET4TOTET10ORTHODOF_CARD1 = (
    FieldSchema("a1", float, 0, 16, 0.0),
    FieldSchema("a2", float, 16, 16, 0.0),
    FieldSchema("a3", float, 32, 16, 0.0),
)

_ELEMENTSOLIDTET4TOTET10ORTHODOF_CARD2 = (
    FieldSchema("d1", float, 0, 16, 0.0),
    FieldSchema("d2", float, 16, 16, 0.0),
    FieldSchema("d3", float, 32, 16, 0.0),
)

_ELEMENTSOLIDTET4TOTET10ORTHODOF_CARD3 = (
    FieldSchema("unused", int, 0, 8, None),
    FieldSchema("unused", int, 8, 8, None),
    FieldSchema("ns1", int, 16, 8, None),
    FieldSchema("ns2", int, 24, 8, None),
    FieldSchema("ns3", int, 32, 8, None),
    FieldSchema("ns4", int, 40, 8, None),
    FieldSchema("ns5", int, 48, 8, None),
    FieldSchema("ns6", int, 56, 8, None),
    FieldSchema("ns7", int, 64, 8, None),
    FieldSchema("ns8", int, 72, 8, None),
)

class ElementSolidTet4Totet10OrthoDof(KeywordBase):
    """DYNA ELEMENT_SOLID_TET4TOTET10_ORTHO_DOF keyword"""

    keyword = "ELEMENT"
    subkeyword = "SOLID_TET4TOTET10_ORTHO_DOF"
    _link_fields = {
        "n1": LinkType.NODE,
        "n2": LinkType.NODE,
        "n3": LinkType.NODE,
        "n4": LinkType.NODE,
        "n5": LinkType.NODE,
        "n6": LinkType.NODE,
        "n7": LinkType.NODE,
        "n8": LinkType.NODE,
        "ns1": LinkType.NODE,
        "ns2": LinkType.NODE,
        "ns3": LinkType.NODE,
        "ns4": LinkType.NODE,
        "ns5": LinkType.NODE,
        "ns6": LinkType.NODE,
        "ns7": LinkType.NODE,
        "ns8": LinkType.NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the ElementSolidTet4Totet10OrthoDof class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ELEMENTSOLIDTET4TOTET10ORTHODOF_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSOLIDTET4TOTET10ORTHODOF_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSOLIDTET4TOTET10ORTHODOF_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSOLIDTET4TOTET10ORTHODOF_CARD3,
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
        return self._cards[0].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        """Set the n1 property."""
        self._cards[0].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Nodal point 2.
        """ # nopep8
        return self._cards[0].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        """Set the n2 property."""
        self._cards[0].set_value("n2", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Nodal point 3.
        """ # nopep8
        return self._cards[0].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        """Set the n3 property."""
        self._cards[0].set_value("n3", value)

    @property
    def n4(self) -> typing.Optional[int]:
        """Get or set the Nodal point 4.
        """ # nopep8
        return self._cards[0].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        """Set the n4 property."""
        self._cards[0].set_value("n4", value)

    @property
    def n5(self) -> typing.Optional[int]:
        """Get or set the Nodal point 5.
        """ # nopep8
        return self._cards[0].get_value("n5")

    @n5.setter
    def n5(self, value: int) -> None:
        """Set the n5 property."""
        self._cards[0].set_value("n5", value)

    @property
    def n6(self) -> typing.Optional[int]:
        """Get or set the Nodal point 6.
        """ # nopep8
        return self._cards[0].get_value("n6")

    @n6.setter
    def n6(self, value: int) -> None:
        """Set the n6 property."""
        self._cards[0].set_value("n6", value)

    @property
    def n7(self) -> typing.Optional[int]:
        """Get or set the Nodal point 7.
        """ # nopep8
        return self._cards[0].get_value("n7")

    @n7.setter
    def n7(self, value: int) -> None:
        """Set the n7 property."""
        self._cards[0].set_value("n7", value)

    @property
    def n8(self) -> typing.Optional[int]:
        """Get or set the Nodal point 8.
        """ # nopep8
        return self._cards[0].get_value("n8")

    @n8.setter
    def n8(self, value: int) -> None:
        """Set the n8 property."""
        self._cards[0].set_value("n8", value)

    @property
    def a1(self) -> float:
        """Get or set the x-component of local material direction a.
        """ # nopep8
        return self._cards[1].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[1].set_value("a1", value)

    @property
    def a2(self) -> float:
        """Get or set the y-component of local material direction a.
        """ # nopep8
        return self._cards[1].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[1].set_value("a2", value)

    @property
    def a3(self) -> float:
        """Get or set the z-component of local material direction a.
        """ # nopep8
        return self._cards[1].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[1].set_value("a3", value)

    @property
    def d1(self) -> float:
        """Get or set the x-component of vector in the plane of the material vectors a and b.
        """ # nopep8
        return self._cards[2].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[2].set_value("d1", value)

    @property
    def d2(self) -> float:
        """Get or set the y-component of vector in the plane of the material vectors a and b.
        """ # nopep8
        return self._cards[2].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[2].set_value("d2", value)

    @property
    def d3(self) -> float:
        """Get or set the z-component of vector in the plane of the material vectors a and b.
        """ # nopep8
        return self._cards[2].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[2].set_value("d3", value)

    @property
    def ns1(self) -> typing.Optional[int]:
        """Get or set the Scalar node 1.
        """ # nopep8
        return self._cards[3].get_value("ns1")

    @ns1.setter
    def ns1(self, value: int) -> None:
        """Set the ns1 property."""
        self._cards[3].set_value("ns1", value)

    @property
    def ns2(self) -> typing.Optional[int]:
        """Get or set the Scalar node 2.
        """ # nopep8
        return self._cards[3].get_value("ns2")

    @ns2.setter
    def ns2(self, value: int) -> None:
        """Set the ns2 property."""
        self._cards[3].set_value("ns2", value)

    @property
    def ns3(self) -> typing.Optional[int]:
        """Get or set the Scalar node 3.
        """ # nopep8
        return self._cards[3].get_value("ns3")

    @ns3.setter
    def ns3(self, value: int) -> None:
        """Set the ns3 property."""
        self._cards[3].set_value("ns3", value)

    @property
    def ns4(self) -> typing.Optional[int]:
        """Get or set the Scalar node 4.
        """ # nopep8
        return self._cards[3].get_value("ns4")

    @ns4.setter
    def ns4(self, value: int) -> None:
        """Set the ns4 property."""
        self._cards[3].set_value("ns4", value)

    @property
    def ns5(self) -> typing.Optional[int]:
        """Get or set the Scalar node 5.
        """ # nopep8
        return self._cards[3].get_value("ns5")

    @ns5.setter
    def ns5(self, value: int) -> None:
        """Set the ns5 property."""
        self._cards[3].set_value("ns5", value)

    @property
    def ns6(self) -> typing.Optional[int]:
        """Get or set the Scalar node 6.
        """ # nopep8
        return self._cards[3].get_value("ns6")

    @ns6.setter
    def ns6(self, value: int) -> None:
        """Set the ns6 property."""
        self._cards[3].set_value("ns6", value)

    @property
    def ns7(self) -> typing.Optional[int]:
        """Get or set the Scalar node 7.
        """ # nopep8
        return self._cards[3].get_value("ns7")

    @ns7.setter
    def ns7(self, value: int) -> None:
        """Set the ns7 property."""
        self._cards[3].set_value("ns7", value)

    @property
    def ns8(self) -> typing.Optional[int]:
        """Get or set the Scalar node 8.
        """ # nopep8
        return self._cards[3].get_value("ns8")

    @ns8.setter
    def ns8(self, value: int) -> None:
        """Set the ns8 property."""
        self._cards[3].set_value("ns8", value)

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
    def ns1_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given ns1."""
        return self._get_link_by_attr("NODE", "nid", self.ns1, "parts")

    @property
    def ns2_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given ns2."""
        return self._get_link_by_attr("NODE", "nid", self.ns2, "parts")

    @property
    def ns3_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given ns3."""
        return self._get_link_by_attr("NODE", "nid", self.ns3, "parts")

    @property
    def ns4_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given ns4."""
        return self._get_link_by_attr("NODE", "nid", self.ns4, "parts")

    @property
    def ns5_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given ns5."""
        return self._get_link_by_attr("NODE", "nid", self.ns5, "parts")

    @property
    def ns6_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given ns6."""
        return self._get_link_by_attr("NODE", "nid", self.ns6, "parts")

    @property
    def ns7_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given ns7."""
        return self._get_link_by_attr("NODE", "nid", self.ns7, "parts")

    @property
    def ns8_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given ns8."""
        return self._get_link_by_attr("NODE", "nid", self.ns8, "parts")

