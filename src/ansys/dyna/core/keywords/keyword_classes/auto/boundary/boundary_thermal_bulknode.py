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

"""Module providing the BoundaryThermalBulknode class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_BOUNDARYTHERMALBULKNODE_CARD0 = (
    FieldSchema("nid", int, 0, 10, None),
    FieldSchema("pid", int, 10, 10, None),
    FieldSchema("nbnseg", int, 20, 10, None),
    FieldSchema("vol", float, 30, 10, None),
    FieldSchema("lcid", int, 40, 10, None),
    FieldSchema("h", float, 50, 10, None),
    FieldSchema("aexp", float, 60, 10, None),
    FieldSchema("bexp", float, 70, 10, None),
)

_BOUNDARYTHERMALBULKNODE_CARD1 = (
    FieldSchema("n1", int, 0, 10, None),
    FieldSchema("n2", int, 10, 10, None),
    FieldSchema("n3", int, 20, 10, None),
    FieldSchema("n4", int, 30, 10, None),
)

class BoundaryThermalBulknode(KeywordBase):
    """DYNA BOUNDARY_THERMAL_BULKNODE keyword"""

    keyword = "BOUNDARY"
    subkeyword = "THERMAL_BULKNODE"
    _link_fields = {
        "nid": LinkType.NODE,
        "n1": LinkType.NODE,
        "n2": LinkType.NODE,
        "n3": LinkType.NODE,
        "n4": LinkType.NODE,
        "lcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the BoundaryThermalBulknode class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYTHERMALBULKNODE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _BOUNDARYTHERMALBULKNODE_CARD1,
                **kwargs,
            ),        ]
    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Bulk node number.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[0].set_value("nid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Bulk node part id.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def nbnseg(self) -> typing.Optional[int]:
        """Get or set the Number of bulk node segments attached to this bulk node
        """ # nopep8
        return self._cards[0].get_value("nbnseg")

    @nbnseg.setter
    def nbnseg(self, value: int) -> None:
        """Set the nbnseg property."""
        self._cards[0].set_value("nbnseg", value)

    @property
    def vol(self) -> typing.Optional[float]:
        """Get or set the Bulk node volume..
        """ # nopep8
        return self._cards[0].get_value("vol")

    @vol.setter
    def vol(self, value: float) -> None:
        """Set the vol property."""
        self._cards[0].set_value("vol", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for H.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def h(self) -> typing.Optional[float]:
        """Get or set the Heat transfer coefficient.
        """ # nopep8
        return self._cards[0].get_value("h")

    @h.setter
    def h(self, value: float) -> None:
        """Set the h property."""
        self._cards[0].set_value("h", value)

    @property
    def aexp(self) -> typing.Optional[float]:
        """Get or set the A exponent.
        """ # nopep8
        return self._cards[0].get_value("aexp")

    @aexp.setter
    def aexp(self, value: float) -> None:
        """Set the aexp property."""
        self._cards[0].set_value("aexp", value)

    @property
    def bexp(self) -> typing.Optional[float]:
        """Get or set the B exponent.
        """ # nopep8
        return self._cards[0].get_value("bexp")

    @bexp.setter
    def bexp(self, value: float) -> None:
        """Set the bexp property."""
        self._cards[0].set_value("bexp", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Nodal point numbers.
        """ # nopep8
        return self._cards[1].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        """Set the n1 property."""
        self._cards[1].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Nodal point numbers.
        """ # nopep8
        return self._cards[1].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        """Set the n2 property."""
        self._cards[1].set_value("n2", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Nodal point numbers.
        """ # nopep8
        return self._cards[1].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        """Set the n3 property."""
        self._cards[1].set_value("n3", value)

    @property
    def n4(self) -> typing.Optional[int]:
        """Get or set the Nodal point numbers.
        """ # nopep8
        return self._cards[1].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        """Set the n4 property."""
        self._cards[1].set_value("n4", value)

    @property
    def nid_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given nid."""
        return self._get_link_by_attr("NODE", "nid", self.nid, "parts")

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
    def lcid_link(self) -> DefineCurve:
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

