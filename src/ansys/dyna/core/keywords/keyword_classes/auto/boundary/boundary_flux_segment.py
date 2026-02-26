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

"""Module providing the BoundaryFluxSegment class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_BOUNDARYFLUXSEGMENT_CARD0 = (
    FieldSchema("n1", int, 0, 10, None),
    FieldSchema("n2", int, 10, 10, None),
    FieldSchema("n3", int, 20, 10, None),
    FieldSchema("n4", int, 30, 10, None),
)

_BOUNDARYFLUXSEGMENT_CARD1 = (
    FieldSchema("lcid", int, 0, 10, None),
    FieldSchema("mlc1", float, 10, 10, 1.0),
    FieldSchema("mlc2", float, 20, 10, 1.0),
    FieldSchema("mlc3", float, 30, 10, 1.0),
    FieldSchema("mlc4", float, 40, 10, 1.0),
    FieldSchema("loc", int, 50, 10, 0),
    FieldSchema("nhisv", int, 60, 10, 0),
    FieldSchema("unused", int, 70, 10, None),
)

_BOUNDARYFLUXSEGMENT_CARD2 = (
    FieldSchema("nhisv1", float, 0, 10, 0.0),
    FieldSchema("nhisv2", float, 10, 10, 0.0),
    FieldSchema("nhisv3", float, 20, 10, 0.0),
    FieldSchema("nhisv4", float, 30, 10, 0.0),
    FieldSchema("nhisv5", float, 40, 10, 0.0),
    FieldSchema("nhisv6", float, 50, 10, 0.0),
    FieldSchema("nhisv7", float, 60, 10, 0.0),
    FieldSchema("nhisv8", float, 70, 10, 0.0),
)

class BoundaryFluxSegment(KeywordBase):
    """DYNA BOUNDARY_FLUX_SEGMENT keyword"""

    keyword = "BOUNDARY"
    subkeyword = "FLUX_SEGMENT"
    _link_fields = {
        "n1": LinkType.NODE,
        "n2": LinkType.NODE,
        "n3": LinkType.NODE,
        "n4": LinkType.NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the BoundaryFluxSegment class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYFLUXSEGMENT_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _BOUNDARYFLUXSEGMENT_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _BOUNDARYFLUXSEGMENT_CARD2,
                **kwargs,
            ),        ]
    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the First node ID defining the segment.
        """ # nopep8
        return self._cards[0].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        """Set the n1 property."""
        self._cards[0].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Second node ID defining the segment.
        """ # nopep8
        return self._cards[0].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        """Set the n2 property."""
        self._cards[0].set_value("n2", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Third node ID defining the segment.
        """ # nopep8
        return self._cards[0].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        """Set the n3 property."""
        self._cards[0].set_value("n3", value)

    @property
    def n4(self) -> typing.Optional[int]:
        """Get or set the Fourth node ID defining the segment.
        """ # nopep8
        return self._cards[0].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        """Set the n4 property."""
        self._cards[0].set_value("n4", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for heat flux, see *DEFINE_CURVE:
        GT.0: function versus time,
        EQ.0: use constant multiplier values at nodes,
        LT.0: function versus temperature.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[1].set_value("lcid", value)

    @property
    def mlc1(self) -> float:
        """Get or set the Curve multiplier at node N1.
        """ # nopep8
        return self._cards[1].get_value("mlc1")

    @mlc1.setter
    def mlc1(self, value: float) -> None:
        """Set the mlc1 property."""
        self._cards[1].set_value("mlc1", value)

    @property
    def mlc2(self) -> float:
        """Get or set the Curve multiplier at node N2.
        """ # nopep8
        return self._cards[1].get_value("mlc2")

    @mlc2.setter
    def mlc2(self, value: float) -> None:
        """Set the mlc2 property."""
        self._cards[1].set_value("mlc2", value)

    @property
    def mlc3(self) -> float:
        """Get or set the Curve multiplier at node N3.
        """ # nopep8
        return self._cards[1].get_value("mlc3")

    @mlc3.setter
    def mlc3(self, value: float) -> None:
        """Set the mlc3 property."""
        self._cards[1].set_value("mlc3", value)

    @property
    def mlc4(self) -> float:
        """Get or set the Curve multiplier at node N4.
        """ # nopep8
        return self._cards[1].get_value("mlc4")

    @mlc4.setter
    def mlc4(self, value: float) -> None:
        """Set the mlc4 property."""
        self._cards[1].set_value("mlc4", value)

    @property
    def loc(self) -> int:
        """Get or set the Application of surface for thermal shell elements, see paramter, TSHELL, in the *CONTROL_SHELL input:
        EQ.-1: lower surface of thermal shell element,
        EQ. 1: upper surface of thermal shell element
        """ # nopep8
        return self._cards[1].get_value("loc")

    @loc.setter
    def loc(self, value: int) -> None:
        """Set the loc property."""
        if value not in [0, -1, 1, None]:
            raise Exception("""loc must be `None` or one of {0,-1,1}.""")
        self._cards[1].set_value("loc", value)

    @property
    def nhisv(self) -> int:
        """Get or set the Number of history variables associated with the flux definition:
        GT.0: A user defined subroutine will be called to compute the flux
        """ # nopep8
        return self._cards[1].get_value("nhisv")

    @nhisv.setter
    def nhisv(self, value: int) -> None:
        """Set the nhisv property."""
        self._cards[1].set_value("nhisv", value)

    @property
    def nhisv1(self) -> float:
        """Get or set the Initial value of history variable 1
        """ # nopep8
        return self._cards[2].get_value("nhisv1")

    @nhisv1.setter
    def nhisv1(self, value: float) -> None:
        """Set the nhisv1 property."""
        self._cards[2].set_value("nhisv1", value)

    @property
    def nhisv2(self) -> float:
        """Get or set the Initial value of history variable 2
        """ # nopep8
        return self._cards[2].get_value("nhisv2")

    @nhisv2.setter
    def nhisv2(self, value: float) -> None:
        """Set the nhisv2 property."""
        self._cards[2].set_value("nhisv2", value)

    @property
    def nhisv3(self) -> float:
        """Get or set the Initial value of history variable 3
        """ # nopep8
        return self._cards[2].get_value("nhisv3")

    @nhisv3.setter
    def nhisv3(self, value: float) -> None:
        """Set the nhisv3 property."""
        self._cards[2].set_value("nhisv3", value)

    @property
    def nhisv4(self) -> float:
        """Get or set the Initial value of history variable 4
        """ # nopep8
        return self._cards[2].get_value("nhisv4")

    @nhisv4.setter
    def nhisv4(self, value: float) -> None:
        """Set the nhisv4 property."""
        self._cards[2].set_value("nhisv4", value)

    @property
    def nhisv5(self) -> float:
        """Get or set the Initial value of history variable 5
        """ # nopep8
        return self._cards[2].get_value("nhisv5")

    @nhisv5.setter
    def nhisv5(self, value: float) -> None:
        """Set the nhisv5 property."""
        self._cards[2].set_value("nhisv5", value)

    @property
    def nhisv6(self) -> float:
        """Get or set the Initial value of history variable 6
        """ # nopep8
        return self._cards[2].get_value("nhisv6")

    @nhisv6.setter
    def nhisv6(self, value: float) -> None:
        """Set the nhisv6 property."""
        self._cards[2].set_value("nhisv6", value)

    @property
    def nhisv7(self) -> float:
        """Get or set the Initial value of history variable 7
        """ # nopep8
        return self._cards[2].get_value("nhisv7")

    @nhisv7.setter
    def nhisv7(self, value: float) -> None:
        """Set the nhisv7 property."""
        self._cards[2].set_value("nhisv7", value)

    @property
    def nhisv8(self) -> float:
        """Get or set the Initial value of history variable 8
        """ # nopep8
        return self._cards[2].get_value("nhisv8")

    @nhisv8.setter
    def nhisv8(self, value: float) -> None:
        """Set the nhisv8 property."""
        self._cards[2].set_value("nhisv8", value)

    @property
    def n1_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n1."""
        return self._get_link_by_attr("NODE", "nid", self.n1, "parts")

    @property
    def n2_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n2."""
        return self._get_link_by_attr("NODE", "nid", self.n2, "parts")

    @property
    def n3_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n3."""
        return self._get_link_by_attr("NODE", "nid", self.n3, "parts")

    @property
    def n4_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n4."""
        return self._get_link_by_attr("NODE", "nid", self.n4, "parts")

