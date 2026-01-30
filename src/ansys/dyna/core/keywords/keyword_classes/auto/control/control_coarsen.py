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

"""Module providing the ControlCoarsen class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_CONTROLCOARSEN_CARD0 = (
    FieldSchema("icoarse", int, 0, 10, 0),
    FieldSchema("angle", float, 10, 10, None),
    FieldSchema("nseed", int, 20, 10, 0),
    FieldSchema("psid", int, 30, 10, None),
    FieldSchema("smax", float, 40, 10, None),
)

_CONTROLCOARSEN_CARD1 = (
    FieldSchema("n1", int, 0, 10, 0),
    FieldSchema("n2", int, 10, 10, 0),
    FieldSchema("n3", int, 20, 10, 0),
    FieldSchema("n4", int, 30, 10, 0),
    FieldSchema("n5", int, 40, 10, 0),
    FieldSchema("n6", int, 50, 10, 0),
    FieldSchema("n7", int, 60, 10, 0),
    FieldSchema("n8", int, 70, 10, 0),
)

class ControlCoarsen(KeywordBase):
    """DYNA CONTROL_COARSEN keyword"""

    keyword = "CONTROL"
    subkeyword = "COARSEN"
    _link_fields = {
        "n1": LinkType.NODE,
        "n2": LinkType.NODE,
        "n3": LinkType.NODE,
        "n4": LinkType.NODE,
        "n5": LinkType.NODE,
        "n6": LinkType.NODE,
        "n7": LinkType.NODE,
        "n8": LinkType.NODE,
        "psid": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ControlCoarsen class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLCOARSEN_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTROLCOARSEN_CARD1,
                **kwargs,
            ),        ]
    @property
    def icoarse(self) -> int:
        """Get or set the Coarsening flag:
        EQ.0: Do not coarsen (default),
        EQ.1: Coarsen mesh at beginning of simulation.
        EQ.2: Coarsen mesh at beginning of simulation for forming model
        """ # nopep8
        return self._cards[0].get_value("icoarse")

    @icoarse.setter
    def icoarse(self, value: int) -> None:
        """Set the icoarse property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""icoarse must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("icoarse", value)

    @property
    def angle(self) -> typing.Optional[float]:
        """Get or set the Allowable angle change between neighboring elements. Adjacent elements which are flat to within ANGLE degrees are merged.
        Suggested starting value = 8.0 degrees.
        """ # nopep8
        return self._cards[0].get_value("angle")

    @angle.setter
    def angle(self, value: float) -> None:
        """Set the angle property."""
        self._cards[0].set_value("angle", value)

    @property
    def nseed(self) -> int:
        """Get or set the Number of seed nodes (optional).
        EQ.0: use only automatic searching (default).
        EQ.n: also search starting with node IDs given below (maximum = 8 nodes).
        """ # nopep8
        return self._cards[0].get_value("nseed")

    @nseed.setter
    def nseed(self, value: int) -> None:
        """Set the nseed property."""
        self._cards[0].set_value("nseed", value)

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part set ID. all the parts defined in this set will be prevented from been coarsened.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[0].set_value("psid", value)

    @property
    def smax(self) -> typing.Optional[float]:
        """Get or set the Max element size. For ICOARSE=2 no elements larger then this size will be created.
        """ # nopep8
        return self._cards[0].get_value("smax")

    @smax.setter
    def smax(self, value: float) -> None:
        """Set the smax property."""
        self._cards[0].set_value("smax", value)

    @property
    def n1(self) -> int:
        """Get or set the Optional list of seed node IDs for extra searching.
        """ # nopep8
        return self._cards[1].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        """Set the n1 property."""
        self._cards[1].set_value("n1", value)

    @property
    def n2(self) -> int:
        """Get or set the Optional list of seed node IDs for extra searching.
        """ # nopep8
        return self._cards[1].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        """Set the n2 property."""
        self._cards[1].set_value("n2", value)

    @property
    def n3(self) -> int:
        """Get or set the Optional list of seed node IDs for extra searching.
        """ # nopep8
        return self._cards[1].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        """Set the n3 property."""
        self._cards[1].set_value("n3", value)

    @property
    def n4(self) -> int:
        """Get or set the Optional list of seed node IDs for extra searching.
        """ # nopep8
        return self._cards[1].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        """Set the n4 property."""
        self._cards[1].set_value("n4", value)

    @property
    def n5(self) -> int:
        """Get or set the Optional list of seed node IDs for extra searching.
        """ # nopep8
        return self._cards[1].get_value("n5")

    @n5.setter
    def n5(self, value: int) -> None:
        """Set the n5 property."""
        self._cards[1].set_value("n5", value)

    @property
    def n6(self) -> int:
        """Get or set the Optional list of seed node IDs for extra searching.
        """ # nopep8
        return self._cards[1].get_value("n6")

    @n6.setter
    def n6(self, value: int) -> None:
        """Set the n6 property."""
        self._cards[1].set_value("n6", value)

    @property
    def n7(self) -> int:
        """Get or set the Optional list of seed node IDs for extra searching.
        """ # nopep8
        return self._cards[1].get_value("n7")

    @n7.setter
    def n7(self, value: int) -> None:
        """Set the n7 property."""
        self._cards[1].set_value("n7", value)

    @property
    def n8(self) -> int:
        """Get or set the Optional list of seed node IDs for extra searching.
        """ # nopep8
        return self._cards[1].get_value("n8")

    @n8.setter
    def n8(self, value: int) -> None:
        """Set the n8 property."""
        self._cards[1].set_value("n8", value)

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
    def psid_link(self) -> KeywordBase:
        """Get the SET_PART_* keyword for psid."""
        return self._get_set_link("PART", self.psid)

    @psid_link.setter
    def psid_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psid."""
        self.psid = value.sid

