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

"""Module providing the DatabaseFrequencyBinaryD3Acc class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_DATABASEFREQUENCYBINARYD3ACC_CARD0 = (
    FieldSchema("binary", int, 0, 10, None),
)

_DATABASEFREQUENCYBINARYD3ACC_CARD1 = (
    FieldSchema("nid1", int, 0, 10, 0),
    FieldSchema("nid2", int, 10, 10, 0),
    FieldSchema("nid3", int, 20, 10, 0),
    FieldSchema("nid4", int, 30, 10, 0),
    FieldSchema("nid5", int, 40, 10, 0),
    FieldSchema("nid6", int, 50, 10, 0),
    FieldSchema("nid7", int, 60, 10, 0),
    FieldSchema("nid8", int, 70, 10, 0),
)

class DatabaseFrequencyBinaryD3Acc(KeywordBase):
    """DYNA DATABASE_FREQUENCY_BINARY_D3ACC keyword"""

    keyword = "DATABASE"
    subkeyword = "FREQUENCY_BINARY_D3ACC"
    _link_fields = {
        "nid1": LinkType.NODE,
        "nid2": LinkType.NODE,
        "nid3": LinkType.NODE,
        "nid4": LinkType.NODE,
        "nid5": LinkType.NODE,
        "nid6": LinkType.NODE,
        "nid7": LinkType.NODE,
        "nid8": LinkType.NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the DatabaseFrequencyBinaryD3Acc class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DATABASEFREQUENCYBINARYD3ACC_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DATABASEFREQUENCYBINARYD3ACC_CARD1,
                **kwargs,
            ),        ]
    @property
    def binary(self) -> typing.Optional[int]:
        """Get or set the Flag for writing the binary plot file.  See Remark 1.
        EQ.0:	Off
        EQ.1 : Write the binary plot file.
        EQ.2 : Write the complex variable binary plot file D3SSD(OPTION1 = D3SSD) or include the individual mode response in the binary plot file D3SPCM(OPTION1‌ = D3SPCM).
        EQ.3 : Write the binary plot file which combines response spectrum analysis results and other structural analysis results provided by the file specified with Card  2c(OPTION1‌ = D3SPCM).
        EQ.90 : Write only real part of frequency response(D3SSD only).
        EQ.91 : Write only imaginary part of frequency response(D3SSD only).
        """ # nopep8
        return self._cards[0].get_value("binary")

    @binary.setter
    def binary(self, value: int) -> None:
        """Set the binary property."""
        self._cards[0].set_value("binary", value)

    @property
    def nid1(self) -> int:
        """Get or set the Field point node ID for writing D3ACC file (up to 10 NID are allowed)
        """ # nopep8
        return self._cards[1].get_value("nid1")

    @nid1.setter
    def nid1(self, value: int) -> None:
        """Set the nid1 property."""
        self._cards[1].set_value("nid1", value)

    @property
    def nid2(self) -> int:
        """Get or set the Field point node ID for writing D3ACC file (up to 10 NID are allowed)
        """ # nopep8
        return self._cards[1].get_value("nid2")

    @nid2.setter
    def nid2(self, value: int) -> None:
        """Set the nid2 property."""
        self._cards[1].set_value("nid2", value)

    @property
    def nid3(self) -> int:
        """Get or set the Field point node ID for writing D3ACC file (up to 10 NID are allowed)
        """ # nopep8
        return self._cards[1].get_value("nid3")

    @nid3.setter
    def nid3(self, value: int) -> None:
        """Set the nid3 property."""
        self._cards[1].set_value("nid3", value)

    @property
    def nid4(self) -> int:
        """Get or set the Field point node ID for writing D3ACC file (up to 10 NID are allowed)
        """ # nopep8
        return self._cards[1].get_value("nid4")

    @nid4.setter
    def nid4(self, value: int) -> None:
        """Set the nid4 property."""
        self._cards[1].set_value("nid4", value)

    @property
    def nid5(self) -> int:
        """Get or set the Field point node ID for writing D3ACC file (up to 10 NID are allowed)
        """ # nopep8
        return self._cards[1].get_value("nid5")

    @nid5.setter
    def nid5(self, value: int) -> None:
        """Set the nid5 property."""
        self._cards[1].set_value("nid5", value)

    @property
    def nid6(self) -> int:
        """Get or set the Field point node ID for writing D3ACC file (up to 10 NID are allowed)
        """ # nopep8
        return self._cards[1].get_value("nid6")

    @nid6.setter
    def nid6(self, value: int) -> None:
        """Set the nid6 property."""
        self._cards[1].set_value("nid6", value)

    @property
    def nid7(self) -> int:
        """Get or set the Field point node ID for writing D3ACC file (up to 10 NID are allowed)
        """ # nopep8
        return self._cards[1].get_value("nid7")

    @nid7.setter
    def nid7(self, value: int) -> None:
        """Set the nid7 property."""
        self._cards[1].set_value("nid7", value)

    @property
    def nid8(self) -> int:
        """Get or set the Field point node ID for writing D3ACC file (up to 10 NID are allowed)
        """ # nopep8
        return self._cards[1].get_value("nid8")

    @nid8.setter
    def nid8(self, value: int) -> None:
        """Set the nid8 property."""
        self._cards[1].set_value("nid8", value)

    @property
    def nid1_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given nid1."""
        return self._get_link_by_attr("NODE", "nid", self.nid1, "parts")

    @property
    def nid2_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given nid2."""
        return self._get_link_by_attr("NODE", "nid", self.nid2, "parts")

    @property
    def nid3_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given nid3."""
        return self._get_link_by_attr("NODE", "nid", self.nid3, "parts")

    @property
    def nid4_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given nid4."""
        return self._get_link_by_attr("NODE", "nid", self.nid4, "parts")

    @property
    def nid5_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given nid5."""
        return self._get_link_by_attr("NODE", "nid", self.nid5, "parts")

    @property
    def nid6_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given nid6."""
        return self._get_link_by_attr("NODE", "nid", self.nid6, "parts")

    @property
    def nid7_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given nid7."""
        return self._get_link_by_attr("NODE", "nid", self.nid7, "parts")

    @property
    def nid8_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given nid8."""
        return self._get_link_by_attr("NODE", "nid", self.nid8, "parts")

