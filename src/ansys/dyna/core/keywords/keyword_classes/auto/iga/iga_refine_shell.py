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

"""Module providing the IgaRefineShell class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_IGAREFINESHELL_CARD0 = (
    FieldSchema("rid", int, 0, 10, None),
    FieldSchema("rtyp", int, 10, 10, None),
)

_IGAREFINESHELL_CARD1 = (
    FieldSchema("hrtyp", int, 0, 10, None),
    FieldSchema("rr", float, 10, 10, None),
    FieldSchema("rs", float, 20, 10, None),
)

_IGAREFINESHELL_CARD2 = (
    FieldSchema("tr", int, 0, 10, None),
    FieldSchema("ts", int, 10, 10, None),
)

class IgaRefineShell(KeywordBase):
    """DYNA IGA_REFINE_SHELL keyword"""

    keyword = "IGA"
    subkeyword = "REFINE_SHELL"

    def __init__(self, **kwargs):
        """Initialize the IgaRefineShell class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _IGAREFINESHELL_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _IGAREFINESHELL_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _IGAREFINESHELL_CARD2,
                **kwargs,
            ),
        ]
    @property
    def rid(self) -> typing.Optional[int]:
        """Get or set the Isogeometric shell refinement ID. A unique number must be chosen.
        """ # nopep8
        return self._cards[0].get_value("rid")

    @rid.setter
    def rid(self, value: int) -> None:
        """Set the rid property."""
        self._cards[0].set_value("rid", value)

    @property
    def rtyp(self) -> typing.Optional[int]:
        """Get or set the Refinement type:
        EQ.1: h - refinement(knot insertion)
        EQ.2: k - refinement, meaning refinement followed by h - refinement
        EQ.3: p - refinement(degree elevation)
        """ # nopep8
        return self._cards[0].get_value("rtyp")

    @rtyp.setter
    def rtyp(self, value: int) -> None:
        """Set the rtyp property."""
        self._cards[0].set_value("rtyp", value)

    @property
    def hrtyp(self) -> typing.Optional[int]:
        """Get or set the h-refinement type:
        EQ.1: Number of subdivisions - based.Each nonzero knot span is subdivided to NINT(RR)?NINT(RS) equal knot spans.Here, NINT(x)rounds x to the nearest integer.
        """ # nopep8
        return self._cards[1].get_value("hrtyp")

    @hrtyp.setter
    def hrtyp(self, value: int) -> None:
        """Set the hrtyp property."""
        self._cards[1].set_value("hrtyp", value)

    @property
    def rr(self) -> typing.Optional[float]:
        """Get or set the NINT(RR) is the number of equal parametric segments in the local r-direction. NINT (RR ) means the input RR is rounded to the nearest integer.
        """ # nopep8
        return self._cards[1].get_value("rr")

    @rr.setter
    def rr(self, value: float) -> None:
        """Set the rr property."""
        self._cards[1].set_value("rr", value)

    @property
    def rs(self) -> typing.Optional[float]:
        """Get or set the NINT(RS) is the number of equal parametric segments in the local s-direction. NINT (RS ) means the input RS is rounded to the nearest integer.
        """ # nopep8
        return self._cards[1].get_value("rs")

    @rs.setter
    def rs(self, value: float) -> None:
        """Set the rs property."""
        self._cards[1].set_value("rs", value)

    @property
    def tr(self) -> typing.Optional[int]:
        """Get or set the Target polynomial degree in the local r-direction; see Remark 1.
        """ # nopep8
        return self._cards[2].get_value("tr")

    @tr.setter
    def tr(self, value: int) -> None:
        """Set the tr property."""
        self._cards[2].set_value("tr", value)

    @property
    def ts(self) -> typing.Optional[int]:
        """Get or set the Target polynomial degree in the local s-direction, see Remark 1.
        """ # nopep8
        return self._cards[2].get_value("ts")

    @ts.setter
    def ts(self, value: int) -> None:
        """Set the ts property."""
        self._cards[2].set_value("ts", value)

