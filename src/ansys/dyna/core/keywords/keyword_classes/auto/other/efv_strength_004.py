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

"""Module providing the EfvStrength004 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVSTRENGTH004_CARD0 = (
    FieldSchema("strid", int, 0, 10, None),
    FieldSchema("g", float, 10, 10, None),
    FieldSchema("a", float, 20, 10, None),
    FieldSchema("b", float, 30, 10, None),
    FieldSchema("n", float, 40, 10, None),
    FieldSchema("c", float, 50, 10, None),
    FieldSchema("m", float, 60, 10, None),
    FieldSchema("tm", float, 70, 10, None),
)

_EFVSTRENGTH004_CARD1 = (
    FieldSchema("epsrtref", float, 0, 10, 1.0),
    FieldSchema("iepsrtc", int, 10, 10, 2),
)

class EfvStrength004(KeywordBase):
    """DYNA EFV_STRENGTH_004 keyword"""

    keyword = "EFV"
    subkeyword = "STRENGTH_004"

    def __init__(self, **kwargs):
        """Initialize the EfvStrength004 class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTH004_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTH004_CARD1,
                **kwargs,
            ),
        ]
    @property
    def strid(self) -> typing.Optional[int]:
        """Get or set the Strength model identification. A unique number or label must be used (see Remark Error! Reference source not found. in *EFV_MAT)...
        """ # nopep8
        return self._cards[0].get_value("strid")

    @strid.setter
    def strid(self, value: int) -> None:
        """Set the strid property."""
        self._cards[0].set_value("strid", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the Shear modulus
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        """Set the g property."""
        self._cards[0].set_value("g", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Initial yield stress, A. See Remark 1.
        """ # nopep8
        return self._cards[0].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[0].set_value("a", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Hardening constant, B. See Remark 1.
        """ # nopep8
        return self._cards[0].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[0].set_value("b", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Hardening exponent, n. See Remark 1
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[0].set_value("n", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Strain rate constant, C. See Remark 1.
        """ # nopep8
        return self._cards[0].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[0].set_value("c", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Thermal softening exponent, m. See Remark 1
        """ # nopep8
        return self._cards[0].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        """Set the m property."""
        self._cards[0].set_value("m", value)

    @property
    def tm(self) -> typing.Optional[float]:
        """Get or set the Melting temperature, T_melt . See Remark 1.
        """ # nopep8
        return self._cards[0].get_value("tm")

    @tm.setter
    def tm(self, value: float) -> None:
        """Set the tm property."""
        self._cards[0].set_value("tm", value)

    @property
    def epsrtref(self) -> float:
        """Get or set the Reference strain rate in units of [time]-1. See Remark 1.
        """ # nopep8
        return self._cards[1].get_value("epsrtref")

    @epsrtref.setter
    def epsrtref(self, value: float) -> None:
        """Set the epsrtref property."""
        self._cards[1].set_value("epsrtref", value)

    @property
    def iepsrtc(self) -> int:
        """Get or set the Strain rate correction flag (see Remark 2):
        EQ.1: No correction
        EQ.2 First order (default)
        EQ.3: Implicit
        """ # nopep8
        return self._cards[1].get_value("iepsrtc")

    @iepsrtc.setter
    def iepsrtc(self, value: int) -> None:
        """Set the iepsrtc property."""
        if value not in [2, 1, 3, None]:
            raise Exception("""iepsrtc must be `None` or one of {2,1,3}.""")
        self._cards[1].set_value("iepsrtc", value)

