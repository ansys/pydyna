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

"""Module providing the EfvStrengthZerilliArmstrong class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVSTRENGTHZERILLIARMSTRONG_CARD0 = (
    FieldSchema("strid", int, 0, 10, None),
    FieldSchema("g", float, 10, 10, None),
    FieldSchema("sigy", float, 20, 10, None),
    FieldSchema("c1", float, 30, 10, None),
    FieldSchema("c2", float, 40, 10, None),
    FieldSchema("c3", float, 50, 10, None),
    FieldSchema("c4", float, 60, 10, None),
    FieldSchema("c5", float, 70, 10, None),
)

_EFVSTRENGTHZERILLIARMSTRONG_CARD1 = (
    FieldSchema("n", float, 0, 10, None),
    FieldSchema("epsrtref", float, 10, 10, None),
    FieldSchema("iepsrtc", int, 20, 10, 0),
)

class EfvStrengthZerilliArmstrong(KeywordBase):
    """DYNA EFV_STRENGTH_ZERILLI_ARMSTRONG keyword"""

    keyword = "EFV"
    subkeyword = "STRENGTH_ZERILLI_ARMSTRONG"

    def __init__(self, **kwargs):
        """Initialize the EfvStrengthZerilliArmstrong class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTHZERILLIARMSTRONG_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTHZERILLIARMSTRONG_CARD1,
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
    def sigy(self) -> typing.Optional[float]:
        """Get or set the Initial yield stress, o_(y,0). See Remark 1.
        """ # nopep8
        return self._cards[0].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        """Set the sigy property."""
        self._cards[0].set_value("sigy", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the First hardening constant, C_1.
        """ # nopep8
        return self._cards[0].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        """Set the c1 property."""
        self._cards[0].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the Second hardening constant, C_2
        """ # nopep8
        return self._cards[0].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        """Set the c2 property."""
        self._cards[0].set_value("c2", value)

    @property
    def c3(self) -> typing.Optional[float]:
        """Get or set the Third hardening constant, C_3
        """ # nopep8
        return self._cards[0].get_value("c3")

    @c3.setter
    def c3(self, value: float) -> None:
        """Set the c3 property."""
        self._cards[0].set_value("c3", value)

    @property
    def c4(self) -> typing.Optional[float]:
        """Get or set the Fourth hardening constant, C_4
        """ # nopep8
        return self._cards[0].get_value("c4")

    @c4.setter
    def c4(self, value: float) -> None:
        """Set the c4 property."""
        self._cards[0].set_value("c4", value)

    @property
    def c5(self) -> typing.Optional[float]:
        """Get or set the Fifth hardening constant, C_5
        """ # nopep8
        return self._cards[0].get_value("c5")

    @c5.setter
    def c5(self, value: float) -> None:
        """Set the c5 property."""
        self._cards[0].set_value("c5", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Hardening constant, n
        """ # nopep8
        return self._cards[1].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[1].set_value("n", value)

    @property
    def epsrtref(self) -> typing.Optional[float]:
        """Get or set the Reference strain rate in units of [time]-1
        """ # nopep8
        return self._cards[1].get_value("epsrtref")

    @epsrtref.setter
    def epsrtref(self, value: float) -> None:
        """Set the epsrtref property."""
        self._cards[1].set_value("epsrtref", value)

    @property
    def iepsrtc(self) -> int:
        """Get or set the Strain rate correction flag (see Remark 2):
        EQ.0: No correction
        EQ.1: Implicit
        """ # nopep8
        return self._cards[1].get_value("iepsrtc")

    @iepsrtc.setter
    def iepsrtc(self, value: int) -> None:
        """Set the iepsrtc property."""
        if value not in [0, 1, None]:
            raise Exception("""iepsrtc must be `None` or one of {0,1}.""")
        self._cards[1].set_value("iepsrtc", value)

