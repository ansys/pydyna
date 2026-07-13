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

"""Module providing the EfvStrength005 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVSTRENGTH005_CARD0 = (
    FieldSchema("strid", int, 0, 10, None),
    FieldSchema("g", float, 10, 10, None),
    FieldSchema("sigy0", float, 20, 10, None),
    FieldSchema("c", float, 30, 10, None),
    FieldSchema("m", float, 40, 10, None),
    FieldSchema("tm", float, 50, 10, None),
    FieldSchema("epsrtref", float, 60, 10, 1.0),
    FieldSchema("unused", float, 70, 10, None),
)

_EFVSTRENGTH005_CARD1 = (
    FieldSchema("eps1", float, 0, 10, None),
    FieldSchema("eps2", float, 10, 10, None),
    FieldSchema("eps3", float, 20, 10, None),
    FieldSchema("eps4", float, 30, 10, None),
    FieldSchema("eps5", float, 40, 10, None),
    FieldSchema("eps6", float, 50, 10, None),
    FieldSchema("eps7", float, 60, 10, None),
    FieldSchema("eps8", float, 70, 10, None),
)

_EFVSTRENGTH005_CARD2 = (
    FieldSchema("eps9", float, 0, 10, None),
    FieldSchema("eps10", float, 10, 10, None),
)

_EFVSTRENGTH005_CARD3 = (
    FieldSchema("sigy1", float, 0, 10, None),
    FieldSchema("sigy2", float, 10, 10, None),
    FieldSchema("sigy3", float, 20, 10, None),
    FieldSchema("sigy4", float, 30, 10, None),
    FieldSchema("sigy5", float, 40, 10, None),
    FieldSchema("sigy6", float, 50, 10, None),
    FieldSchema("sigy7", float, 60, 10, None),
    FieldSchema("sigy8", float, 70, 10, None),
)

_EFVSTRENGTH005_CARD4 = (
    FieldSchema("sigy9", float, 0, 10, None),
    FieldSchema("sigy10", float, 10, 10, None),
)

class EfvStrength005(KeywordBase):
    """DYNA EFV_STRENGTH_005 keyword"""

    keyword = "EFV"
    subkeyword = "STRENGTH_005"

    def __init__(self, **kwargs):
        """Initialize the EfvStrength005 class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTH005_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTH005_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTH005_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTH005_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTH005_CARD4,
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
    def sigy0(self) -> typing.Optional[float]:
        """Get or set the Initial yield stress (at zero plastic strain).
        """ # nopep8
        return self._cards[0].get_value("sigy0")

    @sigy0.setter
    def sigy0(self, value: float) -> None:
        """Set the sigy0 property."""
        self._cards[0].set_value("sigy0", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Strain rate constant, C. See Remark 1
        """ # nopep8
        return self._cards[0].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[0].set_value("c", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Thermal softening exponent, m
        """ # nopep8
        return self._cards[0].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        """Set the m property."""
        self._cards[0].set_value("m", value)

    @property
    def tm(self) -> typing.Optional[float]:
        """Get or set the Melting temperature, T_melt .
        """ # nopep8
        return self._cards[0].get_value("tm")

    @tm.setter
    def tm(self, value: float) -> None:
        """Set the tm property."""
        self._cards[0].set_value("tm", value)

    @property
    def epsrtref(self) -> float:
        """Get or set the Reference strain rate in units of [time]-1.
        """ # nopep8
        return self._cards[0].get_value("epsrtref")

    @epsrtref.setter
    def epsrtref(self, value: float) -> None:
        """Set the epsrtref property."""
        self._cards[0].set_value("epsrtref", value)

    @property
    def eps1(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values in the yield stress as a function of effective plastic strain curve
        """ # nopep8
        return self._cards[1].get_value("eps1")

    @eps1.setter
    def eps1(self, value: float) -> None:
        """Set the eps1 property."""
        self._cards[1].set_value("eps1", value)

    @property
    def eps2(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values in the yield stress as a function of effective plastic strain curve
        """ # nopep8
        return self._cards[1].get_value("eps2")

    @eps2.setter
    def eps2(self, value: float) -> None:
        """Set the eps2 property."""
        self._cards[1].set_value("eps2", value)

    @property
    def eps3(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values in the yield stress as a function of effective plastic strain curve
        """ # nopep8
        return self._cards[1].get_value("eps3")

    @eps3.setter
    def eps3(self, value: float) -> None:
        """Set the eps3 property."""
        self._cards[1].set_value("eps3", value)

    @property
    def eps4(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values in the yield stress as a function of effective plastic strain curve
        """ # nopep8
        return self._cards[1].get_value("eps4")

    @eps4.setter
    def eps4(self, value: float) -> None:
        """Set the eps4 property."""
        self._cards[1].set_value("eps4", value)

    @property
    def eps5(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values in the yield stress as a function of effective plastic strain curve
        """ # nopep8
        return self._cards[1].get_value("eps5")

    @eps5.setter
    def eps5(self, value: float) -> None:
        """Set the eps5 property."""
        self._cards[1].set_value("eps5", value)

    @property
    def eps6(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values in the yield stress as a function of effective plastic strain curve
        """ # nopep8
        return self._cards[1].get_value("eps6")

    @eps6.setter
    def eps6(self, value: float) -> None:
        """Set the eps6 property."""
        self._cards[1].set_value("eps6", value)

    @property
    def eps7(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values in the yield stress as a function of effective plastic strain curve
        """ # nopep8
        return self._cards[1].get_value("eps7")

    @eps7.setter
    def eps7(self, value: float) -> None:
        """Set the eps7 property."""
        self._cards[1].set_value("eps7", value)

    @property
    def eps8(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values in the yield stress as a function of effective plastic strain curve
        """ # nopep8
        return self._cards[1].get_value("eps8")

    @eps8.setter
    def eps8(self, value: float) -> None:
        """Set the eps8 property."""
        self._cards[1].set_value("eps8", value)

    @property
    def eps9(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values in the yield stress as a function of effective plastic strain curve
        """ # nopep8
        return self._cards[2].get_value("eps9")

    @eps9.setter
    def eps9(self, value: float) -> None:
        """Set the eps9 property."""
        self._cards[2].set_value("eps9", value)

    @property
    def eps10(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values in the yield stress as a function of effective plastic strain curve
        """ # nopep8
        return self._cards[2].get_value("eps10")

    @eps10.setter
    def eps10(self, value: float) -> None:
        """Set the eps10 property."""
        self._cards[2].set_value("eps10", value)

    @property
    def sigy1(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of effective plastic strain curve
        """ # nopep8
        return self._cards[3].get_value("sigy1")

    @sigy1.setter
    def sigy1(self, value: float) -> None:
        """Set the sigy1 property."""
        self._cards[3].set_value("sigy1", value)

    @property
    def sigy2(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of effective plastic strain curve
        """ # nopep8
        return self._cards[3].get_value("sigy2")

    @sigy2.setter
    def sigy2(self, value: float) -> None:
        """Set the sigy2 property."""
        self._cards[3].set_value("sigy2", value)

    @property
    def sigy3(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of effective plastic strain curve
        """ # nopep8
        return self._cards[3].get_value("sigy3")

    @sigy3.setter
    def sigy3(self, value: float) -> None:
        """Set the sigy3 property."""
        self._cards[3].set_value("sigy3", value)

    @property
    def sigy4(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of effective plastic strain curve
        """ # nopep8
        return self._cards[3].get_value("sigy4")

    @sigy4.setter
    def sigy4(self, value: float) -> None:
        """Set the sigy4 property."""
        self._cards[3].set_value("sigy4", value)

    @property
    def sigy5(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of effective plastic strain curve
        """ # nopep8
        return self._cards[3].get_value("sigy5")

    @sigy5.setter
    def sigy5(self, value: float) -> None:
        """Set the sigy5 property."""
        self._cards[3].set_value("sigy5", value)

    @property
    def sigy6(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of effective plastic strain curve
        """ # nopep8
        return self._cards[3].get_value("sigy6")

    @sigy6.setter
    def sigy6(self, value: float) -> None:
        """Set the sigy6 property."""
        self._cards[3].set_value("sigy6", value)

    @property
    def sigy7(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of effective plastic strain curve
        """ # nopep8
        return self._cards[3].get_value("sigy7")

    @sigy7.setter
    def sigy7(self, value: float) -> None:
        """Set the sigy7 property."""
        self._cards[3].set_value("sigy7", value)

    @property
    def sigy8(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of effective plastic strain curve
        """ # nopep8
        return self._cards[3].get_value("sigy8")

    @sigy8.setter
    def sigy8(self, value: float) -> None:
        """Set the sigy8 property."""
        self._cards[3].set_value("sigy8", value)

    @property
    def sigy9(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of effective plastic strain curve
        """ # nopep8
        return self._cards[4].get_value("sigy9")

    @sigy9.setter
    def sigy9(self, value: float) -> None:
        """Set the sigy9 property."""
        self._cards[4].set_value("sigy9", value)

    @property
    def sigy10(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of effective plastic strain curve
        """ # nopep8
        return self._cards[4].get_value("sigy10")

    @sigy10.setter
    def sigy10(self, value: float) -> None:
        """Set the sigy10 property."""
        self._cards[4].set_value("sigy10", value)

