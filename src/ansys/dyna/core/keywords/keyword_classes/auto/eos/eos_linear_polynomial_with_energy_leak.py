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

"""Module providing the EosLinearPolynomialWithEnergyLeak class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EOSLINEARPOLYNOMIALWITHENERGYLEAK_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("c0", float, 10, 10, None),
    FieldSchema("c1", float, 20, 10, None),
    FieldSchema("c2", float, 30, 10, None),
    FieldSchema("c3", float, 40, 10, None),
    FieldSchema("c4", float, 50, 10, None),
    FieldSchema("c5", float, 60, 10, None),
    FieldSchema("c6", float, 70, 10, None),
)

_EOSLINEARPOLYNOMIALWITHENERGYLEAK_CARD1 = (
    FieldSchema("e0", float, 0, 10, None),
    FieldSchema("v0", float, 10, 10, None),
    FieldSchema("lcid", int, 20, 10, None),
)

class EosLinearPolynomialWithEnergyLeak(KeywordBase):
    """DYNA EOS_LINEAR_POLYNOMIAL_WITH_ENERGY_LEAK keyword"""

    keyword = "EOS"
    subkeyword = "LINEAR_POLYNOMIAL_WITH_ENERGY_LEAK"

    def __init__(self, **kwargs):
        """Initialize the EosLinearPolynomialWithEnergyLeak class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EOSLINEARPOLYNOMIALWITHENERGYLEAK_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EOSLINEARPOLYNOMIALWITHENERGYLEAK_CARD1,
                **kwargs,
            ),        ]
    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state label.
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def c0(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("c0")

    @c0.setter
    def c0(self, value: float) -> None:
        """Set the c0 property."""
        self._cards[0].set_value("c0", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        """Set the c1 property."""
        self._cards[0].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        """Set the c2 property."""
        self._cards[0].set_value("c2", value)

    @property
    def c3(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("c3")

    @c3.setter
    def c3(self, value: float) -> None:
        """Set the c3 property."""
        self._cards[0].set_value("c3", value)

    @property
    def c4(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("c4")

    @c4.setter
    def c4(self, value: float) -> None:
        """Set the c4 property."""
        self._cards[0].set_value("c4", value)

    @property
    def c5(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("c5")

    @c5.setter
    def c5(self, value: float) -> None:
        """Set the c5 property."""
        self._cards[0].set_value("c5", value)

    @property
    def c6(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("c6")

    @c6.setter
    def c6(self, value: float) -> None:
        """Set the c6 property."""
        self._cards[0].set_value("c6", value)

    @property
    def e0(self) -> typing.Optional[float]:
        """Get or set the Initial internal energy.
        """ # nopep8
        return self._cards[1].get_value("e0")

    @e0.setter
    def e0(self, value: float) -> None:
        """Set the e0 property."""
        self._cards[1].set_value("e0", value)

    @property
    def v0(self) -> typing.Optional[float]:
        """Get or set the Initial relative volume.
        """ # nopep8
        return self._cards[1].get_value("v0")

    @v0.setter
    def v0(self, value: float) -> None:
        """Set the v0 property."""
        self._cards[1].set_value("v0", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID, which can be the ID of *DEFINE_‌CURVE,
        *DEFINE_‌CURVE_‌FUNCTION or *DEFINE_‌FUNCTION, defining the energy deposition rate.
        If an energy leak rate is intended, do not specify a negative ordinate in LCID,
        rather, use the constant(s) in the equation of state, e.g., set C4 to a negative value.
        If *DEFINE_‌FUNCTION is used, the input of the defined function is time.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[1].set_value("lcid", value)

