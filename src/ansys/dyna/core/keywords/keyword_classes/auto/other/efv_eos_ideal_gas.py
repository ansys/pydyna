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

"""Module providing the EfvEosIdealGas class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVEOSIDEALGAS_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("gamma", float, 10, 10, None),
    FieldSchema("c", float, 20, 10, None),
    FieldSchema("pshift", float, 30, 10, None),
    FieldSchema("tref", float, 40, 10, 293.0),
    FieldSchema("cv", float, 50, 10, None),
    FieldSchema("tc", float, 60, 10, None),
)

class EfvEosIdealGas(KeywordBase):
    """DYNA EFV_EOS_IDEAL_GAS keyword"""

    keyword = "EFV"
    subkeyword = "EOS_IDEAL_GAS"

    def __init__(self, **kwargs):
        """Initialize the EfvEosIdealGas class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVEOSIDEALGAS_CARD0,
                **kwargs,
            ),
        ]
    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state identification. A unique number or label must be used.(see Remark Error! Reference source not found. in *EFV_MAT).
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def gamma(self) -> typing.Optional[float]:
        """Get or set the Adiabatic exponent, r. See Remark 1.
        """ # nopep8
        return self._cards[0].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        """Set the gamma property."""
        self._cards[0].set_value("gamma", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Adabiatic constant, c. See Remark 1.
        """ # nopep8
        return self._cards[0].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[0].set_value("c", value)

    @property
    def pshift(self) -> typing.Optional[float]:
        """Get or set the Pressure shift, p_shift . See Remark 1
        """ # nopep8
        return self._cards[0].get_value("pshift")

    @pshift.setter
    def pshift(self, value: float) -> None:
        """Set the pshift property."""
        self._cards[0].set_value("pshift", value)

    @property
    def tref(self) -> float:
        """Get or set the Reference temperature
        """ # nopep8
        return self._cards[0].get_value("tref")

    @tref.setter
    def tref(self, value: float) -> None:
        """Set the tref property."""
        self._cards[0].set_value("tref", value)

    @property
    def cv(self) -> typing.Optional[float]:
        """Get or set the Specific heat capacity
        """ # nopep8
        return self._cards[0].get_value("cv")

    @cv.setter
    def cv(self, value: float) -> None:
        """Set the cv property."""
        self._cards[0].set_value("cv", value)

    @property
    def tc(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity
        """ # nopep8
        return self._cards[0].get_value("tc")

    @tc.setter
    def tc(self, value: float) -> None:
        """Set the tc property."""
        self._cards[0].set_value("tc", value)

