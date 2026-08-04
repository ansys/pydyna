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

"""Module providing the CeseMat002 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CESEMAT002_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("mu0", float, 10, 10, 0.001716),
    FieldSchema("smu", float, 10, 10, 111.0),
    FieldSchema("k0", float, 10, 10, 0.0241),
    FieldSchema("sk", float, 10, 10, 194.0),
    FieldSchema("t0", float, 10, 10, 273.0),
)

class CeseMat002(KeywordBase):
    """DYNA CESE_MAT_002 keyword"""

    keyword = "CESE"
    subkeyword = "MAT_002"

    def __init__(self, **kwargs):
        """Initialize the CeseMat002 class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CESEMAT002_CARD0,
                **kwargs,
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identifier
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def mu0(self) -> float:
        """Get or set the Two coefficients appearing in the equation derived by combining Sutherland's formula with the power law for dilute gases:
        u / u_0 = (T / T_0) ** (3?2)  (T_0 + S_u) / (T + S_u)   .
        u_0 is a reference value,and S_u is an effective temperature called the Sutherland constant, which is characteristic of the gas.For air at moderate temperatures,
        u_0 = 1.716x10 ** (-5)  Ns ?m ** 2 ,S_u = 111 K
        """ # nopep8
        return self._cards[0].get_value("mu0")

    @mu0.setter
    def mu0(self, value: float) -> None:
        """Set the mu0 property."""
        self._cards[0].set_value("mu0", value)

    @property
    def smu(self) -> float:
        """Get or set the Two coefficients appearing in the equation derived by combining Sutherland's formula with the power law for dilute gases:
        u / u_0 = (T / T_0) ** (3?2)  (T_0 + S_u) / (T + S_u)   .
        u_0 is a reference value,and S_u is an effective temperature called the Sutherland constant, which is characteristic of the gas.For air at moderate temperatures,
        u_0 = 1.716x10 ** (-5)  Ns ?m ** 2 ,S_u = 111 K
        """ # nopep8
        return self._cards[0].get_value("smu")

    @smu.setter
    def smu(self, value: float) -> None:
        """Set the smu property."""
        self._cards[0].set_value("smu", value)

    @property
    def k0(self) -> float:
        """Get or set the Two coefficients appearing in the equation derived by combining Sutherland's formula with the power law for dilute gases:
        k / k_0 = (T / T_0) ** (3?2)  (T_0 + S_k) / (T + S_k)   .
        Here k is the thermal conductivity, k_0 is a reference value,and S_k is the Sutherland constant, which is characteristic of the gas.For air at moderate temperatures,
        k_0 = 0.0241  W ?m ,S_k = 194 K
        """ # nopep8
        return self._cards[0].get_value("k0")

    @k0.setter
    def k0(self, value: float) -> None:
        """Set the k0 property."""
        self._cards[0].set_value("k0", value)

    @property
    def sk(self) -> float:
        """Get or set the Two coefficients appearing in the equation derived by combining Sutherland's formula with the power law for dilute gases:
        k / k_0 = (T / T_0) ** (3?2)  (T_0 + S_k) / (T + S_k)   .
        Here k is the thermal conductivity, k_0 is a reference value,and S_k is the Sutherland constant, which is characteristic of the gas.For air at moderate temperatures,
        k_0 = 0.0241  W ?m ,S_k = 194 K
        """ # nopep8
        return self._cards[0].get_value("sk")

    @sk.setter
    def sk(self, value: float) -> None:
        """Set the sk property."""
        self._cards[0].set_value("sk", value)

    @property
    def t0(self) -> float:
        """Get or set the Reference temperature, T_0. The default value (273.0) is for air, in degrees K.
        """ # nopep8
        return self._cards[0].get_value("t0")

    @t0.setter
    def t0(self, value: float) -> None:
        """Set the t0 property."""
        self._cards[0].set_value("t0", value)

