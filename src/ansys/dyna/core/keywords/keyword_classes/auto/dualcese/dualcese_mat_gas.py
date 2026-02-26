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

"""Module providing the DualceseMatGas class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DUALCESEMATGAS_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("c1", float, 10, 10, 1.458e-06),
    FieldSchema("c2", float, 20, 10, 110.4),
    FieldSchema("prnd", float, 30, 10, 0.72),
)

class DualceseMatGas(KeywordBase):
    """DYNA DUALCESE_MAT_GAS keyword"""

    keyword = "DUALCESE"
    subkeyword = "MAT_GAS"

    def __init__(self, **kwargs):
        """Initialize the DualceseMatGas class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEMATGAS_CARD0,
                **kwargs,
            ),        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material ID
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def c1(self) -> float:
        """Get or set the Two coefficients in the Sutherland’s formula for viscosity
        """ # nopep8
        return self._cards[0].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        """Set the c1 property."""
        self._cards[0].set_value("c1", value)

    @property
    def c2(self) -> float:
        """Get or set the Two coefficients in the Sutherland’s formula for viscosity
        """ # nopep8
        return self._cards[0].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        """Set the c2 property."""
        self._cards[0].set_value("c2", value)

    @property
    def prnd(self) -> float:
        """Get or set the The Prandtl Number (used to determine the coefficient of thermal conductivity). It is approximately constant for most gases. For air at standard conditions PRND = 0.72
        """ # nopep8
        return self._cards[0].get_value("prnd")

    @prnd.setter
    def prnd(self, value: float) -> None:
        """Set the prnd property."""
        self._cards[0].set_value("prnd", value)

