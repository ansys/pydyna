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

"""Module providing the DualceseEosIdealGas class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DUALCESEEOSIDEALGAS_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("cv", float, 10, 10, 717.5),
    FieldSchema("cp", float, 20, 10, 1004.5),
    FieldSchema("e0", float, 30, 10, 0.0),
)

class DualceseEosIdealGas(KeywordBase):
    """DYNA DUALCESE_EOS_IDEAL_GAS keyword"""

    keyword = "DUALCESE"
    subkeyword = "EOS_IDEAL_GAS"

    def __init__(self, **kwargs):
        """Initialize the DualceseEosIdealGas class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEEOSIDEALGAS_CARD0,
                **kwargs,
            ),        ]
    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state identifier
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def cv(self) -> float:
        """Get or set the Specific heat at constant volume
        """ # nopep8
        return self._cards[0].get_value("cv")

    @cv.setter
    def cv(self, value: float) -> None:
        """Set the cv property."""
        self._cards[0].set_value("cv", value)

    @property
    def cp(self) -> float:
        """Get or set the Specific heat at constant pressure
        """ # nopep8
        return self._cards[0].get_value("cp")

    @cp.setter
    def cp(self, value: float) -> None:
        """Set the cp property."""
        self._cards[0].set_value("cp", value)

    @property
    def e0(self) -> float:
        """Get or set the Represents the heat of detonation released during the reactions, or the constant rate of afterburn energy added (E0 = 0.0 is the default), e_0
        """ # nopep8
        return self._cards[0].get_value("e0")

    @e0.setter
    def e0(self, value: float) -> None:
        """Set the e0 property."""
        self._cards[0].set_value("e0", value)

