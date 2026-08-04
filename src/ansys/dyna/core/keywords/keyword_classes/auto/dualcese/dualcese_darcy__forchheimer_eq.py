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

"""Module providing the DualceseDarcy_ForchheimerEq class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DUALCESEDARCY_FORCHHEIMEREQ_CARD0 = (
    FieldSchema("eqid", int, 0, 10, None),
    FieldSchema("kp", float, 10, 10, None),
    FieldSchema("beta", float, 20, 10, None),
    FieldSchema("lc_kp", int, 30, 10, None),
    FieldSchema("lc_beta", int, 40, 10, None),
)

class DualceseDarcy_ForchheimerEq(KeywordBase):
    """DYNA DUALCESE_DARCY-FORCHHEIMER_EQ keyword"""

    keyword = "DUALCESE"
    subkeyword = "DARCY-FORCHHEIMER_EQ"

    def __init__(self, **kwargs):
        """Initialize the DualceseDarcy_ForchheimerEq class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEDARCY_FORCHHEIMEREQ_CARD0,
                **kwargs,
            ),
        ]
    @property
    def eqid(self) -> typing.Optional[int]:
        """Get or set the Equation ID of the porous medium model (see *DUALCESE_POROUS_SPECIFY_JUMP
        """ # nopep8
        return self._cards[0].get_value("eqid")

    @eqid.setter
    def eqid(self, value: int) -> None:
        """Set the eqid property."""
        self._cards[0].set_value("eqid", value)

    @property
    def kp(self) -> typing.Optional[float]:
        """Get or set the Permeability of the porous medium
        """ # nopep8
        return self._cards[0].get_value("kp")

    @kp.setter
    def kp(self, value: float) -> None:
        """Set the kp property."""
        self._cards[0].set_value("kp", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Inertial resistance coefficient of the porous medium
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[0].set_value("beta", value)

    @property
    def lc_kp(self) -> typing.Optional[int]:
        """Get or set the Load curve to modify the permeability (KP) of the porous medium as a function of time. If used, KP is a scale factor for the curve.
        """ # nopep8
        return self._cards[0].get_value("lc_kp")

    @lc_kp.setter
    def lc_kp(self, value: int) -> None:
        """Set the lc_kp property."""
        self._cards[0].set_value("lc_kp", value)

    @property
    def lc_beta(self) -> typing.Optional[int]:
        """Get or set the Load curve to modify the inertial resistance coefficient (BETA) as a function of time. If used, BETA is a scale factor for the curve.
        """ # nopep8
        return self._cards[0].get_value("lc_beta")

    @lc_beta.setter
    def lc_beta(self, value: int) -> None:
        """Set the lc_beta property."""
        self._cards[0].set_value("lc_beta", value)

