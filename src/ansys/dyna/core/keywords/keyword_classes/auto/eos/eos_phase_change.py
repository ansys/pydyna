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

"""Module providing the EosPhaseChange class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EOSPHASECHANGE_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("rhol", float, 10, 10, None),
    FieldSchema("rhov", float, 20, 10, None),
    FieldSchema("cl", float, 30, 10, None),
    FieldSchema("cv", float, 40, 10, None),
    FieldSchema("gamal", float, 50, 10, None),
    FieldSchema("pv", float, 60, 10, None),
    FieldSchema("kl", float, 70, 10, None),
)

class EosPhaseChange(KeywordBase):
    """DYNA EOS_PHASE_CHANGE keyword"""

    keyword = "EOS"
    subkeyword = "PHASE_CHANGE"

    def __init__(self, **kwargs):
        """Initialize the EosPhaseChange class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EOSPHASECHANGE_CARD0,
                **kwargs,
            ),        ]
    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state ID, a unique number or label must be specified.
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def rhol(self) -> typing.Optional[float]:
        """Get or set the Density of liquid
        """ # nopep8
        return self._cards[0].get_value("rhol")

    @rhol.setter
    def rhol(self, value: float) -> None:
        """Set the rhol property."""
        self._cards[0].set_value("rhol", value)

    @property
    def rhov(self) -> typing.Optional[float]:
        """Get or set the Density of saturated vapor
        """ # nopep8
        return self._cards[0].get_value("rhov")

    @rhov.setter
    def rhov(self, value: float) -> None:
        """Set the rhov property."""
        self._cards[0].set_value("rhov", value)

    @property
    def cl(self) -> typing.Optional[float]:
        """Get or set the Speed of sound of liquid.
        """ # nopep8
        return self._cards[0].get_value("cl")

    @cl.setter
    def cl(self, value: float) -> None:
        """Set the cl property."""
        self._cards[0].set_value("cl", value)

    @property
    def cv(self) -> typing.Optional[float]:
        """Get or set the Speed of sound of vapor.
        """ # nopep8
        return self._cards[0].get_value("cv")

    @cv.setter
    def cv(self, value: float) -> None:
        """Set the cv property."""
        self._cards[0].set_value("cv", value)

    @property
    def gamal(self) -> typing.Optional[float]:
        """Get or set the Gamma constant of liquid.
        """ # nopep8
        return self._cards[0].get_value("gamal")

    @gamal.setter
    def gamal(self, value: float) -> None:
        """Set the gamal property."""
        self._cards[0].set_value("gamal", value)

    @property
    def pv(self) -> typing.Optional[float]:
        """Get or set the Pressure of vapor.
        """ # nopep8
        return self._cards[0].get_value("pv")

    @pv.setter
    def pv(self, value: float) -> None:
        """Set the pv property."""
        self._cards[0].set_value("pv", value)

    @property
    def kl(self) -> typing.Optional[float]:
        """Get or set the Bulk compressibility of liquid.
        """ # nopep8
        return self._cards[0].get_value("kl")

    @kl.setter
    def kl(self, value: float) -> None:
        """Set the kl property."""
        self._cards[0].set_value("kl", value)

