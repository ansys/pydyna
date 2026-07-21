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

"""Module providing the DualceseEosCavHomogEquilib class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DUALCESEEOSCAVHOMOGEQUILIB_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("rhova", float, 10, 10, 0.8),
    FieldSchema("rholiq", float, 20, 10, 880.0),
    FieldSchema("avap", float, 30, 10, 334.0),
    FieldSchema("aliq", float, 40, 10, 1386.0),
    FieldSchema("muvap", float, 50, 10, 1.435e-05),
    FieldSchema("muliq", float, 60, 10, 0.0001586),
    FieldSchema("psatvap", float, 70, 10, 12000.0),
)

class DualceseEosCavHomogEquilib(KeywordBase):
    """DYNA DUALCESE_EOS_CAV_HOMOG_EQUILIB keyword"""

    keyword = "DUALCESE"
    subkeyword = "EOS_CAV_HOMOG_EQUILIB"

    def __init__(self, **kwargs):
        """Initialize the DualceseEosCavHomogEquilib class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEEOSCAVHOMOGEQUILIB_CARD0,
                **kwargs,
            ),
        ]
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
    def rhova(self) -> float:
        """Get or set the density of the saturated vapor
        """ # nopep8
        return self._cards[0].get_value("rhova")

    @rhova.setter
    def rhova(self, value: float) -> None:
        """Set the rhova property."""
        self._cards[0].set_value("rhova", value)

    @property
    def rholiq(self) -> float:
        """Get or set the density of the saturated liquid
        """ # nopep8
        return self._cards[0].get_value("rholiq")

    @rholiq.setter
    def rholiq(self, value: float) -> None:
        """Set the rholiq property."""
        self._cards[0].set_value("rholiq", value)

    @property
    def avap(self) -> float:
        """Get or set the sound speed of the saturated vapor
        """ # nopep8
        return self._cards[0].get_value("avap")

    @avap.setter
    def avap(self, value: float) -> None:
        """Set the avap property."""
        self._cards[0].set_value("avap", value)

    @property
    def aliq(self) -> float:
        """Get or set the sound speed of the saturated liquid
        """ # nopep8
        return self._cards[0].get_value("aliq")

    @aliq.setter
    def aliq(self, value: float) -> None:
        """Set the aliq property."""
        self._cards[0].set_value("aliq", value)

    @property
    def muvap(self) -> float:
        """Get or set the dynamic viscosity of the vapor
        """ # nopep8
        return self._cards[0].get_value("muvap")

    @muvap.setter
    def muvap(self, value: float) -> None:
        """Set the muvap property."""
        self._cards[0].set_value("muvap", value)

    @property
    def muliq(self) -> float:
        """Get or set the dynamic viscosity of the liquid
        """ # nopep8
        return self._cards[0].get_value("muliq")

    @muliq.setter
    def muliq(self, value: float) -> None:
        """Set the muliq property."""
        self._cards[0].set_value("muliq", value)

    @property
    def psatvap(self) -> float:
        """Get or set the pressure of the saturated vapor
        """ # nopep8
        return self._cards[0].get_value("psatvap")

    @psatvap.setter
    def psatvap(self, value: float) -> None:
        """Set the psatvap property."""
        self._cards[0].set_value("psatvap", value)

