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

"""Module providing the CeseEosCavHomogEquilib class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CESEEOSCAVHOMOGEQUILIB_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("p_vap", float, 10, 10, 0.8),
    FieldSchema("p_liq", float, 20, 10, 880.0),
    FieldSchema("a_vap", float, 30, 10, 334.0),
    FieldSchema("a_liq", float, 40, 10, 1386.0),
    FieldSchema("u_vap", float, 50, 10, 1.435e-05),
    FieldSchema("u_liq", float, 60, 10, 0.0001586),
    FieldSchema("p_sat_vap", float, 70, 10, 12000.0),
)

class CeseEosCavHomogEquilib(KeywordBase):
    """DYNA CESE_EOS_CAV_HOMOG_EQUILIB keyword"""

    keyword = "CESE"
    subkeyword = "EOS_CAV_HOMOG_EQUILIB"

    def __init__(self, **kwargs):
        """Initialize the CeseEosCavHomogEquilib class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CESEEOSCAVHOMOGEQUILIB_CARD0,
                **kwargs,
            ),        ]
    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state identification.
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def p_vap(self) -> float:
        """Get or set the Density of the saturated vapor.
        """ # nopep8
        return self._cards[0].get_value("p_vap")

    @p_vap.setter
    def p_vap(self, value: float) -> None:
        """Set the p_vap property."""
        self._cards[0].set_value("p_vap", value)

    @property
    def p_liq(self) -> float:
        """Get or set the Density of the saturated liquid.
        """ # nopep8
        return self._cards[0].get_value("p_liq")

    @p_liq.setter
    def p_liq(self, value: float) -> None:
        """Set the p_liq property."""
        self._cards[0].set_value("p_liq", value)

    @property
    def a_vap(self) -> float:
        """Get or set the Sound speed of the saturated vapor.
        """ # nopep8
        return self._cards[0].get_value("a_vap")

    @a_vap.setter
    def a_vap(self, value: float) -> None:
        """Set the a_vap property."""
        self._cards[0].set_value("a_vap", value)

    @property
    def a_liq(self) -> float:
        """Get or set the Sound speed of the saturated liquid.
        """ # nopep8
        return self._cards[0].get_value("a_liq")

    @a_liq.setter
    def a_liq(self, value: float) -> None:
        """Set the a_liq property."""
        self._cards[0].set_value("a_liq", value)

    @property
    def u_vap(self) -> float:
        """Get or set the Dynamic viscosity of the vapor.
        """ # nopep8
        return self._cards[0].get_value("u_vap")

    @u_vap.setter
    def u_vap(self, value: float) -> None:
        """Set the u_vap property."""
        self._cards[0].set_value("u_vap", value)

    @property
    def u_liq(self) -> float:
        """Get or set the Dynamic viscosity of the liquid.
        """ # nopep8
        return self._cards[0].get_value("u_liq")

    @u_liq.setter
    def u_liq(self, value: float) -> None:
        """Set the u_liq property."""
        self._cards[0].set_value("u_liq", value)

    @property
    def p_sat_vap(self) -> float:
        """Get or set the Pressure of the saturated vapor.
        """ # nopep8
        return self._cards[0].get_value("p_sat_vap")

    @p_sat_vap.setter
    def p_sat_vap(self, value: float) -> None:
        """Set the p_sat_vap property."""
        self._cards[0].set_value("p_sat_vap", value)

