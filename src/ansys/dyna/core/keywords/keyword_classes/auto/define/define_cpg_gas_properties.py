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

"""Module providing the DefineCpgGasProperties class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINECPGGASPROPERTIES_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("xmm", float, 10, 10, None),
    FieldSchema("cp0", float, 20, 10, 0.0),
    FieldSchema("cp1", float, 30, 10, 0.0),
    FieldSchema("cp2", float, 40, 10, 0.0),
    FieldSchema("cp3", float, 50, 10, 0.0),
    FieldSchema("cp4", float, 60, 10, 0.0),
)

_DEFINECPGGASPROPERTIES_CARD1 = (
    FieldSchema("mu", float, 0, 10, 0.0),
)

_DEFINECPGGASPROPERTIES_CARD2 = (
    FieldSchema("tc", int, 0, 10, 0),
)

_DEFINECPGGASPROPERTIES_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineCpgGasProperties(KeywordBase):
    """DYNA DEFINE_CPG_GAS_PROPERTIES keyword"""

    keyword = "DEFINE"
    subkeyword = "CPG_GAS_PROPERTIES"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineCpgGasProperties class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINECPGGASPROPERTIES_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINECPGGASPROPERTIES_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINECPGGASPROPERTIES_CARD2,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineCpgGasProperties._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINECPGGASPROPERTIES_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Unique ID for this card
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def xmm(self) -> typing.Optional[float]:
        """Get or set the Molar mass
        """ # nopep8
        return self._cards[0].get_value("xmm")

    @xmm.setter
    def xmm(self, value: float) -> None:
        """Set the xmm property."""
        self._cards[0].set_value("xmm", value)

    @property
    def cp0(self) -> float:
        """Get or set the Coefficients of temperature dependent specific heat with constant pressure
        """ # nopep8
        return self._cards[0].get_value("cp0")

    @cp0.setter
    def cp0(self, value: float) -> None:
        """Set the cp0 property."""
        self._cards[0].set_value("cp0", value)

    @property
    def cp1(self) -> float:
        """Get or set the Coefficients of temperature dependent specific heat with constant pressure
        """ # nopep8
        return self._cards[0].get_value("cp1")

    @cp1.setter
    def cp1(self, value: float) -> None:
        """Set the cp1 property."""
        self._cards[0].set_value("cp1", value)

    @property
    def cp2(self) -> float:
        """Get or set the Coefficients of temperature dependent specific heat with constant pressure
        """ # nopep8
        return self._cards[0].get_value("cp2")

    @cp2.setter
    def cp2(self, value: float) -> None:
        """Set the cp2 property."""
        self._cards[0].set_value("cp2", value)

    @property
    def cp3(self) -> float:
        """Get or set the Coefficients of temperature dependent specific heat with constant pressure
        """ # nopep8
        return self._cards[0].get_value("cp3")

    @cp3.setter
    def cp3(self, value: float) -> None:
        """Set the cp3 property."""
        self._cards[0].set_value("cp3", value)

    @property
    def cp4(self) -> float:
        """Get or set the Coefficients of temperature dependent specific heat with constant pressure
        """ # nopep8
        return self._cards[0].get_value("cp4")

    @cp4.setter
    def cp4(self, value: float) -> None:
        """Set the cp4 property."""
        self._cards[0].set_value("cp4", value)

    @property
    def mu(self) -> float:
        """Get or set the Dynamic viscosity associated with the gas. See Remark 1
        """ # nopep8
        return self._cards[1].get_value("mu")

    @mu.setter
    def mu(self, value: float) -> None:
        """Set the mu property."""
        self._cards[1].set_value("mu", value)

    @property
    def tc(self) -> int:
        """Get or set the Thermal conductivity of the gas. See Remark 2
        """ # nopep8
        return self._cards[2].get_value("tc")

    @tc.setter
    def tc(self, value: int) -> None:
        """Set the tc property."""
        self._cards[2].set_value("tc", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[3].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

