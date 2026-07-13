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

"""Module providing the EosAeratedWater class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EOSAERATEDWATER_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("vfair", float, 10, 10, None),
    FieldSchema("roair", float, 20, 10, None),
    FieldSchema("bulkw", float, 30, 10, None),
    FieldSchema("p0", float, 40, 10, None),
    FieldSchema("pmax", float, 50, 10, None),
)

_EOSAERATEDWATER_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class EosAeratedWater(KeywordBase):
    """DYNA EOS_AERATED_WATER keyword"""

    keyword = "EOS"
    subkeyword = "AERATED_WATER"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the EosAeratedWater class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EOSAERATEDWATER_CARD0,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = EosAeratedWater._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _EOSAERATEDWATER_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state ID. A unique number or label must be specified (see *PART).
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def vfair(self) -> typing.Optional[float]:
        """Get or set the Volume fraction of air in the air-water mixture. A tiny amount say 1.e-4 could ensure pressure is positive under extreme expansion
        """ # nopep8
        return self._cards[0].get_value("vfair")

    @vfair.setter
    def vfair(self, value: float) -> None:
        """Set the vfair property."""
        self._cards[0].set_value("vfair", value)

    @property
    def roair(self) -> typing.Optional[float]:
        """Get or set the Initial air density. It is the air density in the volume occupied by air; not air density in the air-water mixture.
        """ # nopep8
        return self._cards[0].get_value("roair")

    @roair.setter
    def roair(self, value: float) -> None:
        """Set the roair property."""
        self._cards[0].set_value("roair", value)

    @property
    def bulkw(self) -> typing.Optional[float]:
        """Get or set the Water bulk modulus.
        """ # nopep8
        return self._cards[0].get_value("bulkw")

    @bulkw.setter
    def bulkw(self, value: float) -> None:
        """Set the bulkw property."""
        self._cards[0].set_value("bulkw", value)

    @property
    def p0(self) -> typing.Optional[float]:
        """Get or set the Initial pressure of the air-water mixture
        """ # nopep8
        return self._cards[0].get_value("p0")

    @p0.setter
    def p0(self, value: float) -> None:
        """Set the p0 property."""
        self._cards[0].set_value("p0", value)

    @property
    def pmax(self) -> typing.Optional[float]:
        """Get or set the A rough guess of the maximum pressure value of the air-water mixture during the simulation. Please see Remark 1.
        """ # nopep8
        return self._cards[0].get_value("pmax")

    @pmax.setter
    def pmax(self, value: float) -> None:
        """Set the pmax property."""
        self._cards[0].set_value("pmax", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[1].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

