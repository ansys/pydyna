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

"""Module providing the EosMieGruneisen class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EOSMIEGRUNEISEN_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("gamma", float, 10, 10, None),
    FieldSchema("a1", float, 20, 10, None),
    FieldSchema("a2", float, 30, 10, None),
    FieldSchema("a3", float, 40, 10, None),
    FieldSchema("pel", float, 50, 10, None),
    FieldSchema("pco", float, 60, 10, None),
    FieldSchema("n", float, 70, 10, None),
)

_EOSMIEGRUNEISEN_CARD1 = (
    FieldSchema("alpha0", float, 0, 10, None),
    FieldSchema("e0", float, 10, 10, None),
    FieldSchema("v0", float, 20, 10, None),
)

class EosMieGruneisen(KeywordBase):
    """DYNA EOS_MIE_GRUNEISEN keyword"""

    keyword = "EOS"
    subkeyword = "MIE_GRUNEISEN"

    def __init__(self, **kwargs):
        """Initialize the EosMieGruneisen class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EOSMIEGRUNEISEN_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EOSMIEGRUNEISEN_CARD1,
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
    def gamma(self) -> typing.Optional[float]:
        """Get or set the Gruneisen gamma.
        """ # nopep8
        return self._cards[0].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        """Set the gamma property."""
        self._cards[0].set_value("gamma", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Hugoniot polynomial coefficient
        """ # nopep8
        return self._cards[0].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[0].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Hugoniot polynomial coefficient.
        """ # nopep8
        return self._cards[0].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[0].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Hugoniot polynomial coefficient.
        """ # nopep8
        return self._cards[0].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[0].set_value("a3", value)

    @property
    def pel(self) -> typing.Optional[float]:
        """Get or set the Crush pressure.
        """ # nopep8
        return self._cards[0].get_value("pel")

    @pel.setter
    def pel(self, value: float) -> None:
        """Set the pel property."""
        self._cards[0].set_value("pel", value)

    @property
    def pco(self) -> typing.Optional[float]:
        """Get or set the Compaction pressure.
        """ # nopep8
        return self._cards[0].get_value("pco")

    @pco.setter
    def pco(self, value: float) -> None:
        """Set the pco property."""
        self._cards[0].set_value("pco", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Porosity exponent.
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[0].set_value("n", value)

    @property
    def alpha0(self) -> typing.Optional[float]:
        """Get or set the Initial porosity.
        """ # nopep8
        return self._cards[1].get_value("alpha0")

    @alpha0.setter
    def alpha0(self, value: float) -> None:
        """Set the alpha0 property."""
        self._cards[1].set_value("alpha0", value)

    @property
    def e0(self) -> typing.Optional[float]:
        """Get or set the Initial internal energy.
        """ # nopep8
        return self._cards[1].get_value("e0")

    @e0.setter
    def e0(self, value: float) -> None:
        """Set the e0 property."""
        self._cards[1].set_value("e0", value)

    @property
    def v0(self) -> typing.Optional[float]:
        """Get or set the Initial relative volume.
        """ # nopep8
        return self._cards[1].get_value("v0")

    @v0.setter
    def v0(self, value: float) -> None:
        """Set the v0 property."""
        self._cards[1].set_value("v0", value)

