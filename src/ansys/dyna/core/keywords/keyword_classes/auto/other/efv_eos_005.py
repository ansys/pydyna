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

"""Module providing the EfvEos005 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVEOS005_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("a1", float, 10, 10, None),
    FieldSchema("a2", float, 20, 10, None),
    FieldSchema("a3", float, 30, 10, None),
    FieldSchema("gamma0", float, 40, 10, None),
    FieldSchema("h", float, 50, 10, None),
    FieldSchema("es", float, 60, 10, None),
)

_EFVEOS005_CARD1 = (
    FieldSchema("t1", float, 0, 10, None),
    FieldSchema("t2", float, 10, 10, None),
    FieldSchema("tref", float, 20, 10, 0.0),
    FieldSchema("cp", float, 30, 10, 0.0),
    FieldSchema("tc", float, 40, 10, 0.0),
    FieldSchema("form", int, 50, 10, 0),
)

class EfvEos005(KeywordBase):
    """DYNA EFV_EOS_005 keyword"""

    keyword = "EFV"
    subkeyword = "EOS_005"

    def __init__(self, **kwargs):
        """Initialize the EfvEos005 class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVEOS005_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOS005_CARD1,
                **kwargs,
            ),
        ]
    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state identification. A unique number or label must be used.(see Remark Error! Reference source not found. in *EFV_MAT).
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Parameter A1
        """ # nopep8
        return self._cards[0].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[0].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Parameter A2
        """ # nopep8
        return self._cards[0].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[0].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Parameter A3
        """ # nopep8
        return self._cards[0].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[0].set_value("a3", value)

    @property
    def gamma0(self) -> typing.Optional[float]:
        """Get or set the Gruneisen coefficient,
        """ # nopep8
        return self._cards[0].get_value("gamma0")

    @gamma0.setter
    def gamma0(self, value: float) -> None:
        """Set the gamma0 property."""
        self._cards[0].set_value("gamma0", value)

    @property
    def h(self) -> typing.Optional[float]:
        """Get or set the Expansion coefficient, H
        """ # nopep8
        return self._cards[0].get_value("h")

    @h.setter
    def h(self, value: float) -> None:
        """Set the h property."""
        self._cards[0].set_value("h", value)

    @property
    def es(self) -> typing.Optional[float]:
        """Get or set the Sublimation energy, e_s
        """ # nopep8
        return self._cards[0].get_value("es")

    @es.setter
    def es(self, value: float) -> None:
        """Set the es property."""
        self._cards[0].set_value("es", value)

    @property
    def t1(self) -> typing.Optional[float]:
        """Get or set the Parameter T_1
        """ # nopep8
        return self._cards[1].get_value("t1")

    @t1.setter
    def t1(self, value: float) -> None:
        """Set the t1 property."""
        self._cards[1].set_value("t1", value)

    @property
    def t2(self) -> typing.Optional[float]:
        """Get or set the Parameter T_2
        """ # nopep8
        return self._cards[1].get_value("t2")

    @t2.setter
    def t2(self, value: float) -> None:
        """Set the t2 property."""
        self._cards[1].set_value("t2", value)

    @property
    def tref(self) -> float:
        """Get or set the Reference temperature
        """ # nopep8
        return self._cards[1].get_value("tref")

    @tref.setter
    def tref(self, value: float) -> None:
        """Set the tref property."""
        self._cards[1].set_value("tref", value)

    @property
    def cp(self) -> float:
        """Get or set the Specific heat
        """ # nopep8
        return self._cards[1].get_value("cp")

    @cp.setter
    def cp(self, value: float) -> None:
        """Set the cp property."""
        self._cards[1].set_value("cp", value)

    @property
    def tc(self) -> float:
        """Get or set the Thermal conductivity
        """ # nopep8
        return self._cards[1].get_value("tc")

    @tc.setter
    def tc(self, value: float) -> None:
        """Set the tc property."""
        self._cards[1].set_value("tc", value)

    @property
    def form(self) -> int:
        """Get or set the Puff formulation:
        EQ.0: Original
        EQ.1: New(beta)
        """ # nopep8
        return self._cards[1].get_value("form")

    @form.setter
    def form(self, value: int) -> None:
        """Set the form property."""
        if value not in [0, 1, None]:
            raise Exception("""form must be `None` or one of {0,1}.""")
        self._cards[1].set_value("form", value)

