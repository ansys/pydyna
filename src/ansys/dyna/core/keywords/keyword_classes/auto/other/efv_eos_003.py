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

"""Module providing the EfvEos003 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVEOS003_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("gamma0", float, 10, 10, None),
    FieldSchema("q2", float, 20, 10, None),
    FieldSchema("tref", float, 30, 10, 293.0),
    FieldSchema("cp", float, 40, 10, None),
    FieldSchema("tc", float, 50, 10, None),
)

_EFVEOS003_CARD1 = (
    FieldSchema("c1", float, 0, 10, None),
    FieldSchema("s1", float, 10, 10, None),
    FieldSchema("c2", float, 20, 10, None),
    FieldSchema("s2", float, 30, 10, None),
    FieldSchema("ve", float, 40, 10, None),
    FieldSchema("vb", float, 50, 10, None),
)

class EfvEos003(KeywordBase):
    """DYNA EFV_EOS_003 keyword"""

    keyword = "EFV"
    subkeyword = "EOS_003"

    def __init__(self, **kwargs):
        """Initialize the EfvEos003 class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVEOS003_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOS003_CARD1,
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
    def gamma0(self) -> typing.Optional[float]:
        """Get or set the Gruneisen coefficient,
        """ # nopep8
        return self._cards[0].get_value("gamma0")

    @gamma0.setter
    def gamma0(self, value: float) -> None:
        """Set the gamma0 property."""
        self._cards[0].set_value("gamma0", value)

    @property
    def q2(self) -> typing.Optional[float]:
        """Get or set the Constant for the quadratic curve, q_2
        """ # nopep8
        return self._cards[0].get_value("q2")

    @q2.setter
    def q2(self, value: float) -> None:
        """Set the q2 property."""
        self._cards[0].set_value("q2", value)

    @property
    def tref(self) -> float:
        """Get or set the Reference temperature
        """ # nopep8
        return self._cards[0].get_value("tref")

    @tref.setter
    def tref(self, value: float) -> None:
        """Set the tref property."""
        self._cards[0].set_value("tref", value)

    @property
    def cp(self) -> typing.Optional[float]:
        """Get or set the Specific heat capacity
        """ # nopep8
        return self._cards[0].get_value("cp")

    @cp.setter
    def cp(self, value: float) -> None:
        """Set the cp property."""
        self._cards[0].set_value("cp", value)

    @property
    def tc(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity
        """ # nopep8
        return self._cards[0].get_value("tc")

    @tc.setter
    def tc(self, value: float) -> None:
        """Set the tc property."""
        self._cards[0].set_value("tc", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the Constant, c_1
        """ # nopep8
        return self._cards[1].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        """Set the c1 property."""
        self._cards[1].set_value("c1", value)

    @property
    def s1(self) -> typing.Optional[float]:
        """Get or set the Constant, s_1
        """ # nopep8
        return self._cards[1].get_value("s1")

    @s1.setter
    def s1(self, value: float) -> None:
        """Set the s1 property."""
        self._cards[1].set_value("s1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the Constant, c_2
        """ # nopep8
        return self._cards[1].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        """Set the c2 property."""
        self._cards[1].set_value("c2", value)

    @property
    def s2(self) -> typing.Optional[float]:
        """Get or set the Constant, s_2
        """ # nopep8
        return self._cards[1].get_value("s2")

    @s2.setter
    def s2(self, value: float) -> None:
        """Set the s2 property."""
        self._cards[1].set_value("s2", value)

    @property
    def ve(self) -> typing.Optional[float]:
        """Get or set the Relative volume, V_E/V_0
        """ # nopep8
        return self._cards[1].get_value("ve")

    @ve.setter
    def ve(self, value: float) -> None:
        """Set the ve property."""
        self._cards[1].set_value("ve", value)

    @property
    def vb(self) -> typing.Optional[float]:
        """Get or set the Relative volume, V_B/V_0
        """ # nopep8
        return self._cards[1].get_value("vb")

    @vb.setter
    def vb(self, value: float) -> None:
        """Set the vb property."""
        self._cards[1].set_value("vb", value)

