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

"""Module providing the Eos021 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EOS021_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("eost", int, 10, 10, None),
    FieldSchema("lmc", int, 20, 10, None),
    FieldSchema("nhv", int, 30, 10, None),
    FieldSchema("ivect", int, 40, 10, None),
    FieldSchema("eo", float, 50, 10, None),
    FieldSchema("vo", float, 60, 10, None),
    FieldSchema("bulk", float, 70, 10, None),
)

_EOS021_CARD1 = (
    FieldSchema("p1", float, 0, 10, None),
    FieldSchema("p2", float, 10, 10, None),
    FieldSchema("p3", float, 20, 10, None),
    FieldSchema("p4", float, 30, 10, None),
    FieldSchema("p5", float, 40, 10, None),
    FieldSchema("p6", float, 50, 10, None),
    FieldSchema("p7", float, 60, 10, None),
    FieldSchema("p8", float, 70, 10, None),
)

class Eos021(KeywordBase):
    """DYNA EOS_021 keyword"""

    keyword = "EOS"
    subkeyword = "021"

    def __init__(self, **kwargs):
        """Initialize the Eos021 class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EOS021_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EOS021_CARD1,
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
    def eost(self) -> typing.Optional[int]:
        """Get or set the User equation of state type (21-30 inclusive). A number between 21 and 30 has to be chosen.
        """ # nopep8
        return self._cards[0].get_value("eost")

    @eost.setter
    def eost(self, value: int) -> None:
        """Set the eost property."""
        self._cards[0].set_value("eost", value)

    @property
    def lmc(self) -> typing.Optional[int]:
        """Get or set the Length of material constant array which is equal to the number of material constants to be input. (LMC ≤ 48)
        """ # nopep8
        return self._cards[0].get_value("lmc")

    @lmc.setter
    def lmc(self, value: int) -> None:
        """Set the lmc property."""
        self._cards[0].set_value("lmc", value)

    @property
    def nhv(self) -> typing.Optional[int]:
        """Get or set the Number of history variables to be stored.
        """ # nopep8
        return self._cards[0].get_value("nhv")

    @nhv.setter
    def nhv(self, value: int) -> None:
        """Set the nhv property."""
        self._cards[0].set_value("nhv", value)

    @property
    def ivect(self) -> typing.Optional[int]:
        """Get or set the Vectorization flag (on = 1). A vectorized user subroutine must be supplied.
        """ # nopep8
        return self._cards[0].get_value("ivect")

    @ivect.setter
    def ivect(self, value: int) -> None:
        """Set the ivect property."""
        self._cards[0].set_value("ivect", value)

    @property
    def eo(self) -> typing.Optional[float]:
        """Get or set the Initial internal energy.
        """ # nopep8
        return self._cards[0].get_value("eo")

    @eo.setter
    def eo(self, value: float) -> None:
        """Set the eo property."""
        self._cards[0].set_value("eo", value)

    @property
    def vo(self) -> typing.Optional[float]:
        """Get or set the Initial relative volume.
        """ # nopep8
        return self._cards[0].get_value("vo")

    @vo.setter
    def vo(self, value: float) -> None:
        """Set the vo property."""
        self._cards[0].set_value("vo", value)

    @property
    def bulk(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus. This value is used in the calculation of the contact surface stiffness.
        """ # nopep8
        return self._cards[0].get_value("bulk")

    @bulk.setter
    def bulk(self, value: float) -> None:
        """Set the bulk property."""
        self._cards[0].set_value("bulk", value)

    @property
    def p1(self) -> typing.Optional[float]:
        """Get or set the material parameter.
        """ # nopep8
        return self._cards[1].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        """Set the p1 property."""
        self._cards[1].set_value("p1", value)

    @property
    def p2(self) -> typing.Optional[float]:
        """Get or set the material parameter.
        """ # nopep8
        return self._cards[1].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        """Set the p2 property."""
        self._cards[1].set_value("p2", value)

    @property
    def p3(self) -> typing.Optional[float]:
        """Get or set the material parameter.
        """ # nopep8
        return self._cards[1].get_value("p3")

    @p3.setter
    def p3(self, value: float) -> None:
        """Set the p3 property."""
        self._cards[1].set_value("p3", value)

    @property
    def p4(self) -> typing.Optional[float]:
        """Get or set the material parameter.
        """ # nopep8
        return self._cards[1].get_value("p4")

    @p4.setter
    def p4(self, value: float) -> None:
        """Set the p4 property."""
        self._cards[1].set_value("p4", value)

    @property
    def p5(self) -> typing.Optional[float]:
        """Get or set the material parameter.
        """ # nopep8
        return self._cards[1].get_value("p5")

    @p5.setter
    def p5(self, value: float) -> None:
        """Set the p5 property."""
        self._cards[1].set_value("p5", value)

    @property
    def p6(self) -> typing.Optional[float]:
        """Get or set the material parameter.
        """ # nopep8
        return self._cards[1].get_value("p6")

    @p6.setter
    def p6(self, value: float) -> None:
        """Set the p6 property."""
        self._cards[1].set_value("p6", value)

    @property
    def p7(self) -> typing.Optional[float]:
        """Get or set the material parameter.
        """ # nopep8
        return self._cards[1].get_value("p7")

    @p7.setter
    def p7(self, value: float) -> None:
        """Set the p7 property."""
        self._cards[1].set_value("p7", value)

    @property
    def p8(self) -> typing.Optional[float]:
        """Get or set the material parameter.
        """ # nopep8
        return self._cards[1].get_value("p8")

    @p8.setter
    def p8(self, value: float) -> None:
        """Set the p8 property."""
        self._cards[1].set_value("p8", value)

