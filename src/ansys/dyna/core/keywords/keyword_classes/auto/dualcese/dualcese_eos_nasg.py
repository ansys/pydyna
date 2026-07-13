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

"""Module providing the DualceseEosNasg class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DUALCESEEOSNASG_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("gamma", float, 10, 10, None),
    FieldSchema("cv", float, 20, 10, None),
    FieldSchema("pinf", float, 30, 10, None),
    FieldSchema("q", float, 40, 10, None),
    FieldSchema("qp", float, 50, 10, None),
    FieldSchema("b", float, 60, 10, None),
    FieldSchema("w", float, 70, 10, None),
)

class DualceseEosNasg(KeywordBase):
    """DYNA DUALCESE_EOS_NASG keyword"""

    keyword = "DUALCESE"
    subkeyword = "EOS_NASG"

    def __init__(self, **kwargs):
        """Initialize the DualceseEosNasg class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEEOSNASG_CARD0,
                **kwargs,
            ),
        ]
    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state ID
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def gamma(self) -> typing.Optional[float]:
        """Get or set the Ratio of specific heats,
        """ # nopep8
        return self._cards[0].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        """Set the gamma property."""
        self._cards[0].set_value("gamma", value)

    @property
    def cv(self) -> typing.Optional[float]:
        """Get or set the Specific heat at constant volume
        """ # nopep8
        return self._cards[0].get_value("cv")

    @cv.setter
    def cv(self, value: float) -> None:
        """Set the cv property."""
        self._cards[0].set_value("cv", value)

    @property
    def pinf(self) -> typing.Optional[float]:
        """Get or set the Parameter for a given phase, p_ (dimensions: [M/(LT**2 )]). See Le Metayer and Saurel [2016].
        """ # nopep8
        return self._cards[0].get_value("pinf")

    @pinf.setter
    def pinf(self, value: float) -> None:
        """Set the pinf property."""
        self._cards[0].set_value("pinf", value)

    @property
    def q(self) -> typing.Optional[float]:
        """Get or set the Parameter for a given phase, q (dimensions: [L**2/T**2 ]). q is the heat bond for the phase. See Le Metayer and Saurel [2016].
        """ # nopep8
        return self._cards[0].get_value("q")

    @q.setter
    def q(self, value: float) -> None:
        """Set the q property."""
        self._cards[0].set_value("q", value)

    @property
    def qp(self) -> typing.Optional[float]:
        """Get or set the Parameter for a given phase, q**' (dimensions: [L**2/(T**2 )], where  represents the dimension of temperature). See Le Metayer and Saurel [2016].
        """ # nopep8
        return self._cards[0].get_value("qp")

    @qp.setter
    def qp(self, value: float) -> None:
        """Set the qp property."""
        self._cards[0].set_value("qp", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Parameter for a given phase, b (dimensions: [L**3/M]). b indicates the fluids covolume. See Le Metayer and Saurel [2016].
        """ # nopep8
        return self._cards[0].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[0].set_value("b", value)

    @property
    def w(self) -> typing.Optional[float]:
        """Get or set the Molar mass of this fluid
        """ # nopep8
        return self._cards[0].get_value("w")

    @w.setter
    def w(self, value: float) -> None:
        """Set the w property."""
        self._cards[0].set_value("w", value)

