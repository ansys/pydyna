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

"""Module providing the EfvFailureCumulativeDamage class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVFAILURECUMULATIVEDAMAGE_CARD0 = (
    FieldSchema("failid", int, 0, 10, None),
    FieldSchema("eps1", float, 10, 10, None),
    FieldSchema("eps2", float, 20, 10, None),
    FieldSchema("dmax", float, 30, 10, 1.0),
)

class EfvFailureCumulativeDamage(KeywordBase):
    """DYNA EFV_FAILURE_CUMULATIVE_DAMAGE keyword"""

    keyword = "EFV"
    subkeyword = "FAILURE_CUMULATIVE_DAMAGE"

    def __init__(self, **kwargs):
        """Initialize the EfvFailureCumulativeDamage class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVFAILURECUMULATIVEDAMAGE_CARD0,
                **kwargs,
            ),
        ]
    @property
    def failid(self) -> typing.Optional[int]:
        """Get or set the Failure model identification. A unique number or label must be used (see Remark Error! Reference source not found. in *EFV_MAT).
        """ # nopep8
        return self._cards[0].get_value("failid")

    @failid.setter
    def failid(self, value: int) -> None:
        """Set the failid property."""
        self._cards[0].set_value("failid", value)

    @property
    def eps1(self) -> typing.Optional[float]:
        """Get or set the Maximum effective plastic strain for which damage is 0.0
        """ # nopep8
        return self._cards[0].get_value("eps1")

    @eps1.setter
    def eps1(self, value: float) -> None:
        """Set the eps1 property."""
        self._cards[0].set_value("eps1", value)

    @property
    def eps2(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain at which damage reaches its maximum value specified by DMAX
        """ # nopep8
        return self._cards[0].get_value("eps2")

    @eps2.setter
    def eps2(self, value: float) -> None:
        """Set the eps2 property."""
        self._cards[0].set_value("eps2", value)

    @property
    def dmax(self) -> float:
        """Get or set the Maximum value of damage, D_max
        """ # nopep8
        return self._cards[0].get_value("dmax")

    @dmax.setter
    def dmax(self, value: float) -> None:
        """Set the dmax property."""
        self._cards[0].set_value("dmax", value)

