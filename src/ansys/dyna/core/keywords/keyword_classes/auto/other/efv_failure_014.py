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

"""Module providing the EfvFailure014 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVFAILURE014_CARD0 = (
    FieldSchema("failid", int, 0, 10, None),
    FieldSchema("d1", float, 10, 10, None),
    FieldSchema("d2", float, 20, 10, None),
    FieldSchema("d3", float, 30, 10, None),
    FieldSchema("d4", float, 40, 10, None),
    FieldSchema("d5", float, 50, 10, None),
    FieldSchema("tmelt", float, 60, 10, None),
    FieldSchema("epsrtref", float, 70, 10, 1.0),
)

class EfvFailure014(KeywordBase):
    """DYNA EFV_FAILURE_014 keyword"""

    keyword = "EFV"
    subkeyword = "FAILURE_014"

    def __init__(self, **kwargs):
        """Initialize the EfvFailure014 class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVFAILURE014_CARD0,
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
    def d1(self) -> typing.Optional[float]:
        """Get or set the Damage constant D1
        """ # nopep8
        return self._cards[0].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[0].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Damage constant D2
        """ # nopep8
        return self._cards[0].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[0].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Damage constant D3
        """ # nopep8
        return self._cards[0].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[0].set_value("d3", value)

    @property
    def d4(self) -> typing.Optional[float]:
        """Get or set the Damage constant D4
        """ # nopep8
        return self._cards[0].get_value("d4")

    @d4.setter
    def d4(self, value: float) -> None:
        """Set the d4 property."""
        self._cards[0].set_value("d4", value)

    @property
    def d5(self) -> typing.Optional[float]:
        """Get or set the Damage constant D5
        """ # nopep8
        return self._cards[0].get_value("d5")

    @d5.setter
    def d5(self, value: float) -> None:
        """Set the d5 property."""
        self._cards[0].set_value("d5", value)

    @property
    def tmelt(self) -> typing.Optional[float]:
        """Get or set the Melting temperature, T_melt
        """ # nopep8
        return self._cards[0].get_value("tmelt")

    @tmelt.setter
    def tmelt(self, value: float) -> None:
        """Set the tmelt property."""
        self._cards[0].set_value("tmelt", value)

    @property
    def epsrtref(self) -> float:
        """Get or set the Reference strain rate in units of [time]-1
        """ # nopep8
        return self._cards[0].get_value("epsrtref")

    @epsrtref.setter
    def epsrtref(self, value: float) -> None:
        """Set the epsrtref property."""
        self._cards[0].set_value("epsrtref", value)

