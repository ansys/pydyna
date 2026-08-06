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

"""Module providing the EfvUnits class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVUNITS_CARD0 = (
    FieldSchema("unitm", int, 0, 10, 1),
    FieldSchema("unitl", int, 10, 10, 1),
    FieldSchema("unitt", int, 20, 10, 2),
)

class EfvUnits(KeywordBase):
    """DYNA EFV_UNITS keyword"""

    keyword = "EFV"
    subkeyword = "UNITS"

    def __init__(self, **kwargs):
        """Initialize the EfvUnits class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVUNITS_CARD0,
                **kwargs,
            ),
        ]
    @property
    def unitm(self) -> int:
        """Get or set the Mass unit:
        EQ.1: milligram
        EQ.2: gram
        EQ.3: kilogram
        EQ.4: pound mass
        EQ.5: picogram
        """ # nopep8
        return self._cards[0].get_value("unitm")

    @unitm.setter
    def unitm(self, value: int) -> None:
        """Set the unitm property."""
        if value not in [1, 2, 3, 4, 5, None]:
            raise Exception("""unitm must be `None` or one of {1,2,3,4,5}.""")
        self._cards[0].set_value("unitm", value)

    @property
    def unitl(self) -> int:
        """Get or set the Length unit:
        EQ.1: millimeter
        EQ.2: centimeter
        EQ.3: meter
        EQ.4: inch
        EQ.5: foot
        EQ.6: micrometer
        """ # nopep8
        return self._cards[0].get_value("unitl")

    @unitl.setter
    def unitl(self, value: int) -> None:
        """Set the unitl property."""
        if value not in [1, 2, 3, 4, 5, 6, None]:
            raise Exception("""unitl must be `None` or one of {1,2,3,4,5,6}.""")
        self._cards[0].set_value("unitl", value)

    @property
    def unitt(self) -> int:
        """Get or set the Time unit:
        EQ.1: microsecond
        EQ.2: millisecond
        EQ.3: second
        """ # nopep8
        return self._cards[0].get_value("unitt")

    @unitt.setter
    def unitt(self, value: int) -> None:
        """Set the unitt property."""
        if value not in [2, 1, 3, None]:
            raise Exception("""unitt must be `None` or one of {2,1,3}.""")
        self._cards[0].set_value("unitt", value)

