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

"""Module providing the DatabaseAdams class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DATABASEADAMS_CARD0 = (
    FieldSchema("iflag", int, 0, 10, 0),
    FieldSchema("m_units", int, 10, 10, None),
    FieldSchema("l_units", int, 20, 10, None),
    FieldSchema("t_units", int, 30, 10, None),
)

class DatabaseAdams(KeywordBase):
    """DYNA DATABASE_ADAMS keyword"""

    keyword = "DATABASE"
    subkeyword = "ADAMS"

    def __init__(self, **kwargs):
        """Initialize the DatabaseAdams class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DATABASEADAMS_CARD0,
                **kwargs,
            ),
        ]
    @property
    def iflag(self) -> int:
        """Get or set the Flag controlling write of d3mnf after eigenvalue analysis:
        EQ.0: Do not write(default).
        EQ.1: Write to d3mnf.
        """ # nopep8
        return self._cards[0].get_value("iflag")

    @iflag.setter
    def iflag(self, value: int) -> None:
        """Set the iflag property."""
        if value not in [0, 1, None]:
            raise Exception("""iflag must be `None` or one of {0,1}.""")
        self._cards[0].set_value("iflag", value)

    @property
    def m_units(self) -> typing.Optional[int]:
        """Get or set the Mass units of measure used in the model:
        EQ.-1: Kilogram
        EQ.-2: Gram
        EQ.-3: Megagram (metric ton)
        EQ.-4: lbf x s2/in (psi-compatible)
        EQ.-5: Slug
        EQ.-6: Pound-mass.
        """ # nopep8
        return self._cards[0].get_value("m_units")

    @m_units.setter
    def m_units(self, value: int) -> None:
        """Set the m_units property."""
        self._cards[0].set_value("m_units", value)

    @property
    def l_units(self) -> typing.Optional[int]:
        """Get or set the Length units of measure used in the model:
        EQ.-1: Meter
        EQ.-2: Centimeter
        EQ.-3: Millimeter
        EQ.-4: Inch
        EQ.-5: Foot.
        """ # nopep8
        return self._cards[0].get_value("l_units")

    @l_units.setter
    def l_units(self, value: int) -> None:
        """Set the l_units property."""
        self._cards[0].set_value("l_units", value)

    @property
    def t_units(self) -> typing.Optional[int]:
        """Get or set the Time units of measure used in the model:
        EQ.-1: Second
        EQ.-2: Millisecond
        EQ.-3: Minute
        EQ.-4: Hour.
        """ # nopep8
        return self._cards[0].get_value("t_units")

    @t_units.setter
    def t_units(self, value: int) -> None:
        """Set the t_units property."""
        self._cards[0].set_value("t_units", value)

