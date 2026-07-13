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

"""Module providing the EmDatabaseCircuitsource class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EMDATABASECIRCUITSOURCE_CARD0 = (
    FieldSchema("circid", int, 0, 10, None),
    FieldSchema("imp", int, 10, 10, 0),
)

class EmDatabaseCircuitsource(KeywordBase):
    """DYNA EM_DATABASE_CIRCUITSOURCE keyword"""

    keyword = "EM"
    subkeyword = "DATABASE_CIRCUITSOURCE"

    def __init__(self, **kwargs):
        """Initialize the EmDatabaseCircuitsource class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMDATABASECIRCUITSOURCE_CARD0,
                **kwargs,
            ),
        ]
    @property
    def circid(self) -> typing.Optional[int]:
        """Get or set the Source circuit ID for which total magnetic flux is calculated and output to em_CircuitSource_CIRCID.dat. See Remark 1.
        """ # nopep8
        return self._cards[0].get_value("circid")

    @circid.setter
    def circid(self, value: int) -> None:
        """Set the circid property."""
        self._cards[0].set_value("circid", value)

    @property
    def imp(self) -> int:
        """Get or set the Include the source circuit with ID CIRCID in the impedance matrix calculation and output.
        EQ.0: Not included.
        EQ.1: Included.See Remark 2.
        """ # nopep8
        return self._cards[0].get_value("imp")

    @imp.setter
    def imp(self, value: int) -> None:
        """Set the imp property."""
        self._cards[0].set_value("imp", value)

