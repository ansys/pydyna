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

"""Module providing the DatabaseFormat class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DATABASEFORMAT_CARD0 = (
    FieldSchema("iform", int, 0, 10, 0),
    FieldSchema("ibinary", int, 10, 10, 0),
)

class DatabaseFormat(KeywordBase):
    """DYNA DATABASE_FORMAT keyword"""

    keyword = "DATABASE"
    subkeyword = "FORMAT"

    def __init__(self, **kwargs):
        """Initialize the DatabaseFormat class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DATABASEFORMAT_CARD0,
                **kwargs,
            ),        ]
    @property
    def iform(self) -> int:
        """Get or set the Output format for D3PLOT and D3THDT files
        EQ.0: LS-DYNA database format (default),
        EQ.1: ANSYS database format,
        EQ.2: Both LS-DYNA and ANSYS database formats.
        """ # nopep8
        return self._cards[0].get_value("iform")

    @iform.setter
    def iform(self, value: int) -> None:
        """Set the iform property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""iform must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("iform", value)

    @property
    def ibinary(self) -> int:
        """Get or set the Word size of the binary output files (D3PLOT , D3THDT, D3DRLF) and interface files for 64-bit computers such as CRAY and NEC.
        EQ.0: default 64-bit format,
        EQ.1: 32-bit IEEE format.
        """ # nopep8
        return self._cards[0].get_value("ibinary")

    @ibinary.setter
    def ibinary(self, value: int) -> None:
        """Set the ibinary property."""
        if value not in [0, 1, None]:
            raise Exception("""ibinary must be `None` or one of {0,1}.""")
        self._cards[0].set_value("ibinary", value)

