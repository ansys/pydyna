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

"""Module providing the DatabaseSale class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DATABASESALE_CARD0 = (
    FieldSchema("d3sale", int, 0, 10, 0),
    FieldSchema("dmpmsh", int, 10, 10, 0),
)

class DatabaseSale(KeywordBase):
    """DYNA DATABASE_SALE keyword"""

    keyword = "DATABASE"
    subkeyword = "SALE"

    def __init__(self, **kwargs):
        """Initialize the DatabaseSale class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DATABASESALE_CARD0,
                **kwargs,
            ),
        ]
    @property
    def d3sale(self) -> int:
        """Get or set the Flag for where to output the S-ALE part data (see Remarks 1 and 2):
        EQ.0: Output S-ALE part data to d3plot.
        EQ.1: Write S-ALE part data to binary, LSDA-format database d3sale.
        """ # nopep8
        return self._cards[0].get_value("d3sale")

    @d3sale.setter
    def d3sale(self, value: int) -> None:
        """Set the d3sale property."""
        if value not in [0, 1, None]:
            raise Exception("""d3sale must be `None` or one of {0,1}.""")
        self._cards[0].set_value("d3sale", value)

    @property
    def dmpmsh(self) -> int:
        """Get or set the Flag for outputting the generated S-ALE mesh (see Remark 3):
        EQ.0: Do not output the S-ALE mesh.
        EQ.1: Output the S-ALE mesh into a file named salemsh.inc.
        """ # nopep8
        return self._cards[0].get_value("dmpmsh")

    @dmpmsh.setter
    def dmpmsh(self, value: int) -> None:
        """Set the dmpmsh property."""
        if value not in [0, 1, None]:
            raise Exception("""dmpmsh must be `None` or one of {0,1}.""")
        self._cards[0].set_value("dmpmsh", value)

