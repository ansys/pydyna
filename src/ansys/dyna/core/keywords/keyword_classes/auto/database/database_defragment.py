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

"""Module providing the DatabaseDefragment class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DATABASEDEFRAGMENT_CARD0 = (
    FieldSchema("dt", float, 0, 10, None),
    FieldSchema("binary", int, 10, 10, 0),
)

class DatabaseDefragment(KeywordBase):
    """DYNA DATABASE_DEFRAGMENT keyword"""

    keyword = "DATABASE"
    subkeyword = "DEFRAGMENT"

    def __init__(self, **kwargs):
        """Initialize the DatabaseDefragment class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DATABASEDEFRAGMENT_CARD0,
                **kwargs,
            ),
        ]
    @property
    def dt(self) -> typing.Optional[float]:
        """Get or set the Time interval between output. If DT is zero, no output is printed.
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        """Set the dt property."""
        self._cards[0].set_value("dt", value)

    @property
    def binary(self) -> int:
        """Get or set the Flag for binary output:
        EQ.0: Do not write the file.
        EQ.1: Write data to an ASCII file.
        EQ.2: Write data to binary database binout.
        EQ.3: Write data to an ASCII file and to binary database binout.
        """ # nopep8
        return self._cards[0].get_value("binary")

    @binary.setter
    def binary(self, value: int) -> None:
        """Set the binary property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""binary must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("binary", value)

