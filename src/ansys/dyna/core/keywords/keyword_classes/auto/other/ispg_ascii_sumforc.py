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

"""Module providing the IspgAsciiSumforc class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ISPGASCIISUMFORC_CARD0 = (
    FieldSchema("dt", float, 0, 10, None),
    FieldSchema("coord", float, 10, 10, None),
    FieldSchema("ndir", int, 20, 10, 3),
)

class IspgAsciiSumforc(KeywordBase):
    """DYNA ISPG_ASCII_SUMFORC keyword"""

    keyword = "ISPG"
    subkeyword = "ASCII_SUMFORC"

    def __init__(self, **kwargs):
        """Initialize the IspgAsciiSumforc class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ISPGASCIISUMFORC_CARD0,
                **kwargs,
            ),
        ]
    @property
    def dt(self) -> typing.Optional[float]:
        """Get or set the Time interval for the data output
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        """Set the dt property."""
        self._cards[0].set_value("dt", value)

    @property
    def coord(self) -> typing.Optional[float]:
        """Get or set the Position along the direction defined by NDIR of the cutting plane for the force summation
        """ # nopep8
        return self._cards[0].get_value("coord")

    @coord.setter
    def coord(self, value: float) -> None:
        """Set the coord property."""
        self._cards[0].set_value("coord", value)

    @property
    def ndir(self) -> int:
        """Get or set the Normal direction of the cutting plane:
        EQ.1: Along the global X - axis
        EQ.2: Along the global Y - axis
        EQ.3: Along the global Z - axis
        """ # nopep8
        return self._cards[0].get_value("ndir")

    @ndir.setter
    def ndir(self, value: int) -> None:
        """Set the ndir property."""
        if value not in [3, 1, 2, None]:
            raise Exception("""ndir must be `None` or one of {3,1,2}.""")
        self._cards[0].set_value("ndir", value)

