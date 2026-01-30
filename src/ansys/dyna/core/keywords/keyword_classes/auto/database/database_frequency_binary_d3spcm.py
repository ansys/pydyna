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

"""Module providing the DatabaseFrequencyBinaryD3Spcm class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DATABASEFREQUENCYBINARYD3SPCM_CARD0 = (
    FieldSchema("binary", int, 0, 10, 0),
)

_DATABASEFREQUENCYBINARYD3SPCM_CARD1 = (
    FieldSchema("istate", int, 0, 10, None),
    FieldSchema("filename", str, 10, 70, None),
)

class DatabaseFrequencyBinaryD3Spcm(KeywordBase):
    """DYNA DATABASE_FREQUENCY_BINARY_D3SPCM keyword"""

    keyword = "DATABASE"
    subkeyword = "FREQUENCY_BINARY_D3SPCM"

    def __init__(self, **kwargs):
        """Initialize the DatabaseFrequencyBinaryD3Spcm class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DATABASEFREQUENCYBINARYD3SPCM_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DATABASEFREQUENCYBINARYD3SPCM_CARD1,
                **kwargs,
            ),        ]
    @property
    def binary(self) -> int:
        """Get or set the Flag for writing the binary plot file.
        EQ.0: Off
        EQ.1: write the binary plot file
        EQ.2: include the individual mode response in the binary plot file D3SPCM.
        EQ.3: 	Write the binary plot file which combines response spectrum analysis results and other structural analysis results provided by Card 2c (OPTION1‌ = D3SPCM).
        """ # nopep8
        return self._cards[0].get_value("binary")

    @binary.setter
    def binary(self, value: int) -> None:
        """Set the binary property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""binary must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("binary", value)

    @property
    def istate(self) -> typing.Optional[int]:
        """Get or set the State number in a binary plot file with name FILENAME. The structural analysis results at this state will be combined with the results from the current run.
        """ # nopep8
        return self._cards[1].get_value("istate")

    @istate.setter
    def istate(self, value: int) -> None:
        """Set the istate property."""
        self._cards[1].set_value("istate", value)

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the Path and file name of precomputed structural response binary plot file (see Remark 4).
        """ # nopep8
        return self._cards[1].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        """Set the filename property."""
        self._cards[1].set_value("filename", value)

