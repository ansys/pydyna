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

import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DatabaseFrequencyBinaryD3Spcm(KeywordBase):
    """DYNA DATABASE_FREQUENCY_BINARY_D3SPCM keyword"""

    keyword = "DATABASE"
    subkeyword = "FREQUENCY_BINARY_D3SPCM"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "binary",
                        int,
                        0,
                        10,
                        kwargs.get("binary", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "istate",
                        int,
                        0,
                        10,
                        kwargs.get("istate")
                    ),
                    Field(
                        "filename",
                        str,
                        10,
                        70,
                        kwargs.get("filename")
                    ),
                ],
            ),
        ]

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
        if value not in [0, 1, 2, 3]:
            raise Exception("""binary must be one of {0,1,2,3}""")
        self._cards[0].set_value("binary", value)

    @property
    def istate(self) -> typing.Optional[int]:
        """Get or set the State number in a binary plot file with name FILENAME. The structural analysis results at this state will be combined with the results from the current run.
        """ # nopep8
        return self._cards[1].get_value("istate")

    @istate.setter
    def istate(self, value: int) -> None:
        self._cards[1].set_value("istate", value)

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the Path and file name of precomputed structural response binary plot file (see Remark 4).
        """ # nopep8
        return self._cards[1].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        self._cards[1].set_value("filename", value)

