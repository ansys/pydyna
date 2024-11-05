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

class DatabaseFrequencyBinaryD3Psd(KeywordBase):
    """DYNA DATABASE_FREQUENCY_BINARY_D3PSD keyword"""

    keyword = "DATABASE"
    subkeyword = "FREQUENCY_BINARY_D3PSD"

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
                        kwargs.get("binary")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "fmin",
                        float,
                        0,
                        10,
                        kwargs.get("fmin", 0.0)
                    ),
                    Field(
                        "fmax",
                        float,
                        10,
                        10,
                        kwargs.get("fmax", 0.0)
                    ),
                    Field(
                        "nfreq",
                        int,
                        20,
                        10,
                        kwargs.get("nfreq", 0)
                    ),
                    Field(
                        "fspace",
                        int,
                        30,
                        10,
                        kwargs.get("fspace", 0)
                    ),
                    Field(
                        "lcfreq",
                        int,
                        40,
                        10,
                        kwargs.get("lcfreq", 0)
                    ),
                ],
            ),
        ]

    @property
    def binary(self) -> typing.Optional[int]:
        """Get or set the Flag for writing the binary plot file.  See Remark 1.
        EQ.0:	Off
        EQ.1 : Write the binary plot file.
        EQ.2 : Write the complex variable binary plot file D3SSD(OPTION1 = D3SSD) or include the individual mode response in the binary plot file D3SPCM(OPTION1‌ = D3SPCM).
        EQ.3 : Write the binary plot file which combines response spectrum analysis results and other structural analysis results provided by the file specified with Card  2c(OPTION1‌ = D3SPCM).
        EQ.90 : Write only real part of frequency response(D3SSD only).
        EQ.91 : Write only imaginary part of frequency response(D3SSD only).
        """ # nopep8
        return self._cards[0].get_value("binary")

    @binary.setter
    def binary(self, value: int) -> None:
        self._cards[0].set_value("binary", value)

    @property
    def fmin(self) -> float:
        """Get or set the Minimum frequency for output (cycles/time).
        """ # nopep8
        return self._cards[1].get_value("fmin")

    @fmin.setter
    def fmin(self, value: float) -> None:
        self._cards[1].set_value("fmin", value)

    @property
    def fmax(self) -> float:
        """Get or set the Maximum frequency for output (cycles/time).
        """ # nopep8
        return self._cards[1].get_value("fmax")

    @fmax.setter
    def fmax(self, value: float) -> None:
        self._cards[1].set_value("fmax", value)

    @property
    def nfreq(self) -> int:
        """Get or set the Number of frequencies for output.
        """ # nopep8
        return self._cards[1].get_value("nfreq")

    @nfreq.setter
    def nfreq(self, value: int) -> None:
        self._cards[1].set_value("nfreq", value)

    @property
    def fspace(self) -> int:
        """Get or set the Frequency spacing option for output:
        EQ.0: linear,
        EQ.1: logarithmic,
        EQ.2: biased.
        EQ.3:	Eigenfrequencies only
        """ # nopep8
        return self._cards[1].get_value("fspace")

    @fspace.setter
    def fspace(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""fspace must be one of {0,1,2,3}""")
        self._cards[1].set_value("fspace", value)

    @property
    def lcfreq(self) -> int:
        """Get or set the Load Curve ID defining the frequencies for output.
        """ # nopep8
        return self._cards[1].get_value("lcfreq")

    @lcfreq.setter
    def lcfreq(self, value: int) -> None:
        self._cards[1].set_value("lcfreq", value)

