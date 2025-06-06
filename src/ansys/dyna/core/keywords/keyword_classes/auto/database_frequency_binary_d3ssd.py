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

"""Module providing the DatabaseFrequencyBinaryD3Ssd class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DatabaseFrequencyBinaryD3Ssd(KeywordBase):
    """DYNA DATABASE_FREQUENCY_BINARY_D3SSD keyword"""

    keyword = "DATABASE"
    subkeyword = "FREQUENCY_BINARY_D3SSD"

    def __init__(self, **kwargs):
        """Initialize the DatabaseFrequencyBinaryD3Ssd class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "binary",
                        int,
                        0,
                        10,
                        0,
                        **kwargs,
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
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "fmax",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "nfreq",
                        int,
                        20,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "fspace",
                        int,
                        30,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "lcfreq",
                        int,
                        40,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def binary(self) -> int:
        """Get or set the Flag for writing the binary plot file.
        EQ.0: Off
        EQ.1: write the binary plot file
        EQ.2: write the complex variable binary plot file.
        EQ.90: write only real part of frequency response.
        EQ.91: write only imaginary part of frequency response.
        """ # nopep8
        return self._cards[0].get_value("binary")

    @binary.setter
    def binary(self, value: int) -> None:
        """Set the binary property."""
        if value not in [0, 1, 2, 90, 91, None]:
            raise Exception("""binary must be `None` or one of {0,1,2,90,91}.""")
        self._cards[0].set_value("binary", value)

    @property
    def fmin(self) -> float:
        """Get or set the Minimum frequency for output (cycles/time).
        """ # nopep8
        return self._cards[1].get_value("fmin")

    @fmin.setter
    def fmin(self, value: float) -> None:
        """Set the fmin property."""
        self._cards[1].set_value("fmin", value)

    @property
    def fmax(self) -> float:
        """Get or set the Maximum frequency for output (cycles/time).
        """ # nopep8
        return self._cards[1].get_value("fmax")

    @fmax.setter
    def fmax(self, value: float) -> None:
        """Set the fmax property."""
        self._cards[1].set_value("fmax", value)

    @property
    def nfreq(self) -> int:
        """Get or set the Number of frequencies for output.
        """ # nopep8
        return self._cards[1].get_value("nfreq")

    @nfreq.setter
    def nfreq(self, value: int) -> None:
        """Set the nfreq property."""
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
        """Set the fspace property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""fspace must be `None` or one of {0,1,2,3}.""")
        self._cards[1].set_value("fspace", value)

    @property
    def lcfreq(self) -> int:
        """Get or set the Load Curve ID defining the frequencies for output.
        """ # nopep8
        return self._cards[1].get_value("lcfreq")

    @lcfreq.setter
    def lcfreq(self, value: int) -> None:
        """Set the lcfreq property."""
        self._cards[1].set_value("lcfreq", value)

