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

class DatabaseSecforcFilter(KeywordBase):
    """DYNA DATABASE_SECFORC_FILTER keyword"""

    keyword = "DATABASE"
    subkeyword = "SECFORC_FILTER"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "dt",
                        float,
                        0,
                        10,
                        kwargs.get("dt", 0.0)
                    ),
                    Field(
                        "binary",
                        int,
                        10,
                        10,
                        kwargs.get("binary", 0)
                    ),
                    Field(
                        "lcur",
                        int,
                        20,
                        10,
                        kwargs.get("lcur", 0)
                    ),
                    Field(
                        "ioopt",
                        int,
                        30,
                        10,
                        kwargs.get("ioopt", 1)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "rate",
                        float,
                        0,
                        10,
                        kwargs.get("rate", 0)
                    ),
                    Field(
                        "cutoff",
                        float,
                        10,
                        10,
                        kwargs.get("cutoff")
                    ),
                    Field(
                        "window",
                        float,
                        20,
                        10,
                        kwargs.get("window")
                    ),
                    Field(
                        "type",
                        int,
                        30,
                        10,
                        kwargs.get("type", 0)
                    ),
                ],
            ),
        ]

    @property
    def dt(self) -> float:
        """Get or set the Time interval between outputs. If DT is zero, no output is printed, This field will be used for all selected ASCII_options that have no unique DT value specified
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[0].set_value("dt", value)

    @property
    def binary(self) -> int:
        """Get or set the Flag for binary file
        EQ.1: ASCII file is written. This is the default on serial and shared memory computers.
        EQ.2:Data written to a binary database, which contains data that would otherwise be output to the ASCII file. The ASCII file in this case is not created. This is the default on distributed memory computers.
        EQ.3: ASCII file is written and the data is also written to the binary database.
        """ # nopep8
        return self._cards[0].get_value("binary")

    @binary.setter
    def binary(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""binary must be one of {0,1,2,3}""")
        self._cards[0].set_value("binary", value)

    @property
    def lcur(self) -> int:
        """Get or set the Optional load curveid specifying time interval between dumps.
        """ # nopep8
        return self._cards[0].get_value("lcur")

    @lcur.setter
    def lcur(self, value: int) -> None:
        self._cards[0].set_value("lcur", value)

    @property
    def ioopt(self) -> int:
        """Get or set the Flag to govern behavior of the plot frequency load curve defined by LCUR:
        EQ.1: At the time each plot is generated, the load curve value is added to the current time to determine the next plot time. (default)
        EQ.2: At the time each plot is generated, the next plot time T is computed so that T = the current time plus the load curve value at time T.
        EQ.3: A plot is generated for each abscissa point in the load curve definition. The actual value of the load curve is ignored.
        """ # nopep8
        return self._cards[0].get_value("ioopt")

    @ioopt.setter
    def ioopt(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""ioopt must be one of {1,2,3}""")
        self._cards[0].set_value("ioopt", value)

    @property
    def rate(self) -> float:
        """Get or set the Time interval T between filter sampling
        """ # nopep8
        return self._cards[1].get_value("rate")

    @rate.setter
    def rate(self, value: float) -> None:
        self._cards[1].set_value("rate", value)

    @property
    def cutoff(self) -> typing.Optional[float]:
        """Get or set the Frequency cut-off C in Hz.
        """ # nopep8
        return self._cards[1].get_value("cutoff")

    @cutoff.setter
    def cutoff(self, value: float) -> None:
        self._cards[1].set_value("cutoff", value)

    @property
    def window(self) -> typing.Optional[float]:
        """Get or set the The width of the window W in units of time for storing the single,
        forward filtering required for the TYPE = 2 filter option.
        Increasing the width of the window will increase the memory
        required for the analysis. A window that is too narrow will
        reduce the amplitude of the filtered result significantly, and
        values below 15 are not recommended for that reason. In general,
        the results for the TYPE = 2 option are sensitive to the width of
        the window and experimentation is required
        """ # nopep8
        return self._cards[1].get_value("window")

    @window.setter
    def window(self, value: float) -> None:
        self._cards[1].set_value("window", value)

    @property
    def type(self) -> int:
        """Get or set the Flag for filtering options.
        EQ.0: No filtering (default).
        EQ.1: Single pass, forward Butterworth filtering.
        EQ.2: Two pass filtering over the specified time window.
        Backward Butterworth filtering is applied to the forward
        Butterworth results that have been stored. This option
        improves the phase accuracy significantly at the expense
        of memory.
        """ # nopep8
        return self._cards[1].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""type must be one of {0,1,2}""")
        self._cards[1].set_value("type", value)

