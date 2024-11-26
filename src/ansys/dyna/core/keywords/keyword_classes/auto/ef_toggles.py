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

class EfToggles(KeywordBase):
    """DYNA EF_TOGGLES keyword"""

    keyword = "EF"
    subkeyword = "TOGGLES"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "iprint1",
                        int,
                        0,
                        10,
                        kwargs.get("iprint1", 0)
                    ),
                    Field(
                        "iprint2",
                        int,
                        10,
                        10,
                        kwargs.get("iprint2", 0)
                    ),
                    Field(
                        "iprint3",
                        int,
                        20,
                        10,
                        kwargs.get("iprint3", 0)
                    ),
                    Field(
                        "iprint4",
                        int,
                        30,
                        10,
                        kwargs.get("iprint4", 0)
                    ),
                    Field(
                        "idata",
                        int,
                        40,
                        10,
                        kwargs.get("idata", 0)
                    ),
                    Field(
                        "itraces",
                        int,
                        50,
                        10,
                        kwargs.get("itraces", 0)
                    ),
                    Field(
                        "irstrt",
                        int,
                        60,
                        10,
                        kwargs.get("irstrt")
                    ),
                ],
            ),
        ]

    @property
    def iprint1(self) -> int:
        """Get or set the Controls output of exchange fractions to the d3hsp file. In almost all situations this should be set to 0 because the “exchange factors” are written to the file exchfl.
        EQ.0:	do not write exchange fractions
        EQ.1 : write exchange fraction.
        """ # nopep8
        return self._cards[0].get_value("iprint1")

    @iprint1.setter
    def iprint1(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""iprint1 must be one of {0,1}""")
        self._cards[0].set_value("iprint1", value)

    @property
    def iprint2(self) -> int:
        """Get or set the Controls output of a list of lost photons to the d3hsp file. This is useful for debugging.
        EQ.0:	do not write lost photon listEQ.1 : write lost photon list
        """ # nopep8
        return self._cards[0].get_value("iprint2")

    @iprint2.setter
    def iprint2(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""iprint2 must be one of {0,1}""")
        self._cards[0].set_value("iprint2", value)

    @property
    def iprint3(self) -> int:
        """Get or set the Controls output about the grid algorithm to the d3hsp file
        EQ.0:	do not write grid algorithm information
        EQ.1 : write grid algorithm information
        """ # nopep8
        return self._cards[0].get_value("iprint3")

    @iprint3.setter
    def iprint3(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""iprint3 must be one of {0,1}""")
        self._cards[0].set_value("iprint3", value)

    @property
    def iprint4(self) -> int:
        """Get or set the Controls output about material information pertaining to exchange factors to the d3hsp file.
        EQ.0:	do not write material information
        EQ.1 : write material information
        """ # nopep8
        return self._cards[0].get_value("iprint4")

    @iprint4.setter
    def iprint4(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""iprint4 must be one of {0,1}""")
        self._cards[0].set_value("iprint4", value)

    @property
    def idata(self) -> int:
        """Get or set the Controls execution
        EQ.0:	run proceeds
        EQ.1 : terminate after input parameter check
        """ # nopep8
        return self._cards[0].get_value("idata")

    @idata.setter
    def idata(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""idata must be one of {0,1}""")
        self._cards[0].set_value("idata", value)

    @property
    def itraces(self) -> int:
        """Get or set the ITRACES Controls output of photon trajectories.
        EQ.0:	do not write trajectory information
        EQ.1 : write trajectory information.This file becomes large quickly and is only useful for debugging
        """ # nopep8
        return self._cards[0].get_value("itraces")

    @itraces.setter
    def itraces(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""itraces must be one of {0,1}""")
        self._cards[0].set_value("itraces", value)

    @property
    def irstrt(self) -> typing.Optional[int]:
        """Get or set the IRESTART should be set either to 1 or 0.  If IRESTART is set to 1 then LS-DYNA restarts the exchange factor solver.  If IRESTART is set to 1 and a .crh file exists, the Monte Carlo solver will pick up where it left off prior to a crash.  If there is a .nij file but no .crh file, then LS-DYNA will recycle the results of the previous exchange factor running emitting more photons to increase accuracy
        """ # nopep8
        return self._cards[0].get_value("irstrt")

    @irstrt.setter
    def irstrt(self, value: int) -> None:
        self._cards[0].set_value("irstrt", value)

