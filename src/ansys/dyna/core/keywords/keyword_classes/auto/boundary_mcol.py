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

class BoundaryMcol(KeywordBase):
    """DYNA BOUNDARY_MCOL keyword"""

    keyword = "BOUNDARY"
    subkeyword = "MCOL"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nmcol",
                        int,
                        0,
                        10,
                        kwargs.get("nmcol", 2)
                    ),
                    Field(
                        "mxstep",
                        int,
                        10,
                        10,
                        kwargs.get("mxstep")
                    ),
                    Field(
                        "endtmcol",
                        float,
                        20,
                        10,
                        kwargs.get("endtmcol", 0.0)
                    ),
                    Field(
                        "tsubc",
                        float,
                        30,
                        10,
                        kwargs.get("tsubc", 0.0)
                    ),
                    Field(
                        "prtmcol",
                        float,
                        40,
                        10,
                        kwargs.get("prtmcol")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "rbmcol",
                        int,
                        0,
                        10,
                        kwargs.get("rbmcol", 2)
                    ),
                    Field(
                        "mcolfile",
                        str,
                        10,
                        60,
                        kwargs.get("mcolfile")
                    ),
                ],
            ),
        ]

    @property
    def nmcol(self) -> int:
        """Get or set the Number of ships in MCOL coupling.
        """ # nopep8
        return self._cards[0].get_value("nmcol")

    @nmcol.setter
    def nmcol(self, value: int) -> None:
        self._cards[0].set_value("nmcol", value)

    @property
    def mxstep(self) -> typing.Optional[int]:
        """Get or set the Maximum of time step in MCOL calculation. If the number of MCOL time steps exceeds MXSTEP, then LS-DYNA will terminate.
        """ # nopep8
        return self._cards[0].get_value("mxstep")

    @mxstep.setter
    def mxstep(self, value: int) -> None:
        self._cards[0].set_value("mxstep", value)

    @property
    def endtmcol(self) -> float:
        """Get or set the Uncoupling termination time, see Remark 2 below. EQ. 0.0: set to LS-DYNA termination time
        """ # nopep8
        return self._cards[0].get_value("endtmcol")

    @endtmcol.setter
    def endtmcol(self, value: float) -> None:
        self._cards[0].set_value("endtmcol", value)

    @property
    def tsubc(self) -> float:
        """Get or set the Time interval for MCOL subcycling.
        EQ. 0.0: no subcycling
        """ # nopep8
        return self._cards[0].get_value("tsubc")

    @tsubc.setter
    def tsubc(self, value: float) -> None:
        self._cards[0].set_value("tsubc", value)

    @property
    def prtmcol(self) -> typing.Optional[float]:
        """Get or set the Time interval for output of MCOL rigid body data.
        """ # nopep8
        return self._cards[0].get_value("prtmcol")

    @prtmcol.setter
    def prtmcol(self, value: float) -> None:
        self._cards[0].set_value("prtmcol", value)

    @property
    def rbmcol(self) -> int:
        """Get or set the LS-DYNA rigid body material assignment for the ship.
        """ # nopep8
        return self._cards[1].get_value("rbmcol")

    @rbmcol.setter
    def rbmcol(self, value: int) -> None:
        self._cards[1].set_value("rbmcol", value)

    @property
    def mcolfile(self) -> typing.Optional[str]:
        """Get or set the Filename containing MCOL input parameters for the ship.
        """ # nopep8
        return self._cards[1].get_value("mcolfile")

    @mcolfile.setter
    def mcolfile(self, value: str) -> None:
        self._cards[1].set_value("mcolfile", value)

