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

class ControlImplicitSsdDirect(KeywordBase):
    """DYNA CONTROL_IMPLICIT_SSD_DIRECT keyword"""

    keyword = "CONTROL"
    subkeyword = "IMPLICIT_SSD_DIRECT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "issflg",
                        int,
                        0,
                        10,
                        kwargs.get("issflg", 0)
                    ),
                    Field(
                        "fmin",
                        float,
                        10,
                        10,
                        kwargs.get("fmin")
                    ),
                    Field(
                        "fmax",
                        float,
                        20,
                        10,
                        kwargs.get("fmax")
                    ),
                    Field(
                        "nfreq",
                        int,
                        30,
                        10,
                        kwargs.get("nfreq", 1)
                    ),
                    Field(
                        "loss",
                        float,
                        40,
                        10,
                        kwargs.get("loss", 0.0)
                    ),
                    Field(
                        "fspace",
                        float,
                        50,
                        10,
                        kwargs.get("fspace", 0)
                    ),
                    Field(
                        "fractn",
                        int,
                        60,
                        10,
                        kwargs.get("fractn", 1)
                    ),
                ],
            ),
        ]

    @property
    def issflg(self) -> int:
        """Get or set the Complex steady state vibration flag:
        EQ.0:	Off
        EQ.1 : On.
        """ # nopep8
        return self._cards[0].get_value("issflg")

    @issflg.setter
    def issflg(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""issflg must be one of {0,1}""")
        self._cards[0].set_value("issflg", value)

    @property
    def fmin(self) -> typing.Optional[float]:
        """Get or set the Minimum frequency in the solution. Units are Hertz.
        """ # nopep8
        return self._cards[0].get_value("fmin")

    @fmin.setter
    def fmin(self, value: float) -> None:
        self._cards[0].set_value("fmin", value)

    @property
    def fmax(self) -> typing.Optional[float]:
        """Get or set the Maximum frequency in the solution. Units are Hertz.
        """ # nopep8
        return self._cards[0].get_value("fmax")

    @fmax.setter
    def fmax(self, value: float) -> None:
        self._cards[0].set_value("fmax", value)

    @property
    def nfreq(self) -> int:
        """Get or set the Number of frequencies in the solution.
        """ # nopep8
        return self._cards[0].get_value("nfreq")

    @nfreq.setter
    def nfreq(self, value: int) -> None:
        self._cards[0].set_value("nfreq", value)

    @property
    def loss(self) -> float:
        """Get or set the Structural loss factor.
        """ # nopep8
        return self._cards[0].get_value("loss")

    @loss.setter
    def loss(self, value: float) -> None:
        self._cards[0].set_value("loss", value)

    @property
    def fspace(self) -> float:
        """Get or set the Solution frequency assignment strategy:
        EQ.0:	The frequency is interpolated linearly between FMIN and FMAX.This is the default strategy.
        EQ.1 : The frequency is interpolated on a log scale between FMIN and FMAX, so they are biased to lower frequencies.
        EQ.2 : The frequency is interpolated on a fractional octave scale starting with FMIN.Integer FRACTN is the octave fraction.
        The formula for the active frequency in Hertz is "FACTIVE" = "FMIN" (2.0) ^ (1 / "FRACTN")) ^ (("IFREQ" - 1)).
        IFREQ is the ith frequency in the solution.FMAX is ignored.
        LT.0 : | "FSPACE" | is a load curve ID for assigning active frequencies.The abscissa is frequencies in the solutionand the ordinate is the active frequency in Hertz.FMINand FMAX are ignored.
        """ # nopep8
        return self._cards[0].get_value("fspace")

    @fspace.setter
    def fspace(self, value: float) -> None:
        self._cards[0].set_value("fspace", value)

    @property
    def fractn(self) -> int:
        """Get or set the Octave fraction. For example, FRACTN = 3 means 1⁄3 octave spacing.
        """ # nopep8
        return self._cards[0].get_value("fractn")

    @fractn.setter
    def fractn(self, value: int) -> None:
        self._cards[0].set_value("fractn", value)

