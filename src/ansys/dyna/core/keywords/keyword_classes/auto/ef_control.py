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

class EfControl(KeywordBase):
    """DYNA EF_CONTROL keyword"""

    keyword = "EF"
    subkeyword = "CONTROL"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nphton",
                        int,
                        0,
                        10,
                        kwargs.get("nphton")
                    ),
                    Field(
                        "nrefs",
                        int,
                        10,
                        10,
                        kwargs.get("nrefs")
                    ),
                    Field(
                        "nwarns",
                        int,
                        20,
                        10,
                        kwargs.get("nwarns")
                    ),
                    Field(
                        "nlost",
                        int,
                        30,
                        10,
                        kwargs.get("nlost")
                    ),
                    Field(
                        "nloops",
                        int,
                        40,
                        10,
                        kwargs.get("nloops")
                    ),
                    Field(
                        "errodef",
                        float,
                        50,
                        10,
                        kwargs.get("errodef")
                    ),
                    Field(
                        "inseed",
                        int,
                        60,
                        10,
                        kwargs.get("inseed")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nphton",
                        int,
                        0,
                        10,
                        kwargs.get("nphton")
                    ),
                    Field(
                        "nrefs",
                        int,
                        10,
                        10,
                        kwargs.get("nrefs")
                    ),
                    Field(
                        "nwarns",
                        int,
                        20,
                        10,
                        kwargs.get("nwarns")
                    ),
                    Field(
                        "nlost",
                        int,
                        30,
                        10,
                        kwargs.get("nlost")
                    ),
                ],
            ),
        ]

    @property
    def nphton(self) -> typing.Optional[int]:
        """Get or set the The base number of photons emitted per band per surface per convergence loop.  Note that NPHT from *BOUNDARY_‌RADIATION_‌SET_‌EF_‌CALCULATE also effects the number of photons emitted per surface per band per convergence loop
        """ # nopep8
        return self._cards[0].get_value("nphton")

    @nphton.setter
    def nphton(self, value: int) -> None:
        self._cards[0].set_value("nphton", value)

    @property
    def nrefs(self) -> typing.Optional[int]:
        """Get or set the The maximum number of reflections allowed per photon before LS-DYNA issues a warning
        """ # nopep8
        return self._cards[0].get_value("nrefs")

    @nrefs.setter
    def nrefs(self, value: int) -> None:
        self._cards[0].set_value("nrefs", value)

    @property
    def nwarns(self) -> typing.Optional[int]:
        """Get or set the The maximum number of warnings allowed per surface before the run is aborted
        """ # nopep8
        return self._cards[0].get_value("nwarns")

    @nwarns.setter
    def nwarns(self, value: int) -> None:
        self._cards[0].set_value("nwarns", value)

    @property
    def nlost(self) -> typing.Optional[int]:
        """Get or set the The maximum number of lost photons allowed per surface.  Round off error often causes the loss of photons, so this number ought not to be set too small (usually the default is reasonable).
        """ # nopep8
        return self._cards[0].get_value("nlost")

    @nlost.setter
    def nlost(self, value: int) -> None:
        self._cards[0].set_value("nlost", value)

    @property
    def nloops(self) -> typing.Optional[int]:
        """Get or set the This specifies the maximum number of convergence loops.  If the relative error obtained upon the completion of a run is not within the specified tolerances, LS-DYNA will rerun the model combining the results of all previous runs together with the results of the present run to obtain a more accurate result.  LS-DYNA will rerun the problem NLOOPS times to achieve error margins within the specified tolerances.  If the desired level of convergence is not obtained within NLOOPS iterations LS-DYNA error terminates.
        """ # nopep8
        return self._cards[0].get_value("nloops")

    @nloops.setter
    def nloops(self, value: int) -> None:
        self._cards[0].set_value("nloops", value)

    @property
    def errodef(self) -> typing.Optional[float]:
        """Get or set the Specifies that tolerance for convergence of the surface exchange fractions.  This may be overridden on a surface by surface basis with the ERRMAX setting. (see *BOUNDARY_‌RADIATION_‌SET_‌EF_‌CALCULATE)
        """ # nopep8
        return self._cards[0].get_value("errodef")

    @errodef.setter
    def errodef(self, value: float) -> None:
        self._cards[0].set_value("errodef", value)

    @property
    def inseed(self) -> typing.Optional[int]:
        """Get or set the Tells LS-DYNA how to obtain an initial seed for the Monte Carlo random number generator.
        EQ.0:	use date and time.
        GT.0 : use INSEED as seed.
        LT.0 : use a default seed.
        """ # nopep8
        return self._cards[0].get_value("inseed")

    @inseed.setter
    def inseed(self, value: int) -> None:
        self._cards[0].set_value("inseed", value)

    @property
    def nphton(self) -> typing.Optional[int]:
        """Get or set the The base number of photons emitted per band per surface per convergence loop.  Note that NPHT from *BOUNDARY_‌RADIATION_‌SET_‌EF_‌CALCULATE also effects the number of photons emitted per surface per band per convergence loop
        """ # nopep8
        return self._cards[1].get_value("nphton")

    @nphton.setter
    def nphton(self, value: int) -> None:
        self._cards[1].set_value("nphton", value)

    @property
    def nrefs(self) -> typing.Optional[int]:
        """Get or set the The maximum number of reflections allowed per photon before LS-DYNA issues a warning
        """ # nopep8
        return self._cards[1].get_value("nrefs")

    @nrefs.setter
    def nrefs(self, value: int) -> None:
        self._cards[1].set_value("nrefs", value)

    @property
    def nwarns(self) -> typing.Optional[int]:
        """Get or set the The maximum number of warnings allowed per surface before the run is aborted
        """ # nopep8
        return self._cards[1].get_value("nwarns")

    @nwarns.setter
    def nwarns(self, value: int) -> None:
        self._cards[1].set_value("nwarns", value)

    @property
    def nlost(self) -> typing.Optional[int]:
        """Get or set the The maximum number of lost photons allowed per surface.  Round off error often causes the loss of photons, so this number ought not to be set too small (usually the default is reasonable).
        """ # nopep8
        return self._cards[1].get_value("nlost")

    @nlost.setter
    def nlost(self, value: int) -> None:
        self._cards[1].set_value("nlost", value)

