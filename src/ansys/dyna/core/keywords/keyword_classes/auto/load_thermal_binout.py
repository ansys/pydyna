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

class LoadThermalBinout(KeywordBase):
    """DYNA LOAD_THERMAL_BINOUT keyword"""

    keyword = "LOAD"
    subkeyword = "THERMAL_BINOUT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "deftemp",
                        float,
                        0,
                        10,
                        kwargs.get("deftemp", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "filename",
                        str,
                        0,
                        80,
                        kwargs.get("filename")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "startt",
                        float,
                        0,
                        10,
                        kwargs.get("startt", 0.0)
                    ),
                    Field(
                        "tsf",
                        float,
                        10,
                        10,
                        kwargs.get("tsf", 1.0)
                    ),
                ],
            ),
        ]

    @property
    def deftemp(self) -> float:
        """Get or set the Default temperature that is applied to nodes no temperature information is provided in the binout file(s) for.
        """ # nopep8
        return self._cards[0].get_value("deftemp")

    @deftemp.setter
    def deftemp(self, value: float) -> None:
        self._cards[0].set_value("deftemp", value)

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the Name of the file that contains the temperature information.
        """ # nopep8
        return self._cards[1].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        self._cards[1].set_value("filename", value)

    @property
    def startt(self) -> float:
        """Get or set the Start time Tstart for the temperature mapping.  Until this point in time the nodal temperature for the first step provided in the file is used.
        """ # nopep8
        return self._cards[2].get_value("startt")

    @startt.setter
    def startt(self, value: float) -> None:
        self._cards[2].set_value("startt", value)

    @property
    def tsf(self) -> float:
        """Get or set the Time scale factor that represents the speed-up factor of the mechanical analysis to the previous thermal analysis.
        """ # nopep8
        return self._cards[2].get_value("tsf")

    @tsf.setter
    def tsf(self, value: float) -> None:
        self._cards[2].set_value("tsf", value)

