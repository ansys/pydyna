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

class IncludeNastran(KeywordBase):
    """DYNA INCLUDE_NASTRAN keyword"""

    keyword = "INCLUDE"
    subkeyword = "NASTRAN"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
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
                        "beamdf",
                        int,
                        0,
                        10,
                        kwargs.get("beamdf", 2)
                    ),
                    Field(
                        "shelldf",
                        int,
                        10,
                        10,
                        kwargs.get("shelldf", 21)
                    ),
                    Field(
                        "soliddf",
                        int,
                        20,
                        10,
                        kwargs.get("soliddf", 18)
                    ),
                ],
            ),
        ]

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the File name of file to be included in this keyword file.
        Maximum 80 charcters. If the STAMPED_PART option is active, this is the DYNAIN file containing the results from metal stamping.
        """ # nopep8
        return self._cards[0].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        self._cards[0].set_value("filename", value)

    @property
    def beamdf(self) -> int:
        """Get or set the LS-DYNA beam element type. Defaults to type 2.
        """ # nopep8
        return self._cards[1].get_value("beamdf")

    @beamdf.setter
    def beamdf(self, value: int) -> None:
        self._cards[1].set_value("beamdf", value)

    @property
    def shelldf(self) -> int:
        """Get or set the LS-DYNA shell element type. Defaults to type 21.
        """ # nopep8
        return self._cards[1].get_value("shelldf")

    @shelldf.setter
    def shelldf(self, value: int) -> None:
        self._cards[1].set_value("shelldf", value)

    @property
    def soliddf(self) -> int:
        """Get or set the LS-DYNA solid element type. Defaults to type 18.
        """ # nopep8
        return self._cards[1].get_value("soliddf")

    @soliddf.setter
    def soliddf(self, value: int) -> None:
        self._cards[1].set_value("soliddf", value)

