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

class CeseDatabaseFluxavg(KeywordBase):
    """DYNA CESE_DATABASE_FLUXAVG keyword"""

    keyword = "CESE"
    subkeyword = "DATABASE_FLUXAVG"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "outlv",
                        int,
                        0,
                        10,
                        kwargs.get("outlv", 0)
                    ),
                    Field(
                        "dtout",
                        float,
                        10,
                        10,
                        kwargs.get("dtout", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "elsid",
                        int,
                        0,
                        10,
                        kwargs.get("elsid")
                    ),
                ],
            ),
        ]

    @property
    def outlv(self) -> int:
        """Get or set the Determines if the output file should be dumped
        EQ.0: No output file is generated.
        EQ.1: The output file giving the average fluxes is generated.
        """ # nopep8
        return self._cards[0].get_value("outlv")

    @outlv.setter
    def outlv(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""outlv must be one of {0,1}""")
        self._cards[0].set_value("outlv", value)

    @property
    def dtout(self) -> float:
        """Get or set the Time interval to print the output. If DTOUT is equal to 0.0, then the CESE timestep will be used.
        """ # nopep8
        return self._cards[0].get_value("dtout")

    @dtout.setter
    def dtout(self, value: float) -> None:
        self._cards[0].set_value("dtout", value)

    @property
    def elsid(self) -> typing.Optional[int]:
        """Get or set the Solid Elements Set ID.
        """ # nopep8
        return self._cards[1].get_value("elsid")

    @elsid.setter
    def elsid(self, value: int) -> None:
        self._cards[1].set_value("elsid", value)

