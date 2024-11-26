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

class DatabaseRve(KeywordBase):
    """DYNA DATABASE_RVE keyword"""

    keyword = "DATABASE"
    subkeyword = "RVE"

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
                        "bina",
                        int,
                        10,
                        10,
                        kwargs.get("bina", 0)
                    ),
                ],
            ),
        ]

    @property
    def dt(self) -> float:
        """Get or set the Time interval for the output of RVE homogenization results to the rveout file.
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[0].set_value("dt", value)

    @property
    def bina(self) -> int:
        """Get or set the Type of the output file:
        EQ. 0:	ASCII database file named “rveout”.
        EQ. 1 : LS - DYNA binary database.
        """ # nopep8
        return self._cards[0].get_value("bina")

    @bina.setter
    def bina(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""bina must be one of {0,1}""")
        self._cards[0].set_value("bina", value)

