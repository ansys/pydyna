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

class ControlSubcycle(KeywordBase):
    """DYNA CONTROL_SUBCYCLE keyword"""

    keyword = "CONTROL"
    subkeyword = "SUBCYCLE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "k",
                        int,
                        0,
                        10,
                        kwargs.get("k", 16)
                    ),
                    Field(
                        "l",
                        int,
                        10,
                        10,
                        kwargs.get("l", 1)
                    ),
                ],
            ),
        ]

    @property
    def k(self) -> int:
        """Get or set the Ratio between the largest and smallest time step
        """ # nopep8
        return self._cards[0].get_value("k")

    @k.setter
    def k(self, value: int) -> None:
        if value not in [16, 1, 2, 4, 8, 32, 64]:
            raise Exception("""k must be one of {16,1,2,4,8,32,64}""")
        self._cards[0].set_value("k", value)

    @property
    def l(self) -> int:
        """Get or set the The relative time step at which external forces such as contacts and loads are calculated
        """ # nopep8
        return self._cards[0].get_value("l")

    @l.setter
    def l(self, value: int) -> None:
        if value not in [1, 2, 4, 8, 16, 32, 64]:
            raise Exception("""l must be one of {1,2,4,8,16,32,64}""")
        self._cards[0].set_value("l", value)

