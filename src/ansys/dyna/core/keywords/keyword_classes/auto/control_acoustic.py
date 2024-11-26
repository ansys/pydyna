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

class ControlAcoustic(KeywordBase):
    """DYNA CONTROL_ACOUSTIC keyword"""

    keyword = "CONTROL"
    subkeyword = "ACOUSTIC"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "macdvp",
                        int,
                        0,
                        10,
                        kwargs.get("macdvp", 0)
                    ),
                ],
            ),
        ]

    @property
    def macdvp(self) -> int:
        """Get or set the Calculate the nodal displacements and velocities of *MAT_‌ACOUSTIC volume elements for inclusion in d3plot and time-history files.
        EQ.0:	Acoustic nodal motions will not be calculated
        EQ.1 : Acoustic nodal motions will be calculated
        """ # nopep8
        return self._cards[0].get_value("macdvp")

    @macdvp.setter
    def macdvp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""macdvp must be one of {0,1}""")
        self._cards[0].set_value("macdvp", value)

