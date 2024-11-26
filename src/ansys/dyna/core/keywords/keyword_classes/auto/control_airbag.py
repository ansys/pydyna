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

class ControlAirbag(KeywordBase):
    """DYNA CONTROL_AIRBAG keyword"""

    keyword = "CONTROL"
    subkeyword = "AIRBAG"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ckerr",
                        int,
                        0,
                        10,
                        kwargs.get("ckerr", 0)
                    ),
                ],
            ),
        ]

    @property
    def ckerr(self) -> int:
        """Get or set the Flag to check and report of CV airbag segments for the input
        a.open(free) edge
        b.segment should come from SHELL / SOLID element
        EQ.0:	Do not check(default).
        EQ.1 : Check for free edges,and if there is a free edge in the airbag surface, output nodes of the free edge to d3hsp, issue a warning,and continue the run.Check for segment,and if there is a segment not from an element, output the segment to d3hsp, issue a error messageand terminate the run.
        EQ.2 : Check for free edgesand if there is a free edge in the airbag surface, output nodes of the free edge to d3hsp, issue an error,and terminate the run.Check for segment,and if there is a segment not from an element, output the segment to d3hsp, issue a error messageand terminate the run.
        """ # nopep8
        return self._cards[0].get_value("ckerr")

    @ckerr.setter
    def ckerr(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""ckerr must be one of {0,1,2}""")
        self._cards[0].set_value("ckerr", value)

