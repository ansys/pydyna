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

class ControlConstranined(KeywordBase):
    """DYNA CONTROL_CONSTRANINED keyword"""

    keyword = "CONTROL"
    subkeyword = "CONSTRANINED"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "sprchk",
                        int,
                        0,
                        10,
                        kwargs.get("sprchk", 0)
                    ),
                ],
            ),
        ]

    @property
    def sprchk(self) -> int:
        """Get or set the SPR2/SPR3 initialization check:
        EQ.0:	automatically increase search radius to find enough nodes(default)
        EQ.1 : same as 0 but also write a warning
        EQ.2 : error termination if not enough nodes found immediately
        """ # nopep8
        return self._cards[0].get_value("sprchk")

    @sprchk.setter
    def sprchk(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""sprchk must be one of {0,1,2}""")
        self._cards[0].set_value("sprchk", value)

