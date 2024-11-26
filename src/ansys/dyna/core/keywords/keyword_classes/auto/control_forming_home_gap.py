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

class ControlFormingHomeGap(KeywordBase):
    """DYNA CONTROL_FORMING_HOME_GAP keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_HOME_GAP"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "psidu",
                        int,
                        0,
                        10,
                        kwargs.get("psidu")
                    ),
                    Field(
                        "psidl",
                        int,
                        10,
                        10,
                        kwargs.get("psidl")
                    ),
                    Field(
                        "gap",
                        float,
                        20,
                        10,
                        kwargs.get("gap")
                    ),
                    Field(
                        "mvinc",
                        float,
                        30,
                        10,
                        kwargs.get("mvinc")
                    ),
                    Field(
                        "istop",
                        int,
                        40,
                        10,
                        kwargs.get("istop", 0)
                    ),
                ],
            ),
        ]

    @property
    def psidu(self) -> typing.Optional[int]:
        """Get or set the Part set ID of the tools above the blank (upper tools)
        """ # nopep8
        return self._cards[0].get_value("psidu")

    @psidu.setter
    def psidu(self, value: int) -> None:
        self._cards[0].set_value("psidu", value)

    @property
    def psidl(self) -> typing.Optional[int]:
        """Get or set the Part set ID of the tools below the blank (lower tools)
        """ # nopep8
        return self._cards[0].get_value("psidl")

    @psidl.setter
    def psidl(self, value: int) -> None:
        self._cards[0].set_value("psidl", value)

    @property
    def gap(self) -> typing.Optional[float]:
        """Get or set the Minimum gap allowed between the upper and lower tools
        """ # nopep8
        return self._cards[0].get_value("gap")

    @gap.setter
    def gap(self, value: float) -> None:
        self._cards[0].set_value("gap", value)

    @property
    def mvinc(self) -> typing.Optional[float]:
        """Get or set the Incremental movement of tools from home position to starting position to check the gap
        """ # nopep8
        return self._cards[0].get_value("mvinc")

    @mvinc.setter
    def mvinc(self, value: float) -> None:
        self._cards[0].set_value("mvinc", value)

    @property
    def istop(self) -> int:
        """Get or set the How to proceed if the minimum gap found is less than GAP:
        EQ.0:	Output a warning message.Job continues
        EQ.1 : Terminate the job.
        """ # nopep8
        return self._cards[0].get_value("istop")

    @istop.setter
    def istop(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""istop must be one of {0,1}""")
        self._cards[0].set_value("istop", value)

