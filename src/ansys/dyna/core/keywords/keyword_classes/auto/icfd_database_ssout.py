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

class IcfdDatabaseSsout(KeywordBase):
    """DYNA ICFD_DATABASE_SSOUT keyword"""

    keyword = "ICFD"
    subkeyword = "DATABASE_SSOUT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "out",
                        int,
                        0,
                        10,
                        kwargs.get("out", 0)
                    ),
                    Field(
                        "outdt",
                        int,
                        10,
                        10,
                        kwargs.get("outdt", 0)
                    ),
                    Field(
                        "lcidsf",
                        int,
                        20,
                        10,
                        kwargs.get("lcidsf")
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        50,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "poff",
                        float,
                        70,
                        10,
                        kwargs.get("poff", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def out(self) -> int:
        """Get or set the Determines if the solver should retrieve the pressure loads and how to output it:
        EQ.0:	Inactive
        EQ.1 : The fluid solver will collect the segment sets (see * SET_SEGMENT) that are part of a FSI boundary and retrieve the pressure for subsequent print out in icfd_pressegand icfd_lcsegid
        """ # nopep8
        return self._cards[0].get_value("out")

    @out.setter
    def out(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""out must be one of {0,1}""")
        self._cards[0].set_value("out", value)

    @property
    def outdt(self) -> int:
        """Get or set the Frequency of the pressure extraction. If left as 0., the solver will extract the pressure of the fluid on the FSI boundary at every timestep which is (not recommended due to its high memory and calculation cost).
        """ # nopep8
        return self._cards[0].get_value("outdt")

    @outdt.setter
    def outdt(self, value: int) -> None:
        self._cards[0].set_value("outdt", value)

    @property
    def lcidsf(self) -> typing.Optional[int]:
        """Get or set the Option load curve ID to apply a scale factor on the fluid pressure output.
        """ # nopep8
        return self._cards[0].get_value("lcidsf")

    @lcidsf.setter
    def lcidsf(self, value: int) -> None:
        self._cards[0].set_value("lcidsf", value)

    @property
    def poff(self) -> float:
        """Get or set the Optional pressure offset on the fluid pressure output
        """ # nopep8
        return self._cards[0].get_value("poff")

    @poff.setter
    def poff(self, value: float) -> None:
        self._cards[0].set_value("poff", value)

