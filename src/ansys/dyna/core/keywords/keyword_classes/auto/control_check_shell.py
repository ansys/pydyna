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

class ControlCheckShell(KeywordBase):
    """DYNA CONTROL_CHECK_SHELL keyword"""

    keyword = "CONTROL"
    subkeyword = "CHECK_SHELL"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        10,
                        kwargs.get("pid", 0)
                    ),
                    Field(
                        "ifauto",
                        int,
                        10,
                        10,
                        kwargs.get("ifauto", 0)
                    ),
                    Field(
                        "convex",
                        int,
                        20,
                        10,
                        kwargs.get("convex", 1)
                    ),
                    Field(
                        "adpt",
                        int,
                        30,
                        10,
                        kwargs.get("adpt", 1)
                    ),
                    Field(
                        "aratio",
                        float,
                        40,
                        10,
                        kwargs.get("aratio", 0.25)
                    ),
                    Field(
                        "angke",
                        float,
                        50,
                        10,
                        kwargs.get("angke", 150.0)
                    ),
                    Field(
                        "smin",
                        float,
                        60,
                        10,
                        kwargs.get("smin", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> int:
        """Get or set the Part ID to be checked:
        EQ.0: Do not check
        GT.0: Part ID
        LT.0: Part set ID.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def ifauto(self) -> int:
        """Get or set the Flag to automatically correct bad elements:
        EQ.0: Write warning message only
        EQ.1 Fix bad element, write message.
        """ # nopep8
        return self._cards[0].get_value("ifauto")

    @ifauto.setter
    def ifauto(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ifauto must be one of {0,1}""")
        self._cards[0].set_value("ifauto", value)

    @property
    def convex(self) -> int:
        """Get or set the Check element convexity (internal angles less than 180 degrees)
        EQ.0: Do not check
        EQ.1: Check.
        """ # nopep8
        return self._cards[0].get_value("convex")

    @convex.setter
    def convex(self, value: int) -> None:
        if value not in [1, 0]:
            raise Exception("""convex must be one of {1,0}""")
        self._cards[0].set_value("convex", value)

    @property
    def adpt(self) -> int:
        """Get or set the Check adaptive constraints
        EQ.0: Do not check
        EQ.1: Check
        """ # nopep8
        return self._cards[0].get_value("adpt")

    @adpt.setter
    def adpt(self, value: int) -> None:
        if value not in [1, 0]:
            raise Exception("""adpt must be one of {1,0}""")
        self._cards[0].set_value("adpt", value)

    @property
    def aratio(self) -> float:
        """Get or set the Minimum allowable aspect ratio. Elements which do not meet minimum aspect ratio test will be treated according to IFAUTO above
        """ # nopep8
        return self._cards[0].get_value("aratio")

    @aratio.setter
    def aratio(self, value: float) -> None:
        self._cards[0].set_value("aratio", value)

    @property
    def angke(self) -> float:
        """Get or set the Maximum allowable internal angle. Elements which fail this test will be treated according to IFAUTO above.
        """ # nopep8
        return self._cards[0].get_value("angke")

    @angke.setter
    def angke(self, value: float) -> None:
        self._cards[0].set_value("angke", value)

    @property
    def smin(self) -> float:
        """Get or set the Minimum element size. Elements which fail this test will be treated according to IFAUTO above
        """ # nopep8
        return self._cards[0].get_value("smin")

    @smin.setter
    def smin(self, value: float) -> None:
        self._cards[0].set_value("smin", value)

