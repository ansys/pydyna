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

class DampingGlobal(KeywordBase):
    """DYNA DAMPING_GLOBAL keyword"""

    keyword = "DAMPING"
    subkeyword = "GLOBAL"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "lcid",
                        int,
                        0,
                        10,
                        kwargs.get("lcid", 0)
                    ),
                    Field(
                        "valdmp",
                        float,
                        10,
                        10,
                        kwargs.get("valdmp", 0.0)
                    ),
                    Field(
                        "stx",
                        float,
                        20,
                        10,
                        kwargs.get("stx", 0.0)
                    ),
                    Field(
                        "sty",
                        float,
                        30,
                        10,
                        kwargs.get("sty", 0.0)
                    ),
                    Field(
                        "stz",
                        float,
                        40,
                        10,
                        kwargs.get("stz", 0.0)
                    ),
                    Field(
                        "srx",
                        float,
                        50,
                        10,
                        kwargs.get("srx", 0.0)
                    ),
                    Field(
                        "sry",
                        float,
                        60,
                        10,
                        kwargs.get("sry", 0.0)
                    ),
                    Field(
                        "srz",
                        float,
                        70,
                        10,
                        kwargs.get("srz", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def lcid(self) -> int:
        """Get or set the Load curve ID which specifies node system damping:
        EQ.0: a contact damping factor as defined by VALDMP is used,
        EQ.n: system damping is given by load curve n. The damping force applied to each node is f=-d(t)mv, where d(t) is defined by load curve n.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def valdmp(self) -> float:
        """Get or set the System damping constant. Only used if LCID is set to zero.
        """ # nopep8
        return self._cards[0].get_value("valdmp")

    @valdmp.setter
    def valdmp(self, value: float) -> None:
        self._cards[0].set_value("valdmp", value)

    @property
    def stx(self) -> float:
        """Get or set the Scale factor on global x translational damping forces.
        """ # nopep8
        return self._cards[0].get_value("stx")

    @stx.setter
    def stx(self, value: float) -> None:
        self._cards[0].set_value("stx", value)

    @property
    def sty(self) -> float:
        """Get or set the Scale factor on global y translational damping forces.
        """ # nopep8
        return self._cards[0].get_value("sty")

    @sty.setter
    def sty(self, value: float) -> None:
        self._cards[0].set_value("sty", value)

    @property
    def stz(self) -> float:
        """Get or set the Scale factor on global z translational damping forces.
        """ # nopep8
        return self._cards[0].get_value("stz")

    @stz.setter
    def stz(self, value: float) -> None:
        self._cards[0].set_value("stz", value)

    @property
    def srx(self) -> float:
        """Get or set the Scale factor on global x rotational damping moments.
        """ # nopep8
        return self._cards[0].get_value("srx")

    @srx.setter
    def srx(self, value: float) -> None:
        self._cards[0].set_value("srx", value)

    @property
    def sry(self) -> float:
        """Get or set the Scale factor on global y rotational damping moments.
        """ # nopep8
        return self._cards[0].get_value("sry")

    @sry.setter
    def sry(self, value: float) -> None:
        self._cards[0].set_value("sry", value)

    @property
    def srz(self) -> float:
        """Get or set the Scale factor on global z rotational damping moments.
        """ # nopep8
        return self._cards[0].get_value("srz")

    @srz.setter
    def srz(self, value: float) -> None:
        self._cards[0].set_value("srz", value)

