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

class IcfdDefineWaveDamping(KeywordBase):
    """DYNA ICFD_DEFINE_WAVE_DAMPING keyword"""

    keyword = "ICFD"
    subkeyword = "DEFINE_WAVE_DAMPING"

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
                        kwargs.get("pid")
                    ),
                    Field(
                        "nid",
                        int,
                        10,
                        10,
                        kwargs.get("nid")
                    ),
                    Field(
                        "l",
                        float,
                        20,
                        10,
                        kwargs.get("l")
                    ),
                    Field(
                        "f1",
                        float,
                        30,
                        10,
                        kwargs.get("f1")
                    ),
                    Field(
                        "f2",
                        float,
                        40,
                        10,
                        kwargs.get("f2")
                    ),
                    Field(
                        "n",
                        int,
                        50,
                        10,
                        kwargs.get("n")
                    ),
                    Field(
                        "lcid",
                        int,
                        60,
                        10,
                        kwargs.get("lcid")
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Point ID defining the start of the damping layer.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Normal ID defined using ICFD_DEFINE_POINT and pointing to the outgoing direction of the damping layer.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[0].set_value("nid", value)

    @property
    def l(self) -> typing.Optional[float]:
        """Get or set the Length of damping layer. If no is value specified, the damping layer will have a length corresponding to five element lengths
        """ # nopep8
        return self._cards[0].get_value("l")

    @l.setter
    def l(self, value: float) -> None:
        self._cards[0].set_value("l", value)

    @property
    def f1(self) -> typing.Optional[float]:
        """Get or set the Linear and quadratic damping factor terms.
        """ # nopep8
        return self._cards[0].get_value("f1")

    @f1.setter
    def f1(self, value: float) -> None:
        self._cards[0].set_value("f1", value)

    @property
    def f2(self) -> typing.Optional[float]:
        """Get or set the Linear and quadratic damping factor terms.
        """ # nopep8
        return self._cards[0].get_value("f2")

    @f2.setter
    def f2(self, value: float) -> None:
        self._cards[0].set_value("f2", value)

    @property
    def n(self) -> typing.Optional[int]:
        """Get or set the Damping term factor.
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: int) -> None:
        self._cards[0].set_value("n", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID acting as temporal scale factor on damping term.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

