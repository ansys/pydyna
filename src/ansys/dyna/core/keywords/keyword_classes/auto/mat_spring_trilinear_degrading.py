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
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class MatSpringTrilinearDegrading(KeywordBase):
    """DYNA MAT_SPRING_TRILINEAR_DEGRADING keyword"""

    keyword = "MAT"
    subkeyword = "SPRING_TRILINEAR_DEGRADING"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "mid",
                        int,
                        0,
                        10,
                        kwargs.get("mid")
                    ),
                    Field(
                        "defl1",
                        float,
                        10,
                        10,
                        kwargs.get("defl1")
                    ),
                    Field(
                        "f1",
                        float,
                        20,
                        10,
                        kwargs.get("f1")
                    ),
                    Field(
                        "defl2",
                        float,
                        30,
                        10,
                        kwargs.get("defl2")
                    ),
                    Field(
                        "f2",
                        float,
                        40,
                        10,
                        kwargs.get("f2")
                    ),
                    Field(
                        "defl3",
                        float,
                        50,
                        10,
                        kwargs.get("defl3")
                    ),
                    Field(
                        "f3",
                        float,
                        60,
                        10,
                        kwargs.get("f3")
                    ),
                    Field(
                        "fflag",
                        float,
                        70,
                        10,
                        kwargs.get("fflag")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatSpringTrilinearDegrading.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material number. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def defl1(self) -> typing.Optional[float]:
        """Get or set the Deflection at point where concrete cracking occurs.
        """ # nopep8
        return self._cards[0].get_value("defl1")

    @defl1.setter
    def defl1(self, value: float) -> None:
        self._cards[0].set_value("defl1", value)

    @property
    def f1(self) -> typing.Optional[float]:
        """Get or set the Force corresponding to DEFL1
        """ # nopep8
        return self._cards[0].get_value("f1")

    @f1.setter
    def f1(self, value: float) -> None:
        self._cards[0].set_value("f1", value)

    @property
    def defl2(self) -> typing.Optional[float]:
        """Get or set the Deflection at point where reinforcement yields
        """ # nopep8
        return self._cards[0].get_value("defl2")

    @defl2.setter
    def defl2(self, value: float) -> None:
        self._cards[0].set_value("defl2", value)

    @property
    def f2(self) -> typing.Optional[float]:
        """Get or set the Force corresponding to DEFL2
        """ # nopep8
        return self._cards[0].get_value("f2")

    @f2.setter
    def f2(self, value: float) -> None:
        self._cards[0].set_value("f2", value)

    @property
    def defl3(self) -> typing.Optional[float]:
        """Get or set the Deflection at complete failure
        """ # nopep8
        return self._cards[0].get_value("defl3")

    @defl3.setter
    def defl3(self, value: float) -> None:
        self._cards[0].set_value("defl3", value)

    @property
    def f3(self) -> typing.Optional[float]:
        """Get or set the Force corresponding to DEFL3
        """ # nopep8
        return self._cards[0].get_value("f3")

    @f3.setter
    def f3(self, value: float) -> None:
        self._cards[0].set_value("f3", value)

    @property
    def fflag(self) -> typing.Optional[float]:
        """Get or set the Failure flag.
        """ # nopep8
        return self._cards[0].get_value("fflag")

    @fflag.setter
    def fflag(self, value: float) -> None:
        self._cards[0].set_value("fflag", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

