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

class MatSpringNonlinearElastic(KeywordBase):
    """DYNA MAT_SPRING_NONLINEAR_ELASTIC keyword"""

    keyword = "MAT"
    subkeyword = "SPRING_NONLINEAR_ELASTIC"
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
                        "lcd",
                        int,
                        10,
                        10,
                        kwargs.get("lcd")
                    ),
                    Field(
                        "lcr",
                        int,
                        20,
                        10,
                        kwargs.get("lcr", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatSpringNonlinearElastic.option_specs[0],
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
    def lcd(self) -> typing.Optional[int]:
        """Get or set the Load curve ID (see *DEFINE_CURVE) describing force as a function of displacement or moment as a function of rotation relationship. The load curve must define the response in the negative and positive quadrants and pass through point Image.  Negative data point(s) must come first in the curve definition, where negative values represent compression in the case of a translational spring.
        LCD may also be a table ID (see *DEFINE_TABLE). The table gives for each loading rate a load curve ID defining the force-displacement (or moment-rotation) curve. Values between the data points are computed by linear interpolation. If a table ID is specified, LCR will be ignored.
        """ # nopep8
        return self._cards[0].get_value("lcd")

    @lcd.setter
    def lcd(self, value: int) -> None:
        self._cards[0].set_value("lcd", value)

    @property
    def lcr(self) -> int:
        """Get or set the Optional load curve describing scale factor on force or moment as a function of relative velocity or rotational velocity. The load curve most define the response in the negative and positive quadrants and pass through point (0,0).
        """ # nopep8
        return self._cards[0].get_value("lcr")

    @lcr.setter
    def lcr(self, value: int) -> None:
        self._cards[0].set_value("lcr", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

