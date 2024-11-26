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

class DefineCurveSmooth(KeywordBase):
    """DYNA DEFINE_CURVE_SMOOTH keyword"""

    keyword = "DEFINE"
    subkeyword = "CURVE_SMOOTH"
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
                        "lcid",
                        int,
                        0,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "sidr",
                        int,
                        10,
                        10,
                        kwargs.get("sidr", 0)
                    ),
                    Field(
                        "dist",
                        float,
                        20,
                        10,
                        kwargs.get("dist")
                    ),
                    Field(
                        "tstart",
                        float,
                        30,
                        10,
                        kwargs.get("tstart")
                    ),
                    Field(
                        "tend",
                        float,
                        40,
                        10,
                        kwargs.get("tend")
                    ),
                    Field(
                        "trise",
                        float,
                        50,
                        10,
                        kwargs.get("trise")
                    ),
                    Field(
                        "vmax",
                        float,
                        60,
                        10,
                        kwargs.get("vmax")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineCurveSmooth.option_specs[0],
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
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID. A unique number must be defined.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def sidr(self) -> int:
        """Get or set the Stress initialization by dynamic relaxation:
        EQ.0: Load curve used in transient analysis only or for other applications (default),
        EQ.1: Load curve used in stress initialization but not transient analysis,
        EQ.2: Load curve applies to both initialization and transient analysis.
        """ # nopep8
        return self._cards[0].get_value("sidr")

    @sidr.setter
    def sidr(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""sidr must be one of {0,1,2}""")
        self._cards[0].set_value("sidr", value)

    @property
    def dist(self) -> typing.Optional[float]:
        """Get or set the Total distance tool will travel (area under curve).
        """ # nopep8
        return self._cards[0].get_value("dist")

    @dist.setter
    def dist(self, value: float) -> None:
        self._cards[0].set_value("dist", value)

    @property
    def tstart(self) -> typing.Optional[float]:
        """Get or set the Time curve starts to rise.
        """ # nopep8
        return self._cards[0].get_value("tstart")

    @tstart.setter
    def tstart(self, value: float) -> None:
        self._cards[0].set_value("tstart", value)

    @property
    def tend(self) -> typing.Optional[float]:
        """Get or set the Time curve returns to zero.
        If TEND is nonzero, VMAX will be computed automatically to satisfy required travel distance DIST. Input either TEND or VMAX.
        """ # nopep8
        return self._cards[0].get_value("tend")

    @tend.setter
    def tend(self, value: float) -> None:
        self._cards[0].set_value("tend", value)

    @property
    def trise(self) -> typing.Optional[float]:
        """Get or set the Rise time.
        """ # nopep8
        return self._cards[0].get_value("trise")

    @trise.setter
    def trise(self, value: float) -> None:
        self._cards[0].set_value("trise", value)

    @property
    def vmax(self) -> typing.Optional[float]:
        """Get or set the Maximum velocity (maximum value of curve).
        If VMAX is nonzero, TEND will be computed automatically to satisfy required travel distance DIST. Input either TEND or VMAX.
        """ # nopep8
        return self._cards[0].get_value("vmax")

    @vmax.setter
    def vmax(self, value: float) -> None:
        self._cards[0].set_value("vmax", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

