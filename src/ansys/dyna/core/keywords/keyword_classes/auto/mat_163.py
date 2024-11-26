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

class Mat163(KeywordBase):
    """DYNA MAT_163 keyword"""

    keyword = "MAT"
    subkeyword = "163"
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
                        "ro",
                        float,
                        10,
                        10,
                        kwargs.get("ro")
                    ),
                    Field(
                        "e",
                        float,
                        20,
                        10,
                        kwargs.get("e")
                    ),
                    Field(
                        "pr",
                        float,
                        30,
                        10,
                        kwargs.get("pr")
                    ),
                    Field(
                        "tid",
                        int,
                        40,
                        10,
                        kwargs.get("tid")
                    ),
                    Field(
                        "tsc",
                        float,
                        50,
                        10,
                        kwargs.get("tsc")
                    ),
                    Field(
                        "damp",
                        float,
                        60,
                        10,
                        kwargs.get("damp")
                    ),
                    Field(
                        "ncycle",
                        float,
                        70,
                        10,
                        kwargs.get("ncycle")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "srclmt",
                        float,
                        0,
                        10,
                        kwargs.get("srclmt")
                    ),
                    Field(
                        "srflag",
                        int,
                        10,
                        10,
                        kwargs.get("srflag")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat163.option_specs[0],
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
        """Get or set the Material identification. A unique number has to be chosen.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def tid(self) -> typing.Optional[int]:
        """Get or set the Table ID defining yield stress versus volumetric strain, y, at different strain rates.
        """ # nopep8
        return self._cards[0].get_value("tid")

    @tid.setter
    def tid(self, value: int) -> None:
        self._cards[0].set_value("tid", value)

    @property
    def tsc(self) -> typing.Optional[float]:
        """Get or set the Tensile stress cutoff. A nonzero, positive value is strongly recommended for realistic behavior.
        """ # nopep8
        return self._cards[0].get_value("tsc")

    @tsc.setter
    def tsc(self, value: float) -> None:
        self._cards[0].set_value("tsc", value)

    @property
    def damp(self) -> typing.Optional[float]:
        """Get or set the Rate senitivity via damping coefficient (.05<recommended value<.50).
        """ # nopep8
        return self._cards[0].get_value("damp")

    @damp.setter
    def damp(self, value: float) -> None:
        self._cards[0].set_value("damp", value)

    @property
    def ncycle(self) -> typing.Optional[float]:
        """Get or set the Number of cycles to determine the average volumetric strain rate.
        """ # nopep8
        return self._cards[0].get_value("ncycle")

    @ncycle.setter
    def ncycle(self, value: float) -> None:
        self._cards[0].set_value("ncycle", value)

    @property
    def srclmt(self) -> typing.Optional[float]:
        """Get or set the Strain rate change limit.
        """ # nopep8
        return self._cards[1].get_value("srclmt")

    @srclmt.setter
    def srclmt(self, value: float) -> None:
        self._cards[1].set_value("srclmt", value)

    @property
    def srflag(self) -> typing.Optional[int]:
        """Get or set the The strain rate in the table may be the true strain rate (SFLAG=0) or the engineering strain rate (SFLAG=1).
        """ # nopep8
        return self._cards[1].get_value("srflag")

    @srflag.setter
    def srflag(self, value: int) -> None:
        self._cards[1].set_value("srflag", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

