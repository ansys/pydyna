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

class Mat138(KeywordBase):
    """DYNA MAT_138 keyword"""

    keyword = "MAT"
    subkeyword = "138"
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
                        "roflg",
                        int,
                        20,
                        10,
                        kwargs.get("roflg", 0)
                    ),
                    Field(
                        "intfail",
                        float,
                        30,
                        10,
                        kwargs.get("intfail")
                    ),
                    Field(
                        "en",
                        float,
                        40,
                        10,
                        kwargs.get("en")
                    ),
                    Field(
                        "et",
                        float,
                        50,
                        10,
                        kwargs.get("et")
                    ),
                    Field(
                        "gic",
                        float,
                        60,
                        10,
                        kwargs.get("gic")
                    ),
                    Field(
                        "giic",
                        float,
                        70,
                        10,
                        kwargs.get("giic")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xmu",
                        float,
                        0,
                        10,
                        kwargs.get("xmu")
                    ),
                    Field(
                        "t",
                        float,
                        10,
                        10,
                        kwargs.get("t")
                    ),
                    Field(
                        "s",
                        float,
                        20,
                        10,
                        kwargs.get("s")
                    ),
                    Field(
                        "und",
                        float,
                        30,
                        10,
                        kwargs.get("und")
                    ),
                    Field(
                        "utd",
                        float,
                        40,
                        10,
                        kwargs.get("utd")
                    ),
                    Field(
                        "gamma",
                        float,
                        50,
                        10,
                        kwargs.get("gamma", 1.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat138.option_specs[0],
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
        """Get or set the Material identification. A unique number has to be used.
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
    def roflg(self) -> int:
        """Get or set the Flag stating whether density is specified per unit area or volume:
        EQ.0:	Specified density is per unit volume(default).
        EQ.1 : Specified density is per unit area for controlling the mass of cohesive elements with an initial volume of zero.
        """ # nopep8
        return self._cards[0].get_value("roflg")

    @roflg.setter
    def roflg(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""roflg must be one of {0,1}""")
        self._cards[0].set_value("roflg", value)

    @property
    def intfail(self) -> typing.Optional[float]:
        """Get or set the The number of integration points required for the cohesive element to be deleted. The value of INTFAIL may range from 1 to 4 with 1 the recommended value.
        LT.0.0:	Employs a Newton - Cotes integration scheme and the element will be deleted when | INTFAIL | integration points have failed.
        EQ.0.0 : Employs a Newton - Cotes integration scheme and the element will not be deleted even if it satisfies the failure criterion.
        GT.0.0 : Employs a Gauss integration scheme and the element will be deleted when INTFAIL integration points have failed.
        """ # nopep8
        return self._cards[0].get_value("intfail")

    @intfail.setter
    def intfail(self, value: float) -> None:
        self._cards[0].set_value("intfail", value)

    @property
    def en(self) -> typing.Optional[float]:
        """Get or set the The stiffness normal to the plane of the cohesive element.
        """ # nopep8
        return self._cards[0].get_value("en")

    @en.setter
    def en(self, value: float) -> None:
        self._cards[0].set_value("en", value)

    @property
    def et(self) -> typing.Optional[float]:
        """Get or set the The stiffness in the plane of the cohesive element
        """ # nopep8
        return self._cards[0].get_value("et")

    @et.setter
    def et(self, value: float) -> None:
        self._cards[0].set_value("et", value)

    @property
    def gic(self) -> typing.Optional[float]:
        """Get or set the Energy release rate for mode I.
        LT.0.0:	Load curve ID = (-GIC) which defines energy release rate for mode I as a function of element size.)
        """ # nopep8
        return self._cards[0].get_value("gic")

    @gic.setter
    def gic(self, value: float) -> None:
        self._cards[0].set_value("gic", value)

    @property
    def giic(self) -> typing.Optional[float]:
        """Get or set the Energy release rate for mode II
        LT.0.0:	Load curve ID = (-GIIC) which defines energy release rate for mode II as a function of element size.)
        """ # nopep8
        return self._cards[0].get_value("giic")

    @giic.setter
    def giic(self, value: float) -> None:
        self._cards[0].set_value("giic", value)

    @property
    def xmu(self) -> typing.Optional[float]:
        """Get or set the Exponent of the mixed mode criteria (see remarks below)
        """ # nopep8
        return self._cards[1].get_value("xmu")

    @xmu.setter
    def xmu(self, value: float) -> None:
        self._cards[1].set_value("xmu", value)

    @property
    def t(self) -> typing.Optional[float]:
        """Get or set the Peak traction in normal direction
        """ # nopep8
        return self._cards[1].get_value("t")

    @t.setter
    def t(self, value: float) -> None:
        self._cards[1].set_value("t", value)

    @property
    def s(self) -> typing.Optional[float]:
        """Get or set the Peak traction in tangential direction
        """ # nopep8
        return self._cards[1].get_value("s")

    @s.setter
    def s(self, value: float) -> None:
        self._cards[1].set_value("s", value)

    @property
    def und(self) -> typing.Optional[float]:
        """Get or set the Ultimate displacement in the normal direction
        """ # nopep8
        return self._cards[1].get_value("und")

    @und.setter
    def und(self, value: float) -> None:
        self._cards[1].set_value("und", value)

    @property
    def utd(self) -> typing.Optional[float]:
        """Get or set the Ultimate displacement in the tangential direction
        """ # nopep8
        return self._cards[1].get_value("utd")

    @utd.setter
    def utd(self, value: float) -> None:
        self._cards[1].set_value("utd", value)

    @property
    def gamma(self) -> float:
        """Get or set the Additional exponent for Benzeggagh-Kenane law (default = 1.0)
        """ # nopep8
        return self._cards[1].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        self._cards[1].set_value("gamma", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)


class MatCohesiveMixedMode(Mat138):
    subkeyword = "COHESIVE_MIXED_MODE"
