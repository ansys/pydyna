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

class Mat186(KeywordBase):
    """DYNA MAT_186 keyword"""

    keyword = "MAT"
    subkeyword = "186"
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
                        "tes",
                        float,
                        40,
                        10,
                        kwargs.get("tes")
                    ),
                    Field(
                        "tslc",
                        int,
                        50,
                        10,
                        kwargs.get("tslc")
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
                        "stfsf",
                        float,
                        30,
                        10,
                        kwargs.get("stfsf")
                    ),
                    Field(
                        "tslc2",
                        float,
                        40,
                        10,
                        kwargs.get("tslc2")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat186.option_specs[0],
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
        """Get or set the Material identification. A unique number or label must be specified.
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
        """Get or set the Flag for whether density is specified per unit area or volume:
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
    def tes(self) -> typing.Optional[float]:
        """Get or set the Type of effective separation parameter (ESP).
        """ # nopep8
        return self._cards[0].get_value("tes")

    @tes.setter
    def tes(self, value: float) -> None:
        self._cards[0].set_value("tes", value)

    @property
    def tslc(self) -> typing.Optional[int]:
        """Get or set the Normalized traction-separation load curve ID. The curve must be normalized in both coordinates and must contain at least three points: (0.0, 0.0), (lambda_0, 1.0), and (1.0, 0.0), which represents the origin, the peak and the complete failure, respectively (see Figure 186.1). A platform can exist in the curve like the tri-linear TSLC (see MAT_185).
        """ # nopep8
        return self._cards[0].get_value("tslc")

    @tslc.setter
    def tslc(self, value: int) -> None:
        self._cards[0].set_value("tslc", value)

    @property
    def gic(self) -> typing.Optional[float]:
        """Get or set the Fracture toughness / energy release rate   for mode I.
        """ # nopep8
        return self._cards[0].get_value("gic")

    @gic.setter
    def gic(self, value: float) -> None:
        self._cards[0].set_value("gic", value)

    @property
    def giic(self) -> typing.Optional[float]:
        """Get or set the Fracture toughness / energy release rate   for mode II.
        """ # nopep8
        return self._cards[0].get_value("giic")

    @giic.setter
    def giic(self, value: float) -> None:
        self._cards[0].set_value("giic", value)

    @property
    def xmu(self) -> typing.Optional[float]:
        """Get or set the Exponent that appears in the power failure criterion (TES=1.0) or the Benzeggagh-Kenane failure criterion (TES=2.0). Recommended values for XMU are between 1.0 and 2.0.
        """ # nopep8
        return self._cards[1].get_value("xmu")

    @xmu.setter
    def xmu(self, value: float) -> None:
        self._cards[1].set_value("xmu", value)

    @property
    def t(self) -> typing.Optional[float]:
        """Get or set the Peak traction in normal direction (mode I).
        """ # nopep8
        return self._cards[1].get_value("t")

    @t.setter
    def t(self, value: float) -> None:
        self._cards[1].set_value("t", value)

    @property
    def s(self) -> typing.Optional[float]:
        """Get or set the Peak traction in tangential direction (mode II).
        """ # nopep8
        return self._cards[1].get_value("s")

    @s.setter
    def s(self, value: float) -> None:
        self._cards[1].set_value("s", value)

    @property
    def stfsf(self) -> typing.Optional[float]:
        """Get or set the Penetration stiffness multiplier for compression. Factor = (1.0+STFSF) is used to scale the compressive stiffness, i.e. no scaling is done with STFSF=0.0 (recommended).
        """ # nopep8
        return self._cards[1].get_value("stfsf")

    @stfsf.setter
    def stfsf(self, value: float) -> None:
        self._cards[1].set_value("stfsf", value)

    @property
    def tslc2(self) -> typing.Optional[float]:
        """Get or set the Normalized traction-separation load curve ID for Mode II. The curve must be normalized in both coordinates and must contain at least three points: (0.0,0.0), (λ_0,1.0), and (1.0,0.0), which represents the origin, the peak and the complete failure, respectively. If not specified, TSLC is used for Mode II behavior as well
        """ # nopep8
        return self._cards[1].get_value("tslc2")

    @tslc2.setter
    def tslc2(self, value: float) -> None:
        self._cards[1].set_value("tslc2", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

