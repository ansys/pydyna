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

class Mat101(KeywordBase):
    """DYNA MAT_101 keyword"""

    keyword = "MAT"
    subkeyword = "101"
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
                        "ratesf",
                        float,
                        40,
                        10,
                        kwargs.get("ratesf")
                    ),
                    Field(
                        "edot0",
                        float,
                        50,
                        10,
                        kwargs.get("edot0")
                    ),
                    Field(
                        "alpha",
                        float,
                        60,
                        10,
                        kwargs.get("alpha")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcss",
                        int,
                        0,
                        10,
                        kwargs.get("lcss")
                    ),
                    Field(
                        "lcfeps",
                        int,
                        10,
                        10,
                        kwargs.get("lcfeps")
                    ),
                    Field(
                        "lcfsig",
                        int,
                        20,
                        10,
                        kwargs.get("lcfsig")
                    ),
                    Field(
                        "lce",
                        int,
                        30,
                        10,
                        kwargs.get("lce")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat101.option_specs[0],
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
        """Get or set the Material identification. An unique number has to be used.
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
        """Get or set the Young's Modulus.
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
    def ratesf(self) -> typing.Optional[float]:
        """Get or set the Constant in plastic strain rate equation.
        """ # nopep8
        return self._cards[0].get_value("ratesf")

    @ratesf.setter
    def ratesf(self, value: float) -> None:
        self._cards[0].set_value("ratesf", value)

    @property
    def edot0(self) -> typing.Optional[float]:
        """Get or set the Reference strain rate.
        """ # nopep8
        return self._cards[0].get_value("edot0")

    @edot0.setter
    def edot0(self, value: float) -> None:
        self._cards[0].set_value("edot0", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Pressure sensitivity factor.
        Default is set to 0.0.
        """ # nopep8
        return self._cards[0].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        self._cards[0].set_value("alpha", value)

    @property
    def lcss(self) -> typing.Optional[int]:
        """Get or set the Load curve ID or Table ID that defines the post yield material behavior. The values of this stress-strain curve are the difference of the yield stress and strain respectively. This means the first values for both stress and strain should be zero. All subsequent values will define softening or hardening.
        """ # nopep8
        return self._cards[1].get_value("lcss")

    @lcss.setter
    def lcss(self, value: int) -> None:
        self._cards[1].set_value("lcss", value)

    @property
    def lcfeps(self) -> typing.Optional[int]:
        """Get or set the Load curve ID that defines the plastic failure strain as a function of strain rate.
        """ # nopep8
        return self._cards[1].get_value("lcfeps")

    @lcfeps.setter
    def lcfeps(self, value: int) -> None:
        self._cards[1].set_value("lcfeps", value)

    @property
    def lcfsig(self) -> typing.Optional[int]:
        """Get or set the Load curve ID that defines the Maximum principal failure stress as a function of strain rate.
        """ # nopep8
        return self._cards[1].get_value("lcfsig")

    @lcfsig.setter
    def lcfsig(self, value: int) -> None:
        self._cards[1].set_value("lcfsig", value)

    @property
    def lce(self) -> typing.Optional[int]:
        """Get or set the Load curve ID that defines the Unloading moduli as a function of plastic strain.
        """ # nopep8
        return self._cards[1].get_value("lce")

    @lce.setter
    def lce(self, value: int) -> None:
        self._cards[1].set_value("lce", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

