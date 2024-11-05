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

class MatRateSensitivePowerlawPlasticity(KeywordBase):
    """DYNA MAT_RATE_SENSITIVE_POWERLAW_PLASTICITY keyword"""

    keyword = "MAT"
    subkeyword = "RATE_SENSITIVE_POWERLAW_PLASTICITY"
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
                        "k",
                        float,
                        40,
                        10,
                        kwargs.get("k")
                    ),
                    Field(
                        "m",
                        float,
                        50,
                        10,
                        kwargs.get("m", 1.0E-04)
                    ),
                    Field(
                        "n",
                        float,
                        60,
                        10,
                        kwargs.get("n", 0.0 )
                    ),
                    Field(
                        "e0",
                        float,
                        70,
                        10,
                        kwargs.get("e0", 2.0E-04)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "vp",
                        float,
                        0,
                        10,
                        kwargs.get("vp", 0.0)
                    ),
                    Field(
                        "eps0",
                        float,
                        10,
                        10,
                        kwargs.get("eps0", 1.0)
                    ),
                    Field(
                        "rfiltf",
                        float,
                        20,
                        10,
                        kwargs.get("rfiltf", 0.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatRateSensitivePowerlawPlasticity.option_specs[0],
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
    def k(self) -> typing.Optional[float]:
        """Get or set the Material constant, k.
        LT.0: absolute value of k is taken as the load curve number that defines k as a function of effective plastic strain.
        """ # nopep8
        return self._cards[0].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        self._cards[0].set_value("k", value)

    @property
    def m(self) -> float:
        """Get or set the Strain hardening coefficient, m
        LT.0: absolute value of m is taken as the load curve number that defines m as a function of effective plastic strain.
        """ # nopep8
        return self._cards[0].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        self._cards[0].set_value("m", value)

    @property
    def n(self) -> float:
        """Get or set the Strain rate sensitivity coefficient, n.
        LT.0: absolute value of n is taken as the load curve number that defines n as a function of effective plastic strain.
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        self._cards[0].set_value("n", value)

    @property
    def e0(self) -> float:
        """Get or set the Initial strain rate (default = 0.0002)
        """ # nopep8
        return self._cards[0].get_value("e0")

    @e0.setter
    def e0(self, value: float) -> None:
        self._cards[0].set_value("e0", value)

    @property
    def vp(self) -> float:
        """Get or set the Formulation for rate effects:
        EQ.0.0: Scale yield stress (default)
        EQ.1.0: Viscoplastic formulation
        """ # nopep8
        return self._cards[1].get_value("vp")

    @vp.setter
    def vp(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""vp must be one of {0.0,1.0}""")
        self._cards[1].set_value("vp", value)

    @property
    def eps0(self) -> float:
        """Get or set the Quasi-static threshold strain rate.  See description under *MAT_‌015.
        """ # nopep8
        return self._cards[1].get_value("eps0")

    @eps0.setter
    def eps0(self, value: float) -> None:
        self._cards[1].set_value("eps0", value)

    @property
    def rfiltf(self) -> float:
        """Get or set the Smoothing factor on the effective strain rate for solid elements when VP = 0.
        """ # nopep8
        return self._cards[1].get_value("rfiltf")

    @rfiltf.setter
    def rfiltf(self, value: float) -> None:
        self._cards[1].set_value("rfiltf", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

