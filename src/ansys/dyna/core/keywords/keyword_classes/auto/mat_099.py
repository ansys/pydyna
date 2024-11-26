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

class Mat099(KeywordBase):
    """DYNA MAT_099 keyword"""

    keyword = "MAT"
    subkeyword = "099"
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
                        "vp",
                        float,
                        40,
                        10,
                        kwargs.get("vp")
                    ),
                    Field(
                        "eppfr",
                        float,
                        50,
                        10,
                        kwargs.get("eppfr", 1.E+16)
                    ),
                    Field(
                        "lcdm",
                        int,
                        60,
                        10,
                        kwargs.get("lcdm")
                    ),
                    Field(
                        "numint",
                        int,
                        70,
                        10,
                        kwargs.get("numint")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "a",
                        float,
                        0,
                        10,
                        kwargs.get("a")
                    ),
                    Field(
                        "b",
                        float,
                        10,
                        10,
                        kwargs.get("b")
                    ),
                    Field(
                        "n",
                        float,
                        20,
                        10,
                        kwargs.get("n")
                    ),
                    Field(
                        "c",
                        float,
                        30,
                        10,
                        kwargs.get("c")
                    ),
                    Field(
                        "psfail",
                        float,
                        40,
                        10,
                        kwargs.get("psfail", 1.0E+17)
                    ),
                    Field(
                        "sigmax",
                        float,
                        50,
                        10,
                        kwargs.get("sigmax", 1.0E+28)
                    ),
                    Field(
                        "sigsat",
                        float,
                        60,
                        10,
                        kwargs.get("sigsat", 1.0E+28)
                    ),
                    Field(
                        "epso",
                        float,
                        70,
                        10,
                        kwargs.get("epso", 1.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat099.option_specs[0],
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
    def vp(self) -> typing.Optional[float]:
        """Get or set the Formulation for rate effects
        EQ.0.0: Scale yield stress (Default),
        EQ.1.0:Viscoplastic formulation.
        This option only applies to the 4-node shell and 8-node thick shell, if and only if through thickness integration points is used.
        """ # nopep8
        return self._cards[0].get_value("vp")

    @vp.setter
    def vp(self, value: float) -> None:
        self._cards[0].set_value("vp", value)

    @property
    def eppfr(self) -> float:
        """Get or set the Plastic strain at which material ruptures(logrithmic).
        """ # nopep8
        return self._cards[0].get_value("eppfr")

    @eppfr.setter
    def eppfr(self, value: float) -> None:
        self._cards[0].set_value("eppfr", value)

    @property
    def lcdm(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining nonlinear damage curve.
        """ # nopep8
        return self._cards[0].get_value("lcdm")

    @lcdm.setter
    def lcdm(self, value: int) -> None:
        self._cards[0].set_value("lcdm", value)

    @property
    def numint(self) -> typing.Optional[int]:
        """Get or set the Number of through thickness integration points which must fail before the element is deleted. (If zero, all points must fail.) The default of all integration points is not recommended since elements undergoing large strain are often not deleted due to nodal fiber rotations which limit strains at active integration points after most points have failed. Better results are obtained if NUMINT is set to 1 or a number less than one half of the number of through thickness points. For example, if four through thickness points are used, NUMINT should not exceed 2, even for fully integrated shells which have 16 integration points.
        """ # nopep8
        return self._cards[0].get_value("numint")

    @numint.setter
    def numint(self, value: int) -> None:
        self._cards[0].set_value("numint", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Input constants.
        See Keyword Manual page 280 (volume two) for further information.
        """ # nopep8
        return self._cards[1].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        self._cards[1].set_value("a", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Input constants.
        See Keyword Manual page 280 (volume two) for further information.
        """ # nopep8
        return self._cards[1].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        self._cards[1].set_value("b", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Input constants
        See Keyword Manual page 280 (volume two) for further information.
        """ # nopep8
        return self._cards[1].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        self._cards[1].set_value("n", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Input constants
        See Keyword Manual page 280 (volume two) for further information.
        """ # nopep8
        return self._cards[1].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        self._cards[1].set_value("c", value)

    @property
    def psfail(self) -> float:
        """Get or set the Effective plastic strain at failure.
        EQ.0: Failure is not considered (default).
        """ # nopep8
        return self._cards[1].get_value("psfail")

    @psfail.setter
    def psfail(self, value: float) -> None:
        self._cards[1].set_value("psfail", value)

    @property
    def sigmax(self) -> float:
        """Get or set the Maximum stress obtainable from work hardening, before rate effects are added(optional). This option is ignored if VP=1.0.
        """ # nopep8
        return self._cards[1].get_value("sigmax")

    @sigmax.setter
    def sigmax(self, value: float) -> None:
        self._cards[1].set_value("sigmax", value)

    @property
    def sigsat(self) -> float:
        """Get or set the Saturation stress which limits the maximum value of effective stress which can develop after rate effects are added.(optional)
        """ # nopep8
        return self._cards[1].get_value("sigsat")

    @sigsat.setter
    def sigsat(self, value: float) -> None:
        self._cards[1].set_value("sigsat", value)

    @property
    def epso(self) -> float:
        """Get or set the Effective plastic strain rate. This value depends on the time units. Typically input 1 for units of seconds, 0.001 for units of milliseconds, 0.000001 for microseconds, etc.
        """ # nopep8
        return self._cards[1].get_value("epso")

    @epso.setter
    def epso(self, value: float) -> None:
        self._cards[1].set_value("epso", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

