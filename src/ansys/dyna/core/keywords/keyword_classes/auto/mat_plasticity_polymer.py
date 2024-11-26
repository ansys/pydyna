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

class MatPlasticityPolymer(KeywordBase):
    """DYNA MAT_PLASTICITY_POLYMER keyword"""

    keyword = "MAT"
    subkeyword = "PLASTICITY_POLYMER"
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
                ],
            ),
            Card(
                [
                    Field(
                        "c",
                        float,
                        0,
                        10,
                        kwargs.get("c")
                    ),
                    Field(
                        "p",
                        float,
                        10,
                        10,
                        kwargs.get("p")
                    ),
                    Field(
                        "lcss",
                        int,
                        20,
                        10,
                        kwargs.get("lcss", 0)
                    ),
                    Field(
                        "lcsr",
                        int,
                        30,
                        10,
                        kwargs.get("lcsr", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "eftx",
                        float,
                        0,
                        10,
                        kwargs.get("eftx", 0.0)
                    ),
                    Field(
                        "damp",
                        float,
                        10,
                        10,
                        kwargs.get("damp")
                    ),
                    Field(
                        "ratefac",
                        float,
                        20,
                        10,
                        kwargs.get("ratefac")
                    ),
                    Field(
                        "lcfail",
                        int,
                        30,
                        10,
                        kwargs.get("lcfail", 0)
                    ),
                    Field(
                        "numint",
                        float,
                        40,
                        10,
                        kwargs.get("numint", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatPlasticityPolymer.option_specs[0],
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
    def c(self) -> typing.Optional[float]:
        """Get or set the Strain rate parameter, C.
        """ # nopep8
        return self._cards[1].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        self._cards[1].set_value("c", value)

    @property
    def p(self) -> typing.Optional[float]:
        """Get or set the Strain rate parameter, P.
        """ # nopep8
        return self._cards[1].get_value("p")

    @p.setter
    def p(self, value: float) -> None:
        self._cards[1].set_value("p", value)

    @property
    def lcss(self) -> int:
        """Get or set the Load curve ID defining effective stress versus total effective strain. The stress versus effective plastic strain curve for the lowest value of strain rate is used if the strain rate falls below the minimum value. Likewise, the stress versus effective plastic strain curve for the highest value of strain rate is used if the strain rate exceeds the maximum value.
        """ # nopep8
        return self._cards[1].get_value("lcss")

    @lcss.setter
    def lcss(self, value: int) -> None:
        self._cards[1].set_value("lcss", value)

    @property
    def lcsr(self) -> int:
        """Get or set the Load curve ID defining strain rate scaling effect on yield stress.
        """ # nopep8
        return self._cards[1].get_value("lcsr")

    @lcsr.setter
    def lcsr(self, value: int) -> None:
        self._cards[1].set_value("lcsr", value)

    @property
    def eftx(self) -> float:
        """Get or set the Failure flag:
        EQ.0.0: failure determined by maximum tensile strain (default),
        EQ.1.0: failure determined only by tensile strain in local x direction,
        EQ.2.0: failure determined only by tensile strain in local y direction.
        """ # nopep8
        return self._cards[2].get_value("eftx")

    @eftx.setter
    def eftx(self, value: float) -> None:
        if value not in [0.0, 1.0, 2.0]:
            raise Exception("""eftx must be one of {0.0,1.0,2.0}""")
        self._cards[2].set_value("eftx", value)

    @property
    def damp(self) -> typing.Optional[float]:
        """Get or set the Stiffness-propotional damping ratio. Typical values are 1e-3 or 1e-4. If set too high instabilites can result.
        """ # nopep8
        return self._cards[2].get_value("damp")

    @damp.setter
    def damp(self, value: float) -> None:
        self._cards[2].set_value("damp", value)

    @property
    def ratefac(self) -> typing.Optional[float]:
        """Get or set the Filtering factor for strain rate effects. Must be between 0 (no filtering) and 1 (infinite filtering) The filter is a simple low pass filter to remove high frequency oscillation from the strain rates before they are used in rate effect calculations. The cut off frequency of the filter is [(1 - RATEFAC)/ timestep] rad/sec.
        """ # nopep8
        return self._cards[2].get_value("ratefac")

    @ratefac.setter
    def ratefac(self, value: float) -> None:
        self._cards[2].set_value("ratefac", value)

    @property
    def lcfail(self) -> int:
        """Get or set the Load curve ID giving variation of failure strain with strain rate. The points on the x-axis should be natural log of strain rate, the y-axis should be the true strain to failure. Typically this is measured by uniaxial tensile test, and the strain values converted to true strain.
        """ # nopep8
        return self._cards[2].get_value("lcfail")

    @lcfail.setter
    def lcfail(self, value: int) -> None:
        self._cards[2].set_value("lcfail", value)

    @property
    def numint(self) -> float:
        """Get or set the Number of integration points which must fail before the element is deleted. This option is available for shells only.
        LT.0.0:	|NUMINT| is percentage of integration points/layers which must fail before shell element fails.
        """ # nopep8
        return self._cards[2].get_value("numint")

    @numint.setter
    def numint(self, value: float) -> None:
        self._cards[2].set_value("numint", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

