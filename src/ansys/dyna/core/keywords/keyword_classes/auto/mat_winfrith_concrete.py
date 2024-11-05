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

class MatWinfrithConcrete(KeywordBase):
    """DYNA MAT_WINFRITH_CONCRETE keyword"""

    keyword = "MAT"
    subkeyword = "WINFRITH_CONCRETE"
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
                        "tm",
                        float,
                        20,
                        10,
                        kwargs.get("tm")
                    ),
                    Field(
                        "pr",
                        float,
                        30,
                        10,
                        kwargs.get("pr")
                    ),
                    Field(
                        "ucs",
                        float,
                        40,
                        10,
                        kwargs.get("ucs")
                    ),
                    Field(
                        "uts",
                        float,
                        50,
                        10,
                        kwargs.get("uts")
                    ),
                    Field(
                        "fe",
                        float,
                        60,
                        10,
                        kwargs.get("fe")
                    ),
                    Field(
                        "asize",
                        float,
                        70,
                        10,
                        kwargs.get("asize")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "e",
                        float,
                        0,
                        10,
                        kwargs.get("e")
                    ),
                    Field(
                        "ys",
                        float,
                        10,
                        10,
                        kwargs.get("ys")
                    ),
                    Field(
                        "eh",
                        float,
                        20,
                        10,
                        kwargs.get("eh")
                    ),
                    Field(
                        "uelong",
                        float,
                        30,
                        10,
                        kwargs.get("uelong")
                    ),
                    Field(
                        "rate",
                        float,
                        40,
                        10,
                        kwargs.get("rate", 0)
                    ),
                    Field(
                        "conm",
                        float,
                        50,
                        10,
                        kwargs.get("conm")
                    ),
                    Field(
                        "conl",
                        float,
                        60,
                        10,
                        kwargs.get("conl")
                    ),
                    Field(
                        "cont",
                        float,
                        70,
                        10,
                        kwargs.get("cont")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "eps1",
                        float,
                        0,
                        10,
                        kwargs.get("eps1")
                    ),
                    Field(
                        "eps2",
                        float,
                        10,
                        10,
                        kwargs.get("eps2")
                    ),
                    Field(
                        "eps3",
                        float,
                        20,
                        10,
                        kwargs.get("eps3")
                    ),
                    Field(
                        "eps4",
                        float,
                        30,
                        10,
                        kwargs.get("eps4")
                    ),
                    Field(
                        "eps5",
                        float,
                        40,
                        10,
                        kwargs.get("eps5")
                    ),
                    Field(
                        "eps6",
                        float,
                        50,
                        10,
                        kwargs.get("eps6")
                    ),
                    Field(
                        "eps7",
                        float,
                        60,
                        10,
                        kwargs.get("eps7")
                    ),
                    Field(
                        "eps8",
                        float,
                        70,
                        10,
                        kwargs.get("eps8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "p1",
                        float,
                        0,
                        10,
                        kwargs.get("p1")
                    ),
                    Field(
                        "p2",
                        float,
                        10,
                        10,
                        kwargs.get("p2")
                    ),
                    Field(
                        "p3",
                        float,
                        20,
                        10,
                        kwargs.get("p3")
                    ),
                    Field(
                        "p4",
                        float,
                        30,
                        10,
                        kwargs.get("p4")
                    ),
                    Field(
                        "p5",
                        float,
                        40,
                        10,
                        kwargs.get("p5")
                    ),
                    Field(
                        "p6",
                        float,
                        50,
                        10,
                        kwargs.get("p6")
                    ),
                    Field(
                        "p7",
                        float,
                        60,
                        10,
                        kwargs.get("p7")
                    ),
                    Field(
                        "p8",
                        float,
                        70,
                        10,
                        kwargs.get("p8")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatWinfrithConcrete.option_specs[0],
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
    def tm(self) -> typing.Optional[float]:
        """Get or set the Tangent modulus (concrete).
        """ # nopep8
        return self._cards[0].get_value("tm")

    @tm.setter
    def tm(self, value: float) -> None:
        self._cards[0].set_value("tm", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def ucs(self) -> typing.Optional[float]:
        """Get or set the Uniaxial compressive strength.
        """ # nopep8
        return self._cards[0].get_value("ucs")

    @ucs.setter
    def ucs(self, value: float) -> None:
        self._cards[0].set_value("ucs", value)

    @property
    def uts(self) -> typing.Optional[float]:
        """Get or set the Uniaxial tensile strength.
        """ # nopep8
        return self._cards[0].get_value("uts")

    @uts.setter
    def uts(self, value: float) -> None:
        self._cards[0].set_value("uts", value)

    @property
    def fe(self) -> typing.Optional[float]:
        """Get or set the Depends on value of RATE below:
        RATE.EQ.0.: Fracture energy (energy per unit area dissipated in opening crack).
        RATE.EQ 1.: Crack width at which crack-normal tensile stress goes to zero.
        """ # nopep8
        return self._cards[0].get_value("fe")

    @fe.setter
    def fe(self, value: float) -> None:
        self._cards[0].set_value("fe", value)

    @property
    def asize(self) -> typing.Optional[float]:
        """Get or set the Aggregate size (radius).
        """ # nopep8
        return self._cards[0].get_value("asize")

    @asize.setter
    def asize(self, value: float) -> None:
        self._cards[0].set_value("asize", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus of rebar.
        """ # nopep8
        return self._cards[1].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        self._cards[1].set_value("e", value)

    @property
    def ys(self) -> typing.Optional[float]:
        """Get or set the Yield stress of rebar.
        """ # nopep8
        return self._cards[1].get_value("ys")

    @ys.setter
    def ys(self, value: float) -> None:
        self._cards[1].set_value("ys", value)

    @property
    def eh(self) -> typing.Optional[float]:
        """Get or set the Hardening modulus of rebar.
        """ # nopep8
        return self._cards[1].get_value("eh")

    @eh.setter
    def eh(self, value: float) -> None:
        self._cards[1].set_value("eh", value)

    @property
    def uelong(self) -> typing.Optional[float]:
        """Get or set the Ultimate elongation before rebar fails.
        """ # nopep8
        return self._cards[1].get_value("uelong")

    @uelong.setter
    def uelong(self, value: float) -> None:
        self._cards[1].set_value("uelong", value)

    @property
    def rate(self) -> float:
        """Get or set the Rate effects:
        EQ.0.0: Strain rate effects are included (default),
        EQ.1.0: Strain rate effects are turned off.Crack widths are stored as extra history variables 30, 31, 32.
        EQ.2.0:Like RATE=1 but includes improved crack algorithm (recommended).  Crack widths are stored as extra history variables 3, 4, 5.
        """ # nopep8
        return self._cards[1].get_value("rate")

    @rate.setter
    def rate(self, value: float) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""rate must be one of {0,1,2}""")
        self._cards[1].set_value("rate", value)

    @property
    def conm(self) -> typing.Optional[float]:
        """Get or set the GT.0: Factor to convert model mass units to kg,
        EQ.-1.: Mass, length, time units in model are lbf*sec 2 /in, inch, sec,
        EQ -2.: Mass, length, time units in model are g, cm, microsec,
        EQ.-3.: Mass, length, time units in model are g, mm, msec,
        EQ.-4.: Mass, length, time units in model are metric ton, mm, sec,
        EQ.-5.: Mass, length, time units in model are kg, mm, msec.
        """ # nopep8
        return self._cards[1].get_value("conm")

    @conm.setter
    def conm(self, value: float) -> None:
        self._cards[1].set_value("conm", value)

    @property
    def conl(self) -> typing.Optional[float]:
        """Get or set the If CONM.GT.0, factor to convert model length units to meters; otherwise CONL is ignored.
        """ # nopep8
        return self._cards[1].get_value("conl")

    @conl.setter
    def conl(self, value: float) -> None:
        self._cards[1].set_value("conl", value)

    @property
    def cont(self) -> typing.Optional[float]:
        """Get or set the If CONM.GT.0, factor to convert model time units to seconds; otherwise CONT is ignored.
        """ # nopep8
        return self._cards[1].get_value("cont")

    @cont.setter
    def cont(self, value: float) -> None:
        self._cards[1].set_value("cont", value)

    @property
    def eps1(self) -> typing.Optional[float]:
        """Get or set the First value of volumetric strain-pressure curve (natural logarithmic values).
        A maximum of 8 values are allowed. The tabulated values must competely cover the expected values in the analysis.
        If the first value is not for a volumetric strain value of zero then the point (0.0,0.0) will be automatically generated and up to a further nine additional values may be defined.
        """ # nopep8
        return self._cards[2].get_value("eps1")

    @eps1.setter
    def eps1(self, value: float) -> None:
        self._cards[2].set_value("eps1", value)

    @property
    def eps2(self) -> typing.Optional[float]:
        """Get or set the Second value of volumetric strain-pressure curve (natural logarithmic values).
        """ # nopep8
        return self._cards[2].get_value("eps2")

    @eps2.setter
    def eps2(self, value: float) -> None:
        self._cards[2].set_value("eps2", value)

    @property
    def eps3(self) -> typing.Optional[float]:
        """Get or set the Third value of volumetric strain-pressure curve (natural logarithmic values).
        """ # nopep8
        return self._cards[2].get_value("eps3")

    @eps3.setter
    def eps3(self, value: float) -> None:
        self._cards[2].set_value("eps3", value)

    @property
    def eps4(self) -> typing.Optional[float]:
        """Get or set the Fourth value of volumetric strain-pressure curve (natural logarithmic values).
        """ # nopep8
        return self._cards[2].get_value("eps4")

    @eps4.setter
    def eps4(self, value: float) -> None:
        self._cards[2].set_value("eps4", value)

    @property
    def eps5(self) -> typing.Optional[float]:
        """Get or set the Fifth value of volumetric strain-pressure curve (natural logarithmic values).
        """ # nopep8
        return self._cards[2].get_value("eps5")

    @eps5.setter
    def eps5(self, value: float) -> None:
        self._cards[2].set_value("eps5", value)

    @property
    def eps6(self) -> typing.Optional[float]:
        """Get or set the Sixth value of volumetric strain-pressure curve (natural logarithmic values).
        """ # nopep8
        return self._cards[2].get_value("eps6")

    @eps6.setter
    def eps6(self, value: float) -> None:
        self._cards[2].set_value("eps6", value)

    @property
    def eps7(self) -> typing.Optional[float]:
        """Get or set the Seventh value of volumetric strain-pressure curve (natural logarithmic values).
        """ # nopep8
        return self._cards[2].get_value("eps7")

    @eps7.setter
    def eps7(self, value: float) -> None:
        self._cards[2].set_value("eps7", value)

    @property
    def eps8(self) -> typing.Optional[float]:
        """Get or set the Eight value of volumetric strain-pressure curve (natural logarithmic values).
        """ # nopep8
        return self._cards[2].get_value("eps8")

    @eps8.setter
    def eps8(self, value: float) -> None:
        self._cards[2].set_value("eps8", value)

    @property
    def p1(self) -> typing.Optional[float]:
        """Get or set the Pressures corresponding to first volumetric strain value.
        """ # nopep8
        return self._cards[3].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        self._cards[3].set_value("p1", value)

    @property
    def p2(self) -> typing.Optional[float]:
        """Get or set the Pressures corresponding to second volumetric strain value.
        """ # nopep8
        return self._cards[3].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        self._cards[3].set_value("p2", value)

    @property
    def p3(self) -> typing.Optional[float]:
        """Get or set the Pressures corresponding to third volumetric strain value.
        """ # nopep8
        return self._cards[3].get_value("p3")

    @p3.setter
    def p3(self, value: float) -> None:
        self._cards[3].set_value("p3", value)

    @property
    def p4(self) -> typing.Optional[float]:
        """Get or set the Pressures corresponding to fourth volumetric strain value.
        """ # nopep8
        return self._cards[3].get_value("p4")

    @p4.setter
    def p4(self, value: float) -> None:
        self._cards[3].set_value("p4", value)

    @property
    def p5(self) -> typing.Optional[float]:
        """Get or set the Pressures corresponding to fifth volumetric strain value.
        """ # nopep8
        return self._cards[3].get_value("p5")

    @p5.setter
    def p5(self, value: float) -> None:
        self._cards[3].set_value("p5", value)

    @property
    def p6(self) -> typing.Optional[float]:
        """Get or set the Pressures corresponding to sixth volumetric strain value.
        """ # nopep8
        return self._cards[3].get_value("p6")

    @p6.setter
    def p6(self, value: float) -> None:
        self._cards[3].set_value("p6", value)

    @property
    def p7(self) -> typing.Optional[float]:
        """Get or set the Pressures corresponding to seventh volumetric strain value.
        """ # nopep8
        return self._cards[3].get_value("p7")

    @p7.setter
    def p7(self, value: float) -> None:
        self._cards[3].set_value("p7", value)

    @property
    def p8(self) -> typing.Optional[float]:
        """Get or set the Pressures corresponding to eight volumetric strain value.
        """ # nopep8
        return self._cards[3].get_value("p8")

    @p8.setter
    def p8(self, value: float) -> None:
        self._cards[3].set_value("p8", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[4].cards[0].set_value("title", value)

