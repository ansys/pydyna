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

class Mat065(KeywordBase):
    """DYNA MAT_065 keyword"""

    keyword = "MAT"
    subkeyword = "065"
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
                        "g",
                        float,
                        20,
                        10,
                        kwargs.get("g")
                    ),
                    Field(
                        "e0",
                        float,
                        30,
                        10,
                        kwargs.get("e0")
                    ),
                    Field(
                        "n",
                        float,
                        40,
                        10,
                        kwargs.get("n")
                    ),
                    Field(
                        "troom",
                        float,
                        50,
                        10,
                        kwargs.get("troom")
                    ),
                    Field(
                        "pc",
                        float,
                        60,
                        10,
                        kwargs.get("pc")
                    ),
                    Field(
                        "spall",
                        float,
                        70,
                        10,
                        kwargs.get("spall", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c1",
                        float,
                        0,
                        10,
                        kwargs.get("c1")
                    ),
                    Field(
                        "c2",
                        float,
                        10,
                        10,
                        kwargs.get("c2")
                    ),
                    Field(
                        "c3",
                        float,
                        20,
                        10,
                        kwargs.get("c3")
                    ),
                    Field(
                        "c4",
                        float,
                        30,
                        10,
                        kwargs.get("c4")
                    ),
                    Field(
                        "c5",
                        float,
                        40,
                        10,
                        kwargs.get("c5")
                    ),
                    Field(
                        "c6",
                        float,
                        50,
                        10,
                        kwargs.get("c6")
                    ),
                    Field(
                        "efail",
                        float,
                        60,
                        10,
                        kwargs.get("efail")
                    ),
                    Field(
                        "vp",
                        float,
                        70,
                        10,
                        kwargs.get("vp", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "b1",
                        float,
                        0,
                        10,
                        kwargs.get("b1")
                    ),
                    Field(
                        "b2",
                        float,
                        10,
                        10,
                        kwargs.get("b2")
                    ),
                    Field(
                        "b3",
                        float,
                        20,
                        10,
                        kwargs.get("b3")
                    ),
                    Field(
                        "g1",
                        float,
                        30,
                        10,
                        kwargs.get("g1")
                    ),
                    Field(
                        "g2",
                        float,
                        40,
                        10,
                        kwargs.get("g2")
                    ),
                    Field(
                        "g3",
                        float,
                        50,
                        10,
                        kwargs.get("g3")
                    ),
                    Field(
                        "g4",
                        float,
                        60,
                        10,
                        kwargs.get("g4")
                    ),
                    Field(
                        "bulk",
                        float,
                        70,
                        10,
                        kwargs.get("bulk")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "m",
                        float,
                        0,
                        10,
                        kwargs.get("m", 0.5)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat065.option_specs[0],
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
    def g(self) -> typing.Optional[float]:
        """Get or set the Shear modulus.
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        self._cards[0].set_value("g", value)

    @property
    def e0(self) -> typing.Optional[float]:
        """Get or set the epsilon-0, Factor to normalize strain rate.
        """ # nopep8
        return self._cards[0].get_value("e0")

    @e0.setter
    def e0(self, value: float) -> None:
        self._cards[0].set_value("e0", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the n, Exponent for bcc metal.
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        self._cards[0].set_value("n", value)

    @property
    def troom(self) -> typing.Optional[float]:
        """Get or set the Room temperature.
        """ # nopep8
        return self._cards[0].get_value("troom")

    @troom.setter
    def troom(self, value: float) -> None:
        self._cards[0].set_value("troom", value)

    @property
    def pc(self) -> typing.Optional[float]:
        """Get or set the Pressure cutoff.
        """ # nopep8
        return self._cards[0].get_value("pc")

    @pc.setter
    def pc(self, value: float) -> None:
        self._cards[0].set_value("pc", value)

    @property
    def spall(self) -> float:
        """Get or set the Spall Type:
        EQ.1.0: minimum pressure limit,
        EQ.2.0: maximum principal stress,
        EQ.3.0: minimum pressure cutoff.
        """ # nopep8
        return self._cards[0].get_value("spall")

    @spall.setter
    def spall(self, value: float) -> None:
        if value not in [1.0, 2.0, 3.0]:
            raise Exception("""spall must be one of {1.0,2.0,3.0}""")
        self._cards[0].set_value("spall", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the C1, coefficient for flow stress.
        """ # nopep8
        return self._cards[1].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        self._cards[1].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the C2, coefficient for flow stress.
        """ # nopep8
        return self._cards[1].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        self._cards[1].set_value("c2", value)

    @property
    def c3(self) -> typing.Optional[float]:
        """Get or set the C3, coefficient for flow stress.
        """ # nopep8
        return self._cards[1].get_value("c3")

    @c3.setter
    def c3(self, value: float) -> None:
        self._cards[1].set_value("c3", value)

    @property
    def c4(self) -> typing.Optional[float]:
        """Get or set the C4, coefficient for flow stress.
        """ # nopep8
        return self._cards[1].get_value("c4")

    @c4.setter
    def c4(self, value: float) -> None:
        self._cards[1].set_value("c4", value)

    @property
    def c5(self) -> typing.Optional[float]:
        """Get or set the C5, coefficient for flow stress.
        """ # nopep8
        return self._cards[1].get_value("c5")

    @c5.setter
    def c5(self, value: float) -> None:
        self._cards[1].set_value("c5", value)

    @property
    def c6(self) -> typing.Optional[float]:
        """Get or set the C6, coefficient for flow stress.
        """ # nopep8
        return self._cards[1].get_value("c6")

    @c6.setter
    def c6(self, value: float) -> None:
        self._cards[1].set_value("c6", value)

    @property
    def efail(self) -> typing.Optional[float]:
        """Get or set the Failure strain for erosion.
        """ # nopep8
        return self._cards[1].get_value("efail")

    @efail.setter
    def efail(self, value: float) -> None:
        self._cards[1].set_value("efail", value)

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
    def b1(self) -> typing.Optional[float]:
        """Get or set the B1, coefficient for polynomial to represent temperature dependency of flow stress yield.
        """ # nopep8
        return self._cards[2].get_value("b1")

    @b1.setter
    def b1(self, value: float) -> None:
        self._cards[2].set_value("b1", value)

    @property
    def b2(self) -> typing.Optional[float]:
        """Get or set the B2, coefficient for polynomial to represent temperature dependency of flow stress yield.
        """ # nopep8
        return self._cards[2].get_value("b2")

    @b2.setter
    def b2(self, value: float) -> None:
        self._cards[2].set_value("b2", value)

    @property
    def b3(self) -> typing.Optional[float]:
        """Get or set the B3, coefficient for polynomial to represent temperature dependency of flow stress yield.
        """ # nopep8
        return self._cards[2].get_value("b3")

    @b3.setter
    def b3(self, value: float) -> None:
        self._cards[2].set_value("b3", value)

    @property
    def g1(self) -> typing.Optional[float]:
        """Get or set the G1, coefficient for defining heat capacity and temperature dependency of heat capacity.
        """ # nopep8
        return self._cards[2].get_value("g1")

    @g1.setter
    def g1(self, value: float) -> None:
        self._cards[2].set_value("g1", value)

    @property
    def g2(self) -> typing.Optional[float]:
        """Get or set the G2, coefficient for defining heat capacity and temperature dependency of heat capacity.
        """ # nopep8
        return self._cards[2].get_value("g2")

    @g2.setter
    def g2(self, value: float) -> None:
        self._cards[2].set_value("g2", value)

    @property
    def g3(self) -> typing.Optional[float]:
        """Get or set the G3, coefficient for defining heat capacity and temperature dependency of heat capacity.
        """ # nopep8
        return self._cards[2].get_value("g3")

    @g3.setter
    def g3(self, value: float) -> None:
        self._cards[2].set_value("g3", value)

    @property
    def g4(self) -> typing.Optional[float]:
        """Get or set the G4, coefficient for defining heat capacity and temperature dependency of heat capacity.
        """ # nopep8
        return self._cards[2].get_value("g4")

    @g4.setter
    def g4(self, value: float) -> None:
        self._cards[2].set_value("g4", value)

    @property
    def bulk(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus defined for shell elements only. Do not input for solid elements.
        """ # nopep8
        return self._cards[2].get_value("bulk")

    @bulk.setter
    def bulk(self, value: float) -> None:
        self._cards[2].set_value("bulk", value)

    @property
    def m(self) -> float:
        """Get or set the m, exponent for FCC metal (default = 0.5).  This field is only used when N = 0.0 on Card 1.
        """ # nopep8
        return self._cards[3].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        self._cards[3].set_value("m", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[4].cards[0].set_value("title", value)

