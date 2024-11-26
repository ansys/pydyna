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

class MatT04(KeywordBase):
    """DYNA MAT_T04 keyword"""

    keyword = "MAT"
    subkeyword = "T04"
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
                        "tmid",
                        int,
                        0,
                        10,
                        kwargs.get("tmid")
                    ),
                    Field(
                        "tro",
                        float,
                        10,
                        10,
                        kwargs.get("tro")
                    ),
                    Field(
                        "tgrlc",
                        int,
                        20,
                        10,
                        kwargs.get("tgrlc")
                    ),
                    Field(
                        "tgmult",
                        float,
                        30,
                        10,
                        kwargs.get("tgmult")
                    ),
                    Field(
                        "aopt",
                        float,
                        40,
                        10,
                        kwargs.get("aopt", 0.0)
                    ),
                    Field(
                        "tlat",
                        float,
                        50,
                        10,
                        kwargs.get("tlat")
                    ),
                    Field(
                        "hlat",
                        float,
                        60,
                        10,
                        kwargs.get("hlat")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "t1",
                        float,
                        0,
                        10,
                        kwargs.get("t1")
                    ),
                    Field(
                        "t2",
                        float,
                        10,
                        10,
                        kwargs.get("t2")
                    ),
                    Field(
                        "t3",
                        float,
                        20,
                        10,
                        kwargs.get("t3")
                    ),
                    Field(
                        "t4",
                        float,
                        30,
                        10,
                        kwargs.get("t4")
                    ),
                    Field(
                        "t5",
                        float,
                        40,
                        10,
                        kwargs.get("t5")
                    ),
                    Field(
                        "t6",
                        float,
                        50,
                        10,
                        kwargs.get("t6")
                    ),
                    Field(
                        "t7",
                        float,
                        60,
                        10,
                        kwargs.get("t7")
                    ),
                    Field(
                        "t8",
                        float,
                        70,
                        10,
                        kwargs.get("t8")
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
                        "c7",
                        float,
                        60,
                        10,
                        kwargs.get("c7")
                    ),
                    Field(
                        "c8",
                        float,
                        70,
                        10,
                        kwargs.get("c8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "(k1-1)",
                        float,
                        0,
                        10,
                        kwargs.get("(k1-1)")
                    ),
                    Field(
                        "(k1-2)",
                        float,
                        10,
                        10,
                        kwargs.get("(k1-2)")
                    ),
                    Field(
                        "(k1-3)",
                        float,
                        20,
                        10,
                        kwargs.get("(k1-3)")
                    ),
                    Field(
                        "(k1-4)",
                        float,
                        30,
                        10,
                        kwargs.get("(k1-4)")
                    ),
                    Field(
                        "(k1-5)",
                        float,
                        40,
                        10,
                        kwargs.get("(k1-5)")
                    ),
                    Field(
                        "(k1-6)",
                        float,
                        50,
                        10,
                        kwargs.get("(k1-6)")
                    ),
                    Field(
                        "(k1-7)",
                        float,
                        60,
                        10,
                        kwargs.get("(k1-7)")
                    ),
                    Field(
                        "(k1-8)",
                        float,
                        70,
                        10,
                        kwargs.get("(k1-8)")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "(k2-1)",
                        float,
                        0,
                        10,
                        kwargs.get("(k2-1)")
                    ),
                    Field(
                        "(k2-2)",
                        float,
                        10,
                        10,
                        kwargs.get("(k2-2)")
                    ),
                    Field(
                        "(k2-3)",
                        float,
                        20,
                        10,
                        kwargs.get("(k2-3)")
                    ),
                    Field(
                        "(k2-4)",
                        float,
                        30,
                        10,
                        kwargs.get("(k2-4)")
                    ),
                    Field(
                        "(k2-5)",
                        float,
                        40,
                        10,
                        kwargs.get("(k2-5)")
                    ),
                    Field(
                        "(k2-6)",
                        float,
                        50,
                        10,
                        kwargs.get("(k2-6)")
                    ),
                    Field(
                        "(k2-7)",
                        float,
                        60,
                        10,
                        kwargs.get("(k2-7)")
                    ),
                    Field(
                        "(k2-8)",
                        float,
                        70,
                        10,
                        kwargs.get("(k2-8)")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "(k3-1)",
                        float,
                        0,
                        10,
                        kwargs.get("(k3-1)")
                    ),
                    Field(
                        "(k-2)",
                        float,
                        10,
                        10,
                        kwargs.get("(k-2)")
                    ),
                    Field(
                        "(k3-3)",
                        float,
                        20,
                        10,
                        kwargs.get("(k3-3)")
                    ),
                    Field(
                        "(k3-4)",
                        float,
                        30,
                        10,
                        kwargs.get("(k3-4)")
                    ),
                    Field(
                        "(k3-5)",
                        float,
                        40,
                        10,
                        kwargs.get("(k3-5)")
                    ),
                    Field(
                        "(k3-6)",
                        float,
                        50,
                        10,
                        kwargs.get("(k3-6)")
                    ),
                    Field(
                        "(k3-7)",
                        float,
                        60,
                        10,
                        kwargs.get("(k3-7)")
                    ),
                    Field(
                        "(k3-8)",
                        float,
                        70,
                        10,
                        kwargs.get("(k3-8)")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xp",
                        float,
                        0,
                        10,
                        kwargs.get("xp")
                    ),
                    Field(
                        "yp",
                        float,
                        10,
                        10,
                        kwargs.get("yp")
                    ),
                    Field(
                        "zp",
                        float,
                        20,
                        10,
                        kwargs.get("zp")
                    ),
                    Field(
                        "a1",
                        float,
                        30,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "a2",
                        float,
                        40,
                        10,
                        kwargs.get("a2")
                    ),
                    Field(
                        "a3",
                        float,
                        50,
                        10,
                        kwargs.get("a3")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "d1",
                        float,
                        0,
                        10,
                        kwargs.get("d1")
                    ),
                    Field(
                        "d2",
                        float,
                        10,
                        10,
                        kwargs.get("d2")
                    ),
                    Field(
                        "d3",
                        float,
                        20,
                        10,
                        kwargs.get("d3")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatT04.option_specs[0],
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
    def tmid(self) -> typing.Optional[int]:
        """Get or set the Thermal material identification, a unique number or label muast be specified.
        """ # nopep8
        return self._cards[0].get_value("tmid")

    @tmid.setter
    def tmid(self, value: int) -> None:
        self._cards[0].set_value("tmid", value)

    @property
    def tro(self) -> typing.Optional[float]:
        """Get or set the Thermal density:
        EQ 0.0 Default structural density.
        """ # nopep8
        return self._cards[0].get_value("tro")

    @tro.setter
    def tro(self, value: float) -> None:
        self._cards[0].set_value("tro", value)

    @property
    def tgrlc(self) -> typing.Optional[int]:
        """Get or set the Thermal generation rate (see *DEFINE_CURVE).
        GT.0:	Load curve ID defining thermal generation rate as a function of time
        EQ.0 : Thermal generation rate is the constant multiplier, TGMULT.
        LT.0 : | TGRLC | is a load curve ID defining thermal generation rate as a function of temperature.
        """ # nopep8
        return self._cards[0].get_value("tgrlc")

    @tgrlc.setter
    def tgrlc(self, value: int) -> None:
        self._cards[0].set_value("tgrlc", value)

    @property
    def tgmult(self) -> typing.Optional[float]:
        """Get or set the Thermal generation rate multiplier:
        EQ.0.0: no heat generation.
        """ # nopep8
        return self._cards[0].get_value("tgmult")

    @tgmult.setter
    def tgmult(self, value: float) -> None:
        self._cards[0].set_value("tgmult", value)

    @property
    def aopt(self) -> float:
        """Get or set the Material axes definition:
        EQ.0.0: Locally orthotropic with material axes by element nodes N1, N2 and N4,
        EQ.1.0: Locally orthotropic with material axes determined by a point in space and global location of element center,
        EQ.2.0: Globally orthotropic with material axes determined by vectors.
        EQ.3.0:	Locally orthotropic with first material axis orthogonal to element normal (defined by element nodes N1, N2 and N4) and to a vector d- Third material direction corresponds to element normal.
        EQ.4.0:	Local orthogonal in cylindrical coordinates with the material axes determined by a vector d,and an originating point, P, which define the centerline axis.
        """ # nopep8
        return self._cards[0].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        if value not in [0.0, 1.0, 2.0, 3.0, 4.0]:
            raise Exception("""aopt must be one of {0.0,1.0,2.0,3.0,4.0}""")
        self._cards[0].set_value("aopt", value)

    @property
    def tlat(self) -> typing.Optional[float]:
        """Get or set the Phase change temperature.
        """ # nopep8
        return self._cards[0].get_value("tlat")

    @tlat.setter
    def tlat(self, value: float) -> None:
        self._cards[0].set_value("tlat", value)

    @property
    def hlat(self) -> typing.Optional[float]:
        """Get or set the Latent heat.
        """ # nopep8
        return self._cards[0].get_value("hlat")

    @hlat.setter
    def hlat(self, value: float) -> None:
        self._cards[0].set_value("hlat", value)

    @property
    def t1(self) -> typing.Optional[float]:
        """Get or set the Temperatures T1.
        """ # nopep8
        return self._cards[1].get_value("t1")

    @t1.setter
    def t1(self, value: float) -> None:
        self._cards[1].set_value("t1", value)

    @property
    def t2(self) -> typing.Optional[float]:
        """Get or set the Temperatures T2.
        """ # nopep8
        return self._cards[1].get_value("t2")

    @t2.setter
    def t2(self, value: float) -> None:
        self._cards[1].set_value("t2", value)

    @property
    def t3(self) -> typing.Optional[float]:
        """Get or set the Temperatures T3.
        """ # nopep8
        return self._cards[1].get_value("t3")

    @t3.setter
    def t3(self, value: float) -> None:
        self._cards[1].set_value("t3", value)

    @property
    def t4(self) -> typing.Optional[float]:
        """Get or set the Temperatures T4.
        """ # nopep8
        return self._cards[1].get_value("t4")

    @t4.setter
    def t4(self, value: float) -> None:
        self._cards[1].set_value("t4", value)

    @property
    def t5(self) -> typing.Optional[float]:
        """Get or set the Temperatures T5.
        """ # nopep8
        return self._cards[1].get_value("t5")

    @t5.setter
    def t5(self, value: float) -> None:
        self._cards[1].set_value("t5", value)

    @property
    def t6(self) -> typing.Optional[float]:
        """Get or set the Temperatures T6.
        """ # nopep8
        return self._cards[1].get_value("t6")

    @t6.setter
    def t6(self, value: float) -> None:
        self._cards[1].set_value("t6", value)

    @property
    def t7(self) -> typing.Optional[float]:
        """Get or set the Temperatures T7.
        """ # nopep8
        return self._cards[1].get_value("t7")

    @t7.setter
    def t7(self, value: float) -> None:
        self._cards[1].set_value("t7", value)

    @property
    def t8(self) -> typing.Optional[float]:
        """Get or set the Temperatures T8.
        """ # nopep8
        return self._cards[1].get_value("t8")

    @t8.setter
    def t8(self, value: float) -> None:
        self._cards[1].set_value("t8", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the Heat capacity at T1.
        """ # nopep8
        return self._cards[2].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        self._cards[2].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the Heat capacity at T2.
        """ # nopep8
        return self._cards[2].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        self._cards[2].set_value("c2", value)

    @property
    def c3(self) -> typing.Optional[float]:
        """Get or set the Heat capacity at T3.
        """ # nopep8
        return self._cards[2].get_value("c3")

    @c3.setter
    def c3(self, value: float) -> None:
        self._cards[2].set_value("c3", value)

    @property
    def c4(self) -> typing.Optional[float]:
        """Get or set the Heat capacity at T4.
        """ # nopep8
        return self._cards[2].get_value("c4")

    @c4.setter
    def c4(self, value: float) -> None:
        self._cards[2].set_value("c4", value)

    @property
    def c5(self) -> typing.Optional[float]:
        """Get or set the Heat capacity at T5.
        """ # nopep8
        return self._cards[2].get_value("c5")

    @c5.setter
    def c5(self, value: float) -> None:
        self._cards[2].set_value("c5", value)

    @property
    def c6(self) -> typing.Optional[float]:
        """Get or set the Heat capacity at T6.
        """ # nopep8
        return self._cards[2].get_value("c6")

    @c6.setter
    def c6(self, value: float) -> None:
        self._cards[2].set_value("c6", value)

    @property
    def c7(self) -> typing.Optional[float]:
        """Get or set the Heat capacity at T7.
        """ # nopep8
        return self._cards[2].get_value("c7")

    @c7.setter
    def c7(self, value: float) -> None:
        self._cards[2].set_value("c7", value)

    @property
    def c8(self) -> typing.Optional[float]:
        """Get or set the Heat capacity at T8.
        """ # nopep8
        return self._cards[2].get_value("c8")

    @c8.setter
    def c8(self, value: float) -> None:
        self._cards[2].set_value("c8", value)

    @property
    def _k1_1_(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity K1 in local x-direction at T1.
        """ # nopep8
        return self._cards[3].get_value("(k1-1)")

    @_k1_1_.setter
    def _k1_1_(self, value: float) -> None:
        self._cards[3].set_value("(k1-1)", value)

    @property
    def _k1_2_(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity K1 in local x-direction at T2.
        """ # nopep8
        return self._cards[3].get_value("(k1-2)")

    @_k1_2_.setter
    def _k1_2_(self, value: float) -> None:
        self._cards[3].set_value("(k1-2)", value)

    @property
    def _k1_3_(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity K1 in local x-direction at T3.
        """ # nopep8
        return self._cards[3].get_value("(k1-3)")

    @_k1_3_.setter
    def _k1_3_(self, value: float) -> None:
        self._cards[3].set_value("(k1-3)", value)

    @property
    def _k1_4_(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity K1 in local x-direction at T4.
        """ # nopep8
        return self._cards[3].get_value("(k1-4)")

    @_k1_4_.setter
    def _k1_4_(self, value: float) -> None:
        self._cards[3].set_value("(k1-4)", value)

    @property
    def _k1_5_(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity K1 in local x-direction at T5.
        """ # nopep8
        return self._cards[3].get_value("(k1-5)")

    @_k1_5_.setter
    def _k1_5_(self, value: float) -> None:
        self._cards[3].set_value("(k1-5)", value)

    @property
    def _k1_6_(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity K1 in local x-direction at T6.
        """ # nopep8
        return self._cards[3].get_value("(k1-6)")

    @_k1_6_.setter
    def _k1_6_(self, value: float) -> None:
        self._cards[3].set_value("(k1-6)", value)

    @property
    def _k1_7_(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity K1 in local x-direction at T7.
        """ # nopep8
        return self._cards[3].get_value("(k1-7)")

    @_k1_7_.setter
    def _k1_7_(self, value: float) -> None:
        self._cards[3].set_value("(k1-7)", value)

    @property
    def _k1_8_(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity K1 in local x-direction at T8.
        """ # nopep8
        return self._cards[3].get_value("(k1-8)")

    @_k1_8_.setter
    def _k1_8_(self, value: float) -> None:
        self._cards[3].set_value("(k1-8)", value)

    @property
    def _k2_1_(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity K2 in local y-direction at T1.
        """ # nopep8
        return self._cards[4].get_value("(k2-1)")

    @_k2_1_.setter
    def _k2_1_(self, value: float) -> None:
        self._cards[4].set_value("(k2-1)", value)

    @property
    def _k2_2_(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity K2 in local y-direction at T2.
        """ # nopep8
        return self._cards[4].get_value("(k2-2)")

    @_k2_2_.setter
    def _k2_2_(self, value: float) -> None:
        self._cards[4].set_value("(k2-2)", value)

    @property
    def _k2_3_(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity K2 in local y-direction at T3.
        """ # nopep8
        return self._cards[4].get_value("(k2-3)")

    @_k2_3_.setter
    def _k2_3_(self, value: float) -> None:
        self._cards[4].set_value("(k2-3)", value)

    @property
    def _k2_4_(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity K2 in local y-direction at T4.
        """ # nopep8
        return self._cards[4].get_value("(k2-4)")

    @_k2_4_.setter
    def _k2_4_(self, value: float) -> None:
        self._cards[4].set_value("(k2-4)", value)

    @property
    def _k2_5_(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity K2 in local y-direction at T5.
        """ # nopep8
        return self._cards[4].get_value("(k2-5)")

    @_k2_5_.setter
    def _k2_5_(self, value: float) -> None:
        self._cards[4].set_value("(k2-5)", value)

    @property
    def _k2_6_(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity K2 in local y-direction at T6.
        """ # nopep8
        return self._cards[4].get_value("(k2-6)")

    @_k2_6_.setter
    def _k2_6_(self, value: float) -> None:
        self._cards[4].set_value("(k2-6)", value)

    @property
    def _k2_7_(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity K2 in local y-direction at T7.
        """ # nopep8
        return self._cards[4].get_value("(k2-7)")

    @_k2_7_.setter
    def _k2_7_(self, value: float) -> None:
        self._cards[4].set_value("(k2-7)", value)

    @property
    def _k2_8_(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity K2 in local y-direction at T8.
        """ # nopep8
        return self._cards[4].get_value("(k2-8)")

    @_k2_8_.setter
    def _k2_8_(self, value: float) -> None:
        self._cards[4].set_value("(k2-8)", value)

    @property
    def _k3_1_(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity K3 in local z-direction at T1.
        """ # nopep8
        return self._cards[5].get_value("(k3-1)")

    @_k3_1_.setter
    def _k3_1_(self, value: float) -> None:
        self._cards[5].set_value("(k3-1)", value)

    @property
    def _k_2_(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity K3 in local z-direction at T2.
        """ # nopep8
        return self._cards[5].get_value("(k-2)")

    @_k_2_.setter
    def _k_2_(self, value: float) -> None:
        self._cards[5].set_value("(k-2)", value)

    @property
    def _k3_3_(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity K3 in local z-direction at T3.
        """ # nopep8
        return self._cards[5].get_value("(k3-3)")

    @_k3_3_.setter
    def _k3_3_(self, value: float) -> None:
        self._cards[5].set_value("(k3-3)", value)

    @property
    def _k3_4_(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity K3 in local z-direction at T4.
        """ # nopep8
        return self._cards[5].get_value("(k3-4)")

    @_k3_4_.setter
    def _k3_4_(self, value: float) -> None:
        self._cards[5].set_value("(k3-4)", value)

    @property
    def _k3_5_(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity K3 in local z-direction at T5.
        """ # nopep8
        return self._cards[5].get_value("(k3-5)")

    @_k3_5_.setter
    def _k3_5_(self, value: float) -> None:
        self._cards[5].set_value("(k3-5)", value)

    @property
    def _k3_6_(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity K3 in local z-direction at T6.
        """ # nopep8
        return self._cards[5].get_value("(k3-6)")

    @_k3_6_.setter
    def _k3_6_(self, value: float) -> None:
        self._cards[5].set_value("(k3-6)", value)

    @property
    def _k3_7_(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity K3 in local z-direction at T7.
        """ # nopep8
        return self._cards[5].get_value("(k3-7)")

    @_k3_7_.setter
    def _k3_7_(self, value: float) -> None:
        self._cards[5].set_value("(k3-7)", value)

    @property
    def _k3_8_(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity K3 in local z-direction at T8.
        """ # nopep8
        return self._cards[5].get_value("(k3-8)")

    @_k3_8_.setter
    def _k3_8_(self, value: float) -> None:
        self._cards[5].set_value("(k3-8)", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the x-coordinate of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[6].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[6].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the y-coordinate of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[6].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[6].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the z-coordinate of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[6].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[6].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[6].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[6].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[6].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[6].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[6].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[6].set_value("a3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2, 3 and 4.
        """ # nopep8
        return self._cards[7].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[7].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2, 3 and 4
        """ # nopep8
        return self._cards[7].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[7].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2, 3 and 4.
        """ # nopep8
        return self._cards[7].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[7].set_value("d3", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[8].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[8].cards[0].set_value("title", value)

