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
from ansys.dyna.core.lib.keyword_base import KeywordBase

class EosTabulatedCompaction(KeywordBase):
    """DYNA EOS_TABULATED_COMPACTION keyword"""

    keyword = "EOS"
    subkeyword = "TABULATED_COMPACTION"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "eosid",
                        int,
                        0,
                        10,
                        kwargs.get("eosid")
                    ),
                    Field(
                        "gama",
                        float,
                        10,
                        10,
                        kwargs.get("gama")
                    ),
                    Field(
                        "e0",
                        float,
                        20,
                        10,
                        kwargs.get("e0")
                    ),
                    Field(
                        "vo",
                        float,
                        30,
                        10,
                        kwargs.get("vo")
                    ),
                    Field(
                        "lcc",
                        int,
                        40,
                        10,
                        kwargs.get("lcc")
                    ),
                    Field(
                        "lct",
                        int,
                        50,
                        10,
                        kwargs.get("lct")
                    ),
                    Field(
                        "lck",
                        int,
                        60,
                        10,
                        kwargs.get("lck")
                    ),
                    Field(
                        "lcid",
                        int,
                        70,
                        10,
                        kwargs.get("lcid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ev1",
                        float,
                        0,
                        16,
                        kwargs.get("ev1")
                    ),
                    Field(
                        "ev2",
                        float,
                        16,
                        16,
                        kwargs.get("ev2")
                    ),
                    Field(
                        "ev3",
                        float,
                        32,
                        16,
                        kwargs.get("ev3")
                    ),
                    Field(
                        "ev4",
                        float,
                        48,
                        16,
                        kwargs.get("ev4")
                    ),
                    Field(
                        "ev5",
                        float,
                        64,
                        16,
                        kwargs.get("ev5")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ev6",
                        float,
                        0,
                        16,
                        kwargs.get("ev6")
                    ),
                    Field(
                        "ev7",
                        float,
                        16,
                        16,
                        kwargs.get("ev7")
                    ),
                    Field(
                        "ev8",
                        float,
                        32,
                        16,
                        kwargs.get("ev8")
                    ),
                    Field(
                        "ev9",
                        float,
                        48,
                        16,
                        kwargs.get("ev9")
                    ),
                    Field(
                        "ev10",
                        float,
                        64,
                        16,
                        kwargs.get("ev10")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c1",
                        float,
                        0,
                        16,
                        kwargs.get("c1")
                    ),
                    Field(
                        "c2",
                        float,
                        16,
                        16,
                        kwargs.get("c2")
                    ),
                    Field(
                        "c3",
                        float,
                        32,
                        16,
                        kwargs.get("c3")
                    ),
                    Field(
                        "c4",
                        float,
                        48,
                        16,
                        kwargs.get("c4")
                    ),
                    Field(
                        "c5",
                        float,
                        64,
                        16,
                        kwargs.get("c5")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c6",
                        float,
                        0,
                        16,
                        kwargs.get("c6")
                    ),
                    Field(
                        "c7",
                        float,
                        16,
                        16,
                        kwargs.get("c7")
                    ),
                    Field(
                        "c8",
                        float,
                        32,
                        16,
                        kwargs.get("c8")
                    ),
                    Field(
                        "c9",
                        float,
                        48,
                        16,
                        kwargs.get("c9")
                    ),
                    Field(
                        "c10",
                        float,
                        64,
                        16,
                        kwargs.get("c10")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "t1",
                        float,
                        0,
                        16,
                        kwargs.get("t1")
                    ),
                    Field(
                        "t2",
                        float,
                        16,
                        16,
                        kwargs.get("t2")
                    ),
                    Field(
                        "t3",
                        float,
                        32,
                        16,
                        kwargs.get("t3")
                    ),
                    Field(
                        "t4",
                        float,
                        48,
                        16,
                        kwargs.get("t4")
                    ),
                    Field(
                        "t5",
                        float,
                        64,
                        16,
                        kwargs.get("t5")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "t6",
                        float,
                        0,
                        16,
                        kwargs.get("t6")
                    ),
                    Field(
                        "t7",
                        float,
                        16,
                        16,
                        kwargs.get("t7")
                    ),
                    Field(
                        "t8",
                        float,
                        32,
                        16,
                        kwargs.get("t8")
                    ),
                    Field(
                        "t9",
                        float,
                        48,
                        16,
                        kwargs.get("t9")
                    ),
                    Field(
                        "t10",
                        float,
                        64,
                        16,
                        kwargs.get("t10")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "k1",
                        float,
                        0,
                        16,
                        kwargs.get("k1")
                    ),
                    Field(
                        "k2",
                        float,
                        16,
                        16,
                        kwargs.get("k2")
                    ),
                    Field(
                        "k3",
                        float,
                        32,
                        16,
                        kwargs.get("k3")
                    ),
                    Field(
                        "k4",
                        float,
                        48,
                        16,
                        kwargs.get("k4")
                    ),
                    Field(
                        "k5",
                        float,
                        64,
                        16,
                        kwargs.get("k5")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "k6",
                        float,
                        0,
                        16,
                        kwargs.get("k6")
                    ),
                    Field(
                        "k7",
                        float,
                        16,
                        16,
                        kwargs.get("k7")
                    ),
                    Field(
                        "k8",
                        float,
                        32,
                        16,
                        kwargs.get("k8")
                    ),
                    Field(
                        "k9",
                        float,
                        48,
                        16,
                        kwargs.get("k9")
                    ),
                    Field(
                        "k10",
                        float,
                        64,
                        16,
                        kwargs.get("k10")
                    ),
                ],
            ),
        ]

    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state label.
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        self._cards[0].set_value("eosid", value)

    @property
    def gama(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("gama")

    @gama.setter
    def gama(self, value: float) -> None:
        self._cards[0].set_value("gama", value)

    @property
    def e0(self) -> typing.Optional[float]:
        """Get or set the Initial internal energy.
        """ # nopep8
        return self._cards[0].get_value("e0")

    @e0.setter
    def e0(self, value: float) -> None:
        self._cards[0].set_value("e0", value)

    @property
    def vo(self) -> typing.Optional[float]:
        """Get or set the Initial relative volume.
        """ # nopep8
        return self._cards[0].get_value("vo")

    @vo.setter
    def vo(self, value: float) -> None:
        self._cards[0].set_value("vo", value)

    @property
    def lcc(self) -> typing.Optional[int]:
        """Get or set the Load curve defining tabulated function .  See equation in Remarks.
        The abscissa values of LCC, LCT and LCK must be negative of the volumetric strain in monotonically increasing order, in contrast to the convention in EOS_9.
        The definition can extend into the tensile regime.
        """ # nopep8
        return self._cards[0].get_value("lcc")

    @lcc.setter
    def lcc(self, value: int) -> None:
        self._cards[0].set_value("lcc", value)

    @property
    def lct(self) -> typing.Optional[int]:
        """Get or set the Load curve defining tabulated function .  See equation in Remarks
        """ # nopep8
        return self._cards[0].get_value("lct")

    @lct.setter
    def lct(self, value: int) -> None:
        self._cards[0].set_value("lct", value)

    @property
    def lck(self) -> typing.Optional[int]:
        """Get or set the Load curve defining tabulated bulk modulus.
        """ # nopep8
        return self._cards[0].get_value("lck")

    @lck.setter
    def lck(self, value: int) -> None:
        self._cards[0].set_value("lck", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID, which can be the ID of *DEFINE_CURVE, *DEFINE_CURVE_FUNCTION or *DEFINE_FUNCTION, defining the energy deposition rate.
        If an energy leak rate is intended, do not specify a negative ordinate in LCID, rather, use the constant(s) in the equation of state, e.g., set GAMMA to a negative value.
        If *DEFINE_FUNCTION is used, the input of the defined function is time.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def ev1(self) -> typing.Optional[float]:
        """Get or set the ln V of first item.
        """ # nopep8
        return self._cards[1].get_value("ev1")

    @ev1.setter
    def ev1(self, value: float) -> None:
        self._cards[1].set_value("ev1", value)

    @property
    def ev2(self) -> typing.Optional[float]:
        """Get or set the ln V of second item.
        """ # nopep8
        return self._cards[1].get_value("ev2")

    @ev2.setter
    def ev2(self, value: float) -> None:
        self._cards[1].set_value("ev2", value)

    @property
    def ev3(self) -> typing.Optional[float]:
        """Get or set the ln V of third item.
        """ # nopep8
        return self._cards[1].get_value("ev3")

    @ev3.setter
    def ev3(self, value: float) -> None:
        self._cards[1].set_value("ev3", value)

    @property
    def ev4(self) -> typing.Optional[float]:
        """Get or set the ln V of fourth item.
        """ # nopep8
        return self._cards[1].get_value("ev4")

    @ev4.setter
    def ev4(self, value: float) -> None:
        self._cards[1].set_value("ev4", value)

    @property
    def ev5(self) -> typing.Optional[float]:
        """Get or set the ln V of fifth item.
        """ # nopep8
        return self._cards[1].get_value("ev5")

    @ev5.setter
    def ev5(self, value: float) -> None:
        self._cards[1].set_value("ev5", value)

    @property
    def ev6(self) -> typing.Optional[float]:
        """Get or set the ln V of sixth item.
        """ # nopep8
        return self._cards[2].get_value("ev6")

    @ev6.setter
    def ev6(self, value: float) -> None:
        self._cards[2].set_value("ev6", value)

    @property
    def ev7(self) -> typing.Optional[float]:
        """Get or set the ln V of seventh item.
        """ # nopep8
        return self._cards[2].get_value("ev7")

    @ev7.setter
    def ev7(self, value: float) -> None:
        self._cards[2].set_value("ev7", value)

    @property
    def ev8(self) -> typing.Optional[float]:
        """Get or set the ln V of eighth item.
        """ # nopep8
        return self._cards[2].get_value("ev8")

    @ev8.setter
    def ev8(self, value: float) -> None:
        self._cards[2].set_value("ev8", value)

    @property
    def ev9(self) -> typing.Optional[float]:
        """Get or set the ln V of ninth item.
        """ # nopep8
        return self._cards[2].get_value("ev9")

    @ev9.setter
    def ev9(self, value: float) -> None:
        self._cards[2].set_value("ev9", value)

    @property
    def ev10(self) -> typing.Optional[float]:
        """Get or set the ln V of tenth item.
        """ # nopep8
        return self._cards[2].get_value("ev10")

    @ev10.setter
    def ev10(self, value: float) -> None:
        self._cards[2].set_value("ev10", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[3].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        self._cards[3].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[3].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        self._cards[3].set_value("c2", value)

    @property
    def c3(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[3].get_value("c3")

    @c3.setter
    def c3(self, value: float) -> None:
        self._cards[3].set_value("c3", value)

    @property
    def c4(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[3].get_value("c4")

    @c4.setter
    def c4(self, value: float) -> None:
        self._cards[3].set_value("c4", value)

    @property
    def c5(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[3].get_value("c5")

    @c5.setter
    def c5(self, value: float) -> None:
        self._cards[3].set_value("c5", value)

    @property
    def c6(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[4].get_value("c6")

    @c6.setter
    def c6(self, value: float) -> None:
        self._cards[4].set_value("c6", value)

    @property
    def c7(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[4].get_value("c7")

    @c7.setter
    def c7(self, value: float) -> None:
        self._cards[4].set_value("c7", value)

    @property
    def c8(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[4].get_value("c8")

    @c8.setter
    def c8(self, value: float) -> None:
        self._cards[4].set_value("c8", value)

    @property
    def c9(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[4].get_value("c9")

    @c9.setter
    def c9(self, value: float) -> None:
        self._cards[4].set_value("c9", value)

    @property
    def c10(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[4].get_value("c10")

    @c10.setter
    def c10(self, value: float) -> None:
        self._cards[4].set_value("c10", value)

    @property
    def t1(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[5].get_value("t1")

    @t1.setter
    def t1(self, value: float) -> None:
        self._cards[5].set_value("t1", value)

    @property
    def t2(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[5].get_value("t2")

    @t2.setter
    def t2(self, value: float) -> None:
        self._cards[5].set_value("t2", value)

    @property
    def t3(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[5].get_value("t3")

    @t3.setter
    def t3(self, value: float) -> None:
        self._cards[5].set_value("t3", value)

    @property
    def t4(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[5].get_value("t4")

    @t4.setter
    def t4(self, value: float) -> None:
        self._cards[5].set_value("t4", value)

    @property
    def t5(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[5].get_value("t5")

    @t5.setter
    def t5(self, value: float) -> None:
        self._cards[5].set_value("t5", value)

    @property
    def t6(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[6].get_value("t6")

    @t6.setter
    def t6(self, value: float) -> None:
        self._cards[6].set_value("t6", value)

    @property
    def t7(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[6].get_value("t7")

    @t7.setter
    def t7(self, value: float) -> None:
        self._cards[6].set_value("t7", value)

    @property
    def t8(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[6].get_value("t8")

    @t8.setter
    def t8(self, value: float) -> None:
        self._cards[6].set_value("t8", value)

    @property
    def t9(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[6].get_value("t9")

    @t9.setter
    def t9(self, value: float) -> None:
        self._cards[6].set_value("t9", value)

    @property
    def t10(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[6].get_value("t10")

    @t10.setter
    def t10(self, value: float) -> None:
        self._cards[6].set_value("t10", value)

    @property
    def k1(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[7].get_value("k1")

    @k1.setter
    def k1(self, value: float) -> None:
        self._cards[7].set_value("k1", value)

    @property
    def k2(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[7].get_value("k2")

    @k2.setter
    def k2(self, value: float) -> None:
        self._cards[7].set_value("k2", value)

    @property
    def k3(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[7].get_value("k3")

    @k3.setter
    def k3(self, value: float) -> None:
        self._cards[7].set_value("k3", value)

    @property
    def k4(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[7].get_value("k4")

    @k4.setter
    def k4(self, value: float) -> None:
        self._cards[7].set_value("k4", value)

    @property
    def k5(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[7].get_value("k5")

    @k5.setter
    def k5(self, value: float) -> None:
        self._cards[7].set_value("k5", value)

    @property
    def k6(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[8].get_value("k6")

    @k6.setter
    def k6(self, value: float) -> None:
        self._cards[8].set_value("k6", value)

    @property
    def k7(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[8].get_value("k7")

    @k7.setter
    def k7(self, value: float) -> None:
        self._cards[8].set_value("k7", value)

    @property
    def k8(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[8].get_value("k8")

    @k8.setter
    def k8(self, value: float) -> None:
        self._cards[8].set_value("k8", value)

    @property
    def k9(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[8].get_value("k9")

    @k9.setter
    def k9(self, value: float) -> None:
        self._cards[8].set_value("k9", value)

    @property
    def k10(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[8].get_value("k10")

    @k10.setter
    def k10(self, value: float) -> None:
        self._cards[8].set_value("k10", value)

