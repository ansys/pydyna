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

class MatThermalIsotropicTd(KeywordBase):
    """DYNA MAT_THERMAL_ISOTROPIC_TD keyword"""

    keyword = "MAT"
    subkeyword = "THERMAL_ISOTROPIC_TD"
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
                        float,
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
                        "tlat",
                        float,
                        40,
                        10,
                        kwargs.get("tlat")
                    ),
                    Field(
                        "hlat",
                        float,
                        50,
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
                        "k1",
                        float,
                        0,
                        10,
                        kwargs.get("k1")
                    ),
                    Field(
                        "k2",
                        float,
                        10,
                        10,
                        kwargs.get("k2")
                    ),
                    Field(
                        "k3",
                        float,
                        20,
                        10,
                        kwargs.get("k3")
                    ),
                    Field(
                        "k4",
                        float,
                        30,
                        10,
                        kwargs.get("k4")
                    ),
                    Field(
                        "k5",
                        float,
                        40,
                        10,
                        kwargs.get("k5")
                    ),
                    Field(
                        "k6",
                        float,
                        50,
                        10,
                        kwargs.get("k6")
                    ),
                    Field(
                        "k7",
                        float,
                        60,
                        10,
                        kwargs.get("k7")
                    ),
                    Field(
                        "k8",
                        float,
                        70,
                        10,
                        kwargs.get("k8")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatThermalIsotropicTd.option_specs[0],
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
        """Get or set the Thermal conductivity at T1al material identification, a unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("tmid")

    @tmid.setter
    def tmid(self, value: int) -> None:
        self._cards[0].set_value("tmid", value)

    @property
    def tro(self) -> typing.Optional[float]:
        """Get or set the Thermal density:
        EQ 0.0 structural density(default).
        """ # nopep8
        return self._cards[0].get_value("tro")

    @tro.setter
    def tro(self, value: float) -> None:
        self._cards[0].set_value("tro", value)

    @property
    def tgrlc(self) -> typing.Optional[float]:
        """Get or set the Thermal generation rate (see *DEFINE_CURVE).
        GT.0:	Load curve ID giving thermal generation rate as a function of time
        EQ.0 : Thermal generation rate is the constant multiplier, TGMULT.
        LT.0 : | TGRLC | is a load curve ID defining thermal generation rate as a function of temperature.
        """ # nopep8
        return self._cards[0].get_value("tgrlc")

    @tgrlc.setter
    def tgrlc(self, value: float) -> None:
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
        """Get or set the Temperature T1.
        """ # nopep8
        return self._cards[1].get_value("t1")

    @t1.setter
    def t1(self, value: float) -> None:
        self._cards[1].set_value("t1", value)

    @property
    def t2(self) -> typing.Optional[float]:
        """Get or set the Temperature T2.
        """ # nopep8
        return self._cards[1].get_value("t2")

    @t2.setter
    def t2(self, value: float) -> None:
        self._cards[1].set_value("t2", value)

    @property
    def t3(self) -> typing.Optional[float]:
        """Get or set the Temperature T3.
        """ # nopep8
        return self._cards[1].get_value("t3")

    @t3.setter
    def t3(self, value: float) -> None:
        self._cards[1].set_value("t3", value)

    @property
    def t4(self) -> typing.Optional[float]:
        """Get or set the Temperature T4.
        """ # nopep8
        return self._cards[1].get_value("t4")

    @t4.setter
    def t4(self, value: float) -> None:
        self._cards[1].set_value("t4", value)

    @property
    def t5(self) -> typing.Optional[float]:
        """Get or set the Temperature T5.
        """ # nopep8
        return self._cards[1].get_value("t5")

    @t5.setter
    def t5(self, value: float) -> None:
        self._cards[1].set_value("t5", value)

    @property
    def t6(self) -> typing.Optional[float]:
        """Get or set the Temperature T6.
        """ # nopep8
        return self._cards[1].get_value("t6")

    @t6.setter
    def t6(self, value: float) -> None:
        self._cards[1].set_value("t6", value)

    @property
    def t7(self) -> typing.Optional[float]:
        """Get or set the Temperature T7.
        """ # nopep8
        return self._cards[1].get_value("t7")

    @t7.setter
    def t7(self, value: float) -> None:
        self._cards[1].set_value("t7", value)

    @property
    def t8(self) -> typing.Optional[float]:
        """Get or set the Temperature T8.
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
    def k1(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity at T1.
        """ # nopep8
        return self._cards[3].get_value("k1")

    @k1.setter
    def k1(self, value: float) -> None:
        self._cards[3].set_value("k1", value)

    @property
    def k2(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity at T2.
        """ # nopep8
        return self._cards[3].get_value("k2")

    @k2.setter
    def k2(self, value: float) -> None:
        self._cards[3].set_value("k2", value)

    @property
    def k3(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity at T3.
        """ # nopep8
        return self._cards[3].get_value("k3")

    @k3.setter
    def k3(self, value: float) -> None:
        self._cards[3].set_value("k3", value)

    @property
    def k4(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity at T4.
        """ # nopep8
        return self._cards[3].get_value("k4")

    @k4.setter
    def k4(self, value: float) -> None:
        self._cards[3].set_value("k4", value)

    @property
    def k5(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity at T5.
        """ # nopep8
        return self._cards[3].get_value("k5")

    @k5.setter
    def k5(self, value: float) -> None:
        self._cards[3].set_value("k5", value)

    @property
    def k6(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity at T6.
        """ # nopep8
        return self._cards[3].get_value("k6")

    @k6.setter
    def k6(self, value: float) -> None:
        self._cards[3].set_value("k6", value)

    @property
    def k7(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity at T7.
        """ # nopep8
        return self._cards[3].get_value("k7")

    @k7.setter
    def k7(self, value: float) -> None:
        self._cards[3].set_value("k7", value)

    @property
    def k8(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity at T8.
        """ # nopep8
        return self._cards[3].get_value("k8")

    @k8.setter
    def k8(self, value: float) -> None:
        self._cards[3].set_value("k8", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[4].cards[0].set_value("title", value)

