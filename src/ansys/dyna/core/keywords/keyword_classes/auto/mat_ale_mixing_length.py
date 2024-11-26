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

class MatAleMixingLength(KeywordBase):
    """DYNA MAT_ALE_MIXING_LENGTH keyword"""

    keyword = "MAT"
    subkeyword = "ALE_MIXING_LENGTH"
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
                        "pc",
                        float,
                        20,
                        10,
                        kwargs.get("pc")
                    ),
                    Field(
                        "mulo",
                        float,
                        30,
                        10,
                        kwargs.get("mulo")
                    ),
                    Field(
                        "muhi",
                        float,
                        40,
                        10,
                        kwargs.get("muhi")
                    ),
                    Field(
                        "rk",
                        float,
                        50,
                        10,
                        kwargs.get("rk")
                    ),
                    Field(
                        "unused",
                        float,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "rn",
                        float,
                        70,
                        10,
                        kwargs.get("rn")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lci",
                        float,
                        0,
                        10,
                        kwargs.get("lci")
                    ),
                    Field(
                        "c1",
                        float,
                        10,
                        10,
                        kwargs.get("c1")
                    ),
                    Field(
                        "c2",
                        float,
                        20,
                        10,
                        kwargs.get("c2")
                    ),
                    Field(
                        "c3",
                        float,
                        30,
                        10,
                        kwargs.get("c3")
                    ),
                    Field(
                        "c4",
                        float,
                        40,
                        10,
                        kwargs.get("c4")
                    ),
                    Field(
                        "c5",
                        float,
                        50,
                        10,
                        kwargs.get("c5")
                    ),
                    Field(
                        "c6",
                        float,
                        60,
                        10,
                        kwargs.get("c6")
                    ),
                    Field(
                        "c7",
                        float,
                        70,
                        10,
                        kwargs.get("c7")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcx",
                        float,
                        0,
                        10,
                        kwargs.get("lcx")
                    ),
                    Field(
                        "d0",
                        float,
                        10,
                        10,
                        kwargs.get("d0")
                    ),
                    Field(
                        "d1",
                        float,
                        20,
                        10,
                        kwargs.get("d1")
                    ),
                    Field(
                        "d2",
                        float,
                        30,
                        10,
                        kwargs.get("d2")
                    ),
                    Field(
                        "e0",
                        float,
                        40,
                        10,
                        kwargs.get("e0")
                    ),
                    Field(
                        "e1",
                        float,
                        50,
                        10,
                        kwargs.get("e1")
                    ),
                    Field(
                        "e2",
                        float,
                        60,
                        10,
                        kwargs.get("e2")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatAleMixingLength.option_specs[0],
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
    def pc(self) -> typing.Optional[float]:
        """Get or set the Pressure cutoff (<=0.0).
        """ # nopep8
        return self._cards[0].get_value("pc")

    @pc.setter
    def pc(self, value: float) -> None:
        self._cards[0].set_value("pc", value)

    @property
    def mulo(self) -> typing.Optional[float]:
        """Get or set the There are 3 possible cases: (1) If MULO > 0.0, and MUHI = 0.0 or is
        not defined, then this is the traditional constant dynamic viscosity coefficientμ. (2) If MULO > 0.0, and MUHI > 0.0, then MULO and
        MUHI are lower and upper viscosity limit values. (3) If MULO is negative (for example, MULO = -1), then a user-input data load
        curve (with LCID = 1) defining dynamic viscosity as a function of equivalent strain rate is used.
        """ # nopep8
        return self._cards[0].get_value("mulo")

    @mulo.setter
    def mulo(self, value: float) -> None:
        self._cards[0].set_value("mulo", value)

    @property
    def muhi(self) -> typing.Optional[float]:
        """Get or set the Upper dynamic viscosity limit (default = 0.0). This is defined only if RK and RN are defined for the variable viscosity case
        """ # nopep8
        return self._cards[0].get_value("muhi")

    @muhi.setter
    def muhi(self, value: float) -> None:
        self._cards[0].set_value("muhi", value)

    @property
    def rk(self) -> typing.Optional[float]:
        """Get or set the Variable dynamic viscosity multiplie
        """ # nopep8
        return self._cards[0].get_value("rk")

    @rk.setter
    def rk(self, value: float) -> None:
        self._cards[0].set_value("rk", value)

    @property
    def rn(self) -> typing.Optional[float]:
        """Get or set the Variable dynamic viscosity exponent
        """ # nopep8
        return self._cards[0].get_value("rn")

    @rn.setter
    def rn(self, value: float) -> None:
        self._cards[0].set_value("rn", value)

    @property
    def lci(self) -> typing.Optional[float]:
        """Get or set the Characteristic length, lci , of the internal turbulent domain.
        """ # nopep8
        return self._cards[1].get_value("lci")

    @lci.setter
    def lci(self, value: float) -> None:
        self._cards[1].set_value("lci", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the Internal flow mixing length polynomial coefficients.
        """ # nopep8
        return self._cards[1].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        self._cards[1].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the Internal flow mixing length polynomial coefficients.
        """ # nopep8
        return self._cards[1].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        self._cards[1].set_value("c2", value)

    @property
    def c3(self) -> typing.Optional[float]:
        """Get or set the Internal flow mixing length polynomial coefficients.
        """ # nopep8
        return self._cards[1].get_value("c3")

    @c3.setter
    def c3(self, value: float) -> None:
        self._cards[1].set_value("c3", value)

    @property
    def c4(self) -> typing.Optional[float]:
        """Get or set the Internal flow mixing length polynomial coefficients
        """ # nopep8
        return self._cards[1].get_value("c4")

    @c4.setter
    def c4(self, value: float) -> None:
        self._cards[1].set_value("c4", value)

    @property
    def c5(self) -> typing.Optional[float]:
        """Get or set the Internal flow mixing length polynomial coefficients
        """ # nopep8
        return self._cards[1].get_value("c5")

    @c5.setter
    def c5(self, value: float) -> None:
        self._cards[1].set_value("c5", value)

    @property
    def c6(self) -> typing.Optional[float]:
        """Get or set the Internal flow mixing length polynomial coefficients
        """ # nopep8
        return self._cards[1].get_value("c6")

    @c6.setter
    def c6(self, value: float) -> None:
        self._cards[1].set_value("c6", value)

    @property
    def c7(self) -> typing.Optional[float]:
        """Get or set the Internal flow mixing length polynomial coefficients
        """ # nopep8
        return self._cards[1].get_value("c7")

    @c7.setter
    def c7(self, value: float) -> None:
        self._cards[1].set_value("c7", value)

    @property
    def lcx(self) -> typing.Optional[float]:
        """Get or set the Characteristic length, lcx , of the external turbulent domain.
        """ # nopep8
        return self._cards[2].get_value("lcx")

    @lcx.setter
    def lcx(self, value: float) -> None:
        self._cards[2].set_value("lcx", value)

    @property
    def d0(self) -> typing.Optional[float]:
        """Get or set the External flow mixing length polynomial coefficients.
        """ # nopep8
        return self._cards[2].get_value("d0")

    @d0.setter
    def d0(self, value: float) -> None:
        self._cards[2].set_value("d0", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the External flow mixing length polynomial coefficients.
        """ # nopep8
        return self._cards[2].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[2].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the External flow mixing length polynomial coefficients.
        """ # nopep8
        return self._cards[2].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[2].set_value("d2", value)

    @property
    def e0(self) -> typing.Optional[float]:
        """Get or set the External flow mixing length polynomial coefficients
        """ # nopep8
        return self._cards[2].get_value("e0")

    @e0.setter
    def e0(self, value: float) -> None:
        self._cards[2].set_value("e0", value)

    @property
    def e1(self) -> typing.Optional[float]:
        """Get or set the External flow mixing length polynomial coefficients
        """ # nopep8
        return self._cards[2].get_value("e1")

    @e1.setter
    def e1(self, value: float) -> None:
        self._cards[2].set_value("e1", value)

    @property
    def e2(self) -> typing.Optional[float]:
        """Get or set the External flow mixing length polynomial coefficients
        """ # nopep8
        return self._cards[2].get_value("e2")

    @e2.setter
    def e2(self, value: float) -> None:
        self._cards[2].set_value("e2", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

