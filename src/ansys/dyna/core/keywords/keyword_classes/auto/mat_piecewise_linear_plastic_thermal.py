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

class MatPiecewiseLinearPlasticThermal(KeywordBase):
    """DYNA MAT_PIECEWISE_LINEAR_PLASTIC_THERMAL keyword"""

    keyword = "MAT"
    subkeyword = "PIECEWISE_LINEAR_PLASTIC_THERMAL"
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
                        "c",
                        float,
                        40,
                        10,
                        kwargs.get("c")
                    ),
                    Field(
                        "p",
                        float,
                        50,
                        10,
                        kwargs.get("p")
                    ),
                    Field(
                        "fail",
                        float,
                        60,
                        10,
                        kwargs.get("fail")
                    ),
                    Field(
                        "tdel",
                        float,
                        70,
                        10,
                        kwargs.get("tdel")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "tabidc",
                        int,
                        0,
                        10,
                        kwargs.get("tabidc")
                    ),
                    Field(
                        "tabidt",
                        int,
                        10,
                        10,
                        kwargs.get("tabidt")
                    ),
                    Field(
                        "lalpha",
                        int,
                        20,
                        10,
                        kwargs.get("lalpha")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "alpha	",
                        float,
                        0,
                        10,
                        kwargs.get("alpha	")
                    ),
                    Field(
                        "tref",
                        float,
                        10,
                        10,
                        kwargs.get("tref")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatPiecewiseLinearPlasticThermal.option_specs[0],
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
        """Get or set the Material identification.  A unique number or label must be specified.
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
        """Get or set the Strain rate parameter.
        """ # nopep8
        return self._cards[0].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        self._cards[0].set_value("c", value)

    @property
    def p(self) -> typing.Optional[float]:
        """Get or set the Strain rate parameter.
        """ # nopep8
        return self._cards[0].get_value("p")

    @p.setter
    def p(self, value: float) -> None:
        self._cards[0].set_value("p", value)

    @property
    def fail(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain when the material fails. Note that for solids the *MAT_ADD_EROSION can be used for additional failure criteria.
        """ # nopep8
        return self._cards[0].get_value("fail")

    @fail.setter
    def fail(self, value: float) -> None:
        self._cards[0].set_value("fail", value)

    @property
    def tdel(self) -> typing.Optional[float]:
        """Get or set the A time step less then TDEL is not allowed. A step size less than TDEL trigger automatic element deletion. This option is ignored for implicit analyses.
        """ # nopep8
        return self._cards[0].get_value("tdel")

    @tdel.setter
    def tdel(self, value: float) -> None:
        self._cards[0].set_value("tdel", value)

    @property
    def tabidc(self) -> typing.Optional[int]:
        """Get or set the Table ID for yield stress in compression.
        """ # nopep8
        return self._cards[1].get_value("tabidc")

    @tabidc.setter
    def tabidc(self, value: int) -> None:
        self._cards[1].set_value("tabidc", value)

    @property
    def tabidt(self) -> typing.Optional[int]:
        """Get or set the Table ID for yield stress in tension.
        """ # nopep8
        return self._cards[1].get_value("tabidt")

    @tabidt.setter
    def tabidt(self, value: int) -> None:
        self._cards[1].set_value("tabidt", value)

    @property
    def lalpha(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for thermal expansion coefficient as a function of temperature.
        """ # nopep8
        return self._cards[1].get_value("lalpha")

    @lalpha.setter
    def lalpha(self, value: int) -> None:
        self._cards[1].set_value("lalpha", value)

    @property
    def alpha	(self) -> typing.Optional[float]:
        """Get or set the Coefficient of thermal expansion.
        """ # nopep8
        return self._cards[2].get_value("alpha	")

    @alpha	.setter
    def alpha	(self, value: float) -> None:
        self._cards[2].set_value("alpha	", value)

    @property
    def tref(self) -> typing.Optional[float]:
        """Get or set the Reference temperature, which is required if and only if LALPHA is given with a negative load curve ID.
        """ # nopep8
        return self._cards[2].get_value("tref")

    @tref.setter
    def tref(self, value: float) -> None:
        self._cards[2].set_value("tref", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

