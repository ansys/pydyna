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

class MatConstrainedSpr2(KeywordBase):
    """DYNA MAT_CONSTRAINED_SPR2 keyword"""

    keyword = "MAT"
    subkeyword = "CONSTRAINED_SPR2"
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
                        "fn",
                        float,
                        20,
                        10,
                        kwargs.get("fn")
                    ),
                    Field(
                        "ft",
                        float,
                        30,
                        10,
                        kwargs.get("ft")
                    ),
                    Field(
                        "dn",
                        float,
                        40,
                        10,
                        kwargs.get("dn")
                    ),
                    Field(
                        "dt",
                        float,
                        50,
                        10,
                        kwargs.get("dt")
                    ),
                    Field(
                        "xin",
                        float,
                        60,
                        10,
                        kwargs.get("xin")
                    ),
                    Field(
                        "xit",
                        float,
                        70,
                        10,
                        kwargs.get("xit")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "alpha1",
                        float,
                        0,
                        10,
                        kwargs.get("alpha1")
                    ),
                    Field(
                        "alpha2",
                        float,
                        10,
                        10,
                        kwargs.get("alpha2")
                    ),
                    Field(
                        "alpha3",
                        float,
                        20,
                        10,
                        kwargs.get("alpha3")
                    ),
                    Field(
                        "expn",
                        float,
                        30,
                        10,
                        kwargs.get("expn", 8.0)
                    ),
                    Field(
                        "expt",
                        float,
                        40,
                        10,
                        kwargs.get("expt", 8.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatConstrainedSpr2.option_specs[0],
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
    def fn(self) -> typing.Optional[float]:
        """Get or set the Rivet strength in tension (pull-out).
        """ # nopep8
        return self._cards[0].get_value("fn")

    @fn.setter
    def fn(self, value: float) -> None:
        self._cards[0].set_value("fn", value)

    @property
    def ft(self) -> typing.Optional[float]:
        """Get or set the Rivet strength in pure shear.
        """ # nopep8
        return self._cards[0].get_value("ft")

    @ft.setter
    def ft(self, value: float) -> None:
        self._cards[0].set_value("ft", value)

    @property
    def dn(self) -> typing.Optional[float]:
        """Get or set the Failure displacement in normal direction.
        """ # nopep8
        return self._cards[0].get_value("dn")

    @dn.setter
    def dn(self, value: float) -> None:
        self._cards[0].set_value("dn", value)

    @property
    def dt(self) -> typing.Optional[float]:
        """Get or set the Failure displacement in tangential direction.
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[0].set_value("dt", value)

    @property
    def xin(self) -> typing.Optional[float]:
        """Get or set the Fraction of failure displacement at maximum normal force.
        """ # nopep8
        return self._cards[0].get_value("xin")

    @xin.setter
    def xin(self, value: float) -> None:
        self._cards[0].set_value("xin", value)

    @property
    def xit(self) -> typing.Optional[float]:
        """Get or set the Fraction of failure displacement at maximum tangential force.
        """ # nopep8
        return self._cards[0].get_value("xit")

    @xit.setter
    def xit(self, value: float) -> None:
        self._cards[0].set_value("xit", value)

    @property
    def alpha1(self) -> typing.Optional[float]:
        """Get or set the Dimensionless parameter scaling the effective displacement.
        """ # nopep8
        return self._cards[1].get_value("alpha1")

    @alpha1.setter
    def alpha1(self, value: float) -> None:
        self._cards[1].set_value("alpha1", value)

    @property
    def alpha2(self) -> typing.Optional[float]:
        """Get or set the Dimensionless parameter scaling the effective displacement.
        """ # nopep8
        return self._cards[1].get_value("alpha2")

    @alpha2.setter
    def alpha2(self, value: float) -> None:
        self._cards[1].set_value("alpha2", value)

    @property
    def alpha3(self) -> typing.Optional[float]:
        """Get or set the Dimensionless parameter scaling the effective displacement.
        """ # nopep8
        return self._cards[1].get_value("alpha3")

    @alpha3.setter
    def alpha3(self, value: float) -> None:
        self._cards[1].set_value("alpha3", value)

    @property
    def expn(self) -> float:
        """Get or set the Exponent value for load function in normal direction.
        """ # nopep8
        return self._cards[1].get_value("expn")

    @expn.setter
    def expn(self, value: float) -> None:
        self._cards[1].set_value("expn", value)

    @property
    def expt(self) -> float:
        """Get or set the Exponent value for load function in tangential direction.
        """ # nopep8
        return self._cards[1].get_value("expt")

    @expt.setter
    def expt(self, value: float) -> None:
        self._cards[1].set_value("expt", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

