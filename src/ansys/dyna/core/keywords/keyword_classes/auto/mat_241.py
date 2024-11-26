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

class Mat241(KeywordBase):
    """DYNA MAT_241 keyword"""

    keyword = "MAT"
    subkeyword = "241"
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
                        "p1",
                        float,
                        30,
                        10,
                        kwargs.get("p1")
                    ),
                    Field(
                        "s1",
                        float,
                        40,
                        10,
                        kwargs.get("s1")
                    ),
                    Field(
                        "p2",
                        float,
                        50,
                        10,
                        kwargs.get("p2")
                    ),
                    Field(
                        "s2",
                        float,
                        60,
                        10,
                        kwargs.get("s2")
                    ),
                    Field(
                        "c",
                        float,
                        70,
                        10,
                        kwargs.get("c")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "epsi",
                        float,
                        0,
                        10,
                        kwargs.get("epsi")
                    ),
                    Field(
                        "t",
                        float,
                        10,
                        10,
                        kwargs.get("t")
                    ),
                    Field(
                        "unused",
                        float,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "alpha",
                        float,
                        30,
                        10,
                        kwargs.get("alpha")
                    ),
                    Field(
                        "sfmax",
                        float,
                        40,
                        10,
                        kwargs.get("sfmax")
                    ),
                    Field(
                        "beta",
                        float,
                        50,
                        10,
                        kwargs.get("beta")
                    ),
                    Field(
                        "dp1",
                        float,
                        60,
                        10,
                        kwargs.get("dp1")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "epfmin",
                        float,
                        0,
                        10,
                        kwargs.get("epfmin")
                    ),
                    Field(
                        "epfmax",
                        float,
                        10,
                        10,
                        kwargs.get("epfmax")
                    ),
                    Field(
                        "k1",
                        float,
                        20,
                        10,
                        kwargs.get("k1")
                    ),
                    Field(
                        "k2",
                        float,
                        30,
                        10,
                        kwargs.get("k2")
                    ),
                    Field(
                        "k3",
                        float,
                        40,
                        10,
                        kwargs.get("k3")
                    ),
                    Field(
                        "fs",
                        float,
                        50,
                        10,
                        kwargs.get("fs")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat241.option_specs[0],
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
        """Get or set the Density
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the Shear modulus
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        self._cards[0].set_value("g", value)

    @property
    def p1(self) -> typing.Optional[float]:
        """Get or set the Pressure point 1 for intact material
        """ # nopep8
        return self._cards[0].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        self._cards[0].set_value("p1", value)

    @property
    def s1(self) -> typing.Optional[float]:
        """Get or set the Effective stress at P1
        """ # nopep8
        return self._cards[0].get_value("s1")

    @s1.setter
    def s1(self, value: float) -> None:
        self._cards[0].set_value("s1", value)

    @property
    def p2(self) -> typing.Optional[float]:
        """Get or set the Pressure point 2 for intact material
        """ # nopep8
        return self._cards[0].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        self._cards[0].set_value("p2", value)

    @property
    def s2(self) -> typing.Optional[float]:
        """Get or set the Effective stress at P2
        """ # nopep8
        return self._cards[0].get_value("s2")

    @s2.setter
    def s2(self, value: float) -> None:
        self._cards[0].set_value("s2", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Strain rate sensitivity factor.
        """ # nopep8
        return self._cards[0].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        self._cards[0].set_value("c", value)

    @property
    def epsi(self) -> typing.Optional[float]:
        """Get or set the Quasi-static threshold strain rate. See *MAT_015.
        """ # nopep8
        return self._cards[1].get_value("epsi")

    @epsi.setter
    def epsi(self, value: float) -> None:
        self._cards[1].set_value("epsi", value)

    @property
    def t(self) -> typing.Optional[float]:
        """Get or set the Maximum tensile pressure strength. This value is positive in tension.
        """ # nopep8
        return self._cards[1].get_value("t")

    @t.setter
    def t(self, value: float) -> None:
        self._cards[1].set_value("t", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Slope of the fractured material strength curve.
        """ # nopep8
        return self._cards[1].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        self._cards[1].set_value("alpha", value)

    @property
    def sfmax(self) -> typing.Optional[float]:
        """Get or set the Maximum strength of the fractured material.
        """ # nopep8
        return self._cards[1].get_value("sfmax")

    @sfmax.setter
    def sfmax(self, value: float) -> None:
        self._cards[1].set_value("sfmax", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Fraction of elastic energy loss converted to hydrostatic energy (affects bulking pressure (history variable 1) that accompanies damage)..
        """ # nopep8
        return self._cards[1].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[1].set_value("beta", value)

    @property
    def dp1(self) -> typing.Optional[float]:
        """Get or set the Maximum compressive pressure strength. This value is positive in compression.
        """ # nopep8
        return self._cards[1].get_value("dp1")

    @dp1.setter
    def dp1(self, value: float) -> None:
        self._cards[1].set_value("dp1", value)

    @property
    def epfmin(self) -> typing.Optional[float]:
        """Get or set the Plastic strain for fracture at tensile pressure.
        """ # nopep8
        return self._cards[2].get_value("epfmin")

    @epfmin.setter
    def epfmin(self, value: float) -> None:
        self._cards[2].set_value("epfmin", value)

    @property
    def epfmax(self) -> typing.Optional[float]:
        """Get or set the Plastic strain for fracture at DP1.
        """ # nopep8
        return self._cards[2].get_value("epfmax")

    @epfmax.setter
    def epfmax(self, value: float) -> None:
        self._cards[2].set_value("epfmax", value)

    @property
    def k1(self) -> typing.Optional[float]:
        """Get or set the First pressure coefficient (equivalent to the bulk modulus).
        """ # nopep8
        return self._cards[2].get_value("k1")

    @k1.setter
    def k1(self, value: float) -> None:
        self._cards[2].set_value("k1", value)

    @property
    def k2(self) -> typing.Optional[float]:
        """Get or set the Second pressure coefficient.
        """ # nopep8
        return self._cards[2].get_value("k2")

    @k2.setter
    def k2(self, value: float) -> None:
        self._cards[2].set_value("k2", value)

    @property
    def k3(self) -> typing.Optional[float]:
        """Get or set the Third pressure coefficient.
        """ # nopep8
        return self._cards[2].get_value("k3")

    @k3.setter
    def k3(self, value: float) -> None:
        self._cards[2].set_value("k3", value)

    @property
    def fs(self) -> typing.Optional[float]:
        """Get or set the Element deletion criteria.
        FS < 0 delete if P < FS (tensile failure).
        FS = 0 no element deletion (default)..
        FS> 0 delete element if the strain > FS.
        """ # nopep8
        return self._cards[2].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        self._cards[2].set_value("fs", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

