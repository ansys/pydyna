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

class MatPitzerCrushableFoam(KeywordBase):
    """DYNA MAT_PITZER_CRUSHABLE_FOAM keyword"""

    keyword = "MAT"
    subkeyword = "PITZER_CRUSHABLE_FOAM"
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
                        "k",
                        float,
                        20,
                        10,
                        kwargs.get("k")
                    ),
                    Field(
                        "g",
                        float,
                        30,
                        10,
                        kwargs.get("g")
                    ),
                    Field(
                        "pr",
                        float,
                        40,
                        10,
                        kwargs.get("pr")
                    ),
                    Field(
                        "ty",
                        float,
                        50,
                        10,
                        kwargs.get("ty")
                    ),
                    Field(
                        "srtv",
                        float,
                        60,
                        10,
                        kwargs.get("srtv")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcpy",
                        int,
                        0,
                        10,
                        kwargs.get("lcpy")
                    ),
                    Field(
                        "lcuys",
                        int,
                        10,
                        10,
                        kwargs.get("lcuys")
                    ),
                    Field(
                        "lcsr",
                        int,
                        20,
                        10,
                        kwargs.get("lcsr")
                    ),
                    Field(
                        "vc",
                        float,
                        30,
                        10,
                        kwargs.get("vc")
                    ),
                    Field(
                        "dflg",
                        float,
                        40,
                        10,
                        kwargs.get("dflg")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatPitzerCrushableFoam.option_specs[0],
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
        """Get or set the Material identification.  A unique number has to be chosen.
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
    def k(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus.
        """ # nopep8
        return self._cards[0].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        self._cards[0].set_value("k", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the Shear modulus.
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        self._cards[0].set_value("g", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def ty(self) -> typing.Optional[float]:
        """Get or set the Tension yield.
        """ # nopep8
        return self._cards[0].get_value("ty")

    @ty.setter
    def ty(self, value: float) -> None:
        self._cards[0].set_value("ty", value)

    @property
    def srtv(self) -> typing.Optional[float]:
        """Get or set the Young's modulus (E).
        """ # nopep8
        return self._cards[0].get_value("srtv")

    @srtv.setter
    def srtv(self, value: float) -> None:
        self._cards[0].set_value("srtv", value)

    @property
    def lcpy(self) -> typing.Optional[int]:
        """Get or set the Load curve ID giving pressure versus volumetric strain, see Figure 20.24 in user's manual.
        """ # nopep8
        return self._cards[1].get_value("lcpy")

    @lcpy.setter
    def lcpy(self, value: int) -> None:
        self._cards[1].set_value("lcpy", value)

    @property
    def lcuys(self) -> typing.Optional[int]:
        """Get or set the Load curve ID giving uniaxial stress versus volumetric strain, see Figure 20.24 in user's manual.
        """ # nopep8
        return self._cards[1].get_value("lcuys")

    @lcuys.setter
    def lcuys(self, value: int) -> None:
        self._cards[1].set_value("lcuys", value)

    @property
    def lcsr(self) -> typing.Optional[int]:
        """Get or set the Load curve ID giving strain rate scale factor versus volumetric strain rate.
        """ # nopep8
        return self._cards[1].get_value("lcsr")

    @lcsr.setter
    def lcsr(self, value: int) -> None:
        self._cards[1].set_value("lcsr", value)

    @property
    def vc(self) -> typing.Optional[float]:
        """Get or set the Viscous damping coefficient (.05<recommended value<.50).
        """ # nopep8
        return self._cards[1].get_value("vc")

    @vc.setter
    def vc(self, value: float) -> None:
        self._cards[1].set_value("vc", value)

    @property
    def dflg(self) -> typing.Optional[float]:
        """Get or set the Density flag:
        EQ.0.0: use initial density
        EQ.1.0: use current density (larger step size with less mass scaling).
        """ # nopep8
        return self._cards[1].get_value("dflg")

    @dflg.setter
    def dflg(self, value: float) -> None:
        self._cards[1].set_value("dflg", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

