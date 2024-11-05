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

class SetPorousAle(KeywordBase):
    """DYNA SET_POROUS_ALE keyword"""

    keyword = "SET"
    subkeyword = "POROUS_ALE"
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
                        "eidbeg",
                        int,
                        0,
                        10,
                        kwargs.get("eidbeg")
                    ),
                    Field(
                        "eidend",
                        int,
                        10,
                        10,
                        kwargs.get("eidend", 0)
                    ),
                    Field(
                        "local",
                        int,
                        20,
                        10,
                        kwargs.get("local", 0)
                    ),
                    Field(
                        "veccid1",
                        int,
                        30,
                        10,
                        kwargs.get("veccid1", 0)
                    ),
                    Field(
                        "veccid2",
                        int,
                        40,
                        10,
                        kwargs.get("veccid2", 0)
                    ),
                    Field(
                        "userdef",
                        int,
                        50,
                        10,
                        kwargs.get("userdef", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "axx",
                        float,
                        0,
                        10,
                        kwargs.get("axx", 0.0)
                    ),
                    Field(
                        "axy",
                        float,
                        10,
                        10,
                        kwargs.get("axy", 0.0)
                    ),
                    Field(
                        "axz",
                        float,
                        20,
                        10,
                        kwargs.get("axz", 0.0)
                    ),
                    Field(
                        "bxx",
                        float,
                        30,
                        10,
                        kwargs.get("bxx", 0.0)
                    ),
                    Field(
                        "bxy",
                        float,
                        40,
                        10,
                        kwargs.get("bxy", 0.0)
                    ),
                    Field(
                        "bxz",
                        float,
                        50,
                        10,
                        kwargs.get("bxz", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ayx",
                        float,
                        0,
                        10,
                        kwargs.get("ayx", 0.0)
                    ),
                    Field(
                        "ayy",
                        float,
                        10,
                        10,
                        kwargs.get("ayy", 0.0)
                    ),
                    Field(
                        "ayz",
                        float,
                        20,
                        10,
                        kwargs.get("ayz", 0.0)
                    ),
                    Field(
                        "byx",
                        float,
                        30,
                        10,
                        kwargs.get("byx", 0.0)
                    ),
                    Field(
                        "byy",
                        float,
                        40,
                        10,
                        kwargs.get("byy", 0.0)
                    ),
                    Field(
                        "byz",
                        float,
                        50,
                        10,
                        kwargs.get("byz", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "azx",
                        float,
                        0,
                        10,
                        kwargs.get("azx", 0.0)
                    ),
                    Field(
                        "azy",
                        float,
                        10,
                        10,
                        kwargs.get("azy", 0.0)
                    ),
                    Field(
                        "azz",
                        float,
                        20,
                        10,
                        kwargs.get("azz", 0.0)
                    ),
                    Field(
                        "bzx",
                        float,
                        30,
                        10,
                        kwargs.get("bzx", 0.0)
                    ),
                    Field(
                        "bzy",
                        float,
                        40,
                        10,
                        kwargs.get("bzy", 0.0)
                    ),
                    Field(
                        "bzz",
                        float,
                        50,
                        10,
                        kwargs.get("bzz", 0.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = SetPorousAle.option_specs[0],
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
    def eidbeg(self) -> typing.Optional[int]:
        """Get or set the EIDBEG is a set of thick porous elements.
        """ # nopep8
        return self._cards[0].get_value("eidbeg")

    @eidbeg.setter
    def eidbeg(self, value: int) -> None:
        self._cards[0].set_value("eidbeg", value)

    @property
    def eidend(self) -> int:
        """Get or set the EIDEND is a set of thin porous elements.
        """ # nopep8
        return self._cards[0].get_value("eidend")

    @eidend.setter
    def eidend(self, value: int) -> None:
        self._cards[0].set_value("eidend", value)

    @property
    def local(self) -> int:
        """Get or set the Flag to activate an element coordinate system:
        EQ.0:The forces are applied in the global directions.
        EQ.1:The forces are applied in a local system attached to the element. The system is consistent with DIREC=1 and CTYPE=12 in *CONSTRAINED_LAGRANGE_IN_SOLID.
        For CTYPE=11, LOCAL is always 1 and the -axis is aligned with the element normal while the -axis passes through the element center and the first node in the element connectivity
        (*ELEMENT_BEAM in 2D or *ELEMENT_SHELL in 3D)
        """ # nopep8
        return self._cards[0].get_value("local")

    @local.setter
    def local(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""local must be one of {0,1}""")
        self._cards[0].set_value("local", value)

    @property
    def veccid1(self) -> int:
        """Get or set the *DEFINE_VECTOR IDs to define a specific coordinate system. VECID1 and VECID2 give the x- and y-direction respectively.
        The z-vector is a cross product of VECID1 and VECID2. If this latter is not
        orthogonal to VECID1, its direction will be corrected with a cross-	product of z- and x-vectors. The vectors are stored as isoparametric.
        """ # nopep8
        return self._cards[0].get_value("veccid1")

    @veccid1.setter
    def veccid1(self, value: int) -> None:
        self._cards[0].set_value("veccid1", value)

    @property
    def veccid2(self) -> int:
        """Get or set the *DEFINE_VECTOR IDs to define a specific coordinate system. VECID1 and VECID2 give the x- and y-direction respectively.
        The z-vector is a cross product of VECID1 and VECID2. If this latter is not
        orthogonal to VECID1, its direction will be corrected with a cross-	product of z- and x-vectors. The vectors are stored as isoparametric.
        """ # nopep8
        return self._cards[0].get_value("veccid2")

    @veccid2.setter
    def veccid2(self, value: int) -> None:
        self._cards[0].set_value("veccid2", value)

    @property
    def userdef(self) -> int:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("userdef")

    @userdef.setter
    def userdef(self, value: int) -> None:
        self._cards[0].set_value("userdef", value)

    @property
    def axx(self) -> float:
        """Get or set the Viscous matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[1].get_value("axx")

    @axx.setter
    def axx(self, value: float) -> None:
        self._cards[1].set_value("axx", value)

    @property
    def axy(self) -> float:
        """Get or set the Viscous matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[1].get_value("axy")

    @axy.setter
    def axy(self, value: float) -> None:
        self._cards[1].set_value("axy", value)

    @property
    def axz(self) -> float:
        """Get or set the Viscous matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[1].get_value("axz")

    @axz.setter
    def axz(self, value: float) -> None:
        self._cards[1].set_value("axz", value)

    @property
    def bxx(self) -> float:
        """Get or set the Inertial matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[1].get_value("bxx")

    @bxx.setter
    def bxx(self, value: float) -> None:
        self._cards[1].set_value("bxx", value)

    @property
    def bxy(self) -> float:
        """Get or set the Inertial matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[1].get_value("bxy")

    @bxy.setter
    def bxy(self, value: float) -> None:
        self._cards[1].set_value("bxy", value)

    @property
    def bxz(self) -> float:
        """Get or set the Inertial matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[1].get_value("bxz")

    @bxz.setter
    def bxz(self, value: float) -> None:
        self._cards[1].set_value("bxz", value)

    @property
    def ayx(self) -> float:
        """Get or set the Viscous matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[2].get_value("ayx")

    @ayx.setter
    def ayx(self, value: float) -> None:
        self._cards[2].set_value("ayx", value)

    @property
    def ayy(self) -> float:
        """Get or set the Viscous matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[2].get_value("ayy")

    @ayy.setter
    def ayy(self, value: float) -> None:
        self._cards[2].set_value("ayy", value)

    @property
    def ayz(self) -> float:
        """Get or set the Viscous matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[2].get_value("ayz")

    @ayz.setter
    def ayz(self, value: float) -> None:
        self._cards[2].set_value("ayz", value)

    @property
    def byx(self) -> float:
        """Get or set the Inertial matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[2].get_value("byx")

    @byx.setter
    def byx(self, value: float) -> None:
        self._cards[2].set_value("byx", value)

    @property
    def byy(self) -> float:
        """Get or set the Inertial matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[2].get_value("byy")

    @byy.setter
    def byy(self, value: float) -> None:
        self._cards[2].set_value("byy", value)

    @property
    def byz(self) -> float:
        """Get or set the Inertial matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[2].get_value("byz")

    @byz.setter
    def byz(self, value: float) -> None:
        self._cards[2].set_value("byz", value)

    @property
    def azx(self) -> float:
        """Get or set the Viscous matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[3].get_value("azx")

    @azx.setter
    def azx(self, value: float) -> None:
        self._cards[3].set_value("azx", value)

    @property
    def azy(self) -> float:
        """Get or set the Viscous matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[3].get_value("azy")

    @azy.setter
    def azy(self, value: float) -> None:
        self._cards[3].set_value("azy", value)

    @property
    def azz(self) -> float:
        """Get or set the Viscous matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[3].get_value("azz")

    @azz.setter
    def azz(self, value: float) -> None:
        self._cards[3].set_value("azz", value)

    @property
    def bzx(self) -> float:
        """Get or set the Inertial matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[3].get_value("bzx")

    @bzx.setter
    def bzx(self, value: float) -> None:
        self._cards[3].set_value("bzx", value)

    @property
    def bzy(self) -> float:
        """Get or set the Inertial matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[3].get_value("bzy")

    @bzy.setter
    def bzy(self, value: float) -> None:
        self._cards[3].set_value("bzy", value)

    @property
    def bzz(self) -> float:
        """Get or set the Inertial matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[3].get_value("bzz")

    @bzz.setter
    def bzz(self, value: float) -> None:
        self._cards[3].set_value("bzz", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[4].cards[0].set_value("title", value)

