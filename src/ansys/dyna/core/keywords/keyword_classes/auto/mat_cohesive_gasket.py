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

class MatCohesiveGasket(KeywordBase):
    """DYNA MAT_COHESIVE_GASKET keyword"""

    keyword = "MAT"
    subkeyword = "COHESIVE_GASKET"
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
                        "roflg",
                        int,
                        20,
                        10,
                        kwargs.get("roflg", 0)
                    ),
                    Field(
                        "intfail",
                        int,
                        30,
                        10,
                        kwargs.get("intfail")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lc",
                        int,
                        0,
                        10,
                        kwargs.get("lc")
                    ),
                    Field(
                        "uc",
                        int,
                        10,
                        10,
                        kwargs.get("uc")
                    ),
                    Field(
                        "eten",
                        float,
                        20,
                        10,
                        kwargs.get("eten")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "etsr",
                        float,
                        0,
                        10,
                        kwargs.get("etsr")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "emem",
                        float,
                        0,
                        10,
                        kwargs.get("emem")
                    ),
                    Field(
                        "pr",
                        float,
                        10,
                        10,
                        kwargs.get("pr")
                    ),
                    Field(
                        "ps",
                        int,
                        20,
                        10,
                        kwargs.get("ps", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatCohesiveGasket.option_specs[0],
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
        """Get or set the Material identification. A unique number
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def roflg(self) -> int:
        """Get or set the Flag for whether density is specified per unit area or volume:
        EQ.0:	Density is per unit volume(default).
        EQ.1 : Density is per unit area for controlling the mass of cohesive elements with an initial volume of zero
        """ # nopep8
        return self._cards[0].get_value("roflg")

    @roflg.setter
    def roflg(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""roflg must be one of {0,1}""")
        self._cards[0].set_value("roflg", value)

    @property
    def intfail(self) -> typing.Optional[int]:
        """Get or set the Quadrature rule. Note that this material has no failure
        LE.0.0:	2 x 2 Newton - Cotes quadrature.
        GT.0.0 : 2 x 2 Gaussian quadrature
        """ # nopep8
        return self._cards[0].get_value("intfail")

    @intfail.setter
    def intfail(self, value: int) -> None:
        self._cards[0].set_value("intfail", value)

    @property
    def lc(self) -> typing.Optional[int]:
        """Get or set the Main load curve ID defining the pressure as function of closure, p = p(c)
        """ # nopep8
        return self._cards[1].get_value("lc")

    @lc.setter
    def lc(self, value: int) -> None:
        self._cards[1].set_value("lc", value)

    @property
    def uc(self) -> typing.Optional[int]:
        """Get or set the Table ID defining the unloading curves
        """ # nopep8
        return self._cards[1].get_value("uc")

    @uc.setter
    def uc(self, value: int) -> None:
        self._cards[1].set_value("uc", value)

    @property
    def eten(self) -> typing.Optional[float]:
        """Get or set the Tensile stiffness
        """ # nopep8
        return self._cards[1].get_value("eten")

    @eten.setter
    def eten(self, value: float) -> None:
        self._cards[1].set_value("eten", value)

    @property
    def etsr(self) -> typing.Optional[float]:
        """Get or set the Transverse shear stiffness
        """ # nopep8
        return self._cards[2].get_value("etsr")

    @etsr.setter
    def etsr(self, value: float) -> None:
        self._cards[2].set_value("etsr", value)

    @property
    def emem(self) -> typing.Optional[float]:
        """Get or set the Membrane stiffness
        """ # nopep8
        return self._cards[3].get_value("emem")

    @emem.setter
    def emem(self, value: float) -> None:
        self._cards[3].set_value("emem", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Membrane Poisson ratio
        """ # nopep8
        return self._cards[3].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[3].set_value("pr", value)

    @property
    def ps(self) -> int:
        """Get or set the Membrane plane stress or plain strain assumption:
        EQ.0:	Plane stress(default)
        EQ.1 : Plane strain
        """ # nopep8
        return self._cards[3].get_value("ps")

    @ps.setter
    def ps(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ps must be one of {0,1}""")
        self._cards[3].set_value("ps", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[4].cards[0].set_value("title", value)

