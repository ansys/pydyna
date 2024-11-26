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

class Mat087(KeywordBase):
    """DYNA MAT_087 keyword"""

    keyword = "MAT"
    subkeyword = "087"
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
                        "pr",
                        float,
                        20,
                        10,
                        kwargs.get("pr")
                    ),
                    Field(
                        "n",
                        int,
                        30,
                        10,
                        kwargs.get("n")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sgl",
                        float,
                        0,
                        10,
                        kwargs.get("sgl")
                    ),
                    Field(
                        "sw",
                        float,
                        10,
                        10,
                        kwargs.get("sw")
                    ),
                    Field(
                        "st",
                        float,
                        20,
                        10,
                        kwargs.get("st")
                    ),
                    Field(
                        "lcid",
                        int,
                        30,
                        10,
                        kwargs.get("lcid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c10",
                        float,
                        0,
                        10,
                        kwargs.get("c10")
                    ),
                    Field(
                        "c01",
                        float,
                        10,
                        10,
                        kwargs.get("c01")
                    ),
                    Field(
                        "c11",
                        float,
                        20,
                        10,
                        kwargs.get("c11")
                    ),
                    Field(
                        "c20",
                        float,
                        30,
                        10,
                        kwargs.get("c20")
                    ),
                    Field(
                        "c02",
                        float,
                        40,
                        10,
                        kwargs.get("c02")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "p0",
                        float,
                        0,
                        10,
                        kwargs.get("p0")
                    ),
                    Field(
                        "phi",
                        float,
                        10,
                        10,
                        kwargs.get("phi")
                    ),
                    Field(
                        "ivs",
                        float,
                        20,
                        10,
                        kwargs.get("ivs")
                    ),
                    Field(
                        "g",
                        float,
                        30,
                        10,
                        kwargs.get("g")
                    ),
                    Field(
                        "beta",
                        float,
                        40,
                        10,
                        kwargs.get("beta")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat087.option_specs[0],
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
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio, typical values are between 0.0 to 0.2. Due to the large compressibility of air, large values of Poisson's ratio generates physically meaningless results.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def n(self) -> typing.Optional[int]:
        """Get or set the Order of fit (currently < 3). If n>0 then a least square fit is computed with uniaxial data. The parameters given on card 2 should be specified. Also see *MAT_MOONEY_RIVLIN_RUBBER (material model 27). A Poisson's ratio of .5 is assumed for the void free rubber during the fit. The Poisson's ratio defined on Card 1 is for the cellular rubber. A void fraction formulation is used.
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: int) -> None:
        self._cards[0].set_value("n", value)

    @property
    def sgl(self) -> typing.Optional[float]:
        """Get or set the Specimen gauge length l0.
        """ # nopep8
        return self._cards[1].get_value("sgl")

    @sgl.setter
    def sgl(self, value: float) -> None:
        self._cards[1].set_value("sgl", value)

    @property
    def sw(self) -> typing.Optional[float]:
        """Get or set the Specimen width.
        """ # nopep8
        return self._cards[1].get_value("sw")

    @sw.setter
    def sw(self, value: float) -> None:
        self._cards[1].set_value("sw", value)

    @property
    def st(self) -> typing.Optional[float]:
        """Get or set the Specimen thickness.
        """ # nopep8
        return self._cards[1].get_value("st")

    @st.setter
    def st(self, value: float) -> None:
        self._cards[1].set_value("st", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID giving the force versus actual change dL in the gauge length.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[1].set_value("lcid", value)

    @property
    def c10(self) -> typing.Optional[float]:
        """Get or set the Coefficient, C10.
        """ # nopep8
        return self._cards[2].get_value("c10")

    @c10.setter
    def c10(self, value: float) -> None:
        self._cards[2].set_value("c10", value)

    @property
    def c01(self) -> typing.Optional[float]:
        """Get or set the Coefficient, C01.
        """ # nopep8
        return self._cards[2].get_value("c01")

    @c01.setter
    def c01(self, value: float) -> None:
        self._cards[2].set_value("c01", value)

    @property
    def c11(self) -> typing.Optional[float]:
        """Get or set the Coefficient, C11.
        """ # nopep8
        return self._cards[2].get_value("c11")

    @c11.setter
    def c11(self, value: float) -> None:
        self._cards[2].set_value("c11", value)

    @property
    def c20(self) -> typing.Optional[float]:
        """Get or set the Coefficient, C20.
        """ # nopep8
        return self._cards[2].get_value("c20")

    @c20.setter
    def c20(self, value: float) -> None:
        self._cards[2].set_value("c20", value)

    @property
    def c02(self) -> typing.Optional[float]:
        """Get or set the Coefficient, C02.
        """ # nopep8
        return self._cards[2].get_value("c02")

    @c02.setter
    def c02(self, value: float) -> None:
        self._cards[2].set_value("c02", value)

    @property
    def p0(self) -> typing.Optional[float]:
        """Get or set the Initial air pressure, P0.
        """ # nopep8
        return self._cards[3].get_value("p0")

    @p0.setter
    def p0(self, value: float) -> None:
        self._cards[3].set_value("p0", value)

    @property
    def phi(self) -> typing.Optional[float]:
        """Get or set the Ratio of cellular rubber to rubber density.
        """ # nopep8
        return self._cards[3].get_value("phi")

    @phi.setter
    def phi(self, value: float) -> None:
        self._cards[3].set_value("phi", value)

    @property
    def ivs(self) -> typing.Optional[float]:
        """Get or set the Initial volumetric strain.
        """ # nopep8
        return self._cards[3].get_value("ivs")

    @ivs.setter
    def ivs(self, value: float) -> None:
        self._cards[3].set_value("ivs", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the Optional shear relaxation modulus, G, for rate effects (viscosity).
        """ # nopep8
        return self._cards[3].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        self._cards[3].set_value("g", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Optional decay constant.
        """ # nopep8
        return self._cards[3].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[3].set_value("beta", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[4].cards[0].set_value("title", value)

