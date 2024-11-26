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

class MatAleIncompressible(KeywordBase):
    """DYNA MAT_ALE_INCOMPRESSIBLE keyword"""

    keyword = "MAT"
    subkeyword = "ALE_INCOMPRESSIBLE"
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
                        "mu",
                        float,
                        30,
                        10,
                        kwargs.get("mu")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "tol",
                        float,
                        0,
                        10,
                        kwargs.get("tol", 1e-8)
                    ),
                    Field(
                        "dtout",
                        float,
                        10,
                        10,
                        kwargs.get("dtout", 1e10)
                    ),
                    Field(
                        "ncg",
                        int,
                        20,
                        10,
                        kwargs.get("ncg", 50)
                    ),
                    Field(
                        "meth",
                        int,
                        30,
                        10,
                        kwargs.get("meth", -7)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatAleIncompressible.option_specs[0],
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
        """Get or set the Material ID. A unique number or label not exceeding 8 charaters
        must be specified. Material ID is referenced in the *PART card and must be unique.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Material density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def pc(self) -> typing.Optional[float]:
        """Get or set the Pressure cutoff (< or = 0.0).
        """ # nopep8
        return self._cards[0].get_value("pc")

    @pc.setter
    def pc(self, value: float) -> None:
        self._cards[0].set_value("pc", value)

    @property
    def mu(self) -> typing.Optional[float]:
        """Get or set the Dynamic viscosity coefficient.
        """ # nopep8
        return self._cards[0].get_value("mu")

    @mu.setter
    def mu(self, value: float) -> None:
        self._cards[0].set_value("mu", value)

    @property
    def tol(self) -> float:
        """Get or set the Tolerance for the convergence of the conjugate gradient.
        """ # nopep8
        return self._cards[1].get_value("tol")

    @tol.setter
    def tol(self, value: float) -> None:
        self._cards[1].set_value("tol", value)

    @property
    def dtout(self) -> float:
        """Get or set the Time interval between screen outputs.
        """ # nopep8
        return self._cards[1].get_value("dtout")

    @dtout.setter
    def dtout(self, value: float) -> None:
        self._cards[1].set_value("dtout", value)

    @property
    def ncg(self) -> int:
        """Get or set the Maximum number of loops in the conjugate gradient.
        """ # nopep8
        return self._cards[1].get_value("ncg")

    @ncg.setter
    def ncg(self, value: int) -> None:
        self._cards[1].set_value("ncg", value)

    @property
    def meth(self) -> int:
        """Get or set the Conjugate gradient methods:
        EQ.-6: solves the poisson equation for the pressure
        EQ.-7: solves the poisson equation for the pressure increment.
        """ # nopep8
        return self._cards[1].get_value("meth")

    @meth.setter
    def meth(self, value: int) -> None:
        if value not in [-7, -6]:
            raise Exception("""meth must be one of {-7,-6}""")
        self._cards[1].set_value("meth", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

