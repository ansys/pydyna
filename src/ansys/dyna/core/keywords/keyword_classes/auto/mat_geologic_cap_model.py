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

class MatGeologicCapModel(KeywordBase):
    """DYNA MAT_GEOLOGIC_CAP_MODEL keyword"""

    keyword = "MAT"
    subkeyword = "GEOLOGIC_CAP_MODEL"
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
                        "bulk",
                        float,
                        20,
                        10,
                        kwargs.get("bulk")
                    ),
                    Field(
                        "g",
                        float,
                        30,
                        10,
                        kwargs.get("g")
                    ),
                    Field(
                        "alpha",
                        float,
                        40,
                        10,
                        kwargs.get("alpha")
                    ),
                    Field(
                        "theta",
                        float,
                        50,
                        10,
                        kwargs.get("theta")
                    ),
                    Field(
                        "gamma",
                        float,
                        60,
                        10,
                        kwargs.get("gamma")
                    ),
                    Field(
                        "beta",
                        float,
                        70,
                        10,
                        kwargs.get("beta")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "r",
                        float,
                        0,
                        10,
                        kwargs.get("r")
                    ),
                    Field(
                        "d",
                        float,
                        10,
                        10,
                        kwargs.get("d")
                    ),
                    Field(
                        "w",
                        float,
                        20,
                        10,
                        kwargs.get("w")
                    ),
                    Field(
                        "x0",
                        float,
                        30,
                        10,
                        kwargs.get("x0")
                    ),
                    Field(
                        "c",
                        float,
                        40,
                        10,
                        kwargs.get("c")
                    ),
                    Field(
                        "n",
                        float,
                        50,
                        10,
                        kwargs.get("n")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "plot",
                        float,
                        0,
                        10,
                        kwargs.get("plot", 1.0)
                    ),
                    Field(
                        "ftype",
                        float,
                        10,
                        10,
                        kwargs.get("ftype", 1.0)
                    ),
                    Field(
                        "vec",
                        float,
                        20,
                        10,
                        kwargs.get("vec", 0.0)
                    ),
                    Field(
                        "toff",
                        float,
                        30,
                        10,
                        kwargs.get("toff")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatGeologicCapModel.option_specs[0],
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
    def bulk(self) -> typing.Optional[float]:
        """Get or set the Initial bulk modulus, K.
        """ # nopep8
        return self._cards[0].get_value("bulk")

    @bulk.setter
    def bulk(self, value: float) -> None:
        self._cards[0].set_value("bulk", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the Initial Shear modulus.
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        self._cards[0].set_value("g", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Failure envelope parameter.
        """ # nopep8
        return self._cards[0].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        self._cards[0].set_value("alpha", value)

    @property
    def theta(self) -> typing.Optional[float]:
        """Get or set the Failure envelope linear coefficient.
        """ # nopep8
        return self._cards[0].get_value("theta")

    @theta.setter
    def theta(self, value: float) -> None:
        self._cards[0].set_value("theta", value)

    @property
    def gamma(self) -> typing.Optional[float]:
        """Get or set the Failure envelope exponential coefficient.
        """ # nopep8
        return self._cards[0].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        self._cards[0].set_value("gamma", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Failure envelope exponent.
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[0].set_value("beta", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Cap, surface axis ratio.
        """ # nopep8
        return self._cards[1].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        self._cards[1].set_value("r", value)

    @property
    def d(self) -> typing.Optional[float]:
        """Get or set the Hardening law exponent.
        """ # nopep8
        return self._cards[1].get_value("d")

    @d.setter
    def d(self, value: float) -> None:
        self._cards[1].set_value("d", value)

    @property
    def w(self) -> typing.Optional[float]:
        """Get or set the Hardening law coefficient.
        """ # nopep8
        return self._cards[1].get_value("w")

    @w.setter
    def w(self, value: float) -> None:
        self._cards[1].set_value("w", value)

    @property
    def x0(self) -> typing.Optional[float]:
        """Get or set the Hardening law exponent.
        """ # nopep8
        return self._cards[1].get_value("x0")

    @x0.setter
    def x0(self, value: float) -> None:
        self._cards[1].set_value("x0", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening coefficient.
        """ # nopep8
        return self._cards[1].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        self._cards[1].set_value("c", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening parameter.
        """ # nopep8
        return self._cards[1].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        self._cards[1].set_value("n", value)

    @property
    def plot(self) -> float:
        """Get or set the Save the following variable for plotting in TAURUS.:
        EQ.1: hardening parameter,
        EQ.2: cap -J 1 axis intercept,
        EQ.3: volumetric plastic strain,
        EQ.4: first stress invarient,
        EQ.5: second stress invarient,
        EQ.6: not used,
        EQ.7: not used,
        EQ.8: response mode number,
        EQ.9: number of iterations.
        """ # nopep8
        return self._cards[2].get_value("plot")

    @plot.setter
    def plot(self, value: float) -> None:
        if value not in [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0]:
            raise Exception("""plot must be one of {1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0}""")
        self._cards[2].set_value("plot", value)

    @property
    def ftype(self) -> float:
        """Get or set the Formulation flag:
        EQ.1: soil or concrete (Cap surface may contract) (default),
        EQ.2: rock (Cap doesn't contract).
        """ # nopep8
        return self._cards[2].get_value("ftype")

    @ftype.setter
    def ftype(self, value: float) -> None:
        if value not in [1.0, 2.0]:
            raise Exception("""ftype must be one of {1.0,2.0}""")
        self._cards[2].set_value("ftype", value)

    @property
    def vec(self) -> float:
        """Get or set the Vectorization flag:
        EQ.0: vectorized (fixed number of iterations) (default),
        EQ.1: fully iterative,
        If the vectorized solution is chosen, the stresses might be slightly off the yield surface; however, on vector computers a much more efficient solution is achieved.
        """ # nopep8
        return self._cards[2].get_value("vec")

    @vec.setter
    def vec(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""vec must be one of {0.0,1.0}""")
        self._cards[2].set_value("vec", value)

    @property
    def toff(self) -> typing.Optional[float]:
        """Get or set the Tension Cut Off, TOFF < 0 (positive in compression).
        """ # nopep8
        return self._cards[2].get_value("toff")

    @toff.setter
    def toff(self, value: float) -> None:
        self._cards[2].set_value("toff", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

