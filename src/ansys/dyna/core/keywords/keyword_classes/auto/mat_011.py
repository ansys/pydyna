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

"""Module providing the Mat011 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class Mat011(KeywordBase):
    """DYNA MAT_011 keyword"""

    keyword = "MAT"
    subkeyword = "011"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat011 class."""
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
                        **kwargs,
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "g0",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "sigo",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "beta",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "n",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "gama",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "sigm",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "b",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "bp",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "h",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "f",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "a",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "tmo",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "gamo",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "sa",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "pc",
                        float,
                        0,
                        10,
                        -1.0E+30,
                        **kwargs,
                    ),
                    Field(
                        "spall",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "rp",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "flag",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "mmn",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "mmx",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "eco",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ec1",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ec2",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ec3",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ec4",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ec5",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ec6",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ec7",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ec8",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ec9",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat011.option_specs[0],
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
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def g0(self) -> typing.Optional[float]:
        """Get or set the Basic shear modulus.
        """ # nopep8
        return self._cards[0].get_value("g0")

    @g0.setter
    def g0(self, value: float) -> None:
        """Set the g0 property."""
        self._cards[0].set_value("g0", value)

    @property
    def sigo(self) -> typing.Optional[float]:
        """Get or set the Sigma-0, see defining equations in keyword manual page 51 (volume two).
        """ # nopep8
        return self._cards[0].get_value("sigo")

    @sigo.setter
    def sigo(self, value: float) -> None:
        """Set the sigo property."""
        self._cards[0].set_value("sigo", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the b, see defining equations in keyword manual page 51 (volume two).
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[0].set_value("beta", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the n, see defining equations in keyword manual page 51 (volume two).
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[0].set_value("n", value)

    @property
    def gama(self) -> typing.Optional[float]:
        """Get or set the Initial plastic strain, see defining equations in keyword manual page 51 (volume two).
        """ # nopep8
        return self._cards[0].get_value("gama")

    @gama.setter
    def gama(self, value: float) -> None:
        """Set the gama property."""
        self._cards[0].set_value("gama", value)

    @property
    def sigm(self) -> typing.Optional[float]:
        """Get or set the Sigma-m, see defining equations in keyword manual page 51 (volume two).
        """ # nopep8
        return self._cards[0].get_value("sigm")

    @sigm.setter
    def sigm(self, value: float) -> None:
        """Set the sigm property."""
        self._cards[0].set_value("sigm", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the b, see defining equations in keyword manual page 51 (volume two).
        """ # nopep8
        return self._cards[1].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[1].set_value("b", value)

    @property
    def bp(self) -> typing.Optional[float]:
        """Get or set the b', see defining equations in keyword manual page 51 (volume two).
        """ # nopep8
        return self._cards[1].get_value("bp")

    @bp.setter
    def bp(self, value: float) -> None:
        """Set the bp property."""
        self._cards[1].set_value("bp", value)

    @property
    def h(self) -> typing.Optional[float]:
        """Get or set the h, see defining equations in keyword manual page 51 (volume two).
        """ # nopep8
        return self._cards[1].get_value("h")

    @h.setter
    def h(self, value: float) -> None:
        """Set the h property."""
        self._cards[1].set_value("h", value)

    @property
    def f(self) -> typing.Optional[float]:
        """Get or set the f, see defining equations in keyword manual page 51 (volume two).
        """ # nopep8
        return self._cards[1].get_value("f")

    @f.setter
    def f(self, value: float) -> None:
        """Set the f property."""
        self._cards[1].set_value("f", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Atomic weight (if = 0.0, RP (R') must be defined).
        """ # nopep8
        return self._cards[1].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[1].set_value("a", value)

    @property
    def tmo(self) -> typing.Optional[float]:
        """Get or set the Tm0, see defining equations in keyword manual page 51 (volume two).
        """ # nopep8
        return self._cards[1].get_value("tmo")

    @tmo.setter
    def tmo(self, value: float) -> None:
        """Set the tmo property."""
        self._cards[1].set_value("tmo", value)

    @property
    def gamo(self) -> typing.Optional[float]:
        """Get or set the gamma-0, see defining equations in keyword manual page 51 (volume two).
        """ # nopep8
        return self._cards[1].get_value("gamo")

    @gamo.setter
    def gamo(self, value: float) -> None:
        """Set the gamo property."""
        self._cards[1].set_value("gamo", value)

    @property
    def sa(self) -> typing.Optional[float]:
        """Get or set the a, see defining equations in keyword manual page 51 (volume two).
        """ # nopep8
        return self._cards[1].get_value("sa")

    @sa.setter
    def sa(self, value: float) -> None:
        """Set the sa property."""
        self._cards[1].set_value("sa", value)

    @property
    def pc(self) -> float:
        """Get or set the Pcut or -sigma-f (default=-1.e+30).
        """ # nopep8
        return self._cards[2].get_value("pc")

    @pc.setter
    def pc(self, value: float) -> None:
        """Set the pc property."""
        self._cards[2].set_value("pc", value)

    @property
    def spall(self) -> float:
        """Get or set the Spall type:
        EQ. 0.0: default set to 2.0,
        EQ. 1.0: P => Pcut ,
        EQ. 2.0: if sigma-max => -Pcut element spalls and tension, p < 0, is never allowed,
        EQ. 3.0: P < -Pcut element spalls and tension, p < 0, is never allowed.
        """ # nopep8
        return self._cards[2].get_value("spall")

    @spall.setter
    def spall(self, value: float) -> None:
        """Set the spall property."""
        if value not in [0.0, 1.0, 2.0, 3.0, None]:
            raise Exception("""spall must be `None` or one of {0.0,1.0,2.0,3.0}.""")
        self._cards[2].set_value("spall", value)

    @property
    def rp(self) -> typing.Optional[float]:
        """Get or set the R'. If R' not equal to 0.0, A is not defined.
        """ # nopep8
        return self._cards[2].get_value("rp")

    @rp.setter
    def rp(self, value: float) -> None:
        """Set the rp property."""
        self._cards[2].set_value("rp", value)

    @property
    def flag(self) -> float:
        """Get or set the Set to 1.0 for mu coefficients for the cold compression energy fit. Default is nu.
        """ # nopep8
        return self._cards[2].get_value("flag")

    @flag.setter
    def flag(self, value: float) -> None:
        """Set the flag property."""
        self._cards[2].set_value("flag", value)

    @property
    def mmn(self) -> typing.Optional[float]:
        """Get or set the mu-min or nu-min . Optional mu or nu minimum value.
        """ # nopep8
        return self._cards[2].get_value("mmn")

    @mmn.setter
    def mmn(self, value: float) -> None:
        """Set the mmn property."""
        self._cards[2].set_value("mmn", value)

    @property
    def mmx(self) -> typing.Optional[float]:
        """Get or set the mu-max or nu-max . Optional mu or nu maximum value.
        """ # nopep8
        return self._cards[2].get_value("mmx")

    @mmx.setter
    def mmx(self, value: float) -> None:
        """Set the mmx property."""
        self._cards[2].set_value("mmx", value)

    @property
    def eco(self) -> typing.Optional[float]:
        """Get or set the Cold compression energy coefficient (optional).
        """ # nopep8
        return self._cards[2].get_value("eco")

    @eco.setter
    def eco(self, value: float) -> None:
        """Set the eco property."""
        self._cards[2].set_value("eco", value)

    @property
    def ec1(self) -> typing.Optional[float]:
        """Get or set the Cold compression energy coefficient (optional).
        """ # nopep8
        return self._cards[2].get_value("ec1")

    @ec1.setter
    def ec1(self, value: float) -> None:
        """Set the ec1 property."""
        self._cards[2].set_value("ec1", value)

    @property
    def ec2(self) -> typing.Optional[float]:
        """Get or set the Cold compression energy coefficient (optional).
        """ # nopep8
        return self._cards[3].get_value("ec2")

    @ec2.setter
    def ec2(self, value: float) -> None:
        """Set the ec2 property."""
        self._cards[3].set_value("ec2", value)

    @property
    def ec3(self) -> typing.Optional[float]:
        """Get or set the Cold compression energy coefficient (optional).
        """ # nopep8
        return self._cards[3].get_value("ec3")

    @ec3.setter
    def ec3(self, value: float) -> None:
        """Set the ec3 property."""
        self._cards[3].set_value("ec3", value)

    @property
    def ec4(self) -> typing.Optional[float]:
        """Get or set the Cold compression energy coefficient (optional).
        """ # nopep8
        return self._cards[3].get_value("ec4")

    @ec4.setter
    def ec4(self, value: float) -> None:
        """Set the ec4 property."""
        self._cards[3].set_value("ec4", value)

    @property
    def ec5(self) -> typing.Optional[float]:
        """Get or set the Cold compression energy coefficient (optional).
        """ # nopep8
        return self._cards[3].get_value("ec5")

    @ec5.setter
    def ec5(self, value: float) -> None:
        """Set the ec5 property."""
        self._cards[3].set_value("ec5", value)

    @property
    def ec6(self) -> typing.Optional[float]:
        """Get or set the Cold compression energy coefficient (optional).
        """ # nopep8
        return self._cards[3].get_value("ec6")

    @ec6.setter
    def ec6(self, value: float) -> None:
        """Set the ec6 property."""
        self._cards[3].set_value("ec6", value)

    @property
    def ec7(self) -> typing.Optional[float]:
        """Get or set the Cold compression energy coefficient (optional).
        """ # nopep8
        return self._cards[3].get_value("ec7")

    @ec7.setter
    def ec7(self, value: float) -> None:
        """Set the ec7 property."""
        self._cards[3].set_value("ec7", value)

    @property
    def ec8(self) -> typing.Optional[float]:
        """Get or set the Cold compression energy coefficient (optional).
        """ # nopep8
        return self._cards[3].get_value("ec8")

    @ec8.setter
    def ec8(self, value: float) -> None:
        """Set the ec8 property."""
        self._cards[3].set_value("ec8", value)

    @property
    def ec9(self) -> typing.Optional[float]:
        """Get or set the Cold compression energy coefficient (optional).
        """ # nopep8
        return self._cards[3].get_value("ec9")

    @ec9.setter
    def ec9(self, value: float) -> None:
        """Set the ec9 property."""
        self._cards[3].set_value("ec9", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[4].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

