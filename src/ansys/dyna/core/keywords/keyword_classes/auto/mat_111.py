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

"""Module providing the Mat111 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class Mat111(KeywordBase):
    """DYNA MAT_111 keyword"""

    keyword = "MAT"
    subkeyword = "111"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat111 class."""
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
                        "g",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "a",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "b",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "n",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "fc",
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
                        "t",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "eps0",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "efmin",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "sfmax",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pc",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "uc",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pl",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ul",
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
                        "d1",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "d2",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "k1",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "k2",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "k3",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "fs",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat111.option_specs[0],
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
        """Get or set the Mass Density
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the Shear modulus
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        """Set the g property."""
        self._cards[0].set_value("g", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Normalized cohesive strength.
        """ # nopep8
        return self._cards[0].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[0].set_value("a", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Normalized pressure hardening.
        """ # nopep8
        return self._cards[0].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[0].set_value("b", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Strain rate coefficient.
        """ # nopep8
        return self._cards[0].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[0].set_value("c", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Pressure hardening exponent.
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[0].set_value("n", value)

    @property
    def fc(self) -> typing.Optional[float]:
        """Get or set the Quasi-static uniaxial compressive strength.
        """ # nopep8
        return self._cards[0].get_value("fc")

    @fc.setter
    def fc(self, value: float) -> None:
        """Set the fc property."""
        self._cards[0].set_value("fc", value)

    @property
    def t(self) -> typing.Optional[float]:
        """Get or set the Maximum tensile hydrostatic pressure.
        """ # nopep8
        return self._cards[1].get_value("t")

    @t.setter
    def t(self, value: float) -> None:
        """Set the t property."""
        self._cards[1].set_value("t", value)

    @property
    def eps0(self) -> typing.Optional[float]:
        """Get or set the Reference strain rate.
        """ # nopep8
        return self._cards[1].get_value("eps0")

    @eps0.setter
    def eps0(self, value: float) -> None:
        """Set the eps0 property."""
        self._cards[1].set_value("eps0", value)

    @property
    def efmin(self) -> typing.Optional[float]:
        """Get or set the Amount of plastic strain before fracture.
        """ # nopep8
        return self._cards[1].get_value("efmin")

    @efmin.setter
    def efmin(self, value: float) -> None:
        """Set the efmin property."""
        self._cards[1].set_value("efmin", value)

    @property
    def sfmax(self) -> typing.Optional[float]:
        """Get or set the Normalized maximum strength.
        """ # nopep8
        return self._cards[1].get_value("sfmax")

    @sfmax.setter
    def sfmax(self, value: float) -> None:
        """Set the sfmax property."""
        self._cards[1].set_value("sfmax", value)

    @property
    def pc(self) -> typing.Optional[float]:
        """Get or set the Crushing pressure.
        """ # nopep8
        return self._cards[1].get_value("pc")

    @pc.setter
    def pc(self, value: float) -> None:
        """Set the pc property."""
        self._cards[1].set_value("pc", value)

    @property
    def uc(self) -> typing.Optional[float]:
        """Get or set the Crushing volumetric strain.
        """ # nopep8
        return self._cards[1].get_value("uc")

    @uc.setter
    def uc(self, value: float) -> None:
        """Set the uc property."""
        self._cards[1].set_value("uc", value)

    @property
    def pl(self) -> typing.Optional[float]:
        """Get or set the Locking pressure.
        """ # nopep8
        return self._cards[1].get_value("pl")

    @pl.setter
    def pl(self, value: float) -> None:
        """Set the pl property."""
        self._cards[1].set_value("pl", value)

    @property
    def ul(self) -> typing.Optional[float]:
        """Get or set the Locking volumetric strain.
        """ # nopep8
        return self._cards[1].get_value("ul")

    @ul.setter
    def ul(self, value: float) -> None:
        """Set the ul property."""
        self._cards[1].set_value("ul", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Damage constant.
        """ # nopep8
        return self._cards[2].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[2].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Damage constant.
        """ # nopep8
        return self._cards[2].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[2].set_value("d2", value)

    @property
    def k1(self) -> typing.Optional[float]:
        """Get or set the Pressure constant.
        """ # nopep8
        return self._cards[2].get_value("k1")

    @k1.setter
    def k1(self, value: float) -> None:
        """Set the k1 property."""
        self._cards[2].set_value("k1", value)

    @property
    def k2(self) -> typing.Optional[float]:
        """Get or set the Pressure constant.
        """ # nopep8
        return self._cards[2].get_value("k2")

    @k2.setter
    def k2(self, value: float) -> None:
        """Set the k2 property."""
        self._cards[2].set_value("k2", value)

    @property
    def k3(self) -> typing.Optional[float]:
        """Get or set the Pressure constant.
        """ # nopep8
        return self._cards[2].get_value("k3")

    @k3.setter
    def k3(self, value: float) -> None:
        """Set the k3 property."""
        self._cards[2].set_value("k3", value)

    @property
    def fs(self) -> typing.Optional[float]:
        """Get or set the Failure type
        """ # nopep8
        return self._cards[2].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        """Set the fs property."""
        self._cards[2].set_value("fs", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[3].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

