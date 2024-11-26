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

class Mat275(KeywordBase):
    """DYNA MAT_275 keyword"""

    keyword = "MAT"
    subkeyword = "275"
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
                ],
            ),
            Card(
                [
                    Field(
                        "a0",
                        float,
                        0,
                        10,
                        kwargs.get("a0")
                    ),
                    Field(
                        "b0",
                        float,
                        10,
                        10,
                        kwargs.get("b0")
                    ),
                    Field(
                        "a1",
                        float,
                        20,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "b1",
                        float,
                        30,
                        10,
                        kwargs.get("b1")
                    ),
                    Field(
                        "m",
                        float,
                        40,
                        10,
                        kwargs.get("m")
                    ),
                    Field(
                        "kapas",
                        float,
                        50,
                        10,
                        kwargs.get("kapas")
                    ),
                    Field(
                        "kapa0",
                        float,
                        60,
                        10,
                        kwargs.get("kapa0")
                    ),
                    Field(
                        "shear",
                        float,
                        70,
                        10,
                        kwargs.get("shear")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "a0",
                        float,
                        0,
                        10,
                        kwargs.get("a0")
                    ),
                    Field(
                        "b0",
                        float,
                        10,
                        10,
                        kwargs.get("b0")
                    ),
                    Field(
                        "a1",
                        float,
                        20,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "b1",
                        float,
                        30,
                        10,
                        kwargs.get("b1")
                    ),
                    Field(
                        "m",
                        float,
                        40,
                        10,
                        kwargs.get("m")
                    ),
                    Field(
                        "kapas",
                        float,
                        50,
                        10,
                        kwargs.get("kapas")
                    ),
                    Field(
                        "kapa0",
                        float,
                        60,
                        10,
                        kwargs.get("kapa0")
                    ),
                    Field(
                        "shear",
                        float,
                        70,
                        10,
                        kwargs.get("shear")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "a0",
                        float,
                        0,
                        10,
                        kwargs.get("a0")
                    ),
                    Field(
                        "b0",
                        float,
                        10,
                        10,
                        kwargs.get("b0")
                    ),
                    Field(
                        "a1",
                        float,
                        20,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "b1",
                        float,
                        30,
                        10,
                        kwargs.get("b1")
                    ),
                    Field(
                        "m",
                        float,
                        40,
                        10,
                        kwargs.get("m")
                    ),
                    Field(
                        "kapas",
                        float,
                        50,
                        10,
                        kwargs.get("kapas")
                    ),
                    Field(
                        "kapa0",
                        float,
                        60,
                        10,
                        kwargs.get("kapa0")
                    ),
                    Field(
                        "shear",
                        float,
                        70,
                        10,
                        kwargs.get("shear")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "a0",
                        float,
                        0,
                        10,
                        kwargs.get("a0")
                    ),
                    Field(
                        "b0",
                        float,
                        10,
                        10,
                        kwargs.get("b0")
                    ),
                    Field(
                        "a1",
                        float,
                        20,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "b1",
                        float,
                        30,
                        10,
                        kwargs.get("b1")
                    ),
                    Field(
                        "m",
                        float,
                        40,
                        10,
                        kwargs.get("m")
                    ),
                    Field(
                        "kapas",
                        float,
                        50,
                        10,
                        kwargs.get("kapas")
                    ),
                    Field(
                        "kapa0",
                        float,
                        60,
                        10,
                        kwargs.get("kapa0")
                    ),
                    Field(
                        "shear",
                        float,
                        70,
                        10,
                        kwargs.get("shear")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "a0",
                        float,
                        0,
                        10,
                        kwargs.get("a0")
                    ),
                    Field(
                        "b0",
                        float,
                        10,
                        10,
                        kwargs.get("b0")
                    ),
                    Field(
                        "a1",
                        float,
                        20,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "b1",
                        float,
                        30,
                        10,
                        kwargs.get("b1")
                    ),
                    Field(
                        "m",
                        float,
                        40,
                        10,
                        kwargs.get("m")
                    ),
                    Field(
                        "kapas",
                        float,
                        50,
                        10,
                        kwargs.get("kapas")
                    ),
                    Field(
                        "kapa0",
                        float,
                        60,
                        10,
                        kwargs.get("kapa0")
                    ),
                    Field(
                        "shear",
                        float,
                        70,
                        10,
                        kwargs.get("shear")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "a0",
                        float,
                        0,
                        10,
                        kwargs.get("a0")
                    ),
                    Field(
                        "b0",
                        float,
                        10,
                        10,
                        kwargs.get("b0")
                    ),
                    Field(
                        "a1",
                        float,
                        20,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "b1",
                        float,
                        30,
                        10,
                        kwargs.get("b1")
                    ),
                    Field(
                        "m",
                        float,
                        40,
                        10,
                        kwargs.get("m")
                    ),
                    Field(
                        "kapas",
                        float,
                        50,
                        10,
                        kwargs.get("kapas")
                    ),
                    Field(
                        "kapa0",
                        float,
                        60,
                        10,
                        kwargs.get("kapa0")
                    ),
                    Field(
                        "shear",
                        float,
                        70,
                        10,
                        kwargs.get("shear")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat275.option_specs[0],
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
        """Get or set the Mass Density
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the Elastic bulk modulus
        """ # nopep8
        return self._cards[0].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        self._cards[0].set_value("k", value)

    @property
    def a0(self) -> typing.Optional[float]:
        """Get or set the Rate dependent understress viscoplastic parameter.
        """ # nopep8
        return self._cards[1].get_value("a0")

    @a0.setter
    def a0(self, value: float) -> None:
        self._cards[1].set_value("a0", value)

    @property
    def b0(self) -> typing.Optional[float]:
        """Get or set the Rate independent understress plasticity parameter.
        """ # nopep8
        return self._cards[1].get_value("b0")

    @b0.setter
    def b0(self, value: float) -> None:
        self._cards[1].set_value("b0", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Rate dependent overstress viscoplastic parameter.
        """ # nopep8
        return self._cards[1].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[1].set_value("a1", value)

    @property
    def b1(self) -> typing.Optional[float]:
        """Get or set the Rate independent overstress plasticity parameter.
        """ # nopep8
        return self._cards[1].get_value("b1")

    @b1.setter
    def b1(self, value: float) -> None:
        self._cards[1].set_value("b1", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Exponential hardening parameter
        """ # nopep8
        return self._cards[1].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        self._cards[1].set_value("m", value)

    @property
    def kapas(self) -> typing.Optional[float]:
        """Get or set the Saturated yield strain.
        """ # nopep8
        return self._cards[1].get_value("kapas")

    @kapas.setter
    def kapas(self, value: float) -> None:
        self._cards[1].set_value("kapas", value)

    @property
    def kapa0(self) -> typing.Optional[float]:
        """Get or set the Initial yield strain.
        """ # nopep8
        return self._cards[1].get_value("kapa0")

    @kapa0.setter
    def kapa0(self, value: float) -> None:
        self._cards[1].set_value("kapa0", value)

    @property
    def shear(self) -> typing.Optional[float]:
        """Get or set the Elastic shear modulus.
        """ # nopep8
        return self._cards[1].get_value("shear")

    @shear.setter
    def shear(self, value: float) -> None:
        self._cards[1].set_value("shear", value)

    @property
    def a0(self) -> typing.Optional[float]:
        """Get or set the Rate dependent understress viscoplastic parameter.
        """ # nopep8
        return self._cards[2].get_value("a0")

    @a0.setter
    def a0(self, value: float) -> None:
        self._cards[2].set_value("a0", value)

    @property
    def b0(self) -> typing.Optional[float]:
        """Get or set the Rate independent understress plasticity parameter.
        """ # nopep8
        return self._cards[2].get_value("b0")

    @b0.setter
    def b0(self, value: float) -> None:
        self._cards[2].set_value("b0", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Rate dependent overstress viscoplastic parameter.
        """ # nopep8
        return self._cards[2].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[2].set_value("a1", value)

    @property
    def b1(self) -> typing.Optional[float]:
        """Get or set the Rate independent overstress plasticity parameter.
        """ # nopep8
        return self._cards[2].get_value("b1")

    @b1.setter
    def b1(self, value: float) -> None:
        self._cards[2].set_value("b1", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Exponential hardening parameter
        """ # nopep8
        return self._cards[2].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        self._cards[2].set_value("m", value)

    @property
    def kapas(self) -> typing.Optional[float]:
        """Get or set the Saturated yield strain.
        """ # nopep8
        return self._cards[2].get_value("kapas")

    @kapas.setter
    def kapas(self, value: float) -> None:
        self._cards[2].set_value("kapas", value)

    @property
    def kapa0(self) -> typing.Optional[float]:
        """Get or set the Initial yield strain.
        """ # nopep8
        return self._cards[2].get_value("kapa0")

    @kapa0.setter
    def kapa0(self, value: float) -> None:
        self._cards[2].set_value("kapa0", value)

    @property
    def shear(self) -> typing.Optional[float]:
        """Get or set the Elastic shear modulus.
        """ # nopep8
        return self._cards[2].get_value("shear")

    @shear.setter
    def shear(self, value: float) -> None:
        self._cards[2].set_value("shear", value)

    @property
    def a0(self) -> typing.Optional[float]:
        """Get or set the Rate dependent understress viscoplastic parameter.
        """ # nopep8
        return self._cards[3].get_value("a0")

    @a0.setter
    def a0(self, value: float) -> None:
        self._cards[3].set_value("a0", value)

    @property
    def b0(self) -> typing.Optional[float]:
        """Get or set the Rate independent understress plasticity parameter.
        """ # nopep8
        return self._cards[3].get_value("b0")

    @b0.setter
    def b0(self, value: float) -> None:
        self._cards[3].set_value("b0", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Rate dependent overstress viscoplastic parameter.
        """ # nopep8
        return self._cards[3].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[3].set_value("a1", value)

    @property
    def b1(self) -> typing.Optional[float]:
        """Get or set the Rate independent overstress plasticity parameter.
        """ # nopep8
        return self._cards[3].get_value("b1")

    @b1.setter
    def b1(self, value: float) -> None:
        self._cards[3].set_value("b1", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Exponential hardening parameter
        """ # nopep8
        return self._cards[3].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        self._cards[3].set_value("m", value)

    @property
    def kapas(self) -> typing.Optional[float]:
        """Get or set the Saturated yield strain.
        """ # nopep8
        return self._cards[3].get_value("kapas")

    @kapas.setter
    def kapas(self, value: float) -> None:
        self._cards[3].set_value("kapas", value)

    @property
    def kapa0(self) -> typing.Optional[float]:
        """Get or set the Initial yield strain.
        """ # nopep8
        return self._cards[3].get_value("kapa0")

    @kapa0.setter
    def kapa0(self, value: float) -> None:
        self._cards[3].set_value("kapa0", value)

    @property
    def shear(self) -> typing.Optional[float]:
        """Get or set the Elastic shear modulus.
        """ # nopep8
        return self._cards[3].get_value("shear")

    @shear.setter
    def shear(self, value: float) -> None:
        self._cards[3].set_value("shear", value)

    @property
    def a0(self) -> typing.Optional[float]:
        """Get or set the Rate dependent understress viscoplastic parameter.
        """ # nopep8
        return self._cards[4].get_value("a0")

    @a0.setter
    def a0(self, value: float) -> None:
        self._cards[4].set_value("a0", value)

    @property
    def b0(self) -> typing.Optional[float]:
        """Get or set the Rate independent understress plasticity parameter.
        """ # nopep8
        return self._cards[4].get_value("b0")

    @b0.setter
    def b0(self, value: float) -> None:
        self._cards[4].set_value("b0", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Rate dependent overstress viscoplastic parameter.
        """ # nopep8
        return self._cards[4].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[4].set_value("a1", value)

    @property
    def b1(self) -> typing.Optional[float]:
        """Get or set the Rate independent overstress plasticity parameter.
        """ # nopep8
        return self._cards[4].get_value("b1")

    @b1.setter
    def b1(self, value: float) -> None:
        self._cards[4].set_value("b1", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Exponential hardening parameter
        """ # nopep8
        return self._cards[4].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        self._cards[4].set_value("m", value)

    @property
    def kapas(self) -> typing.Optional[float]:
        """Get or set the Saturated yield strain.
        """ # nopep8
        return self._cards[4].get_value("kapas")

    @kapas.setter
    def kapas(self, value: float) -> None:
        self._cards[4].set_value("kapas", value)

    @property
    def kapa0(self) -> typing.Optional[float]:
        """Get or set the Initial yield strain.
        """ # nopep8
        return self._cards[4].get_value("kapa0")

    @kapa0.setter
    def kapa0(self, value: float) -> None:
        self._cards[4].set_value("kapa0", value)

    @property
    def shear(self) -> typing.Optional[float]:
        """Get or set the Elastic shear modulus.
        """ # nopep8
        return self._cards[4].get_value("shear")

    @shear.setter
    def shear(self, value: float) -> None:
        self._cards[4].set_value("shear", value)

    @property
    def a0(self) -> typing.Optional[float]:
        """Get or set the Rate dependent understress viscoplastic parameter.
        """ # nopep8
        return self._cards[5].get_value("a0")

    @a0.setter
    def a0(self, value: float) -> None:
        self._cards[5].set_value("a0", value)

    @property
    def b0(self) -> typing.Optional[float]:
        """Get or set the Rate independent understress plasticity parameter.
        """ # nopep8
        return self._cards[5].get_value("b0")

    @b0.setter
    def b0(self, value: float) -> None:
        self._cards[5].set_value("b0", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Rate dependent overstress viscoplastic parameter.
        """ # nopep8
        return self._cards[5].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[5].set_value("a1", value)

    @property
    def b1(self) -> typing.Optional[float]:
        """Get or set the Rate independent overstress plasticity parameter.
        """ # nopep8
        return self._cards[5].get_value("b1")

    @b1.setter
    def b1(self, value: float) -> None:
        self._cards[5].set_value("b1", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Exponential hardening parameter
        """ # nopep8
        return self._cards[5].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        self._cards[5].set_value("m", value)

    @property
    def kapas(self) -> typing.Optional[float]:
        """Get or set the Saturated yield strain.
        """ # nopep8
        return self._cards[5].get_value("kapas")

    @kapas.setter
    def kapas(self, value: float) -> None:
        self._cards[5].set_value("kapas", value)

    @property
    def kapa0(self) -> typing.Optional[float]:
        """Get or set the Initial yield strain.
        """ # nopep8
        return self._cards[5].get_value("kapa0")

    @kapa0.setter
    def kapa0(self, value: float) -> None:
        self._cards[5].set_value("kapa0", value)

    @property
    def shear(self) -> typing.Optional[float]:
        """Get or set the Elastic shear modulus.
        """ # nopep8
        return self._cards[5].get_value("shear")

    @shear.setter
    def shear(self, value: float) -> None:
        self._cards[5].set_value("shear", value)

    @property
    def a0(self) -> typing.Optional[float]:
        """Get or set the Rate dependent understress viscoplastic parameter.
        """ # nopep8
        return self._cards[6].get_value("a0")

    @a0.setter
    def a0(self, value: float) -> None:
        self._cards[6].set_value("a0", value)

    @property
    def b0(self) -> typing.Optional[float]:
        """Get or set the Rate independent understress plasticity parameter.
        """ # nopep8
        return self._cards[6].get_value("b0")

    @b0.setter
    def b0(self, value: float) -> None:
        self._cards[6].set_value("b0", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Rate dependent overstress viscoplastic parameter.
        """ # nopep8
        return self._cards[6].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[6].set_value("a1", value)

    @property
    def b1(self) -> typing.Optional[float]:
        """Get or set the Rate independent overstress plasticity parameter.
        """ # nopep8
        return self._cards[6].get_value("b1")

    @b1.setter
    def b1(self, value: float) -> None:
        self._cards[6].set_value("b1", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Exponential hardening parameter
        """ # nopep8
        return self._cards[6].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        self._cards[6].set_value("m", value)

    @property
    def kapas(self) -> typing.Optional[float]:
        """Get or set the Saturated yield strain.
        """ # nopep8
        return self._cards[6].get_value("kapas")

    @kapas.setter
    def kapas(self, value: float) -> None:
        self._cards[6].set_value("kapas", value)

    @property
    def kapa0(self) -> typing.Optional[float]:
        """Get or set the Initial yield strain.
        """ # nopep8
        return self._cards[6].get_value("kapa0")

    @kapa0.setter
    def kapa0(self, value: float) -> None:
        self._cards[6].set_value("kapa0", value)

    @property
    def shear(self) -> typing.Optional[float]:
        """Get or set the Elastic shear modulus.
        """ # nopep8
        return self._cards[6].get_value("shear")

    @shear.setter
    def shear(self, value: float) -> None:
        self._cards[6].set_value("shear", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[7].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[7].cards[0].set_value("title", value)

