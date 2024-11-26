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
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DualceseInitialHybridSet(KeywordBase):
    """DYNA DUALCESE_INITIAL_HYBRID_SET keyword"""

    keyword = "DUALCESE"
    subkeyword = "INITIAL_HYBRID_SET"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "esid",
                        int,
                        0,
                        10,
                        kwargs.get("esid")
                    ),
                    Field(
                        "ifunc",
                        int,
                        10,
                        10,
                        kwargs.get("ifunc")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "z1",
                        float,
                        0,
                        10,
                        kwargs.get("z1")
                    ),
                    Field(
                        "ra",
                        float,
                        10,
                        10,
                        kwargs.get("ra")
                    ),
                    Field(
                        "uic",
                        float,
                        20,
                        10,
                        kwargs.get("uic")
                    ),
                    Field(
                        "vic",
                        float,
                        30,
                        10,
                        kwargs.get("vic")
                    ),
                    Field(
                        "wic",
                        float,
                        40,
                        10,
                        kwargs.get("wic")
                    ),
                    Field(
                        "rho1",
                        float,
                        50,
                        10,
                        kwargs.get("rho1")
                    ),
                    Field(
                        "rho_a",
                        float,
                        60,
                        10,
                        kwargs.get("rho_a")
                    ),
                    Field(
                        "rho_b",
                        float,
                        70,
                        10,
                        kwargs.get("rho_b")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "pic",
                        float,
                        0,
                        10,
                        kwargs.get("pic")
                    ),
                    Field(
                        "tic",
                        float,
                        10,
                        10,
                        kwargs.get("tic")
                    ),
                ],
            ),
        ]

    @property
    def esid(self) -> typing.Optional[int]:
        """Get or set the Element set ID (see *DUALCESE_ELEMENTSET)
        """ # nopep8
        return self._cards[0].get_value("esid")

    @esid.setter
    def esid(self, value: int) -> None:
        self._cards[0].set_value("esid", value)

    @property
    def ifunc(self) -> typing.Optional[int]:
        """Get or set the Option to define initial conditions using *DEFINE_FUNCTION cards:
        EQ.0:	Not in use.
        EQ.1:All values for initial velocity, pressure, density, and temperature now refer to *DEFINE_FUNCTION IDs. In these functions, the following parameters are allowed: f(x,y,z), meaning that each variable’s initial profile is a function of position
        """ # nopep8
        return self._cards[0].get_value("ifunc")

    @ifunc.setter
    def ifunc(self, value: int) -> None:
        self._cards[0].set_value("ifunc", value)

    @property
    def z1(self) -> typing.Optional[float]:
        """Get or set the Volume fraction of material 1 (or color function).This is usually a value of 0 or 1. For numerical stability, however, use a very small value instead of zero
        """ # nopep8
        return self._cards[1].get_value("z1")

    @z1.setter
    def z1(self, value: float) -> None:
        self._cards[1].set_value("z1", value)

    @property
    def ra(self) -> typing.Optional[float]:
        """Get or set the Mass fraction of the reactant(material) with respect to material 2 (the explosive mixture)
        """ # nopep8
        return self._cards[1].get_value("ra")

    @ra.setter
    def ra(self, value: float) -> None:
        self._cards[1].set_value("ra", value)

    @property
    def uic(self) -> typing.Optional[float]:
        """Get or set the Multiphase flow velocity components in the x, y,and z - directions, respectively
        """ # nopep8
        return self._cards[1].get_value("uic")

    @uic.setter
    def uic(self, value: float) -> None:
        self._cards[1].set_value("uic", value)

    @property
    def vic(self) -> typing.Optional[float]:
        """Get or set the Multiphase flow velocity components in the x, y,and z - directions, respectively
        """ # nopep8
        return self._cards[1].get_value("vic")

    @vic.setter
    def vic(self, value: float) -> None:
        self._cards[1].set_value("vic", value)

    @property
    def wic(self) -> typing.Optional[float]:
        """Get or set the Multiphase flow velocity components in the x, y,and z - directions, respectively
        """ # nopep8
        return self._cards[1].get_value("wic")

    @wic.setter
    def wic(self, value: float) -> None:
        self._cards[1].set_value("wic", value)

    @property
    def rho1(self) -> typing.Optional[float]:
        """Get or set the Density of material 1
        """ # nopep8
        return self._cards[1].get_value("rho1")

    @rho1.setter
    def rho1(self, value: float) -> None:
        self._cards[1].set_value("rho1", value)

    @property
    def rho_a(self) -> typing.Optional[float]:
        """Get or set the Density of the reactant(material a
        """ # nopep8
        return self._cards[1].get_value("rho_a")

    @rho_a.setter
    def rho_a(self, value: float) -> None:
        self._cards[1].set_value("rho_a", value)

    @property
    def rho_b(self) -> typing.Optional[float]:
        """Get or set the Density of the product(material b)
        """ # nopep8
        return self._cards[1].get_value("rho_b")

    @rho_b.setter
    def rho_b(self, value: float) -> None:
        self._cards[1].set_value("rho_b", value)

    @property
    def pic(self) -> typing.Optional[float]:
        """Get or set the Equilibrium multifluid pressure
        """ # nopep8
        return self._cards[2].get_value("pic")

    @pic.setter
    def pic(self, value: float) -> None:
        self._cards[2].set_value("pic", value)

    @property
    def tic(self) -> typing.Optional[float]:
        """Get or set the Equilibrium multifluid temperaturee
        """ # nopep8
        return self._cards[2].get_value("tic")

    @tic.setter
    def tic(self, value: float) -> None:
        self._cards[2].set_value("tic", value)

