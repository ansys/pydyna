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

class DualceseInitialSet(KeywordBase):
    """DYNA DUALCESE_INITIAL_SET keyword"""

    keyword = "DUALCESE"
    subkeyword = "INITIAL_SET"

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
                        "u",
                        float,
                        0,
                        10,
                        kwargs.get("u", 0.0)
                    ),
                    Field(
                        "v",
                        float,
                        10,
                        10,
                        kwargs.get("v", 0.0)
                    ),
                    Field(
                        "w",
                        float,
                        20,
                        10,
                        kwargs.get("w", 0.0)
                    ),
                    Field(
                        "rho",
                        float,
                        30,
                        10,
                        kwargs.get("rho", 1.225)
                    ),
                    Field(
                        "p",
                        float,
                        40,
                        10,
                        kwargs.get("p", 0.0)
                    ),
                    Field(
                        "t",
                        float,
                        50,
                        10,
                        kwargs.get("t", 0.0)
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
    def u(self) -> float:
        """Get or set the x-, y-, z-velocity components respectively
        """ # nopep8
        return self._cards[1].get_value("u")

    @u.setter
    def u(self, value: float) -> None:
        self._cards[1].set_value("u", value)

    @property
    def v(self) -> float:
        """Get or set the x-, y-, z-velocity components respectively
        """ # nopep8
        return self._cards[1].get_value("v")

    @v.setter
    def v(self, value: float) -> None:
        self._cards[1].set_value("v", value)

    @property
    def w(self) -> float:
        """Get or set the x-, y-, z-velocity components respectively
        """ # nopep8
        return self._cards[1].get_value("w")

    @w.setter
    def w(self, value: float) -> None:
        self._cards[1].set_value("w", value)

    @property
    def rho(self) -> float:
        """Get or set the density
        """ # nopep8
        return self._cards[1].get_value("rho")

    @rho.setter
    def rho(self, value: float) -> None:
        self._cards[1].set_value("rho", value)

    @property
    def p(self) -> float:
        """Get or set the pressure
        """ # nopep8
        return self._cards[1].get_value("p")

    @p.setter
    def p(self, value: float) -> None:
        self._cards[1].set_value("p", value)

    @property
    def t(self) -> float:
        """Get or set the temperature
        """ # nopep8
        return self._cards[1].get_value("t")

    @t.setter
    def t(self, value: float) -> None:
        self._cards[1].set_value("t", value)

