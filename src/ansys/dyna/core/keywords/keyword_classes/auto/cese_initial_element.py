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

class CeseInitialElement(KeywordBase):
    """DYNA CESE_INITIAL_ELEMENT keyword"""

    keyword = "CESE"
    subkeyword = "INITIAL_ELEMENT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "eid",
                        int,
                        0,
                        10,
                        kwargs.get("eid")
                    ),
                    Field(
                        "u",
                        float,
                        10,
                        10,
                        kwargs.get("u", 0.0)
                    ),
                    Field(
                        "v",
                        float,
                        20,
                        10,
                        kwargs.get("v", 0.0)
                    ),
                    Field(
                        "w",
                        float,
                        30,
                        10,
                        kwargs.get("w", 0.0)
                    ),
                    Field(
                        "rho",
                        float,
                        40,
                        10,
                        kwargs.get("rho", 1.225)
                    ),
                    Field(
                        "p",
                        float,
                        50,
                        10,
                        kwargs.get("p", 0.0)
                    ),
                    Field(
                        "t",
                        float,
                        60,
                        10,
                        kwargs.get("t", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Solid element ID.
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        self._cards[0].set_value("eid", value)

    @property
    def u(self) -> float:
        """Get or set the x- velocity components respectively.
        """ # nopep8
        return self._cards[0].get_value("u")

    @u.setter
    def u(self, value: float) -> None:
        self._cards[0].set_value("u", value)

    @property
    def v(self) -> float:
        """Get or set the y- velocity components respectively.
        """ # nopep8
        return self._cards[0].get_value("v")

    @v.setter
    def v(self, value: float) -> None:
        self._cards[0].set_value("v", value)

    @property
    def w(self) -> float:
        """Get or set the z- velocity components respectively.
        """ # nopep8
        return self._cards[0].get_value("w")

    @w.setter
    def w(self, value: float) -> None:
        self._cards[0].set_value("w", value)

    @property
    def rho(self) -> float:
        """Get or set the Density.
        """ # nopep8
        return self._cards[0].get_value("rho")

    @rho.setter
    def rho(self, value: float) -> None:
        self._cards[0].set_value("rho", value)

    @property
    def p(self) -> float:
        """Get or set the Pressure .
        """ # nopep8
        return self._cards[0].get_value("p")

    @p.setter
    def p(self, value: float) -> None:
        self._cards[0].set_value("p", value)

    @property
    def t(self) -> float:
        """Get or set the Temperature .
        """ # nopep8
        return self._cards[0].get_value("t")

    @t.setter
    def t(self, value: float) -> None:
        self._cards[0].set_value("t", value)

