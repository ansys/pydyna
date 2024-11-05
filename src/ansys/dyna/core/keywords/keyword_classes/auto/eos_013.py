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

class Eos013(KeywordBase):
    """DYNA EOS_013 keyword"""

    keyword = "EOS"
    subkeyword = "013"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "eosid",
                        int,
                        0,
                        10,
                        kwargs.get("eosid")
                    ),
                    Field(
                        "rhol",
                        float,
                        10,
                        10,
                        kwargs.get("rhol")
                    ),
                    Field(
                        "rhov",
                        float,
                        20,
                        10,
                        kwargs.get("rhov")
                    ),
                    Field(
                        "cl",
                        float,
                        30,
                        10,
                        kwargs.get("cl")
                    ),
                    Field(
                        "cv",
                        float,
                        40,
                        10,
                        kwargs.get("cv")
                    ),
                    Field(
                        "gamal",
                        float,
                        50,
                        10,
                        kwargs.get("gamal")
                    ),
                    Field(
                        "pv",
                        float,
                        60,
                        10,
                        kwargs.get("pv")
                    ),
                    Field(
                        "kl",
                        float,
                        70,
                        10,
                        kwargs.get("kl")
                    ),
                ],
            ),
        ]

    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state ID, a unique number or label must be specified.
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        self._cards[0].set_value("eosid", value)

    @property
    def rhol(self) -> typing.Optional[float]:
        """Get or set the Density of liquid
        """ # nopep8
        return self._cards[0].get_value("rhol")

    @rhol.setter
    def rhol(self, value: float) -> None:
        self._cards[0].set_value("rhol", value)

    @property
    def rhov(self) -> typing.Optional[float]:
        """Get or set the Density of saturated vapor
        """ # nopep8
        return self._cards[0].get_value("rhov")

    @rhov.setter
    def rhov(self, value: float) -> None:
        self._cards[0].set_value("rhov", value)

    @property
    def cl(self) -> typing.Optional[float]:
        """Get or set the Speed of sound of liquid.
        """ # nopep8
        return self._cards[0].get_value("cl")

    @cl.setter
    def cl(self, value: float) -> None:
        self._cards[0].set_value("cl", value)

    @property
    def cv(self) -> typing.Optional[float]:
        """Get or set the Speed of sound of vapor.
        """ # nopep8
        return self._cards[0].get_value("cv")

    @cv.setter
    def cv(self, value: float) -> None:
        self._cards[0].set_value("cv", value)

    @property
    def gamal(self) -> typing.Optional[float]:
        """Get or set the Gamma constant of liquid.
        """ # nopep8
        return self._cards[0].get_value("gamal")

    @gamal.setter
    def gamal(self, value: float) -> None:
        self._cards[0].set_value("gamal", value)

    @property
    def pv(self) -> typing.Optional[float]:
        """Get or set the Pressure of vapor.
        """ # nopep8
        return self._cards[0].get_value("pv")

    @pv.setter
    def pv(self, value: float) -> None:
        self._cards[0].set_value("pv", value)

    @property
    def kl(self) -> typing.Optional[float]:
        """Get or set the Bulk compressibility of liquid.
        """ # nopep8
        return self._cards[0].get_value("kl")

    @kl.setter
    def kl(self, value: float) -> None:
        self._cards[0].set_value("kl", value)

