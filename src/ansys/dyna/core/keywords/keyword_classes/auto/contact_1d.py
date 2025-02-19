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

class Contact1D(KeywordBase):
    """DYNA CONTACT_1D keyword"""

    keyword = "CONTACT"
    subkeyword = "1D"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nsidr",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nsidc",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "err",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "sigc",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "gb",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "smax",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "exp",
                        float,
                        60,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def nsidr(self) -> typing.Optional[int]:
        """Get or set the Nodal set ID for the rebar nodes that slide along the concrete; see* SET_NODE
        """ # nopep8
        return self._cards[0].get_value("nsidr")

    @nsidr.setter
    def nsidr(self, value: int) -> None:
        self._cards[0].set_value("nsidr", value)

    @property
    def nsidc(self) -> typing.Optional[int]:
        """Get or set the Nodal set ID for the concrete nodes that the rebar nodes may slide along; see* SET_NODE
        """ # nopep8
        return self._cards[0].get_value("nsidc")

    @nsidc.setter
    def nsidc(self, value: int) -> None:
        self._cards[0].set_value("nsidc", value)

    @property
    def err(self) -> float:
        """Get or set the External radius of rebar.
        """ # nopep8
        return self._cards[0].get_value("err")

    @err.setter
    def err(self, value: float) -> None:
        self._cards[0].set_value("err", value)

    @property
    def sigc(self) -> float:
        """Get or set the Compressive strength of concrete.
        """ # nopep8
        return self._cards[0].get_value("sigc")

    @sigc.setter
    def sigc(self, value: float) -> None:
        self._cards[0].set_value("sigc", value)

    @property
    def gb(self) -> float:
        """Get or set the Bond shear modulus.
        """ # nopep8
        return self._cards[0].get_value("gb")

    @gb.setter
    def gb(self, value: float) -> None:
        self._cards[0].set_value("gb", value)

    @property
    def smax(self) -> float:
        """Get or set the Maximum shear strain displacement.
        """ # nopep8
        return self._cards[0].get_value("smax")

    @smax.setter
    def smax(self, value: float) -> None:
        self._cards[0].set_value("smax", value)

    @property
    def exp(self) -> float:
        """Get or set the Exponent in damage curve.
        """ # nopep8
        return self._cards[0].get_value("exp")

    @exp.setter
    def exp(self, value: float) -> None:
        self._cards[0].set_value("exp", value)

