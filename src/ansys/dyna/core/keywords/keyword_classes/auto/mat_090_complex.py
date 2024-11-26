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

class Mat090Complex(KeywordBase):
    """DYNA MAT_090_COMPLEX keyword"""

    keyword = "MAT"
    subkeyword = "090_COMPLEX"
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
                        "rhor",
                        float,
                        10,
                        10,
                        kwargs.get("rhor")
                    ),
                    Field(
                        "bulkr",
                        float,
                        20,
                        10,
                        kwargs.get("bulkr")
                    ),
                    Field(
                        "rhoi",
                        float,
                        30,
                        10,
                        kwargs.get("rhoi")
                    ),
                    Field(
                        "bulki",
                        float,
                        40,
                        10,
                        kwargs.get("bulki")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcidrr",
                        int,
                        0,
                        10,
                        kwargs.get("lcidrr")
                    ),
                    Field(
                        "lcidkr",
                        int,
                        10,
                        10,
                        kwargs.get("lcidkr")
                    ),
                    Field(
                        "lcidri",
                        int,
                        20,
                        10,
                        kwargs.get("lcidri")
                    ),
                    Field(
                        "lcidki",
                        int,
                        30,
                        10,
                        kwargs.get("lcidki")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat090Complex.option_specs[0],
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
    def rhor(self) -> typing.Optional[float]:
        """Get or set the Real part of the density.
        """ # nopep8
        return self._cards[0].get_value("rhor")

    @rhor.setter
    def rhor(self, value: float) -> None:
        self._cards[0].set_value("rhor", value)

    @property
    def bulkr(self) -> typing.Optional[float]:
        """Get or set the Real part of the bulk modulus.
        """ # nopep8
        return self._cards[0].get_value("bulkr")

    @bulkr.setter
    def bulkr(self, value: float) -> None:
        self._cards[0].set_value("bulkr", value)

    @property
    def rhoi(self) -> typing.Optional[float]:
        """Get or set the Imaginary part of the density.
        """ # nopep8
        return self._cards[0].get_value("rhoi")

    @rhoi.setter
    def rhoi(self, value: float) -> None:
        self._cards[0].set_value("rhoi", value)

    @property
    def bulki(self) -> typing.Optional[float]:
        """Get or set the Imaginary part of the bulk modulus.
        """ # nopep8
        return self._cards[0].get_value("bulki")

    @bulki.setter
    def bulki(self, value: float) -> None:
        self._cards[0].set_value("bulki", value)

    @property
    def lcidrr(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for specifying frequency variation.
        """ # nopep8
        return self._cards[1].get_value("lcidrr")

    @lcidrr.setter
    def lcidrr(self, value: int) -> None:
        self._cards[1].set_value("lcidrr", value)

    @property
    def lcidkr(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for specifying frequency variation.
        """ # nopep8
        return self._cards[1].get_value("lcidkr")

    @lcidkr.setter
    def lcidkr(self, value: int) -> None:
        self._cards[1].set_value("lcidkr", value)

    @property
    def lcidri(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for specifying frequency variation.
        """ # nopep8
        return self._cards[1].get_value("lcidri")

    @lcidri.setter
    def lcidri(self, value: int) -> None:
        self._cards[1].set_value("lcidri", value)

    @property
    def lcidki(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for specifying frequency variation.
        """ # nopep8
        return self._cards[1].get_value("lcidki")

    @lcidki.setter
    def lcidki(self, value: int) -> None:
        self._cards[1].set_value("lcidki", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

