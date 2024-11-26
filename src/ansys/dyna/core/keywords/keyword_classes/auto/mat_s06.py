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

class MatS06(KeywordBase):
    """DYNA MAT_S06 keyword"""

    keyword = "MAT"
    subkeyword = "S06"
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
                        "lcdl",
                        int,
                        10,
                        10,
                        kwargs.get("lcdl")
                    ),
                    Field(
                        "lcdu",
                        int,
                        20,
                        10,
                        kwargs.get("lcdu")
                    ),
                    Field(
                        "beta",
                        float,
                        30,
                        10,
                        kwargs.get("beta")
                    ),
                    Field(
                        "tyi",
                        float,
                        40,
                        10,
                        kwargs.get("tyi")
                    ),
                    Field(
                        "cyi",
                        float,
                        50,
                        10,
                        kwargs.get("cyi")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatS06.option_specs[0],
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
    def lcdl(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID giving force/torque as a function of displacement/rotation (curve) or as a function of velocity and displacement/rotation (table) for loading;
        """ # nopep8
        return self._cards[0].get_value("lcdl")

    @lcdl.setter
    def lcdl(self, value: int) -> None:
        self._cards[0].set_value("lcdl", value)

    @property
    def lcdu(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID giving force/torque as a function of displacement/rotation (curve) or as a function of velocity and displacement/rotation (table) for unloading
        """ # nopep8
        return self._cards[0].get_value("lcdu")

    @lcdu.setter
    def lcdu(self, value: int) -> None:
        self._cards[0].set_value("lcdu", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Hardening parameter:
        EQ.0.0: tensile and compressive yield with strain softening (negative or zero slope allowed in the force versus disp. load curves),
        NE.0.0: kinematic hardening without strain softening,
        EQ.1.0: isotropic hardening without strain softening.
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[0].set_value("beta", value)

    @property
    def tyi(self) -> typing.Optional[float]:
        """Get or set the Initial yield force in tension ( > 0).
        """ # nopep8
        return self._cards[0].get_value("tyi")

    @tyi.setter
    def tyi(self, value: float) -> None:
        self._cards[0].set_value("tyi", value)

    @property
    def cyi(self) -> typing.Optional[float]:
        """Get or set the Initial yield force in compression ( < 0).
        """ # nopep8
        return self._cards[0].get_value("cyi")

    @cyi.setter
    def cyi(self, value: float) -> None:
        self._cards[0].set_value("cyi", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

