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

class MatAleViscous(KeywordBase):
    """DYNA MAT_ALE_VISCOUS keyword"""

    keyword = "MAT"
    subkeyword = "ALE_VISCOUS"
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
                        "pc",
                        float,
                        20,
                        10,
                        kwargs.get("pc")
                    ),
                    Field(
                        "mulo",
                        float,
                        30,
                        10,
                        kwargs.get("mulo")
                    ),
                    Field(
                        "muhi",
                        float,
                        40,
                        10,
                        kwargs.get("muhi")
                    ),
                    Field(
                        "rk",
                        float,
                        50,
                        10,
                        kwargs.get("rk")
                    ),
                    Field(
                        "unused",
                        float,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "rn",
                        float,
                        70,
                        10,
                        kwargs.get("rn")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatAleViscous.option_specs[0],
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
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def pc(self) -> typing.Optional[float]:
        """Get or set the Pressure cutoff
        """ # nopep8
        return self._cards[0].get_value("pc")

    @pc.setter
    def pc(self, value: float) -> None:
        self._cards[0].set_value("pc", value)

    @property
    def mulo(self) -> typing.Optional[float]:
        """Get or set the There are 4 possible cases (See remark 1):
        1) If MULO=0.0, then inviscid fluid is assumed.
        2) If MULO > 0.0, and MUHI=0.0 or is not defined, then this is the traditional constant dynamic viscosity coefficient.
        3) If MULO > 0.0, and MUHI > 0.0, then MULO and MUHI are lower and upper viscosity limit values for a power-law-like variable viscosity model.
        4) If MULO is negative (for example, MULO = -1), then a user-input data load curve (with LCID=1) defining dynamic viscosity as a function of equivalent strain rate is used

        """ # nopep8
        return self._cards[0].get_value("mulo")

    @mulo.setter
    def mulo(self, value: float) -> None:
        self._cards[0].set_value("mulo", value)

    @property
    def muhi(self) -> typing.Optional[float]:
        """Get or set the Upper dynamic viscosity limit (default=0.0).  This is defined only if RK and RN are defined for the variable viscosity case.
        """ # nopep8
        return self._cards[0].get_value("muhi")

    @muhi.setter
    def muhi(self, value: float) -> None:
        self._cards[0].set_value("muhi", value)

    @property
    def rk(self) -> typing.Optional[float]:
        """Get or set the Variable dynamic viscosity multiplier.
        """ # nopep8
        return self._cards[0].get_value("rk")

    @rk.setter
    def rk(self, value: float) -> None:
        self._cards[0].set_value("rk", value)

    @property
    def rn(self) -> typing.Optional[float]:
        """Get or set the Variable dynamic viscosity exponent.
        """ # nopep8
        return self._cards[0].get_value("rn")

    @rn.setter
    def rn(self, value: float) -> None:
        self._cards[0].set_value("rn", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

