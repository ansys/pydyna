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

class MatSph01(KeywordBase):
    """DYNA MAT_SPH_01 keyword"""

    keyword = "MAT"
    subkeyword = "SPH_01"
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
                        "rc",
                        float,
                        60,
                        10,
                        kwargs.get("rc")
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
                option_spec = MatSph01.option_specs[0],
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
        """Get or set the Material identification. A unique number or label must be specified.
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
        """Get or set the Pressure cutoff (<= 0.0).
        """ # nopep8
        return self._cards[0].get_value("pc")

    @pc.setter
    def pc(self, value: float) -> None:
        self._cards[0].set_value("pc", value)

    @property
    def mulo(self) -> typing.Optional[float]:
        """Get or set the There are 4 possible cases (See Remark 1):
        1. If MULO = 0.0, then inviscid fluid is assumed.
        2. If MULO > 0.0, and MUHI = 0.0 or is not defined, then
        this is the traditional constant dynamic viscosity coefficient 𝜇.
        3. If MULO > 0.0, and MUHI > 0.0, then MULO and MUHI
        are lower and upper viscosity limit values for a powerlaw-like variable viscosity model.
        4. If MULO is negative (for example, MULO = -1), then a
        user-input data load curve (with LCID = 1) defining dynamic
        viscosity as a function of equivalent strain rate is used.
        """ # nopep8
        return self._cards[0].get_value("mulo")

    @mulo.setter
    def mulo(self, value: float) -> None:
        self._cards[0].set_value("mulo", value)

    @property
    def muhi(self) -> typing.Optional[float]:
        """Get or set the There are 2 possible cases:
        5. If MUHI < 0.0, then the viscosity can be defined by the
        user in the file dyn21.F with a routine called f3dm9sph_userdefin.
        The file is part of the general usermat package.
        6. If MUHI > 0.0, then this is the upper dynamic viscosity
        limit (default = 0.0). This is defined only if RK and RN
        are defined for the variable viscosity case.
        """ # nopep8
        return self._cards[0].get_value("muhi")

    @muhi.setter
    def muhi(self, value: float) -> None:
        self._cards[0].set_value("muhi", value)

    @property
    def rk(self) -> typing.Optional[float]:
        """Get or set the Variable dynamic viscosity multiplier. See Remark 6..
        """ # nopep8
        return self._cards[0].get_value("rk")

    @rk.setter
    def rk(self, value: float) -> None:
        self._cards[0].set_value("rk", value)

    @property
    def rc(self) -> typing.Optional[float]:
        """Get or set the Option for Cross viscosity model: See Remark 7.
        RC > 0.0: Cross viscosity model will be used (overwrite all
        other options), values of MULO, MUHI, RK and RN
        will be used in the Cross viscosity model. See Remark 7.
        RC ≤ 0.0: other viscosity model (decided based on above variables) will be used.
        """ # nopep8
        return self._cards[0].get_value("rc")

    @rc.setter
    def rc(self, value: float) -> None:
        self._cards[0].set_value("rc", value)

    @property
    def rn(self) -> typing.Optional[float]:
        """Get or set the Variable dynamic viscosity exponent. See Remark 6.
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

