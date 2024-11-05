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

class MatSph02(KeywordBase):
    """DYNA MAT_SPH_02 keyword"""

    keyword = "MAT"
    subkeyword = "SPH_02"
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
                        "mu",
                        float,
                        20,
                        10,
                        kwargs.get("mu")
                    ),
                    Field(
                        "gamma1",
                        float,
                        30,
                        10,
                        kwargs.get("gamma1")
                    ),
                    Field(
                        "gamma2",
                        float,
                        40,
                        10,
                        kwargs.get("gamma2")
                    ),
                    Field(
                        "stens",
                        float,
                        50,
                        10,
                        kwargs.get("stens")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatSph02.option_specs[0],
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
    def mu(self) -> typing.Optional[float]:
        """Get or set the Dynamic viscosity.
        """ # nopep8
        return self._cards[0].get_value("mu")

    @mu.setter
    def mu(self, value: float) -> None:
        self._cards[0].set_value("mu", value)

    @property
    def gamma1(self) -> typing.Optional[float]:
        """Get or set the Numerical surface tension coefficient. For water,we recommend a coefficient of γ_1=1000 m/s^2. GAMMA1 is only used if IMAT = 0 in *CONTROL_SPH_INCOMPRESSIBLE.
        """ # nopep8
        return self._cards[0].get_value("gamma1")

    @gamma1.setter
    def gamma1(self, value: float) -> None:
        self._cards[0].set_value("gamma1", value)

    @property
    def gamma2(self) -> typing.Optional[float]:
        """Get or set the Numerical surface tension coefficient. For water, we recommend a coefficient of γ_2=1 m/s^2. GAMMA2 is only used if IMAT = 0 in *CONTROL_SPH_INCOMPRESSIBLE
        """ # nopep8
        return self._cards[0].get_value("gamma2")

    @gamma2.setter
    def gamma2(self, value: float) -> None:
        self._cards[0].set_value("gamma2", value)

    @property
    def stens(self) -> typing.Optional[float]:
        """Get or set the Physical surface tension coefficient. Only used if IMAT=1 in *CONTROL_SPH_INCOMPRESSIBLE.
        """ # nopep8
        return self._cards[0].get_value("stens")

    @stens.setter
    def stens(self, value: float) -> None:
        self._cards[0].set_value("stens", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

