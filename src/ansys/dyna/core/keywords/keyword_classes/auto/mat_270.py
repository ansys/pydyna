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

class Mat270(KeywordBase):
    """DYNA MAT_270 keyword"""

    keyword = "MAT"
    subkeyword = "270"
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
                        "lcem",
                        int,
                        20,
                        10,
                        kwargs.get("lcem")
                    ),
                    Field(
                        "lcpr",
                        int,
                        30,
                        10,
                        kwargs.get("lcpr")
                    ),
                    Field(
                        "lcsy",
                        int,
                        40,
                        10,
                        kwargs.get("lcsy")
                    ),
                    Field(
                        "lchr",
                        int,
                        50,
                        10,
                        kwargs.get("lchr")
                    ),
                    Field(
                        "lcat",
                        int,
                        60,
                        10,
                        kwargs.get("lcat")
                    ),
                    Field(
                        "beta",
                        float,
                        70,
                        10,
                        kwargs.get("beta")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "tastart",
                        float,
                        0,
                        10,
                        kwargs.get("tastart")
                    ),
                    Field(
                        "taend",
                        float,
                        10,
                        10,
                        kwargs.get("taend")
                    ),
                    Field(
                        "tlstart",
                        float,
                        20,
                        10,
                        kwargs.get("tlstart")
                    ),
                    Field(
                        "tlend",
                        float,
                        30,
                        10,
                        kwargs.get("tlend")
                    ),
                    Field(
                        "eghost",
                        float,
                        40,
                        10,
                        kwargs.get("eghost")
                    ),
                    Field(
                        "pghost",
                        float,
                        50,
                        10,
                        kwargs.get("pghost")
                    ),
                    Field(
                        "aghost",
                        float,
                        60,
                        10,
                        kwargs.get("aghost")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat270.option_specs[0],
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
        """Get or set the Material identification.
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
    def lcem(self) -> typing.Optional[int]:
        """Get or set the Load curve for Young's modulus as function of temperature.
        """ # nopep8
        return self._cards[0].get_value("lcem")

    @lcem.setter
    def lcem(self, value: int) -> None:
        self._cards[0].set_value("lcem", value)

    @property
    def lcpr(self) -> typing.Optional[int]:
        """Get or set the Load curve for Poisson's ratio as function of temperature.
        """ # nopep8
        return self._cards[0].get_value("lcpr")

    @lcpr.setter
    def lcpr(self, value: int) -> None:
        self._cards[0].set_value("lcpr", value)

    @property
    def lcsy(self) -> typing.Optional[int]:
        """Get or set the Load curve for yield stress as function of temperature.
        """ # nopep8
        return self._cards[0].get_value("lcsy")

    @lcsy.setter
    def lcsy(self, value: int) -> None:
        self._cards[0].set_value("lcsy", value)

    @property
    def lchr(self) -> typing.Optional[int]:
        """Get or set the Load curve for hardening modulus as function of temperature.
        """ # nopep8
        return self._cards[0].get_value("lchr")

    @lchr.setter
    def lchr(self, value: int) -> None:
        self._cards[0].set_value("lchr", value)

    @property
    def lcat(self) -> typing.Optional[int]:
        """Get or set the Load curve for thermal expansion coefficient as function of temperature.
        """ # nopep8
        return self._cards[0].get_value("lcat")

    @lcat.setter
    def lcat(self, value: int) -> None:
        self._cards[0].set_value("lcat", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Fraction isotropic hardening between 0 and 1
        EQ.0: Kinematic hardening
        EQ.1: Isotropic hardening.
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[0].set_value("beta", value)

    @property
    def tastart(self) -> typing.Optional[float]:
        """Get or set the Annealing temperature start.
        """ # nopep8
        return self._cards[1].get_value("tastart")

    @tastart.setter
    def tastart(self, value: float) -> None:
        self._cards[1].set_value("tastart", value)

    @property
    def taend(self) -> typing.Optional[float]:
        """Get or set the Annealing temperature end.
        """ # nopep8
        return self._cards[1].get_value("taend")

    @taend.setter
    def taend(self, value: float) -> None:
        self._cards[1].set_value("taend", value)

    @property
    def tlstart(self) -> typing.Optional[float]:
        """Get or set the Birth temperature start.
        """ # nopep8
        return self._cards[1].get_value("tlstart")

    @tlstart.setter
    def tlstart(self, value: float) -> None:
        self._cards[1].set_value("tlstart", value)

    @property
    def tlend(self) -> typing.Optional[float]:
        """Get or set the Birth temperature end.
        """ # nopep8
        return self._cards[1].get_value("tlend")

    @tlend.setter
    def tlend(self, value: float) -> None:
        self._cards[1].set_value("tlend", value)

    @property
    def eghost(self) -> typing.Optional[float]:
        """Get or set the Young's modulus for ghost (quiet) material.
        """ # nopep8
        return self._cards[1].get_value("eghost")

    @eghost.setter
    def eghost(self, value: float) -> None:
        self._cards[1].set_value("eghost", value)

    @property
    def pghost(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio for ghost (quiet) material.
        """ # nopep8
        return self._cards[1].get_value("pghost")

    @pghost.setter
    def pghost(self, value: float) -> None:
        self._cards[1].set_value("pghost", value)

    @property
    def aghost(self) -> typing.Optional[float]:
        """Get or set the Thermal expansion coefficient for ghost (quiet) material.
        """ # nopep8
        return self._cards[1].get_value("aghost")

    @aghost.setter
    def aghost(self, value: float) -> None:
        self._cards[1].set_value("aghost", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

