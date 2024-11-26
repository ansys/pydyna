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

class MatElasticFluid(KeywordBase):
    """DYNA MAT_ELASTIC_FLUID keyword"""

    keyword = "MAT"
    subkeyword = "ELASTIC_FLUID"
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
                        "e",
                        float,
                        20,
                        10,
                        kwargs.get("e")
                    ),
                    Field(
                        "pr",
                        float,
                        30,
                        10,
                        kwargs.get("pr")
                    ),
                    Field(
                        "da",
                        float,
                        40,
                        10,
                        kwargs.get("da")
                    ),
                    Field(
                        "db",
                        float,
                        50,
                        10,
                        kwargs.get("db")
                    ),
                    Field(
                        "k",
                        float,
                        60,
                        10,
                        kwargs.get("k", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "vc",
                        float,
                        0,
                        10,
                        kwargs.get("vc")
                    ),
                    Field(
                        "cp",
                        float,
                        10,
                        10,
                        kwargs.get("cp", 1.0E+20)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatElasticFluid.option_specs[0],
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
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def da(self) -> typing.Optional[float]:
        """Get or set the Axial damping factor (used for Belytschko-Schwer beam, type 1, only).
        """ # nopep8
        return self._cards[0].get_value("da")

    @da.setter
    def da(self, value: float) -> None:
        self._cards[0].set_value("da", value)

    @property
    def db(self) -> typing.Optional[float]:
        """Get or set the Bending damping factor (used for Belytschko-Schwer beam, type 1, only).
        """ # nopep8
        return self._cards[0].get_value("db")

    @db.setter
    def db(self, value: float) -> None:
        self._cards[0].set_value("db", value)

    @property
    def k(self) -> float:
        """Get or set the Bulk modulus.
        """ # nopep8
        return self._cards[0].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        self._cards[0].set_value("k", value)

    @property
    def vc(self) -> typing.Optional[float]:
        """Get or set the Tensor viscosity coefficient, values between 0.1 and 0.5 should be okay.
        """ # nopep8
        return self._cards[1].get_value("vc")

    @vc.setter
    def vc(self, value: float) -> None:
        self._cards[1].set_value("vc", value)

    @property
    def cp(self) -> float:
        """Get or set the Cavitation pressure (default = 1.0e+20).
        """ # nopep8
        return self._cards[1].get_value("cp")

    @cp.setter
    def cp(self, value: float) -> None:
        self._cards[1].set_value("cp", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

