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

class MatBrittleDamage(KeywordBase):
    """DYNA MAT_BRITTLE_DAMAGE keyword"""

    keyword = "MAT"
    subkeyword = "BRITTLE_DAMAGE"
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
                        "tlimit",
                        float,
                        40,
                        10,
                        kwargs.get("tlimit")
                    ),
                    Field(
                        "slimit",
                        float,
                        50,
                        10,
                        kwargs.get("slimit")
                    ),
                    Field(
                        "ftough",
                        float,
                        60,
                        10,
                        kwargs.get("ftough")
                    ),
                    Field(
                        "sreten",
                        float,
                        70,
                        10,
                        kwargs.get("sreten")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "visc",
                        float,
                        0,
                        10,
                        kwargs.get("visc")
                    ),
                    Field(
                        "fra_rf",
                        float,
                        10,
                        10,
                        kwargs.get("fra_rf")
                    ),
                    Field(
                        "e_rf",
                        float,
                        20,
                        10,
                        kwargs.get("e_rf")
                    ),
                    Field(
                        "ys_rf",
                        float,
                        30,
                        10,
                        kwargs.get("ys_rf")
                    ),
                    Field(
                        "eh_rf",
                        float,
                        40,
                        10,
                        kwargs.get("eh_rf")
                    ),
                    Field(
                        "fs_rf",
                        float,
                        50,
                        10,
                        kwargs.get("fs_rf")
                    ),
                    Field(
                        "sigy",
                        float,
                        60,
                        10,
                        kwargs.get("sigy")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatBrittleDamage.option_specs[0],
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
    def tlimit(self) -> typing.Optional[float]:
        """Get or set the Tensile limit.
        """ # nopep8
        return self._cards[0].get_value("tlimit")

    @tlimit.setter
    def tlimit(self, value: float) -> None:
        self._cards[0].set_value("tlimit", value)

    @property
    def slimit(self) -> typing.Optional[float]:
        """Get or set the Shear limit.
        """ # nopep8
        return self._cards[0].get_value("slimit")

    @slimit.setter
    def slimit(self, value: float) -> None:
        self._cards[0].set_value("slimit", value)

    @property
    def ftough(self) -> typing.Optional[float]:
        """Get or set the Fracture toughness.
        """ # nopep8
        return self._cards[0].get_value("ftough")

    @ftough.setter
    def ftough(self, value: float) -> None:
        self._cards[0].set_value("ftough", value)

    @property
    def sreten(self) -> typing.Optional[float]:
        """Get or set the Shear retention.
        """ # nopep8
        return self._cards[0].get_value("sreten")

    @sreten.setter
    def sreten(self, value: float) -> None:
        self._cards[0].set_value("sreten", value)

    @property
    def visc(self) -> typing.Optional[float]:
        """Get or set the Viscosity.
        """ # nopep8
        return self._cards[1].get_value("visc")

    @visc.setter
    def visc(self, value: float) -> None:
        self._cards[1].set_value("visc", value)

    @property
    def fra_rf(self) -> typing.Optional[float]:
        """Get or set the Fraction of reinforcement in section.
        """ # nopep8
        return self._cards[1].get_value("fra_rf")

    @fra_rf.setter
    def fra_rf(self, value: float) -> None:
        self._cards[1].set_value("fra_rf", value)

    @property
    def e_rf(self) -> typing.Optional[float]:
        """Get or set the Young's modulus of reinforcement.
        """ # nopep8
        return self._cards[1].get_value("e_rf")

    @e_rf.setter
    def e_rf(self, value: float) -> None:
        self._cards[1].set_value("e_rf", value)

    @property
    def ys_rf(self) -> typing.Optional[float]:
        """Get or set the Yield stress of reinforcement.
        """ # nopep8
        return self._cards[1].get_value("ys_rf")

    @ys_rf.setter
    def ys_rf(self, value: float) -> None:
        self._cards[1].set_value("ys_rf", value)

    @property
    def eh_rf(self) -> typing.Optional[float]:
        """Get or set the Hardening modulus of reinforcement.
        """ # nopep8
        return self._cards[1].get_value("eh_rf")

    @eh_rf.setter
    def eh_rf(self, value: float) -> None:
        self._cards[1].set_value("eh_rf", value)

    @property
    def fs_rf(self) -> typing.Optional[float]:
        """Get or set the Failure strain (true) of reinforcement.
        """ # nopep8
        return self._cards[1].get_value("fs_rf")

    @fs_rf.setter
    def fs_rf(self, value: float) -> None:
        self._cards[1].set_value("fs_rf", value)

    @property
    def sigy(self) -> typing.Optional[float]:
        """Get or set the Compressive yield stress.
        EQ.0: no compressive yield.
        """ # nopep8
        return self._cards[1].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        self._cards[1].set_value("sigy", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

