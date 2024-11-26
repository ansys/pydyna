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

class DefineDeCohesive(KeywordBase):
    """DYNA DEFINE_DE_COHESIVE keyword"""

    keyword = "DEFINE"
    subkeyword = "DE_COHESIVE"
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
                        "sid",
                        int,
                        0,
                        10,
                        kwargs.get("sid", 0)
                    ),
                    Field(
                        "styp",
                        int,
                        10,
                        10,
                        kwargs.get("styp", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "gamma",
                        float,
                        0,
                        10,
                        kwargs.get("gamma", 0.0)
                    ),
                    Field(
                        "vol",
                        float,
                        10,
                        10,
                        kwargs.get("vol", 0.0)
                    ),
                    Field(
                        "ang",
                        float,
                        20,
                        10,
                        kwargs.get("ang", 0.0)
                    ),
                    Field(
                        "gap",
                        float,
                        30,
                        10,
                        kwargs.get("gap", 0.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineDeCohesive.option_specs[0],
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
    def sid(self) -> int:
        """Get or set the Node set ID, part set ID or part ID defining DES with cohesive force.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[0].set_value("sid", value)

    @property
    def styp(self) -> int:
        """Get or set the SID type:
        EQ.0:	Node set
        EQ.1:	Part set
        EQ.2: Part.
        """ # nopep8
        return self._cards[0].get_value("styp")

    @styp.setter
    def styp(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""styp must be one of {0,1,2}""")
        self._cards[0].set_value("styp", value)

    @property
    def gamma(self) -> float:
        """Get or set the Liquid surface tension.
        """ # nopep8
        return self._cards[1].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        self._cards[1].set_value("gamma", value)

    @property
    def vol(self) -> float:
        """Get or set the Volume fraction.
        """ # nopep8
        return self._cards[1].get_value("vol")

    @vol.setter
    def vol(self, value: float) -> None:
        self._cards[1].set_value("vol", value)

    @property
    def ang(self) -> float:
        """Get or set the Contact angle.
        """ # nopep8
        return self._cards[1].get_value("ang")

    @ang.setter
    def ang(self, value: float) -> None:
        self._cards[1].set_value("ang", value)

    @property
    def gap(self) -> float:
        """Get or set the Spatial limit for the existence of liquid bridge between particles.
        A liquid bridge will exist when the distance between two particles is less or equal to min(GAP, drup)
        where drup is the rupture distance of the bridge automatically calculated by LS-DYNA.
        """ # nopep8
        return self._cards[1].get_value("gap")

    @gap.setter
    def gap(self, value: float) -> None:
        self._cards[1].set_value("gap", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

