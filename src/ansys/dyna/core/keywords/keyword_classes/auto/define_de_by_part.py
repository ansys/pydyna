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

class DefineDeByPart(KeywordBase):
    """DYNA DEFINE_DE_BY_PART keyword"""

    keyword = "DEFINE"
    subkeyword = "DE_BY_PART"
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
                        "pid",
                        int,
                        0,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "ndamp",
                        float,
                        10,
                        10,
                        kwargs.get("ndamp", 0)
                    ),
                    Field(
                        "tdamp",
                        float,
                        20,
                        10,
                        kwargs.get("tdamp", 0)
                    ),
                    Field(
                        "fric",
                        float,
                        30,
                        10,
                        kwargs.get("fric", 0)
                    ),
                    Field(
                        "fricr",
                        float,
                        40,
                        10,
                        kwargs.get("fricr", 0)
                    ),
                    Field(
                        "normk",
                        float,
                        50,
                        10,
                        kwargs.get("normk", 0.01)
                    ),
                    Field(
                        "sheark",
                        float,
                        60,
                        10,
                        kwargs.get("sheark", 0.2857)
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
                        kwargs.get("gamma", 0)
                    ),
                    Field(
                        "vol",
                        float,
                        10,
                        10,
                        kwargs.get("vol", 0)
                    ),
                    Field(
                        "ang",
                        float,
                        20,
                        10,
                        kwargs.get("ang", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lnorm",
                        int,
                        0,
                        10,
                        kwargs.get("lnorm", 0)
                    ),
                    Field(
                        "lshear",
                        int,
                        10,
                        10,
                        kwargs.get("lshear", 0)
                    ),
                    Field(
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "fricd",
                        float,
                        30,
                        10,
                        kwargs.get("fricd", 0)
                    ),
                    Field(
                        "dc",
                        float,
                        40,
                        10,
                        kwargs.get("dc", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineDeByPart.option_specs[0],
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
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of DES nodes.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def ndamp(self) -> float:
        """Get or set the Normal damping coefficient.
        """ # nopep8
        return self._cards[0].get_value("ndamp")

    @ndamp.setter
    def ndamp(self, value: float) -> None:
        self._cards[0].set_value("ndamp", value)

    @property
    def tdamp(self) -> float:
        """Get or set the Tangential damping coefficient.
        """ # nopep8
        return self._cards[0].get_value("tdamp")

    @tdamp.setter
    def tdamp(self, value: float) -> None:
        self._cards[0].set_value("tdamp", value)

    @property
    def fric(self) -> float:
        """Get or set the Friction coefficient
        EQ.0: 3 DOF
        NE.0: 6 DOF (consider rotational DOF).
        """ # nopep8
        return self._cards[0].get_value("fric")

    @fric.setter
    def fric(self, value: float) -> None:
        self._cards[0].set_value("fric", value)

    @property
    def fricr(self) -> float:
        """Get or set the Rolling friction coefficient.
        """ # nopep8
        return self._cards[0].get_value("fricr")

    @fricr.setter
    def fricr(self, value: float) -> None:
        self._cards[0].set_value("fricr", value)

    @property
    def normk(self) -> float:
        """Get or set the Optional: scale factor of normal spring constant (Default = 0.01).
        """ # nopep8
        return self._cards[0].get_value("normk")

    @normk.setter
    def normk(self, value: float) -> None:
        self._cards[0].set_value("normk", value)

    @property
    def sheark(self) -> float:
        """Get or set the Optional: ratio between ShearK/NormK (Default = 2/7).
        """ # nopep8
        return self._cards[0].get_value("sheark")

    @sheark.setter
    def sheark(self, value: float) -> None:
        self._cards[0].set_value("sheark", value)

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
    def lnorm(self) -> int:
        """Get or set the Load curve ID of a curve that defines normal stiffness as a function of norm penetration ratio
        """ # nopep8
        return self._cards[2].get_value("lnorm")

    @lnorm.setter
    def lnorm(self, value: int) -> None:
        self._cards[2].set_value("lnorm", value)

    @property
    def lshear(self) -> int:
        """Get or set the Load curve ID of a curve that defines shear stiffness as a function of norm penetration ratio
        """ # nopep8
        return self._cards[2].get_value("lshear")

    @lshear.setter
    def lshear(self, value: int) -> None:
        self._cards[2].set_value("lshear", value)

    @property
    def fricd(self) -> float:
        """Get or set the Dynamic coefficient of friction. By default, FRICD = FRICS.
        """ # nopep8
        return self._cards[2].get_value("fricd")

    @fricd.setter
    def fricd(self, value: float) -> None:
        self._cards[2].set_value("fricd", value)

    @property
    def dc(self) -> float:
        """Get or set the Exponential decay coefficient.  See Remarks
        """ # nopep8
        return self._cards[2].get_value("dc")

    @dc.setter
    def dc(self, value: float) -> None:
        self._cards[2].set_value("dc", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

