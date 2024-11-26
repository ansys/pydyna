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

class DefineFormingClamp(KeywordBase):
    """DYNA DEFINE_FORMING_CLAMP keyword"""

    keyword = "DEFINE"
    subkeyword = "FORMING_CLAMP"
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
                        "clp1",
                        int,
                        0,
                        10,
                        kwargs.get("clp1")
                    ),
                    Field(
                        "clp2",
                        int,
                        10,
                        10,
                        kwargs.get("clp2")
                    ),
                    Field(
                        "vid",
                        int,
                        20,
                        10,
                        kwargs.get("vid")
                    ),
                    Field(
                        "gap",
                        float,
                        30,
                        10,
                        kwargs.get("gap", 0.0)
                    ),
                    Field(
                        "at",
                        float,
                        40,
                        10,
                        kwargs.get("at", 0.0)
                    ),
                    Field(
                        "dt",
                        float,
                        50,
                        10,
                        kwargs.get("dt", 0.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineFormingClamp.option_specs[0],
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
    def clp1(self) -> typing.Optional[int]:
        """Get or set the Part ID of a moving rigid body clamp, defined by *PART and *MAT_020 (*MAT_RIGID).
        """ # nopep8
        return self._cards[0].get_value("clp1")

    @clp1.setter
    def clp1(self, value: int) -> None:
        self._cards[0].set_value("clp1", value)

    @property
    def clp2(self) -> typing.Optional[int]:
        """Get or set the Part ID of a fixed rigid body clamp, defined by *PART and *MAT_020.  This is sometimes called “net pad.
        """ # nopep8
        return self._cards[0].get_value("clp2")

    @clp2.setter
    def clp2(self, value: int) -> None:
        self._cards[0].set_value("clp2", value)

    @property
    def vid(self) -> typing.Optional[int]:
        """Get or set the Define CLP1 moving direction:
        GT.0:	Vector ID from *DEFINE_VECTOR, specifying the moving direction of CLP1
        LT.0:	Absolute value is a node ID, whose normal vector will be used to define the moving direction of CLP1.
        """ # nopep8
        return self._cards[0].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        self._cards[0].set_value("vid", value)

    @property
    def gap(self) -> float:
        """Get or set the Final desired distance between CLP1 and CLP2 at the end of clamping.
        """ # nopep8
        return self._cards[0].get_value("gap")

    @gap.setter
    def gap(self, value: float) -> None:
        self._cards[0].set_value("gap", value)

    @property
    def at(self) -> float:
        """Get or set the Begin time for CLP1’s move.
        """ # nopep8
        return self._cards[0].get_value("at")

    @at.setter
    def at(self, value: float) -> None:
        self._cards[0].set_value("at", value)

    @property
    def dt(self) -> float:
        """Get or set the Duration of CLP1’s move.
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[0].set_value("dt", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

