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
from ansys.dyna.core.lib.keyword_base import KeywordBase

class InterfaceDeHbond(KeywordBase):
    """DYNA INTERFACE_DE_HBOND keyword"""

    keyword = "INTERFACE"
    subkeyword = "DE_HBOND"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "id",
                        int,
                        0,
                        10,
                        kwargs.get("id")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "pid1",
                        int,
                        0,
                        10,
                        kwargs.get("pid1")
                    ),
                    Field(
                        "pid2",
                        int,
                        10,
                        10,
                        kwargs.get("pid2")
                    ),
                    Field(
                        "ptype1",
                        int,
                        20,
                        10,
                        kwargs.get("ptype1", 0)
                    ),
                    Field(
                        "ptype2",
                        int,
                        30,
                        10,
                        kwargs.get("ptype2", 0)
                    ),
                    Field(
                        "frmdl",
                        int,
                        40,
                        10,
                        kwargs.get("frmdl", 1)
                    ),
                    Field(
                        "frgk",
                        float,
                        50,
                        10,
                        kwargs.get("frgk")
                    ),
                    Field(
                        "frgs",
                        float,
                        60,
                        10,
                        kwargs.get("frgs")
                    ),
                    Field(
                        "dmg",
                        float,
                        70,
                        10,
                        kwargs.get("dmg", 1.0)
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Interface ID. All interfaces should have a unique ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def pid1(self) -> typing.Optional[int]:
        """Get or set the First part ID.
        """ # nopep8
        return self._cards[1].get_value("pid1")

    @pid1.setter
    def pid1(self, value: int) -> None:
        self._cards[1].set_value("pid1", value)

    @property
    def pid2(self) -> typing.Optional[int]:
        """Get or set the Second part ID. PID1 and PID2 define the bonds that this fracture
        model is applied to. There are three combinations as
        Case a: PID1.EQ.0 This is the default model for all bonds, overriding the default	model defined in Card 2 of *DEFINE_DE_HBOND.
        Case b: PID1.GT.0 and PID2.EQ.0 This model is applied to the bonds within part PID1, instead of the default model.
        Case c: PID1.GT.0 and PID2.GT.0 This model is applied to the bonds between parts PID1 and
        PID2 only, but not to those within part PID1 or part PID2 (as in case b).
        Notes:
        1. The default fracture model is applied to all parts that are not specified in case b.
        2. The fracture model of the part with a smaller part id is applied to the bonds between two different parts if not specified in case c..
        """ # nopep8
        return self._cards[1].get_value("pid2")

    @pid2.setter
    def pid2(self, value: int) -> None:
        self._cards[1].set_value("pid2", value)

    @property
    def ptype1(self) -> int:
        """Get or set the First part type:
        EQ.0: DES part set
        EQ.1: DES part.
        """ # nopep8
        return self._cards[1].get_value("ptype1")

    @ptype1.setter
    def ptype1(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ptype1 must be one of {0,1}""")
        self._cards[1].set_value("ptype1", value)

    @property
    def ptype2(self) -> int:
        """Get or set the Second part type:
        EQ.0: DES part set
        EQ.1: DES part.
        """ # nopep8
        return self._cards[1].get_value("ptype2")

    @ptype2.setter
    def ptype2(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ptype2 must be one of {0,1}""")
        self._cards[1].set_value("ptype2", value)

    @property
    def frmdl(self) -> int:
        """Get or set the Fracture model. (same as FRMDL in Card2 of keyword *DEFINE_DE_HBOND.).
        """ # nopep8
        return self._cards[1].get_value("frmdl")

    @frmdl.setter
    def frmdl(self, value: int) -> None:
        self._cards[1].set_value("frmdl", value)

    @property
    def frgk(self) -> typing.Optional[float]:
        """Get or set the Fracture energy release rate for volumetric deformation. (same as	FRGK in Card2 of keyword *DEFINE_DE_HBOND.).
        """ # nopep8
        return self._cards[1].get_value("frgk")

    @frgk.setter
    def frgk(self, value: float) -> None:
        self._cards[1].set_value("frgk", value)

    @property
    def frgs(self) -> typing.Optional[float]:
        """Get or set the Fracture energy release rate for shear deformation. (same as FRGS in Card 2 of keyword *DEFINE_DE_HBOND.).
        """ # nopep8
        return self._cards[1].get_value("frgs")

    @frgs.setter
    def frgs(self, value: float) -> None:
        self._cards[1].set_value("frgs", value)

    @property
    def dmg(self) -> float:
        """Get or set the Continuous damage development. (same as DMG in Card 2 of keyword *DEFINE_DE_HBOND.).
        """ # nopep8
        return self._cards[1].get_value("dmg")

    @dmg.setter
    def dmg(self, value: float) -> None:
        self._cards[1].set_value("dmg", value)

