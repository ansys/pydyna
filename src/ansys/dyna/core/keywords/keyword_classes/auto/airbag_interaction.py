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

class AirbagInteraction(KeywordBase):
    """DYNA AIRBAG_INTERACTION keyword"""

    keyword = "AIRBAG"
    subkeyword = "INTERACTION"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ab1",
                        int,
                        0,
                        10,
                        kwargs.get("ab1")
                    ),
                    Field(
                        "ab2",
                        int,
                        10,
                        10,
                        kwargs.get("ab2")
                    ),
                    Field(
                        "area",
                        float,
                        20,
                        10,
                        kwargs.get("area")
                    ),
                    Field(
                        "sf",
                        float,
                        30,
                        10,
                        kwargs.get("sf")
                    ),
                    Field(
                        "pid",
                        int,
                        40,
                        10,
                        kwargs.get("pid", 0)
                    ),
                    Field(
                        "lcid",
                        int,
                        50,
                        10,
                        kwargs.get("lcid", 0)
                    ),
                    Field(
                        "iflow",
                        int,
                        60,
                        10,
                        kwargs.get("iflow", 0)
                    ),
                ],
            ),
        ]

    @property
    def ab1(self) -> typing.Optional[int]:
        """Get or set the First airbag ID, as defined on *AIRBAG card.
        """ # nopep8
        return self._cards[0].get_value("ab1")

    @ab1.setter
    def ab1(self, value: int) -> None:
        self._cards[0].set_value("ab1", value)

    @property
    def ab2(self) -> typing.Optional[int]:
        """Get or set the Second airbag ID, as defined on *AIRBAG card.
        """ # nopep8
        return self._cards[0].get_value("ab2")

    @ab2.setter
    def ab2(self, value: int) -> None:
        self._cards[0].set_value("ab2", value)

    @property
    def area(self) -> typing.Optional[float]:
        """Get or set the Orifice area between connected bags.
        LT.0.0: |AREA| is the load curve ID defining the orifice area as a function of absolute pressure,
        EQ.0.0: AREA is taken as the surface area of the part ID defined below.
        """ # nopep8
        return self._cards[0].get_value("area")

    @area.setter
    def area(self, value: float) -> None:
        self._cards[0].set_value("area", value)

    @property
    def sf(self) -> typing.Optional[float]:
        """Get or set the Shape factor.
        LT.0.0: |SF| is the load curve ID defining vent orifice coefficient as a function of relative time.
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        self._cards[0].set_value("sf", value)

    @property
    def pid(self) -> int:
        """Get or set the Optional part ID of the partition between the interacting control volumes. AREA is based on this part ID.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def lcid(self) -> int:
        """Get or set the Load curve ID defining mass flow rate versus pressure difference, see *DEFINE_CURVE. If LCID is defined AREA, SF and PID are ignored.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def iflow(self) -> int:
        """Get or set the Flow direction.
        LT.0: One-way flow from AB1 to AB2 only,
        EQ.0: Two-way flow between AB1 and AB2,
        GT.0: One-way flow from AB2 to AB1 only.
        """ # nopep8
        return self._cards[0].get_value("iflow")

    @iflow.setter
    def iflow(self, value: int) -> None:
        self._cards[0].set_value("iflow", value)

