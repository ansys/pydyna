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

class LoadSteadyStateRolling(KeywordBase):
    """DYNA LOAD_STEADY_STATE_ROLLING keyword"""

    keyword = "LOAD"
    subkeyword = "STEADY_STATE_ROLLING"

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
                    Field(
                        "psid",
                        int,
                        10,
                        10,
                        kwargs.get("psid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "n1",
                        int,
                        0,
                        10,
                        kwargs.get("n1")
                    ),
                    Field(
                        "n2",
                        int,
                        10,
                        10,
                        kwargs.get("n2")
                    ),
                    Field(
                        "lcd1",
                        int,
                        20,
                        10,
                        kwargs.get("lcd1")
                    ),
                    Field(
                        "lcd1r",
                        int,
                        30,
                        10,
                        kwargs.get("lcd1r")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "n3",
                        int,
                        0,
                        10,
                        kwargs.get("n3")
                    ),
                    Field(
                        "n4",
                        int,
                        10,
                        10,
                        kwargs.get("n4")
                    ),
                    Field(
                        "lcd2",
                        int,
                        20,
                        10,
                        kwargs.get("lcd2")
                    ),
                    Field(
                        "lcd2r",
                        int,
                        30,
                        10,
                        kwargs.get("lcd2r")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "n5",
                        int,
                        0,
                        10,
                        kwargs.get("n5")
                    ),
                    Field(
                        "n6",
                        int,
                        10,
                        10,
                        kwargs.get("n6")
                    ),
                    Field(
                        "lcd3",
                        int,
                        20,
                        10,
                        kwargs.get("lcd3")
                    ),
                    Field(
                        "lcd3r",
                        int,
                        30,
                        10,
                        kwargs.get("lcd3r")
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Load steady state rolling ID
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part Set ID
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        self._cards[0].set_value("psid", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Node 1 defining rotational axis
        """ # nopep8
        return self._cards[1].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        self._cards[1].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Node 2 defining rotational axis
        """ # nopep8
        return self._cards[1].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        self._cards[1].set_value("n2", value)

    @property
    def lcd1(self) -> typing.Optional[int]:
        """Get or set the Load curve defining angular velocity around rotational axis.
        """ # nopep8
        return self._cards[1].get_value("lcd1")

    @lcd1.setter
    def lcd1(self, value: int) -> None:
        self._cards[1].set_value("lcd1", value)

    @property
    def lcd1r(self) -> typing.Optional[int]:
        """Get or set the Optional load curve defining angular velocity around rotational axis for dynamic relaxation. LCD1 is used during dynamic relaxation if LCD1R isn’t defined.
        """ # nopep8
        return self._cards[1].get_value("lcd1r")

    @lcd1r.setter
    def lcd1r(self, value: int) -> None:
        self._cards[1].set_value("lcd1r", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Node 3 defining turning axis
        """ # nopep8
        return self._cards[2].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        self._cards[2].set_value("n3", value)

    @property
    def n4(self) -> typing.Optional[int]:
        """Get or set the Node 4 defining turning axis
        """ # nopep8
        return self._cards[2].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        self._cards[2].set_value("n4", value)

    @property
    def lcd2(self) -> typing.Optional[int]:
        """Get or set the Load curve defining angular velocity around turning axis.
        """ # nopep8
        return self._cards[2].get_value("lcd2")

    @lcd2.setter
    def lcd2(self, value: int) -> None:
        self._cards[2].set_value("lcd2", value)

    @property
    def lcd2r(self) -> typing.Optional[int]:
        """Get or set the Optional load curve defining angular velocity around turning axis for dynamic relaxation. LCD2 is used during dynamic relaxation if LCD2R isn’t defined
        """ # nopep8
        return self._cards[2].get_value("lcd2r")

    @lcd2r.setter
    def lcd2r(self, value: int) -> None:
        self._cards[2].set_value("lcd2r", value)

    @property
    def n5(self) -> typing.Optional[int]:
        """Get or set the Node 5 defining translational direction
        """ # nopep8
        return self._cards[3].get_value("n5")

    @n5.setter
    def n5(self, value: int) -> None:
        self._cards[3].set_value("n5", value)

    @property
    def n6(self) -> typing.Optional[int]:
        """Get or set the Node 6 defining translational direction
        """ # nopep8
        return self._cards[3].get_value("n6")

    @n6.setter
    def n6(self, value: int) -> None:
        self._cards[3].set_value("n6", value)

    @property
    def lcd3(self) -> typing.Optional[int]:
        """Get or set the Load curve defining translational velocity in translational direction.
        """ # nopep8
        return self._cards[3].get_value("lcd3")

    @lcd3.setter
    def lcd3(self, value: int) -> None:
        self._cards[3].set_value("lcd3", value)

    @property
    def lcd3r(self) -> typing.Optional[int]:
        """Get or set the Optional load curve defining translational velocity in translational direction. LCD3 is used during dynamic relaxation if LCD3R isn’t defined.
        """ # nopep8
        return self._cards[3].get_value("lcd3r")

    @lcd3r.setter
    def lcd3r(self, value: int) -> None:
        self._cards[3].set_value("lcd3r", value)

