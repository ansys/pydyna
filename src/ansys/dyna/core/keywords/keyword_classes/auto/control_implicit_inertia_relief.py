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

class ControlImplicitInertiaRelief(KeywordBase):
    """DYNA CONTROL_IMPLICIT_INERTIA_RELIEF keyword"""

    keyword = "CONTROL"
    subkeyword = "IMPLICIT_INERTIA_RELIEF"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "irflag",
                        int,
                        0,
                        10,
                        kwargs.get("irflag", 0)
                    ),
                    Field(
                        "thresh",
                        float,
                        10,
                        10,
                        kwargs.get("thresh", 0.001)
                    ),
                    Field(
                        "ircnt",
                        int,
                        20,
                        10,
                        kwargs.get("ircnt", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "mode1",
                        int,
                        0,
                        10,
                        kwargs.get("mode1")
                    ),
                    Field(
                        "mode2",
                        int,
                        10,
                        10,
                        kwargs.get("mode2")
                    ),
                    Field(
                        "mode3",
                        int,
                        20,
                        10,
                        kwargs.get("mode3")
                    ),
                    Field(
                        "mode1",
                        int,
                        30,
                        10,
                        kwargs.get("mode1")
                    ),
                    Field(
                        "mode1",
                        int,
                        40,
                        10,
                        kwargs.get("mode1")
                    ),
                    Field(
                        "mode1",
                        int,
                        50,
                        10,
                        kwargs.get("mode1")
                    ),
                    Field(
                        "mode1",
                        int,
                        60,
                        10,
                        kwargs.get("mode1")
                    ),
                    Field(
                        "mode1",
                        int,
                        70,
                        10,
                        kwargs.get("mode1")
                    ),
                ],
            ),
        ]

    @property
    def irflag(self) -> int:
        """Get or set the Inertia relief flag
        EQ.0: do not perform inertia relief.
        EQ 1: do perform inertia relief and use for both implicit and explicit
        EQ.2:	do perform inertia relief but only use for implicit time steps
        """ # nopep8
        return self._cards[0].get_value("irflag")

    @irflag.setter
    def irflag(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""irflag must be one of {0,1,2}""")
        self._cards[0].set_value("irflag", value)

    @property
    def thresh(self) -> float:
        """Get or set the Threshold for what is a rigid body mode. the default is set to 0.001 hertz where it is assumed that the units are in seconds
        """ # nopep8
        return self._cards[0].get_value("thresh")

    @thresh.setter
    def thresh(self, value: float) -> None:
        self._cards[0].set_value("thresh", value)

    @property
    def ircnt(self) -> int:
        """Get or set the The user can specify to use the lowest IRCNT modes instead of using THRESH to determine the number of modes.
        """ # nopep8
        return self._cards[0].get_value("ircnt")

    @ircnt.setter
    def ircnt(self, value: int) -> None:
        self._cards[0].set_value("ircnt", value)

    @property
    def mode1(self) -> typing.Optional[int]:
        """Get or set the Ignore THRESH and IRCNT and use a specific list of modes, skipping those that should not be used
        """ # nopep8
        return self._cards[1].get_value("mode1")

    @mode1.setter
    def mode1(self, value: int) -> None:
        self._cards[1].set_value("mode1", value)

    @property
    def mode2(self) -> typing.Optional[int]:
        """Get or set the Ignore THRESH and IRCNT and use a specific list of modes, skipping those that should not be used
        """ # nopep8
        return self._cards[1].get_value("mode2")

    @mode2.setter
    def mode2(self, value: int) -> None:
        self._cards[1].set_value("mode2", value)

    @property
    def mode3(self) -> typing.Optional[int]:
        """Get or set the Ignore THRESH and IRCNT and use a specific list of modes, skipping those that should not be used.
        """ # nopep8
        return self._cards[1].get_value("mode3")

    @mode3.setter
    def mode3(self, value: int) -> None:
        self._cards[1].set_value("mode3", value)

    @property
    def mode1(self) -> typing.Optional[int]:
        """Get or set the Ignore THRESH and IRCNT and use a specific list of modes, skipping those that should not be used
        """ # nopep8
        return self._cards[1].get_value("mode1")

    @mode1.setter
    def mode1(self, value: int) -> None:
        self._cards[1].set_value("mode1", value)

    @property
    def mode1(self) -> typing.Optional[int]:
        """Get or set the Ignore THRESH and IRCNT and use a specific list of modes, skipping those that should not be used
        """ # nopep8
        return self._cards[1].get_value("mode1")

    @mode1.setter
    def mode1(self, value: int) -> None:
        self._cards[1].set_value("mode1", value)

    @property
    def mode1(self) -> typing.Optional[int]:
        """Get or set the Ignore THRESH and IRCNT and use a specific list of modes, skipping those that should not be used
        """ # nopep8
        return self._cards[1].get_value("mode1")

    @mode1.setter
    def mode1(self, value: int) -> None:
        self._cards[1].set_value("mode1", value)

    @property
    def mode1(self) -> typing.Optional[int]:
        """Get or set the Ignore THRESH and IRCNT and use a specific list of modes, skipping those that should not be used
        """ # nopep8
        return self._cards[1].get_value("mode1")

    @mode1.setter
    def mode1(self, value: int) -> None:
        self._cards[1].set_value("mode1", value)

    @property
    def mode1(self) -> typing.Optional[int]:
        """Get or set the Ignore THRESH and IRCNT and use a specific list of modes, skipping those that should not be used
        """ # nopep8
        return self._cards[1].get_value("mode1")

    @mode1.setter
    def mode1(self, value: int) -> None:
        self._cards[1].set_value("mode1", value)

