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

"""Module providing the ControlCoupling class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class ControlCoupling(KeywordBase):
    """DYNA CONTROL_COUPLING keyword"""

    keyword = "CONTROL"
    subkeyword = "COUPLING"

    def __init__(self, **kwargs):
        """Initialize the ControlCoupling class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "unleng",
                        float,
                        0,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "untime",
                        float,
                        10,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "unforc",
                        float,
                        20,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "timidl",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "flipx",
                        int,
                        40,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "flipy",
                        int,
                        50,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "flipz",
                        int,
                        60,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "subcyl",
                        int,
                        70,
                        10,
                        1,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def unleng(self) -> float:
        """Get or set the Unit conversion factor for length.
        """ # nopep8
        return self._cards[0].get_value("unleng")

    @unleng.setter
    def unleng(self, value: float) -> None:
        """Set the unleng property."""
        self._cards[0].set_value("unleng", value)

    @property
    def untime(self) -> float:
        """Get or set the Unit conversion factor for time.
        """ # nopep8
        return self._cards[0].get_value("untime")

    @untime.setter
    def untime(self, value: float) -> None:
        """Set the untime property."""
        self._cards[0].set_value("untime", value)

    @property
    def unforc(self) -> float:
        """Get or set the Unit conversion factor for force.
        """ # nopep8
        return self._cards[0].get_value("unforc")

    @unforc.setter
    def unforc(self, value: float) -> None:
        """Set the unforc property."""
        self._cards[0].set_value("unforc", value)

    @property
    def timidl(self) -> float:
        """Get or set the Idle time during which CAL3D or MADYMO is computing and LS-DYNA3D remains inactive.
        """ # nopep8
        return self._cards[0].get_value("timidl")

    @timidl.setter
    def timidl(self, value: float) -> None:
        """Set the timidl property."""
        self._cards[0].set_value("timidl", value)

    @property
    def flipx(self) -> int:
        """Get or set the Flag for flipping X-coordinate of CAL3D/MADYMO3D relative to the LS-DYNA3D model:
        EQ.0: off,
        EQ.1: on.
        """ # nopep8
        return self._cards[0].get_value("flipx")

    @flipx.setter
    def flipx(self, value: int) -> None:
        """Set the flipx property."""
        if value not in [0, 1, None]:
            raise Exception("""flipx must be `None` or one of {0,1}.""")
        self._cards[0].set_value("flipx", value)

    @property
    def flipy(self) -> int:
        """Get or set the Flag for flipping Y-coordinate of CAL3D/MADYMO3D relative to the LS-DYNA3D model:
        EQ.0: off,
        EQ.1: on.
        """ # nopep8
        return self._cards[0].get_value("flipy")

    @flipy.setter
    def flipy(self, value: int) -> None:
        """Set the flipy property."""
        if value not in [0, 1, None]:
            raise Exception("""flipy must be `None` or one of {0,1}.""")
        self._cards[0].set_value("flipy", value)

    @property
    def flipz(self) -> int:
        """Get or set the Flag for flipping Z-coordinate of CAL3D/MADYMO3D relative to the LS-DYNA3D model:
        EQ.0: off,
        EQ.1: on.
        """ # nopep8
        return self._cards[0].get_value("flipz")

    @flipz.setter
    def flipz(self, value: int) -> None:
        """Set the flipz property."""
        if value not in [0, 1, None]:
            raise Exception("""flipz must be `None` or one of {0,1}.""")
        self._cards[0].set_value("flipz", value)

    @property
    def subcyl(self) -> int:
        """Get or set the CAL3D/MADYMO3D subcycling interval (# of cycles) (default =1):
        EQ.n: number of LS-DYNA time steps between each CAL3D/MADYMO3D step.
        """ # nopep8
        return self._cards[0].get_value("subcyl")

    @subcyl.setter
    def subcyl(self, value: int) -> None:
        """Set the subcyl property."""
        self._cards[0].set_value("subcyl", value)

