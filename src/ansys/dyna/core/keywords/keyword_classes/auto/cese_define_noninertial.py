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

class CeseDefineNoninertial(KeywordBase):
    """DYNA CESE_DEFINE_NONINERTIAL keyword"""

    keyword = "CESE"
    subkeyword = "DEFINE_NONINERTIAL"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "freq",
                        float,
                        0,
                        10,
                        kwargs.get("freq")
                    ),
                    Field(
                        "lcid",
                        int,
                        10,
                        10,
                        kwargs.get("lcid", 0)
                    ),
                    Field(
                        "pid",
                        int,
                        20,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "nx",
                        float,
                        30,
                        10,
                        kwargs.get("nx")
                    ),
                    Field(
                        "ny",
                        float,
                        40,
                        10,
                        kwargs.get("ny")
                    ),
                    Field(
                        "nz",
                        float,
                        50,
                        10,
                        kwargs.get("nz")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "l",
                        float,
                        0,
                        10,
                        kwargs.get("l")
                    ),
                    Field(
                        "r",
                        float,
                        10,
                        10,
                        kwargs.get("r")
                    ),
                    Field(
                        "relv",
                        int,
                        20,
                        10,
                        kwargs.get("relv", 0)
                    ),
                ],
            ),
        ]

    @property
    def freq(self) -> typing.Optional[float]:
        """Get or set the Frequency of rotation.
        """ # nopep8
        return self._cards[0].get_value("freq")

    @freq.setter
    def freq(self, value: float) -> None:
        self._cards[0].set_value("freq", value)

    @property
    def lcid(self) -> int:
        """Get or set the Load curve ID for scaling factor of FREQ.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Starting point ID for the reference frame (See *CESE_DEFINE_POINT).
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def nx(self) -> typing.Optional[float]:
        """Get or set the Rotating axis direction.
        """ # nopep8
        return self._cards[0].get_value("nx")

    @nx.setter
    def nx(self, value: float) -> None:
        self._cards[0].set_value("nx", value)

    @property
    def ny(self) -> typing.Optional[float]:
        """Get or set the Rotating axis direction.
        """ # nopep8
        return self._cards[0].get_value("ny")

    @ny.setter
    def ny(self, value: float) -> None:
        self._cards[0].set_value("ny", value)

    @property
    def nz(self) -> typing.Optional[float]:
        """Get or set the Rotating axis direction.
        """ # nopep8
        return self._cards[0].get_value("nz")

    @nz.setter
    def nz(self, value: float) -> None:
        self._cards[0].set_value("nz", value)

    @property
    def l(self) -> typing.Optional[float]:
        """Get or set the Length of rotating frame.
        """ # nopep8
        return self._cards[1].get_value("l")

    @l.setter
    def l(self, value: float) -> None:
        self._cards[1].set_value("l", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Radius of rotating frame.
        """ # nopep8
        return self._cards[1].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        self._cards[1].set_value("r", value)

    @property
    def relv(self) -> int:
        """Get or set the Velocity display mode:
        EQ.0: Relative velocity, only the non-rotating components of the velocity are output.
        EQ.1: Absolute velocity is output.
        """ # nopep8
        return self._cards[1].get_value("relv")

    @relv.setter
    def relv(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""relv must be one of {0,1}""")
        self._cards[1].set_value("relv", value)

