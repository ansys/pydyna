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

class ChangeRigidBodyInertia(KeywordBase):
    """DYNA CHANGE_RIGID_BODY_INERTIA keyword"""

    keyword = "CHANGE"
    subkeyword = "RIGID_BODY_INERTIA"

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
                        "pid",
                        int,
                        10,
                        10,
                        kwargs.get("pid", 0)
                    ),
                    Field(
                        "tm",
                        float,
                        20,
                        10,
                        kwargs.get("tm", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ixx",
                        float,
                        0,
                        10,
                        kwargs.get("ixx")
                    ),
                    Field(
                        "ixy",
                        float,
                        10,
                        10,
                        kwargs.get("ixy", 0)
                    ),
                    Field(
                        "ixz",
                        float,
                        20,
                        10,
                        kwargs.get("ixz", 0)
                    ),
                    Field(
                        "iyy",
                        float,
                        30,
                        10,
                        kwargs.get("iyy", 0)
                    ),
                    Field(
                        "iyz",
                        float,
                        40,
                        10,
                        kwargs.get("iyz", 0)
                    ),
                    Field(
                        "izz",
                        float,
                        50,
                        10,
                        kwargs.get("izz", 0)
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the ID for this change inertia input.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def pid(self) -> int:
        """Get or set the Part ID, see *PART.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def tm(self) -> float:
        """Get or set the Translational mass.
        """ # nopep8
        return self._cards[0].get_value("tm")

    @tm.setter
    def tm(self, value: float) -> None:
        self._cards[0].set_value("tm", value)

    @property
    def ixx(self) -> typing.Optional[float]:
        """Get or set the Ixx, xx component of inertia tensor.
        """ # nopep8
        return self._cards[1].get_value("ixx")

    @ixx.setter
    def ixx(self, value: float) -> None:
        self._cards[1].set_value("ixx", value)

    @property
    def ixy(self) -> float:
        """Get or set the Ixy.
        """ # nopep8
        return self._cards[1].get_value("ixy")

    @ixy.setter
    def ixy(self, value: float) -> None:
        self._cards[1].set_value("ixy", value)

    @property
    def ixz(self) -> float:
        """Get or set the Ixy.
        """ # nopep8
        return self._cards[1].get_value("ixz")

    @ixz.setter
    def ixz(self, value: float) -> None:
        self._cards[1].set_value("ixz", value)

    @property
    def iyy(self) -> float:
        """Get or set the Iyy, yy component of inertia tensor.
        """ # nopep8
        return self._cards[1].get_value("iyy")

    @iyy.setter
    def iyy(self, value: float) -> None:
        self._cards[1].set_value("iyy", value)

    @property
    def iyz(self) -> float:
        """Get or set the Iyz.
        """ # nopep8
        return self._cards[1].get_value("iyz")

    @iyz.setter
    def iyz(self, value: float) -> None:
        self._cards[1].set_value("iyz", value)

    @property
    def izz(self) -> float:
        """Get or set the Izz, zz component of inertia tensor.
        """ # nopep8
        return self._cards[1].get_value("izz")

    @izz.setter
    def izz(self, value: float) -> None:
        self._cards[1].set_value("izz", value)

