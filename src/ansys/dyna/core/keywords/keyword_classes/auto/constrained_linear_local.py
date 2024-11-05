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

class ConstrainedLinearLocal(KeywordBase):
    """DYNA CONSTRAINED_LINEAR_LOCAL keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "LINEAR_LOCAL"

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
                        "nid",
                        int,
                        0,
                        10,
                        kwargs.get("nid")
                    ),
                    Field(
                        "dof",
                        int,
                        10,
                        10,
                        kwargs.get("dof")
                    ),
                    Field(
                        "cid",
                        int,
                        20,
                        10,
                        kwargs.get("cid")
                    ),
                    Field(
                        "coef",
                        float,
                        30,
                        10,
                        kwargs.get("coef")
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the ID for linear constraint definition
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node ID
        """ # nopep8
        return self._cards[1].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[1].set_value("nid", value)

    @property
    def dof(self) -> typing.Optional[int]:
        """Get or set the Degrees of freedom in the local coordinate system;
        EQ.1: displacement along local x-direction
        EQ.2: displacement along local y-direction
        EQ.3: displacement along local z-direction
        EQ.4: local rotation about local x-axis
        EQ.5: local rotation about local y-axis
        EQ.6: local rotation about local z-axis
        """ # nopep8
        return self._cards[1].get_value("dof")

    @dof.setter
    def dof(self, value: int) -> None:
        self._cards[1].set_value("dof", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Local coordinate system ID number. If the number is zero, the global coordinate system is used.
        """ # nopep8
        return self._cards[1].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[1].set_value("cid", value)

    @property
    def coef(self) -> typing.Optional[float]:
        """Get or set the Nonzero coefficient, Ck
        """ # nopep8
        return self._cards[1].get_value("coef")

    @coef.setter
    def coef(self, value: float) -> None:
        self._cards[1].set_value("coef", value)

