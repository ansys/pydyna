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

class ConstrainedMultipleGlobal(KeywordBase):
    """DYNA CONSTRAINED_MULTIPLE_GLOBAL keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "MULTIPLE_GLOBAL"

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
                        "nmp",
                        int,
                        0,
                        10,
                        kwargs.get("nmp")
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
                        "dir",
                        int,
                        10,
                        10,
                        kwargs.get("dir", 1)
                    ),
                    Field(
                        "coef",
                        float,
                        20,
                        10,
                        kwargs.get("coef")
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Constraint set identification. All constraint sets should have a unique set ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def nmp(self) -> typing.Optional[int]:
        """Get or set the Number of nodes to be constrained mutually.
        """ # nopep8
        return self._cards[1].get_value("nmp")

    @nmp.setter
    def nmp(self, value: int) -> None:
        self._cards[1].set_value("nmp", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Nodal ID.
        """ # nopep8
        return self._cards[2].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[2].set_value("nid", value)

    @property
    def dir(self) -> int:
        """Get or set the Direction in three-dimensional space to be constrained
        EQ.1: x direction
        EQ.2: y direction
        EQ.3: z direction.
        """ # nopep8
        return self._cards[2].get_value("dir")

    @dir.setter
    def dir(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""dir must be one of {1,2,3}""")
        self._cards[2].set_value("dir", value)

    @property
    def coef(self) -> typing.Optional[float]:
        """Get or set the Coefficient ¦Ánid in constraint equation.
        """ # nopep8
        return self._cards[2].get_value("coef")

    @coef.setter
    def coef(self, value: float) -> None:
        self._cards[2].set_value("coef", value)

