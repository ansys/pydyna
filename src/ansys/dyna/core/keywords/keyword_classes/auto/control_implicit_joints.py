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

class ControlImplicitJoints(KeywordBase):
    """DYNA CONTROL_IMPLICIT_JOINTS keyword"""

    keyword = "CONTROL"
    subkeyword = "IMPLICIT_JOINTS"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ispher",
                        int,
                        0,
                        10,
                        kwargs.get("ispher", 1)
                    ),
                    Field(
                        "irevol",
                        int,
                        10,
                        10,
                        kwargs.get("irevol", 1)
                    ),
                    Field(
                        "icylin",
                        int,
                        20,
                        10,
                        kwargs.get("icylin", 1)
                    ),
                ],
            ),
        ]

    @property
    def ispher(self) -> int:
        """Get or set the Treatment of spherical joints
        EQ.1: use constraint method for all spherical joints (default)
        EQ.2: use penalty method for all spherical joints
        """ # nopep8
        return self._cards[0].get_value("ispher")

    @ispher.setter
    def ispher(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""ispher must be one of {1,2}""")
        self._cards[0].set_value("ispher", value)

    @property
    def irevol(self) -> int:
        """Get or set the Treatment of revolute joints
        EQ.1: use constraint method for all revolute joints (default)
        EQ.2: use penalty method for all revolute joints
        """ # nopep8
        return self._cards[0].get_value("irevol")

    @irevol.setter
    def irevol(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""irevol must be one of {1,2}""")
        self._cards[0].set_value("irevol", value)

    @property
    def icylin(self) -> int:
        """Get or set the Treatment of cylindrical joints.
        EQ.1: use constraint method for all cylindrical joints (default)
        EQ.2: use penalty method for all cylindrical joints
        """ # nopep8
        return self._cards[0].get_value("icylin")

    @icylin.setter
    def icylin(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""icylin must be one of {1,2}""")
        self._cards[0].set_value("icylin", value)

