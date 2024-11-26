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

class NodeNodes(KeywordBase):
    """DYNA NODE_NODES keyword"""

    keyword = "NODE"
    subkeyword = "NODES"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nid",
                        int,
                        0,
                        8,
                        kwargs.get("nid")
                    ),
                    Field(
                        "x",
                        float,
                        8,
                        16,
                        kwargs.get("x", 0.0)
                    ),
                    Field(
                        "y",
                        float,
                        24,
                        16,
                        kwargs.get("y", 0.0)
                    ),
                    Field(
                        "z",
                        float,
                        40,
                        16,
                        kwargs.get("z", 0.0)
                    ),
                    Field(
                        "tc",
                        int,
                        56,
                        8,
                        kwargs.get("tc", 0)
                    ),
                    Field(
                        "rc",
                        int,
                        64,
                        8,
                        kwargs.get("rc", 0)
                    ),
                ],
            ),
        ]

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node ID.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[0].set_value("nid", value)

    @property
    def x(self) -> float:
        """Get or set the x-coordinate.
        """ # nopep8
        return self._cards[0].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        self._cards[0].set_value("x", value)

    @property
    def y(self) -> float:
        """Get or set the y-coordinate.
        """ # nopep8
        return self._cards[0].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        self._cards[0].set_value("y", value)

    @property
    def z(self) -> float:
        """Get or set the z-coordinate.
        """ # nopep8
        return self._cards[0].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        self._cards[0].set_value("z", value)

    @property
    def tc(self) -> int:
        """Get or set the Translational constraint:
        EQ.0: no constraints,
        EQ.1: constrained x displacement,
        EQ.2: constrained y displacement,
        EQ.3: constrained z displacement,
        EQ.4: constrained x and y displacements,
        EQ.5: constrained y and z displacements,
        EQ.6: constrained z and x displacements,
        EQ.7: constrained x, y, and z displacements.
        """ # nopep8
        return self._cards[0].get_value("tc")

    @tc.setter
    def tc(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6, 7]:
            raise Exception("""tc must be one of {0,1,2,3,4,5,6,7}""")
        self._cards[0].set_value("tc", value)

    @property
    def rc(self) -> int:
        """Get or set the Rotational constraint:
        EQ.0: no constraints,
        EQ.1: constrained x rotation,
        EQ.2: constrained y rotation,
        EQ.3: constrained z rotation,
        EQ.4: constrained x and y rotations,
        EQ.5: constrained y and z rotations,
        EQ.6: constrained z and x rotations,
        EQ.7: constrained x, y, and z rotations.
        """ # nopep8
        return self._cards[0].get_value("rc")

    @rc.setter
    def rc(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6, 7]:
            raise Exception("""rc must be one of {0,1,2,3,4,5,6,7}""")
        self._cards[0].set_value("rc", value)

