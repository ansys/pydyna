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

class ControlFormingStoning(KeywordBase):
    """DYNA CONTROL_FORMING_STONING keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_STONING"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "istone",
                        int,
                        0,
                        10,
                        kwargs.get("istone")
                    ),
                    Field(
                        "length",
                        float,
                        10,
                        10,
                        kwargs.get("length")
                    ),
                    Field(
                        "width",
                        int,
                        20,
                        10,
                        kwargs.get("width")
                    ),
                    Field(
                        "step",
                        float,
                        30,
                        10,
                        kwargs.get("step", 0.5)
                    ),
                    Field(
                        "direction",
                        int,
                        40,
                        10,
                        kwargs.get("direction")
                    ),
                    Field(
                        "reverse",
                        int,
                        50,
                        10,
                        kwargs.get("reverse", 0)
                    ),
                    Field(
                        "method",
                        int,
                        60,
                        10,
                        kwargs.get("method", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "node1",
                        int,
                        0,
                        10,
                        kwargs.get("node1")
                    ),
                    Field(
                        "node2",
                        int,
                        10,
                        10,
                        kwargs.get("node2")
                    ),
                    Field(
                        "setid",
                        int,
                        20,
                        10,
                        kwargs.get("setid")
                    ),
                    Field(
                        "itype",
                        int,
                        30,
                        10,
                        kwargs.get("itype", 1)
                    ),
                ],
            ),
        ]

    @property
    def istone(self) -> typing.Optional[int]:
        """Get or set the activate this capability.
        """ # nopep8
        return self._cards[0].get_value("istone")

    @istone.setter
    def istone(self, value: int) -> None:
        self._cards[0].set_value("istone", value)

    @property
    def length(self) -> typing.Optional[float]:
        """Get or set the the length of the stone.
        """ # nopep8
        return self._cards[0].get_value("length")

    @length.setter
    def length(self, value: float) -> None:
        self._cards[0].set_value("length", value)

    @property
    def width(self) -> typing.Optional[int]:
        """Get or set the Width of the stone
        """ # nopep8
        return self._cards[0].get_value("width")

    @width.setter
    def width(self, value: int) -> None:
        self._cards[0].set_value("width", value)

    @property
    def step(self) -> float:
        """Get or set the The step size for the stone to move in each step
        """ # nopep8
        return self._cards[0].get_value("step")

    @step.setter
    def step(self, value: float) -> None:
        self._cards[0].set_value("step", value)

    @property
    def direction(self) -> typing.Optional[int]:
        """Get or set the Number of automatically determined stoning direction(s).
        """ # nopep8
        return self._cards[0].get_value("direction")

    @direction.setter
    def direction(self, value: int) -> None:
        self._cards[0].set_value("direction", value)

    @property
    def reverse(self) -> int:
        """Get or set the Surface normal reversing option.
        EQ.0: do not reverse surface normals.
        EQ.1: reverse surface normals
        """ # nopep8
        return self._cards[0].get_value("reverse")

    @reverse.setter
    def reverse(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""reverse must be one of {0,1}""")
        self._cards[0].set_value("reverse", value)

    @property
    def method(self) -> int:
        """Get or set the Stoning method.
        EQ.0: curvature-based method.
        """ # nopep8
        return self._cards[0].get_value("method")

    @method.setter
    def method(self, value: int) -> None:
        self._cards[0].set_value("method", value)

    @property
    def node1(self) -> typing.Optional[int]:
        """Get or set the Node1 and Node2 define the orientation of the stone
        """ # nopep8
        return self._cards[1].get_value("node1")

    @node1.setter
    def node1(self, value: int) -> None:
        self._cards[1].set_value("node1", value)

    @property
    def node2(self) -> typing.Optional[int]:
        """Get or set the Node1 and Node2 define the orientation of the stone.
        """ # nopep8
        return self._cards[1].get_value("node2")

    @node2.setter
    def node2(self, value: int) -> None:
        self._cards[1].set_value("node2", value)

    @property
    def setid(self) -> typing.Optional[int]:
        """Get or set the id, itype 1: node set, itype 2: shell set
        """ # nopep8
        return self._cards[1].get_value("setid")

    @setid.setter
    def setid(self, value: int) -> None:
        self._cards[1].set_value("setid", value)

    @property
    def itype(self) -> int:
        """Get or set the 1:node set, 2 shell set
        """ # nopep8
        return self._cards[1].get_value("itype")

    @itype.setter
    def itype(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""itype must be one of {1,2}""")
        self._cards[1].set_value("itype", value)

