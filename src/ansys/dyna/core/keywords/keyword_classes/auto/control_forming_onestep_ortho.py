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

class ControlFormingOnestepOrtho(KeywordBase):
    """DYNA CONTROL_FORMING_ONESTEP_ORTHO keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_ONESTEP_ORTHO"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "node1",
                        int,
                        10,
                        10,
                        kwargs.get("node1")
                    ),
                    Field(
                        "node2",
                        int,
                        20,
                        10,
                        kwargs.get("node2")
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of the final formed blank mesh.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def node1(self) -> typing.Optional[int]:
        """Get or set the First node in the part to define the metal rolling direction.
        """ # nopep8
        return self._cards[0].get_value("node1")

    @node1.setter
    def node1(self, value: int) -> None:
        self._cards[0].set_value("node1", value)

    @property
    def node2(self) -> typing.Optional[int]:
        """Get or set the Second node in the part to define the metal rolling direction.
        """ # nopep8
        return self._cards[0].get_value("node2")

    @node2.setter
    def node2(self, value: int) -> None:
        self._cards[0].set_value("node2", value)
