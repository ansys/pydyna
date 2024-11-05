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

class ElementShellSourceSink(KeywordBase):
    """DYNA ELEMENT_SHELL_SOURCE_SINK keyword"""

    keyword = "ELEMENT"
    subkeyword = "SHELL_SOURCE_SINK"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nssr",
                        int,
                        0,
                        10,
                        kwargs.get("nssr")
                    ),
                    Field(
                        "nssk",
                        int,
                        10,
                        10,
                        kwargs.get("nssk")
                    ),
                    Field(
                        "pid",
                        int,
                        20,
                        10,
                        kwargs.get("pid")
                    ),
                ],
            ),
        ]

    @property
    def nssr(self) -> typing.Optional[int]:
        """Get or set the Node set at source. Provide an ordered set of nodes between corner nodes, which include the corner nodes
        """ # nopep8
        return self._cards[0].get_value("nssr")

    @nssr.setter
    def nssr(self, value: int) -> None:
        self._cards[0].set_value("nssr", value)

    @property
    def nssk(self) -> typing.Optional[int]:
        """Get or set the Node set at sink. Provide an ordered set of nodes between corner nodes, which include the corner nodes
        """ # nopep8
        return self._cards[0].get_value("nssk")

    @nssk.setter
    def nssk(self, value: int) -> None:
        self._cards[0].set_value("nssk", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of work piece.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

