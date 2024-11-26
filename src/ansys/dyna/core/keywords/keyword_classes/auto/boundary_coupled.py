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

class BoundaryCoupled(KeywordBase):
    """DYNA BOUNDARY_COUPLED keyword"""

    keyword = "BOUNDARY"
    subkeyword = "COUPLED"

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
                        "title",
                        str,
                        10,
                        70,
                        kwargs.get("title")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "set",
                        int,
                        0,
                        10,
                        kwargs.get("set")
                    ),
                    Field(
                        "type",
                        int,
                        10,
                        10,
                        kwargs.get("type", 1)
                    ),
                    Field(
                        "prog",
                        int,
                        20,
                        10,
                        kwargs.get("prog")
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the ID for this coupled boundary.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Descriptive name for this boundary.
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[0].set_value("title", value)

    @property
    def set(self) -> typing.Optional[int]:
        """Get or set the Node set ID.
        """ # nopep8
        return self._cards[1].get_value("set")

    @set.setter
    def set(self, value: int) -> None:
        self._cards[1].set_value("set", value)

    @property
    def type(self) -> int:
        """Get or set the Coupling type:
        EQ.1: node set with force feedback
        EQ.2: node set for multiscale spotwelds.
        """ # nopep8
        return self._cards[1].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""type must be one of {1,2}""")
        self._cards[1].set_value("type", value)

    @property
    def prog(self) -> typing.Optional[int]:
        """Get or set the Program to couple to EQ.1: MPP-DYNA.
        """ # nopep8
        return self._cards[1].get_value("prog")

    @prog.setter
    def prog(self, value: int) -> None:
        self._cards[1].set_value("prog", value)

