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

class ControlFormingPosition(KeywordBase):
    """DYNA CONTROL_FORMING_POSITION keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_POSITION"

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
                        "premove",
                        float,
                        10,
                        10,
                        kwargs.get("premove")
                    ),
                    Field(
                        "target",
                        int,
                        20,
                        10,
                        kwargs.get("target")
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def premove(self) -> typing.Optional[float]:
        """Get or set the The distance to pre-move the tool in the reverse direction of this tool's movement
        """ # nopep8
        return self._cards[0].get_value("premove")

    @premove.setter
    def premove(self, value: float) -> None:
        self._cards[0].set_value("premove", value)

    @property
    def target(self) -> typing.Optional[int]:
        """Get or set the Move part (PID) in the reverse direction of this tool movement, and make sure the minimum distance between PID and TARGET is defined by GAP
        """ # nopep8
        return self._cards[0].get_value("target")

    @target.setter
    def target(self, value: int) -> None:
        self._cards[0].set_value("target", value)

