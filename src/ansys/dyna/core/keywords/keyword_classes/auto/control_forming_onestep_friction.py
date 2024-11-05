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

class ControlFormingOnestepFriction(KeywordBase):
    """DYNA CONTROL_FORMING_ONESTEP_FRICTION keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_ONESTEP_FRICTION"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ndset",
                        int,
                        0,
                        10,
                        kwargs.get("ndset")
                    ),
                    Field(
                        "bdton",
                        float,
                        10,
                        10,
                        kwargs.get("bdton", 0.0)
                    ),
                    Field(
                        "frict",
                        float,
                        20,
                        10,
                        kwargs.get("frict", 0.12)
                    ),
                ],
            ),
        ]

    @property
    def ndset(self) -> typing.Optional[int]:
        """Get or set the Node set ID along the periphery of the part, as defined by keyword *SET_NODE_LIST.
        """ # nopep8
        return self._cards[0].get_value("ndset")

    @ndset.setter
    def ndset(self, value: int) -> None:
        self._cards[0].set_value("ndset", value)

    @property
    def bdton(self) -> float:
        """Get or set the Binder tonnage used to calculate friction force.
        """ # nopep8
        return self._cards[0].get_value("bdton")

    @bdton.setter
    def bdton(self, value: float) -> None:
        self._cards[0].set_value("bdton", value)

    @property
    def frict(self) -> float:
        """Get or set the Coefficient of friction.
        """ # nopep8
        return self._cards[0].get_value("frict")

    @frict.setter
    def frict(self, value: float) -> None:
        self._cards[0].set_value("frict", value)

