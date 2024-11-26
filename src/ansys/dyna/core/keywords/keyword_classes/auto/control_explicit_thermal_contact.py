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

class ControlExplicitThermalContact(KeywordBase):
    """DYNA CONTROL_EXPLICIT_THERMAL_CONTACT keyword"""

    keyword = "CONTROL"
    subkeyword = "EXPLICIT_THERMAL_CONTACT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "partset",
                        int,
                        0,
                        10,
                        kwargs.get("partset")
                    ),
                    Field(
                        "ncycle",
                        int,
                        10,
                        10,
                        kwargs.get("ncycle")
                    ),
                ],
            ),
        ]

    @property
    def partset(self) -> typing.Optional[int]:
        """Get or set the Part set ID (See *SET_PART).
        """ # nopep8
        return self._cards[0].get_value("partset")

    @partset.setter
    def partset(self, value: int) -> None:
        self._cards[0].set_value("partset", value)

    @property
    def ncycle(self) -> typing.Optional[int]:
        """Get or set the Number of cycle between checks of new contact.
        """ # nopep8
        return self._cards[0].get_value("ncycle")

    @ncycle.setter
    def ncycle(self, value: int) -> None:
        self._cards[0].set_value("ncycle", value)

