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

class ControlFormingTrimMerge(KeywordBase):
    """DYNA CONTROL_FORMING_TRIM_MERGE keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_TRIM_MERGE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "imerge",
                        int,
                        0,
                        10,
                        kwargs.get("imerge", 1)
                    ),
                    Field(
                        "gapm",
                        float,
                        10,
                        10,
                        kwargs.get("gapm", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def imerge(self) -> int:
        """Get or set the Activation flag.  Set to '1' (default) to activate this feature.
        """ # nopep8
        return self._cards[0].get_value("imerge")

    @imerge.setter
    def imerge(self, value: int) -> None:
        self._cards[0].set_value("imerge", value)

    @property
    def gapm(self) -> float:
        """Get or set the Gap distance between two open ends of a trim loop curve in the model.
        If the gap is smaller than GAPM, the two open ends of a trim curve will be closed automatically.
        """ # nopep8
        return self._cards[0].get_value("gapm")

    @gapm.setter
    def gapm(self, value: float) -> None:
        self._cards[0].set_value("gapm", value)

