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

class ControlFormingTrimSolidRefinement(KeywordBase):
    """DYNA CONTROL_FORMING_TRIM_SOLID_REFINEMENT keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_TRIM_SOLID_REFINEMENT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "irefine",
                        int,
                        0,
                        10,
                        kwargs.get("irefine", 1)
                    ),
                    Field(
                        "ilevel",
                        int,
                        10,
                        10,
                        kwargs.get("ilevel", 0)
                    ),
                ],
            ),
        ]

    @property
    def irefine(self) -> int:
        """Get or set the A flag to activate the adaptive trimming of a multi-layer sandwiched part. Currently setting this to either 0 or 1 will turn on the adaptive trimming.
        EQ.1:	Activate the adaptive trimming.
        """ # nopep8
        return self._cards[0].get_value("irefine")

    @irefine.setter
    def irefine(self, value: int) -> None:
        self._cards[0].set_value("irefine", value)

    @property
    def ilevel(self) -> int:
        """Get or set the Adaptive refinement level.  Currently setting this variable to any integer other than 0 will refine the mesh one level down along the trim curve.
        EQ.0:	no refinement
        EQ.1 : refine one level down.
        """ # nopep8
        return self._cards[0].get_value("ilevel")

    @ilevel.setter
    def ilevel(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ilevel must be one of {0,1}""")
        self._cards[0].set_value("ilevel", value)

