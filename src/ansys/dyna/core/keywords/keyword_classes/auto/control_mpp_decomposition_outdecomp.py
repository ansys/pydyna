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

class ControlMppDecompositionOutdecomp(KeywordBase):
    """DYNA CONTROL_MPP_DECOMPOSITION_OUTDECOMP keyword"""

    keyword = "CONTROL"
    subkeyword = "MPP_DECOMPOSITION_OUTDECOMP"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "itype",
                        int,
                        0,
                        10,
                        kwargs.get("itype", 1)
                    ),
                ],
            ),
        ]

    @property
    def itype(self) -> int:
        """Get or set the 1: database in ls-prepost format to file decomp_parts.lsprepost.
        2: database in animator format to file decomp_parts.ses
        EQ.3:	database in LS-PrePost format with d3plot state number.
        This allows lsprepost to show the matching d3plot with the decomposition for
        *CONTROL_MPP_DECOMPOSITION_REDECOMPOSITION decomp_parts.lsprepost_s######
        """ # nopep8
        return self._cards[0].get_value("itype")

    @itype.setter
    def itype(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""itype must be one of {1,2,3}""")
        self._cards[0].set_value("itype", value)

