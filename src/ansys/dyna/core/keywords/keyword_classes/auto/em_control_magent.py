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

"""Module providing the EmControlMagent class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class EmControlMagent(KeywordBase):
    """DYNA EM_CONTROL_MAGENT keyword"""

    keyword = "EM"
    subkeyword = "CONTROL_MAGENT"

    def __init__(self, **kwargs):
        """Initialize the EmControlMagent class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "mcomp",
                        int,
                        0,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def mcomp(self) -> int:
        """Get or set the Magnetization vector recomputation:
        EQ.0:	Off.See Remark 1.
        EQ.1 : On.Magnetization recomputation is controlled by NCYCM.
        """ # nopep8
        return self._cards[0].get_value("mcomp")

    @mcomp.setter
    def mcomp(self, value: int) -> None:
        """Set the mcomp property."""
        if value not in [0, 1, None]:
            raise Exception("""mcomp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("mcomp", value)

