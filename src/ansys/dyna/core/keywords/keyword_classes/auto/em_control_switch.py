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

class EmControlSwitch(KeywordBase):
    """DYNA EM_CONTROL_SWITCH keyword"""

    keyword = "EM"
    subkeyword = "CONTROL_SWITCH"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "lcid",
                        int,
                        0,
                        10,
                        kwargs.get("lcid", 0)
                    ),
                    Field(
                        "femcomp",
                        int,
                        10,
                        10,
                        kwargs.get("femcomp", 0)
                    ),
                    Field(
                        "bemcomp",
                        int,
                        20,
                        10,
                        kwargs.get("bemcomp", 0)
                    ),
                ],
            ),
        ]

    @property
    def lcid(self) -> int:
        """Get or set the Load Curve ID.Negative values switch the solver off, positive values switch it back on.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def femcomp(self) -> int:
        """Get or set the Determines if FEM matrices are recomputed each time the EM solver is turned back on:
        EQ.0: FEM matrices are recomputed
        EQ.1: FEM matrices are not recomputed
        """ # nopep8
        return self._cards[0].get_value("femcomp")

    @femcomp.setter
    def femcomp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""femcomp must be one of {0,1}""")
        self._cards[0].set_value("femcomp", value)

    @property
    def bemcomp(self) -> int:
        """Get or set the Determines if BEM matrices are recomputed each time the EM solver is turned back on:
        EQ.0 : BEM matrices are recomputed
        EQ.1 : BEM matrices are not recomputed
        """ # nopep8
        return self._cards[0].get_value("bemcomp")

    @bemcomp.setter
    def bemcomp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""bemcomp must be one of {0,1}""")
        self._cards[0].set_value("bemcomp", value)

