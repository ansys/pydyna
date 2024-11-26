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

class ControlImplicitOrdering(KeywordBase):
    """DYNA CONTROL_IMPLICIT_ORDERING keyword"""

    keyword = "CONTROL"
    subkeyword = "IMPLICIT_ORDERING"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "order",
                        int,
                        0,
                        10,
                        kwargs.get("order", 0)
                    ),
                    Field(
                        "nmetis",
                        int,
                        10,
                        10,
                        kwargs.get("nmetis", 0)
                    ),
                ],
            ),
        ]

    @property
    def order(self) -> int:
        """Get or set the Ordering option (see Remark 1):
        EQ.0:	method set automatically by LS - DYNA
        EQ.1 : MMD, Multiple Minimum Degree.
        EQ.2 : Metis(see Remark 2)
        EQ.4 : LS - GPart
        """ # nopep8
        return self._cards[0].get_value("order")

    @order.setter
    def order(self, value: int) -> None:
        if value not in [0, 1, 2, 4]:
            raise Exception("""order must be one of {0,1,2,4}""")
        self._cards[0].set_value("order", value)

    @property
    def nmetis(self) -> int:
        """Get or set the Number of times to use Metis on each compute node for MPP.
        """ # nopep8
        return self._cards[0].get_value("nmetis")

    @nmetis.setter
    def nmetis(self, value: int) -> None:
        self._cards[0].set_value("nmetis", value)

