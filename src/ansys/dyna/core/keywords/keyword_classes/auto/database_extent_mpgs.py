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

class DatabaseExtentMpgs(KeywordBase):
    """DYNA DATABASE_EXTENT_MPGS keyword"""

    keyword = "DATABASE"
    subkeyword = "EXTENT_MPGS"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "vtype",
                        int,
                        0,
                        10,
                        kwargs.get("vtype", 0)
                    ),
                    Field(
                        "comp",
                        int,
                        10,
                        10,
                        kwargs.get("comp")
                    ),
                ],
            ),
        ]

    @property
    def vtype(self) -> int:
        """Get or set the Variable type:
        EQ.0: node,
        EQ.1: brick
        EQ.2: beam,
        EQ.3: shell,
        EQ.4: thick shell.
        """ # nopep8
        return self._cards[0].get_value("vtype")

    @vtype.setter
    def vtype(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4]:
            raise Exception("""vtype must be one of {0,1,2,3,4}""")
        self._cards[0].set_value("vtype", value)

    @property
    def comp(self) -> typing.Optional[int]:
        """Get or set the Component number ID. For the corresponding VTYPE, integer components from the following tables can be chosen:
        VTYPE.EQ.0: Table 9.1,
        VTYPE.EQ.1: Table 9.2,
        VTYPE.EQ.2: not supported,
        VTYPE.EQ.3: Table 9.3,
        VTYPE.EQ.4: not supported.
        Please see keyword manual 9.12-9.15.
        """ # nopep8
        return self._cards[0].get_value("comp")

    @comp.setter
    def comp(self, value: int) -> None:
        self._cards[0].set_value("comp", value)

