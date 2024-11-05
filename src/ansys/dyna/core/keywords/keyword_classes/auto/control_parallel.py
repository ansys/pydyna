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

class ControlParallel(KeywordBase):
    """DYNA CONTROL_PARALLEL keyword"""

    keyword = "CONTROL"
    subkeyword = "PARALLEL"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ncpu",
                        int,
                        0,
                        10,
                        kwargs.get("ncpu", 1)
                    ),
                    Field(
                        "numrhs",
                        int,
                        10,
                        10,
                        kwargs.get("numrhs", 0)
                    ),
                    Field(
                        "const",
                        int,
                        20,
                        10,
                        kwargs.get("const", 2)
                    ),
                    Field(
                        "para",
                        int,
                        30,
                        10,
                        kwargs.get("para", 0)
                    ),
                ],
            ),
        ]

    @property
    def ncpu(self) -> int:
        """Get or set the Number of cpus used.
        """ # nopep8
        return self._cards[0].get_value("ncpu")

    @ncpu.setter
    def ncpu(self, value: int) -> None:
        self._cards[0].set_value("ncpu", value)

    @property
    def numrhs(self) -> int:
        """Get or set the Number of right-hand sides allocated in memory:
        EQ.0: same as NCPU, always recommended,
        EQ.1: allocate only one.
        """ # nopep8
        return self._cards[0].get_value("numrhs")

    @numrhs.setter
    def numrhs(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""numrhs must be one of {0,1}""")
        self._cards[0].set_value("numrhs", value)

    @property
    def const(self) -> int:
        """Get or set the Consistency flag for parallel solution (NCPU >1).
        EQ.1: on
        EQ.2: off, for a faster solution (default).
        """ # nopep8
        return self._cards[0].get_value("const")

    @const.setter
    def const(self, value: int) -> None:
        if value not in [2, 1]:
            raise Exception("""const must be one of {2,1}""")
        self._cards[0].set_value("const", value)

    @property
    def para(self) -> int:
        """Get or set the Flag for parallel force assembly if CONST=1.
        EQ.0: off
        EQ.1: on
        EQ.2: on
        """ # nopep8
        return self._cards[0].get_value("para")

    @para.setter
    def para(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""para must be one of {0,1,2}""")
        self._cards[0].set_value("para", value)

