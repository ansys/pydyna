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

class ControlMppDecompositionArrangeParts(KeywordBase):
    """DYNA CONTROL_MPP_DECOMPOSITION_ARRANGE_PARTS keyword"""

    keyword = "CONTROL"
    subkeyword = "MPP_DECOMPOSITION_ARRANGE_PARTS"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "id",
                        int,
                        0,
                        10,
                        kwargs.get("id")
                    ),
                    Field(
                        "type",
                        int,
                        10,
                        10,
                        kwargs.get("type", 0)
                    ),
                    Field(
                        "nproc",
                        int,
                        20,
                        10,
                        kwargs.get("nproc")
                    ),
                    Field(
                        "frstp",
                        int,
                        30,
                        10,
                        kwargs.get("frstp")
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Part ID/Part set ID
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def type(self) -> int:
        """Get or set the EQ. 0: Part ID to be distributed to all processors
        1: Part Set ID to be distributed to all processors
        10: Part ID to be lumped into one processor
        11: Part Set ID to be lumped into one processor.
        EQ.20: Part ID to be lumped into one processor with MPP load balanced
        EQ.21: Part Set ID to be lumped into one processor with MPP load balanced
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        if value not in [0, 1, 10, 11, 20, 21]:
            raise Exception("""type must be one of {0,1,10,11,20,21}""")
        self._cards[0].set_value("type", value)

    @property
    def nproc(self) -> typing.Optional[int]:
        """Get or set the Used only for TYPE equal to 0 or 1. Number of processors will
        be used for decomposition and this Part ID/Part set ID will be
        distributed to NPROC of processors.
        """ # nopep8
        return self._cards[0].get_value("nproc")

    @nproc.setter
    def nproc(self, value: int) -> None:
        self._cards[0].set_value("nproc", value)

    @property
    def frstp(self) -> typing.Optional[int]:
        """Get or set the Used only for TYPE equal to 0 or 1. Starting MPP rank ID (rank
        starts from 0).
        """ # nopep8
        return self._cards[0].get_value("frstp")

    @frstp.setter
    def frstp(self, value: int) -> None:
        self._cards[0].set_value("frstp", value)

