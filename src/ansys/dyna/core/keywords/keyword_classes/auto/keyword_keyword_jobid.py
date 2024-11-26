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

class KeywordKeywordJobid(KeywordBase):
    """DYNA KEYWORD_KEYWORD_JOBID keyword"""

    keyword = "KEYWORD"
    subkeyword = "KEYWORD_JOBID"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "memory",
                        str,
                        0,
                        10,
                        kwargs.get("memory")
                    ),
                    Field(
                        "memory2",
                        str,
                        10,
                        10,
                        kwargs.get("memory2")
                    ),
                    Field(
                        "ncpu",
                        int,
                        20,
                        10,
                        kwargs.get("ncpu")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "jobname",
                        str,
                        0,
                        256,
                        kwargs.get("jobname")
                    ),
                ],
            ),
        ]

    @property
    def memory(self) -> typing.Optional[str]:
        """Get or set the Memory size in units of words to be allocated.
        """ # nopep8
        return self._cards[0].get_value("memory")

    @memory.setter
    def memory(self, value: str) -> None:
        self._cards[0].set_value("memory", value)

    @property
    def memory2(self) -> typing.Optional[str]:
        """Get or set the For MPP, defines the memory allocation in words for each of the cores except the first core.
        """ # nopep8
        return self._cards[0].get_value("memory2")

    @memory2.setter
    def memory2(self, value: str) -> None:
        self._cards[0].set_value("memory2", value)

    @property
    def ncpu(self) -> typing.Optional[int]:
        """Get or set the Number of CPUs "n" to be used during the analysis
        """ # nopep8
        return self._cards[0].get_value("ncpu")

    @ncpu.setter
    def ncpu(self, value: int) -> None:
        self._cards[0].set_value("ncpu", value)

    @property
    def jobname(self) -> typing.Optional[str]:
        """Get or set the job name
        """ # nopep8
        return self._cards[1].get_value("jobname")

    @jobname.setter
    def jobname(self, value: str) -> None:
        self._cards[1].set_value("jobname", value)

