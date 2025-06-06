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

"""Module providing the KeywordKeywordId class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class KeywordKeywordId(KeywordBase):
    """DYNA KEYWORD_KEYWORD_ID keyword"""

    keyword = "KEYWORD"
    subkeyword = "KEYWORD_ID"

    def __init__(self, **kwargs):
        """Initialize the KeywordKeywordId class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "memory",
                        str,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "memory2",
                        str,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ncpu",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "project",
                        str,
                        0,
                        20,
                        **kwargs,
                    ),
                    Field(
                        "num",
                        str,
                        20,
                        20,
                        **kwargs,
                    ),
                    Field(
                        "stage",
                        str,
                        40,
                        40,
                        **kwargs,
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
        """Set the memory property."""
        self._cards[0].set_value("memory", value)

    @property
    def memory2(self) -> typing.Optional[str]:
        """Get or set the For MPP, defines the memory allocation in words for each of the cores except the first core.
        """ # nopep8
        return self._cards[0].get_value("memory2")

    @memory2.setter
    def memory2(self, value: str) -> None:
        """Set the memory2 property."""
        self._cards[0].set_value("memory2", value)

    @property
    def ncpu(self) -> typing.Optional[int]:
        """Get or set the Number of CPUs "n" to be used during the analysis
        """ # nopep8
        return self._cards[0].get_value("ncpu")

    @ncpu.setter
    def ncpu(self, value: int) -> None:
        """Set the ncpu property."""
        self._cards[0].set_value("ncpu", value)

    @property
    def project(self) -> typing.Optional[str]:
        """Get or set the First part of filename prefix.
        """ # nopep8
        return self._cards[1].get_value("project")

    @project.setter
    def project(self, value: str) -> None:
        """Set the project property."""
        self._cards[1].set_value("project", value)

    @property
    def num(self) -> typing.Optional[str]:
        """Get or set the Second part of filename prefix
        """ # nopep8
        return self._cards[1].get_value("num")

    @num.setter
    def num(self, value: str) -> None:
        """Set the num property."""
        self._cards[1].set_value("num", value)

    @property
    def stage(self) -> typing.Optional[str]:
        """Get or set the Third part of filename prefix
        """ # nopep8
        return self._cards[1].get_value("stage")

    @stage.setter
    def stage(self, value: str) -> None:
        """Set the stage property."""
        self._cards[1].set_value("stage", value)

