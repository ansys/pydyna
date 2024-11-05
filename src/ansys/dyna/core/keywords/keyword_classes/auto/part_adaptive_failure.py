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

class PartAdaptiveFailure(KeywordBase):
    """DYNA PART_ADAPTIVE_FAILURE keyword"""

    keyword = "PART"
    subkeyword = "ADAPTIVE_FAILURE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "t",
                        float,
                        10,
                        10,
                        kwargs.get("t")
                    ),
                    Field(
                        "term",
                        int,
                        0,
                        10,
                        kwargs.get("term", 0)
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def t(self) -> typing.Optional[float]:
        """Get or set the Thickness. When the thickness of the part reaches this minimum value the part is split into two parts. The value for T should be on the order of the element thickness of a typical element.
        """ # nopep8
        return self._cards[0].get_value("t")

    @t.setter
    def t(self, value: float) -> None:
        self._cards[0].set_value("t", value)

    @property
    def term(self) -> int:
        """Get or set the Control adaptivity after the part separates:
        EQ.0:	continue to adapt part(default)
        EQ.1 : remove only this part from the adaptivity.Other parts will continue to adapt as normal.If there are no remaining parts to be adapted, adaptivity is disabled
        EQ.2 : adaptivity is disabled for all parts.
        """ # nopep8
        return self._cards[0].get_value("term")

    @term.setter
    def term(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""term must be one of {0,1,2}""")
        self._cards[0].set_value("term", value)

