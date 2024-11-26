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

class AleFragmentation(KeywordBase):
    """DYNA ALE_FRAGMENTATION keyword"""

    keyword = "ALE"
    subkeyword = "FRAGMENTATION"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "fr_mmg",
                        int,
                        0,
                        10,
                        kwargs.get("fr_mmg")
                    ),
                    Field(
                        "to_mmg",
                        int,
                        10,
                        10,
                        kwargs.get("to_mmg")
                    ),
                    Field(
                        "fragtyp",
                        int,
                        20,
                        10,
                        kwargs.get("fragtyp", 1)
                    ),
                ],
            ),
        ]

    @property
    def fr_mmg(self) -> typing.Optional[int]:
        """Get or set the This is the AMMGID of the failed material
        """ # nopep8
        return self._cards[0].get_value("fr_mmg")

    @fr_mmg.setter
    def fr_mmg(self, value: int) -> None:
        self._cards[0].set_value("fr_mmg", value)

    @property
    def to_mmg(self) -> typing.Optional[int]:
        """Get or set the This is the AMMGID of the vacuum to which the failed material is being switched
        """ # nopep8
        return self._cards[0].get_value("to_mmg")

    @to_mmg.setter
    def to_mmg(self, value: int) -> None:
        self._cards[0].set_value("to_mmg", value)

    @property
    def fragtyp(self) -> int:
        """Get or set the Flag defining whether the failed material is completely or partially switched to vacuum.
        EQ.1:	Fully switch; all failed material is switched to vacuum.
        EQ.2:	Partially switch; only the volume expansion from the last time step is switched to vacuum
        """ # nopep8
        return self._cards[0].get_value("fragtyp")

    @fragtyp.setter
    def fragtyp(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""fragtyp must be one of {1,2}""")
        self._cards[0].set_value("fragtyp", value)

