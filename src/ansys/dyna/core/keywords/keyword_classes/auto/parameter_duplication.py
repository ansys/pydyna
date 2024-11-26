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

class ParameterDuplication(KeywordBase):
    """DYNA PARAMETER_DUPLICATION keyword"""

    keyword = "PARAMETER"
    subkeyword = "DUPLICATION"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "dflag",
                        int,
                        0,
                        10,
                        kwargs.get("dflag", 1)
                    ),
                ],
            ),
        ]

    @property
    def dflag(self) -> int:
        """Get or set the Flag to control treatment of duplicate parameter definitions:
        EQ.1: issue a warning and ignore the new definition (default)
        EQ.2: issue a warning and accept the new definition
        EQ.3: issue an error and ignore (terminates at end of input)
        EQ.4: accept silently
        EQ.5: ignore silently
        .
        """ # nopep8
        return self._cards[0].get_value("dflag")

    @dflag.setter
    def dflag(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5]:
            raise Exception("""dflag must be one of {1,2,3,4,5}""")
        self._cards[0].set_value("dflag", value)

