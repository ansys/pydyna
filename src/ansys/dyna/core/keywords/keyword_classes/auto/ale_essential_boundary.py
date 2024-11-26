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

class AleEssentialBoundary(KeywordBase):
    """DYNA ALE_ESSENTIAL_BOUNDARY keyword"""

    keyword = "ALE"
    subkeyword = "ESSENTIAL_BOUNDARY"

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
                        "idtype",
                        int,
                        10,
                        10,
                        kwargs.get("idtype", 0)
                    ),
                    Field(
                        "ictype",
                        int,
                        20,
                        10,
                        kwargs.get("ictype", 1)
                    ),
                    Field(
                        "iexcl",
                        int,
                        30,
                        10,
                        kwargs.get("iexcl")
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Set ID defining a part, part set or segment set ID of the ALE mesh boundary.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def idtype(self) -> int:
        """Get or set the Type of set ID:
        EQ.0: part set ID (PSID).
        EQ.1: part ID (PID).
        EQ.2: segment set ID (SGSID).
        """ # nopep8
        return self._cards[0].get_value("idtype")

    @idtype.setter
    def idtype(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""idtype must be one of {0,1,2}""")
        self._cards[0].set_value("idtype", value)

    @property
    def ictype(self) -> int:
        """Get or set the Constraint type:
        EQ.1: No flow through all directions.
        EQ.2: No flow through normal direction. (slip condition)
        """ # nopep8
        return self._cards[0].get_value("ictype")

    @ictype.setter
    def ictype(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""ictype must be one of {1,2}""")
        self._cards[0].set_value("ictype", value)

    @property
    def iexcl(self) -> typing.Optional[int]:
        """Get or set the Segment Set ID to be excluded from applying ALE essential boundary condition. For example, inlet/outlet segments.
        """ # nopep8
        return self._cards[0].get_value("iexcl")

    @iexcl.setter
    def iexcl(self, value: int) -> None:
        self._cards[0].set_value("iexcl", value)

