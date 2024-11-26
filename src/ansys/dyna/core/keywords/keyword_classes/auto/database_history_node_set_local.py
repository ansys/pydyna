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

class DatabaseHistoryNodeSetLocal(KeywordBase):
    """DYNA DATABASE_HISTORY_NODE_SET_LOCAL keyword"""

    keyword = "DATABASE"
    subkeyword = "HISTORY_NODE_SET_LOCAL"

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
                        "cid",
                        int,
                        10,
                        10,
                        kwargs.get("cid")
                    ),
                    Field(
                        "ref",
                        int,
                        20,
                        10,
                        kwargs.get("ref", 0)
                    ),
                    Field(
                        "hfo",
                        int,
                        30,
                        10,
                        kwargs.get("hfo", 0)
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Node set ID. The contents of the files are given in Table 9.1 in the Keyword Manual section 9.14 for nodes.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Coordinate system ID for nodal output. See *DEFINE_COORDINATE options.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[0].set_value("cid", value)

    @property
    def ref(self) -> int:
        """Get or set the Output reference:
        EQ.0: Output is in the local system fixed for all time from the beginning of the calculation (default),
        EQ.1: Output is in the local system which is defined by the *DEFINE_COORDINATE_NODES. The local system can change orientation depending on the movement of the three defining nodes. The defining nodes can belong to either deformable or rigid parts,
        EQ.2: Output is relative to the local system which is defined by the *DEFINE_COORDINATE_NODES option. The local system can change orientation depending on the movement of the three defining nodes. If dynamic relaxation is used, the reference location is reset when convergence is achieved.
        """ # nopep8
        return self._cards[0].get_value("ref")

    @ref.setter
    def ref(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""ref must be one of {0,1,2}""")
        self._cards[0].set_value("ref", value)

    @property
    def hfo(self) -> int:
        """Get or set the Flag for high frequency output into NODOUTHF.
        EQ.0: Nodal data written to NODOUT file only,
        EQ.1: Nodal data also written to NODOUTHF at the higher frequency.
        """ # nopep8
        return self._cards[0].get_value("hfo")

    @hfo.setter
    def hfo(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""hfo must be one of {0,1}""")
        self._cards[0].set_value("hfo", value)

