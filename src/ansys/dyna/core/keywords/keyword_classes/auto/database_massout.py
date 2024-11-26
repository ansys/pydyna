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

class DatabaseMassout(KeywordBase):
    """DYNA DATABASE_MASSOUT keyword"""

    keyword = "DATABASE"
    subkeyword = "MASSOUT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "setid",
                        int,
                        0,
                        10,
                        kwargs.get("setid")
                    ),
                    Field(
                        "ndflg",
                        int,
                        10,
                        10,
                        kwargs.get("ndflg", 1)
                    ),
                    Field(
                        "rbflg",
                        int,
                        20,
                        10,
                        kwargs.get("rbflg", 0)
                    ),
                ],
            ),
        ]

    @property
    def setid(self) -> typing.Optional[int]:
        """Get or set the Optional set ID.
        EQ.0: mass output for all nodes,
        LT.0: no output,
        GT.0: set ID identifying nodes whose mass will be output.
        """ # nopep8
        return self._cards[0].get_value("setid")

    @setid.setter
    def setid(self, value: int) -> None:
        self._cards[0].set_value("setid", value)

    @property
    def ndflg(self) -> int:
        """Get or set the Database extent:
        EQ.1: output translational mass for deformable nodes identified by	SETID (default),
        EQ.2: output translational mass and rotary inertias for the deformable	nodes identified by the SETID.
        EQ.3: output translational mass for deformable and rigid nodes identified by SETID (default),
        EQ.4: output translational mass and rotary inertias for the deformable	and rigid nodes identified by the SETID.
        """ # nopep8
        return self._cards[0].get_value("ndflg")

    @ndflg.setter
    def ndflg(self, value: int) -> None:
        if value not in [1, 2, 3, 4]:
            raise Exception("""ndflg must be one of {1,2,3,4}""")
        self._cards[0].set_value("ndflg", value)

    @property
    def rbflg(self) -> int:
        """Get or set the Rigid body data:
        EQ.0: no output for rigid bodies,
        EQ.1: output rigid body mass and inertia.
        """ # nopep8
        return self._cards[0].get_value("rbflg")

    @rbflg.setter
    def rbflg(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""rbflg must be one of {0,1}""")
        self._cards[0].set_value("rbflg", value)

