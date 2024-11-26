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

class EmBoundaryPrescribed(KeywordBase):
    """DYNA EM_BOUNDARY_PRESCRIBED keyword"""

    keyword = "EM"
    subkeyword = "BOUNDARY_PRESCRIBED"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "bpid",
                        int,
                        0,
                        10,
                        kwargs.get("bpid")
                    ),
                    Field(
                        "bptype",
                        int,
                        10,
                        10,
                        kwargs.get("bptype", 1)
                    ),
                    Field(
                        "settype",
                        int,
                        20,
                        10,
                        kwargs.get("settype", 1)
                    ),
                    Field(
                        "setid",
                        int,
                        30,
                        10,
                        kwargs.get("setid")
                    ),
                    Field(
                        "val",
                        float,
                        40,
                        10,
                        kwargs.get("val", 0.0)
                    ),
                    Field(
                        "lcid",
                        int,
                        50,
                        10,
                        kwargs.get("lcid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "birtht",
                        float,
                        0,
                        10,
                        kwargs.get("birtht", 0.0)
                    ),
                    Field(
                        "deatht",
                        float,
                        10,
                        10,
                        kwargs.get("deatht", 1e28)
                    ),
                ],
            ),
        ]

    @property
    def bpid(self) -> typing.Optional[int]:
        """Get or set the ID of the Prescribed boundary.
        .
        """ # nopep8
        return self._cards[0].get_value("bpid")

    @bpid.setter
    def bpid(self, value: int) -> None:
        self._cards[0].set_value("bpid", value)

    @property
    def bptype(self) -> int:
        """Get or set the Boundary Prescribed type:
        EQ.1:Short (Scalar Potential set to 0.)
        EQ.2:Prescribed Resistance (Robin B.C).
        EQ.3:Prescribed Scalar Potential (Dirichlet B.C)
        EQ.4:Prescribed Current Density (Neumann B.C).
        """ # nopep8
        return self._cards[0].get_value("bptype")

    @bptype.setter
    def bptype(self, value: int) -> None:
        if value not in [1, 2, 3, 4]:
            raise Exception("""bptype must be one of {1,2,3,4}""")
        self._cards[0].set_value("bptype", value)

    @property
    def settype(self) -> int:
        """Get or set the Set type:
        EQ.1:Segment Set.
        EQ.2: Node Set.
        EQ.3: Fluid part. See *ICFD_PART.
        """ # nopep8
        return self._cards[0].get_value("settype")

    @settype.setter
    def settype(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""settype must be one of {1,2,3}""")
        self._cards[0].set_value("settype", value)

    @property
    def setid(self) -> typing.Optional[int]:
        """Get or set the set ID
        .
        """ # nopep8
        return self._cards[0].get_value("setid")

    @setid.setter
    def setid(self, value: int) -> None:
        self._cards[0].set_value("setid", value)

    @property
    def val(self) -> float:
        """Get or set the Value of the Resistance, current density or potential depending on BPTYPE.Ignored if LCID is defined
        .
        """ # nopep8
        return self._cards[0].get_value("val")

    @val.setter
    def val(self, value: float) -> None:
        self._cards[0].set_value("val", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining the value of the resistance, voltage or current function of time
        .
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def birtht(self) -> float:
        """Get or set the Birth times for that prescribed boundary.

        """ # nopep8
        return self._cards[1].get_value("birtht")

    @birtht.setter
    def birtht(self, value: float) -> None:
        self._cards[1].set_value("birtht", value)

    @property
    def deatht(self) -> float:
        """Get or set the Death times for that prescribed boundary.

        """ # nopep8
        return self._cards[1].get_value("deatht")

    @deatht.setter
    def deatht(self, value: float) -> None:
        self._cards[1].set_value("deatht", value)

