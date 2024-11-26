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

class AleStructuredFsi(KeywordBase):
    """DYNA ALE_STRUCTURED_FSI keyword"""

    keyword = "ALE"
    subkeyword = "STRUCTURED_FSI"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "coupid",
                        int,
                        0,
                        10,
                        kwargs.get("coupid")
                    ),
                    Field(
                        "title",
                        str,
                        10,
                        70,
                        kwargs.get("title")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lstrsid",
                        int,
                        0,
                        10,
                        kwargs.get("lstrsid")
                    ),
                    Field(
                        "alesid",
                        int,
                        10,
                        10,
                        kwargs.get("alesid")
                    ),
                    Field(
                        "lstrstyp",
                        int,
                        20,
                        10,
                        kwargs.get("lstrstyp", 0)
                    ),
                    Field(
                        "alestyp",
                        int,
                        30,
                        10,
                        kwargs.get("alestyp", 0)
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        50,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "mcoup",
                        int,
                        70,
                        10,
                        kwargs.get("mcoup")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "start",
                        float,
                        0,
                        10,
                        kwargs.get("start", 0.0)
                    ),
                    Field(
                        "end",
                        float,
                        10,
                        10,
                        kwargs.get("end", 1.0e10)
                    ),
                    Field(
                        "pfac",
                        float,
                        20,
                        10,
                        kwargs.get("pfac", 0.1)
                    ),
                    Field(
                        "fric",
                        float,
                        30,
                        10,
                        kwargs.get("fric", 0.0)
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "flip",
                        int,
                        50,
                        10,
                        kwargs.get("flip", 0)
                    ),
                ],
            ),
        ]

    @property
    def coupid(self) -> typing.Optional[int]:
        """Get or set the Coupling (card) ID number.  If not defined, LS-DYNA will assign an internal coupling ID based on the order of appearance in the input deck.
        """ # nopep8
        return self._cards[0].get_value("coupid")

    @coupid.setter
    def coupid(self, value: int) -> None:
        self._cards[0].set_value("coupid", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the A description of this coupling definition (A70).
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[0].set_value("title", value)

    @property
    def lstrsid(self) -> typing.Optional[int]:
        """Get or set the Set ID defining a part, part set, or segment set ID of the Lagrangian structure (see *PART, *SET_PART or *SET_SEGMENT).
        """ # nopep8
        return self._cards[1].get_value("lstrsid")

    @lstrsid.setter
    def lstrsid(self, value: int) -> None:
        self._cards[1].set_value("lstrsid", value)

    @property
    def alesid(self) -> typing.Optional[int]:
        """Get or set the Set ID defining a part or part set ID of the Structured ALE mesh (see *PART).
        """ # nopep8
        return self._cards[1].get_value("alesid")

    @alesid.setter
    def alesid(self, value: int) -> None:
        self._cards[1].set_value("alesid", value)

    @property
    def lstrstyp(self) -> int:
        """Get or set the Set type of LSTRSID:
        EQ.0:	part set ID (PSID).
        EQ.1:	part ID (PID).
        EQ.2:	segment set ID (SGSID).
        """ # nopep8
        return self._cards[1].get_value("lstrstyp")

    @lstrstyp.setter
    def lstrstyp(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""lstrstyp must be one of {0,1,2}""")
        self._cards[1].set_value("lstrstyp", value)

    @property
    def alestyp(self) -> int:
        """Get or set the Set type of ALESID:
        EQ.0:	part set ID (PSID).
        EQ.1:	part ID (PID).
        """ # nopep8
        return self._cards[1].get_value("alestyp")

    @alestyp.setter
    def alestyp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""alestyp must be one of {0,1}""")
        self._cards[1].set_value("alestyp", value)

    @property
    def mcoup(self) -> typing.Optional[int]:
        """Get or set the Which Multi-material(s) to be coupled (Remark 1):
        EQ.0:	couple with all multi-material groups,
        EQ.-N:	-N is the ID of *SET_MULTI-MATERIAL_GROUP.
        """ # nopep8
        return self._cards[1].get_value("mcoup")

    @mcoup.setter
    def mcoup(self, value: int) -> None:
        self._cards[1].set_value("mcoup", value)

    @property
    def start(self) -> float:
        """Get or set the Start time for coupling.
        """ # nopep8
        return self._cards[2].get_value("start")

    @start.setter
    def start(self, value: float) -> None:
        self._cards[2].set_value("start", value)

    @property
    def end(self) -> float:
        """Get or set the End time for coupling.
        """ # nopep8
        return self._cards[2].get_value("end")

    @end.setter
    def end(self, value: float) -> None:
        self._cards[2].set_value("end", value)

    @property
    def pfac(self) -> float:
        """Get or set the Penalty factor. PFAC is a scale factor for scaling the estimated stiffness of the interacting(coupling) system.It is used to compute the coupling forces to be distributed on the Lagrangian and ALE parts.
        GT.0:Fraction of estimated critical stiffness.
        LT.1:PFAC must be an integer, and PFAC is a load curve ID. The curve defines the coupling pressure on the axis as a function of the penetration along the axis.
        """ # nopep8
        return self._cards[2].get_value("pfac")

    @pfac.setter
    def pfac(self, value: float) -> None:
        self._cards[2].set_value("pfac", value)

    @property
    def fric(self) -> float:
        """Get or set the Friction Coefficient.  Friction force is evaluated as normal force multiplied by friction coefficient.
        GT.0:	Constant friction coefficient
        EQ. - N : Variable friction coefficient; defined by a TABLE ID = N.The table is to look up the friction coefficient value given a pair of(coupling pressure, relative velocity)..
        """ # nopep8
        return self._cards[2].get_value("fric")

    @fric.setter
    def fric(self, value: float) -> None:
        self._cards[2].set_value("fric", value)

    @property
    def flip(self) -> int:
        """Get or set the A Lagrangian segment will couple to fluid on only one side of the segment.The assump tion is segment normal points to f luids to be coupl ed. If that is not the case , set flip to 1.
        EQ.0:N o action.
        EQ.1:Flip the segment normal so it points to fluids to be coupled.
        """ # nopep8
        return self._cards[2].get_value("flip")

    @flip.setter
    def flip(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""flip must be one of {0,1}""")
        self._cards[2].set_value("flip", value)

