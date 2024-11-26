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

class AleCouplingNodalConstraint(KeywordBase):
    """DYNA ALE_COUPLING_NODAL_CONSTRAINT keyword"""

    keyword = "ALE"
    subkeyword = "COUPLING_NODAL_CONSTRAINT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "strsid",
                        int,
                        0,
                        10,
                        kwargs.get("strsid")
                    ),
                    Field(
                        "alesid",
                        int,
                        10,
                        10,
                        kwargs.get("alesid")
                    ),
                    Field(
                        "strsty",
                        int,
                        20,
                        10,
                        kwargs.get("strsty", 0)
                    ),
                    Field(
                        "alesty",
                        int,
                        30,
                        10,
                        kwargs.get("alesty", 0)
                    ),
                    Field(
                        "ctype",
                        int,
                        40,
                        10,
                        kwargs.get("ctype", 0)
                    ),
                    Field(
                        "mcoup",
                        int,
                        50,
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
                        kwargs.get("start", 0)
                    ),
                    Field(
                        "end",
                        float,
                        10,
                        10,
                        kwargs.get("end", 1.0E10)
                    ),
                    Field(
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "frcmin",
                        float,
                        50,
                        10,
                        kwargs.get("frcmin", 0.5)
                    ),
                ],
            ),
        ]

    @property
    def strsid(self) -> typing.Optional[int]:
        """Get or set the Set ID defining a part, part set or segment set ID of the structure (see *PART, *SET_‌PART or *SET_‌SEGMENT). The structure may include Lagrangian solid, shell, beam, thick shell, or discrete sphere elements. EFG, SPH, or EFG nodes may be used, but the boundary conditions may not be satisfied
        """ # nopep8
        return self._cards[0].get_value("strsid")

    @strsid.setter
    def strsid(self, value: int) -> None:
        self._cards[0].set_value("strsid", value)

    @property
    def alesid(self) -> typing.Optional[int]:
        """Get or set the Set ID defining a part or part set ID of the ALE solid elements (see *PART or *SET_‌PART).
        """ # nopep8
        return self._cards[0].get_value("alesid")

    @alesid.setter
    def alesid(self, value: int) -> None:
        self._cards[0].set_value("alesid", value)

    @property
    def strsty(self) -> int:
        """Get or set the Set type of STRSID
        EQ.0: Part set ID (PSID).
        EQ.1: Part ID (PID).
        EQ.2: Segment set ID (SGSID).
        EQ.3: Node set ID(NSID)
        """ # nopep8
        return self._cards[0].get_value("strsty")

    @strsty.setter
    def strsty(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""strsty must be one of {0,1,2,3}""")
        self._cards[0].set_value("strsty", value)

    @property
    def alesty(self) -> int:
        """Get or set the Master set type of "MASTER"
        EQ.0: Part set ID (PSID).
        EQ.1: Part ID (PID).
        """ # nopep8
        return self._cards[0].get_value("alesty")

    @alesty.setter
    def alesty(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""alesty must be one of {0,1}""")
        self._cards[0].set_value("alesty", value)

    @property
    def ctype(self) -> int:
        """Get or set the Coupling type:
        EQ.1: Constrained acceleration.
        EQ.2: Constrained acceleration and velocity.
        """ # nopep8
        return self._cards[0].get_value("ctype")

    @ctype.setter
    def ctype(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""ctype must be one of {0,1,2}""")
        self._cards[0].set_value("ctype", value)

    @property
    def mcoup(self) -> typing.Optional[int]:
        """Get or set the Multi-material option (CTYPE 4, 5, 6, 11 and 12, ).
        EQ.0: Couple with all multi-material groups,
        EQ.-n: refers to a set ID of an ALE multi-material groups defined in *SET_MULTI-MATERIAL_GROUP card in which its set ID=n.
        """ # nopep8
        return self._cards[0].get_value("mcoup")

    @mcoup.setter
    def mcoup(self, value: int) -> None:
        self._cards[0].set_value("mcoup", value)

    @property
    def start(self) -> float:
        """Get or set the Start time for coupling.
        """ # nopep8
        return self._cards[1].get_value("start")

    @start.setter
    def start(self, value: float) -> None:
        self._cards[1].set_value("start", value)

    @property
    def end(self) -> float:
        """Get or set the End time for coupling.
        """ # nopep8
        return self._cards[1].get_value("end")

    @end.setter
    def end(self, value: float) -> None:
        self._cards[1].set_value("end", value)

    @property
    def frcmin(self) -> float:
        """Get or set the Only to be used with nonzero MCOUP. Minimum volume fraction of the fluid materials included in the list of AMMGs to activate coupling. Default value is 0.5. Reducing FRCMIN (typically, between 0.1 and 0.3) would turn on coupling earlier to prevent leakage in hypervelocity impact cases.
        """ # nopep8
        return self._cards[1].get_value("frcmin")

    @frcmin.setter
    def frcmin(self, value: float) -> None:
        self._cards[1].set_value("frcmin", value)

