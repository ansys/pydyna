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

class AleFsiSwitchMmg(KeywordBase):
    """DYNA ALE_FSI_SWITCH_MMG keyword"""

    keyword = "ALE"
    subkeyword = "FSI_SWITCH_MMG"

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
                        "sid",
                        int,
                        0,
                        10,
                        kwargs.get("sid")
                    ),
                    Field(
                        "stype",
                        int,
                        10,
                        10,
                        kwargs.get("stype", 0)
                    ),
                    Field(
                        "nquad",
                        int,
                        20,
                        10,
                        kwargs.get("nquad", 1)
                    ),
                    Field(
                        "xoff",
                        float,
                        30,
                        10,
                        kwargs.get("xoff", 0.0)
                    ),
                    Field(
                        "btime",
                        float,
                        40,
                        10,
                        kwargs.get("btime", 0.0)
                    ),
                    Field(
                        "dtime",
                        float,
                        50,
                        10,
                        kwargs.get("dtime", 1.0e20)
                    ),
                    Field(
                        "nfreq",
                        int,
                        60,
                        10,
                        kwargs.get("nfreq", 1)
                    ),
                    Field(
                        "nfold",
                        int,
                        70,
                        10,
                        kwargs.get("nfold", 0)
                    ),
                ],
            ),
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
                        "xclen",
                        float,
                        20,
                        10,
                        kwargs.get("xclen", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Switch list ID,
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Switch list title .
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[0].set_value("title", value)

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the A set ID defining a monitoring surface over which an ALE fluid flows across, and its ALE multi-material-group-ID (AMMGID) is switched.  The monitoring surface may be a Lagrangian shell structure, or simply a segment set, and it does not have to be included in the coupling definition.
        """ # nopep8
        return self._cards[1].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[1].set_value("sid", value)

    @property
    def stype(self) -> int:
        """Get or set the Set ID type of the above SID.
        EQ.0: Part set ID (PSID) (default).
        EQ.1: Part ID (PID).
        EQ.2: Segment set ID (SGSID)
        """ # nopep8
        return self._cards[1].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""stype must be one of {0,1,2}""")
        self._cards[1].set_value("stype", value)

    @property
    def nquad(self) -> int:
        """Get or set the The number of flow-sensor points to be distributed over each monitoring surface/segment.  There should be enough sensor points distributed to monitor the flow in each ALE element intersected by this monitoring surface (default=1).
        """ # nopep8
        return self._cards[1].get_value("nquad")

    @nquad.setter
    def nquad(self, value: int) -> None:
        self._cards[1].set_value("nquad", value)

    @property
    def xoff(self) -> float:
        """Get or set the Offset distance away from the monitoring surface, beyond which the AMMGID is switched.  The direction of XOFF depends on the normal vector of the monitoring segment.  This offset distance should be at least 1 ALE element width away from, and beyond the monitoring interface (default=0.0).
        """ # nopep8
        return self._cards[1].get_value("xoff")

    @xoff.setter
    def xoff(self, value: float) -> None:
        self._cards[1].set_value("xoff", value)

    @property
    def btime(self) -> float:
        """Get or set the Start time for the AMMGID switch to be activated (default=0.0).
        """ # nopep8
        return self._cards[1].get_value("btime")

    @btime.setter
    def btime(self, value: float) -> None:
        self._cards[1].set_value("btime", value)

    @property
    def dtime(self) -> float:
        """Get or set the Ending time for the AMMGID switch (default=1.0E20).
        """ # nopep8
        return self._cards[1].get_value("dtime")

    @dtime.setter
    def dtime(self, value: float) -> None:
        self._cards[1].set_value("dtime", value)

    @property
    def nfreq(self) -> int:
        """Get or set the Number of computational cycles between ALE switch check (default=1).
        """ # nopep8
        return self._cards[1].get_value("nfreq")

    @nfreq.setter
    def nfreq(self, value: int) -> None:
        self._cards[1].set_value("nfreq", value)

    @property
    def nfold(self) -> int:
        """Get or set the Flag for checking folding logic (default=0=off).  If NFOLD=1=on, then LS-DYNA will check if the monitoring segment is in the fold, applicable to airbag.  If the monitoring segment is still located within a folded (shell) region, then no switching is allowed yet until it has unfolded.
        """ # nopep8
        return self._cards[1].get_value("nfold")

    @nfold.setter
    def nfold(self, value: int) -> None:
        self._cards[1].set_value("nfold", value)

    @property
    def fr_mmg(self) -> typing.Optional[int]:
        """Get or set the This is the AMMG-SID before the switch.  The AMMG-SID corresponds to the SID defined under the *SET_MULTI-MATERIAL_GROUP_LIST (SMMGL) card.  This SID points to one or more AMMGs (remark 1).
        """ # nopep8
        return self._cards[2].get_value("fr_mmg")

    @fr_mmg.setter
    def fr_mmg(self, value: int) -> None:
        self._cards[2].set_value("fr_mmg", value)

    @property
    def to_mmg(self) -> typing.Optional[int]:
        """Get or set the This is the AMMG-SID after the switch.  The AMMG-SID corresponds to the SID defined under the *SET_MULTI-MATERIAL_GROUP_LIST card. This SID points to one or more AMMGs (remark 1).
        """ # nopep8
        return self._cards[2].get_value("to_mmg")

    @to_mmg.setter
    def to_mmg(self, value: int) -> None:
        self._cards[2].set_value("to_mmg", value)

    @property
    def xclen(self) -> float:
        """Get or set the This is an absolute distance for distributing the flow sensor points over over the ALE elements.  To make sure that at least 1 sensor point, defined on each Lagrangian segment, is present in each ALE element to track the flow of an AMMG, XLEN may be estimated as roughly half the length of the smallest ALE element in the mesh.  This overwrites the NQUAD distribution of sensor points (default=0.0).
        """ # nopep8
        return self._cards[2].get_value("xclen")

    @xclen.setter
    def xclen(self, value: float) -> None:
        self._cards[2].set_value("xclen", value)

