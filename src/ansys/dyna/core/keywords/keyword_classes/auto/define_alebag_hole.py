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
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DefineAlebagHole(KeywordBase):
    """DYNA DEFINE_ALEBAG_HOLE keyword"""

    keyword = "DEFINE"
    subkeyword = "ALEBAG_HOLE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "holeid",
                        int,
                        0,
                        10,
                        kwargs.get("holeid")
                    ),
                    Field(
                        "sid",
                        int,
                        10,
                        10,
                        kwargs.get("sid")
                    ),
                    Field(
                        "sidtype",
                        int,
                        20,
                        10,
                        kwargs.get("sidtype", 0)
                    ),
                    Field(
                        "nquad",
                        int,
                        30,
                        10,
                        kwargs.get("nquad", 1)
                    ),
                    Field(
                        "xoff",
                        float,
                        40,
                        10,
                        kwargs.get("xoff", 0.0)
                    ),
                    Field(
                        "nfold",
                        int,
                        50,
                        10,
                        kwargs.get("nfold", 0)
                    ),
                    Field(
                        "xclen",
                        float,
                        60,
                        10,
                        kwargs.get("xclen", 0.0)
                    ),
                    Field(
                        "int/ext",
                        int,
                        70,
                        10,
                        kwargs.get("int/ext", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineAlebagHole.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]

    @property
    def holeid(self) -> typing.Optional[int]:
        """Get or set the Bag hole definition ID, referred in *AIRBAG_ADVANCED_ALE.
        """ # nopep8
        return self._cards[0].get_value("holeid")

    @holeid.setter
    def holeid(self, value: int) -> None:
        self._cards[0].set_value("holeid", value)

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Set of Lagrange shell elements to interact with inflator gas
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[0].set_value("sid", value)

    @property
    def sidtype(self) -> int:
        """Get or set the Type of SID
        EQ:'PSET' or '0' for set of parts
        EQ:'PART' or '1' for part
        """ # nopep8
        return self._cards[0].get_value("sidtype")

    @sidtype.setter
    def sidtype(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""sidtype must be one of {0,1}""")
        self._cards[0].set_value("sidtype", value)

    @property
    def nquad(self) -> int:
        """Get or set the The number of flow-sensor points to be distributed over each monitoring surface/segment.  There should be enough sensor points distributed to monitor the flow in each ALE element intersected by this monitoring surface (default=1).
        """ # nopep8
        return self._cards[0].get_value("nquad")

    @nquad.setter
    def nquad(self, value: int) -> None:
        self._cards[0].set_value("nquad", value)

    @property
    def xoff(self) -> float:
        """Get or set the Offset distance away from the monitoring surface, beyond which the AMMGID is switched.  The direction of XOFF depends on the normal vector of the monitoring segment.  This offset distance should be at least 1 ALE element width away from, and beyond the monitoring interface (default=0.0).
        """ # nopep8
        return self._cards[0].get_value("xoff")

    @xoff.setter
    def xoff(self, value: float) -> None:
        self._cards[0].set_value("xoff", value)

    @property
    def nfold(self) -> int:
        """Get or set the Flag for checking folding logic (default=0=off).  If NFOLD=1=on, then LS-DYNA will check if the monitoring segment is in the fold, applicable to airbag.  If the monitoring segment is still located within a folded (shell) region, then no switching is allowed yet until it has unfolded.
        """ # nopep8
        return self._cards[0].get_value("nfold")

    @nfold.setter
    def nfold(self, value: int) -> None:
        self._cards[0].set_value("nfold", value)

    @property
    def xclen(self) -> float:
        """Get or set the This is an absolute distance for distributing the flow sensor points over over the ALE elements.  To make sure that at least 1 sensor point, defined on each Lagrangian segment, is present in each ALE element to track the flow of an AMMG, XLEN may be estimated as roughly half the length of the smallest ALE element in the mesh.  This overwrites the NQUAD distribution of sensor points (default=0.0).
        """ # nopep8
        return self._cards[0].get_value("xclen")

    @xclen.setter
    def xclen(self, value: float) -> None:
        self._cards[0].set_value("xclen", value)

    @property
    def int_ext(self) -> int:
        """Get or set the 
        EQ: 0 'EXT' if the hole is an external hole
        EQ: 1 'INT' if the hole is an internal hole
        """ # nopep8
        return self._cards[0].get_value("int/ext")

    @int_ext.setter
    def int_ext(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""int_ext must be one of {0,1}""")
        self._cards[0].set_value("int/ext", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

