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

class DefineDeToSurfaceTied(KeywordBase):
    """DYNA DEFINE_DE_TO_SURFACE_TIED keyword"""

    keyword = "DEFINE"
    subkeyword = "DE_TO_SURFACE_TIED"
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
                        "slave",
                        int,
                        0,
                        10,
                        kwargs.get("slave", 0)
                    ),
                    Field(
                        "master",
                        int,
                        10,
                        10,
                        kwargs.get("master", 0)
                    ),
                    Field(
                        "stype",
                        int,
                        20,
                        10,
                        kwargs.get("stype", 0)
                    ),
                    Field(
                        "mtype",
                        int,
                        30,
                        10,
                        kwargs.get("mtype", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nflf",
                        float,
                        0,
                        10,
                        kwargs.get("nflf")
                    ),
                    Field(
                        "sflf",
                        float,
                        10,
                        10,
                        kwargs.get("sflf")
                    ),
                    Field(
                        "nen",
                        float,
                        20,
                        10,
                        kwargs.get("nen", 2.)
                    ),
                    Field(
                        "mes",
                        float,
                        30,
                        10,
                        kwargs.get("mes", 2.)
                    ),
                    Field(
                        "lcid",
                        int,
                        40,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "nsort",
                        int,
                        50,
                        10,
                        kwargs.get("nsort", 100)
                    ),
                    Field(
                        "maxgap",
                        float,
                        60,
                        10,
                        kwargs.get("maxgap")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineDeToSurfaceTied.option_specs[0],
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
    def slave(self) -> int:
        """Get or set the DES nodes.
        """ # nopep8
        return self._cards[0].get_value("slave")

    @slave.setter
    def slave(self, value: int) -> None:
        self._cards[0].set_value("slave", value)

    @property
    def master(self) -> int:
        """Get or set the Shell set.
        """ # nopep8
        return self._cards[0].get_value("master")

    @master.setter
    def master(self, value: int) -> None:
        self._cards[0].set_value("master", value)

    @property
    def stype(self) -> int:
        """Get or set the EQ.0: Slave node set
        EQ.1: Slave node
        EQ.2: Slave part set
        EQ.3: Slave part.
        """ # nopep8
        return self._cards[0].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""stype must be one of {0,1,2,3}""")
        self._cards[0].set_value("stype", value)

    @property
    def mtype(self) -> int:
        """Get or set the EQ.0: Part set
        EQ.1: Part.
        """ # nopep8
        return self._cards[0].get_value("mtype")

    @mtype.setter
    def mtype(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""mtype must be one of {0,1}""")
        self._cards[0].set_value("mtype", value)

    @property
    def nflf(self) -> typing.Optional[float]:
        """Get or set the Normal failure force. Only tensile failure, i.e., tensile normal forces, will be considered in the failure criterion.
        """ # nopep8
        return self._cards[1].get_value("nflf")

    @nflf.setter
    def nflf(self, value: float) -> None:
        self._cards[1].set_value("nflf", value)

    @property
    def sflf(self) -> typing.Optional[float]:
        """Get or set the Shear failure force.
        """ # nopep8
        return self._cards[1].get_value("sflf")

    @sflf.setter
    def sflf(self, value: float) -> None:
        self._cards[1].set_value("sflf", value)

    @property
    def nen(self) -> float:
        """Get or set the Exponent for normal force.
        """ # nopep8
        return self._cards[1].get_value("nen")

    @nen.setter
    def nen(self, value: float) -> None:
        self._cards[1].set_value("nen", value)

    @property
    def mes(self) -> float:
        """Get or set the Exponent for shear force. Failure criterion.
        """ # nopep8
        return self._cards[1].get_value("mes")

    @mes.setter
    def mes(self, value: float) -> None:
        self._cards[1].set_value("mes", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Optional curve ID defining a scale factor vs. time.  The scale factor is applied to NFLF and SFLF, making the failure forces time-dependent.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[1].set_value("lcid", value)

    @property
    def nsort(self) -> int:
        """Get or set the Number of cycle between bucket sort.
        """ # nopep8
        return self._cards[1].get_value("nsort")

    @nsort.setter
    def nsort(self, value: int) -> None:
        self._cards[1].set_value("nsort", value)

    @property
    def maxgap(self) -> typing.Optional[float]:
        """Get or set the Maximum gap between DES and master surface:
        GT.0.0:	defines the ratio of the DES radius as the maximum gap, that is, MAXGAP × r_DES
        LT.0.0 : absolute value is used as the maximum gap
        """ # nopep8
        return self._cards[1].get_value("maxgap")

    @maxgap.setter
    def maxgap(self, value: float) -> None:
        self._cards[1].set_value("maxgap", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

