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

class SetNodeListGenerateIncrementCollect(KeywordBase):
    """DYNA SET_NODE_LIST_GENERATE_INCREMENT_COLLECT keyword"""

    keyword = "SET"
    subkeyword = "NODE_LIST_GENERATE_INCREMENT_COLLECT"
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
                        "sid",
                        int,
                        0,
                        10,
                        kwargs.get("sid")
                    ),
                    Field(
                        "da1",
                        float,
                        10,
                        10,
                        kwargs.get("da1", 0.0)
                    ),
                    Field(
                        "da2",
                        float,
                        20,
                        10,
                        kwargs.get("da2", 0.0)
                    ),
                    Field(
                        "da3",
                        float,
                        30,
                        10,
                        kwargs.get("da3", 0.0)
                    ),
                    Field(
                        "da4",
                        float,
                        40,
                        10,
                        kwargs.get("da4", 0.0)
                    ),
                    Field(
                        "solver",
                        str,
                        50,
                        10,
                        kwargs.get("solver", "MECH")
                    ),
                    Field(
                        "its",
                        str,
                        60,
                        10,
                        kwargs.get("its", "1")
                    ),
                    Field(
                        "unused",
                        str,
                        70,
                        10,
                        kwargs.get("unused")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "bbeg",
                        int,
                        0,
                        10,
                        kwargs.get("bbeg")
                    ),
                    Field(
                        "bend",
                        int,
                        10,
                        10,
                        kwargs.get("bend")
                    ),
                    Field(
                        "incr",
                        int,
                        20,
                        10,
                        kwargs.get("incr")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = SetNodeListGenerateIncrementCollect.option_specs[0],
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
    def sid(self) -> typing.Optional[int]:
        """Get or set the Node set ID. All node sets should have a unique set ID.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[0].set_value("sid", value)

    @property
    def da1(self) -> float:
        """Get or set the First nodal attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da1")

    @da1.setter
    def da1(self, value: float) -> None:
        self._cards[0].set_value("da1", value)

    @property
    def da2(self) -> float:
        """Get or set the Second nodal attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da2")

    @da2.setter
    def da2(self, value: float) -> None:
        self._cards[0].set_value("da2", value)

    @property
    def da3(self) -> float:
        """Get or set the Third nodal attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da3")

    @da3.setter
    def da3(self, value: float) -> None:
        self._cards[0].set_value("da3", value)

    @property
    def da4(self) -> float:
        """Get or set the Fourth nodal attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da4")

    @da4.setter
    def da4(self, value: float) -> None:
        self._cards[0].set_value("da4", value)

    @property
    def solver(self) -> str:
        """Get or set the EQ.MECH: mechanics.
        EQ.CESE: CE/SE compressible fluid flow solver.
        EQ.ICFD: Incompressible fluid flow solver.
        """ # nopep8
        return self._cards[0].get_value("solver")

    @solver.setter
    def solver(self, value: str) -> None:
        if value not in ["MECH", "CESE", "ICFD"]:
            raise Exception("""solver must be one of {"MECH","CESE","ICFD"}""")
        self._cards[0].set_value("solver", value)

    @property
    def its(self) -> str:
        """Get or set the Specify coupling type across different scales in two-scale co-simulation. This flag should only be included for node sets that provide coupling information in the input file referred to by *INCLUDE_COSIM;
        EQ.1:	Tied contact coupling
        EQ.2 : Solid - in - shell immersed coupling
        """ # nopep8
        return self._cards[0].get_value("its")

    @its.setter
    def its(self, value: str) -> None:
        if value not in ["1", "2"]:
            raise Exception("""its must be one of {"1","2"}""")
        self._cards[0].set_value("its", value)

    @property
    def bbeg(self) -> typing.Optional[int]:
        """Get or set the First node ID in the first block.
        """ # nopep8
        return self._cards[1].get_value("bbeg")

    @bbeg.setter
    def bbeg(self, value: int) -> None:
        self._cards[1].set_value("bbeg", value)

    @property
    def bend(self) -> typing.Optional[int]:
        """Get or set the Last node ID in the first block.
        """ # nopep8
        return self._cards[1].get_value("bend")

    @bend.setter
    def bend(self, value: int) -> None:
        self._cards[1].set_value("bend", value)

    @property
    def incr(self) -> typing.Optional[int]:
        """Get or set the Node ID increment. Node IDs BBEG, BBEG+INCR, BBEG + 2*INCR, and so on through BEND are added to the set.
        """ # nopep8
        return self._cards[1].get_value("incr")

    @incr.setter
    def incr(self, value: int) -> None:
        self._cards[1].set_value("incr", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)
