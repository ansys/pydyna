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

class SetSolidGenerateIncrementCollect(KeywordBase):
    """DYNA SET_SOLID_GENERATE_INCREMENT_COLLECT keyword"""

    keyword = "SET"
    subkeyword = "SOLID_GENERATE_INCREMENT_COLLECT"
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
                        "solver",
                        str,
                        10,
                        10,
                        kwargs.get("solver", "MECH")
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
                option_spec = SetSolidGenerateIncrementCollect.option_specs[0],
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
        """Get or set the Solid element set ID. All solid sets should have a unique set ID.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[0].set_value("sid", value)

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
    def bbeg(self) -> typing.Optional[int]:
        """Get or set the First solid element ID in block.
        """ # nopep8
        return self._cards[1].get_value("bbeg")

    @bbeg.setter
    def bbeg(self, value: int) -> None:
        self._cards[1].set_value("bbeg", value)

    @property
    def bend(self) -> typing.Optional[int]:
        """Get or set the Last solid element ID in block.
        """ # nopep8
        return self._cards[1].get_value("bend")

    @bend.setter
    def bend(self, value: int) -> None:
        self._cards[1].set_value("bend", value)

    @property
    def incr(self) -> typing.Optional[int]:
        """Get or set the Solid ID increment. Solid IDs BBEG, BBEG + INCR, BBEG + 2*INCR, and so on through BEND are added to the set.
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
