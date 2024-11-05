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

class SetSolidGenerateCollect(KeywordBase):
    """DYNA SET_SOLID_GENERATE_COLLECT keyword"""

    keyword = "SET"
    subkeyword = "SOLID_GENERATE_COLLECT"
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
                        "b1beg",
                        int,
                        0,
                        10,
                        kwargs.get("b1beg")
                    ),
                    Field(
                        "b1end",
                        int,
                        10,
                        10,
                        kwargs.get("b1end")
                    ),
                    Field(
                        "b2beg",
                        int,
                        20,
                        10,
                        kwargs.get("b2beg")
                    ),
                    Field(
                        "b2end",
                        int,
                        30,
                        10,
                        kwargs.get("b2end")
                    ),
                    Field(
                        "b3beg",
                        int,
                        40,
                        10,
                        kwargs.get("b3beg")
                    ),
                    Field(
                        "b3end",
                        int,
                        50,
                        10,
                        kwargs.get("b3end")
                    ),
                    Field(
                        "b4beg",
                        int,
                        60,
                        10,
                        kwargs.get("b4beg")
                    ),
                    Field(
                        "b4end",
                        int,
                        70,
                        10,
                        kwargs.get("b4end")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = SetSolidGenerateCollect.option_specs[0],
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
    def b1beg(self) -> typing.Optional[int]:
        """Get or set the First solid element ID in the first block.
        """ # nopep8
        return self._cards[1].get_value("b1beg")

    @b1beg.setter
    def b1beg(self, value: int) -> None:
        self._cards[1].set_value("b1beg", value)

    @property
    def b1end(self) -> typing.Optional[int]:
        """Get or set the Last solid element ID in the first block.
        """ # nopep8
        return self._cards[1].get_value("b1end")

    @b1end.setter
    def b1end(self, value: int) -> None:
        self._cards[1].set_value("b1end", value)

    @property
    def b2beg(self) -> typing.Optional[int]:
        """Get or set the First solid element ID in the second block.
        """ # nopep8
        return self._cards[1].get_value("b2beg")

    @b2beg.setter
    def b2beg(self, value: int) -> None:
        self._cards[1].set_value("b2beg", value)

    @property
    def b2end(self) -> typing.Optional[int]:
        """Get or set the Last solid element ID in the second block.
        """ # nopep8
        return self._cards[1].get_value("b2end")

    @b2end.setter
    def b2end(self, value: int) -> None:
        self._cards[1].set_value("b2end", value)

    @property
    def b3beg(self) -> typing.Optional[int]:
        """Get or set the First solid element ID in the third block.
        """ # nopep8
        return self._cards[1].get_value("b3beg")

    @b3beg.setter
    def b3beg(self, value: int) -> None:
        self._cards[1].set_value("b3beg", value)

    @property
    def b3end(self) -> typing.Optional[int]:
        """Get or set the Last solid element ID in the third block.
        """ # nopep8
        return self._cards[1].get_value("b3end")

    @b3end.setter
    def b3end(self, value: int) -> None:
        self._cards[1].set_value("b3end", value)

    @property
    def b4beg(self) -> typing.Optional[int]:
        """Get or set the First solid element ID in the fourth block.
        """ # nopep8
        return self._cards[1].get_value("b4beg")

    @b4beg.setter
    def b4beg(self, value: int) -> None:
        self._cards[1].set_value("b4beg", value)

    @property
    def b4end(self) -> typing.Optional[int]:
        """Get or set the Last solid element ID in the fourth block.
        """ # nopep8
        return self._cards[1].get_value("b4end")

    @b4end.setter
    def b4end(self, value: int) -> None:
        self._cards[1].set_value("b4end", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

