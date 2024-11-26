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

class SetSolidCollect(KeywordBase):
    """DYNA SET_SOLID_COLLECT keyword"""

    keyword = "SET"
    subkeyword = "SOLID_COLLECT"
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
                        "k1",
                        int,
                        0,
                        10,
                        kwargs.get("k1")
                    ),
                    Field(
                        "k2",
                        int,
                        10,
                        10,
                        kwargs.get("k2")
                    ),
                    Field(
                        "k3",
                        int,
                        20,
                        10,
                        kwargs.get("k3")
                    ),
                    Field(
                        "k4",
                        int,
                        30,
                        10,
                        kwargs.get("k4")
                    ),
                    Field(
                        "k5",
                        int,
                        40,
                        10,
                        kwargs.get("k5")
                    ),
                    Field(
                        "k6",
                        int,
                        50,
                        10,
                        kwargs.get("k6")
                    ),
                    Field(
                        "k7",
                        int,
                        60,
                        10,
                        kwargs.get("k7")
                    ),
                    Field(
                        "k8",
                        int,
                        70,
                        10,
                        kwargs.get("k8")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = SetSolidCollect.option_specs[0],
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
        """Get or set the Solid element set ID. All shell sets should have a unique set ID.
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
    def k1(self) -> typing.Optional[int]:
        """Get or set the First solid element ID of the set.
        """ # nopep8
        return self._cards[1].get_value("k1")

    @k1.setter
    def k1(self, value: int) -> None:
        self._cards[1].set_value("k1", value)

    @property
    def k2(self) -> typing.Optional[int]:
        """Get or set the Second solid element ID of the set.
        """ # nopep8
        return self._cards[1].get_value("k2")

    @k2.setter
    def k2(self, value: int) -> None:
        self._cards[1].set_value("k2", value)

    @property
    def k3(self) -> typing.Optional[int]:
        """Get or set the Third solid element ID of the set.
        """ # nopep8
        return self._cards[1].get_value("k3")

    @k3.setter
    def k3(self, value: int) -> None:
        self._cards[1].set_value("k3", value)

    @property
    def k4(self) -> typing.Optional[int]:
        """Get or set the Fourth solid element ID of the set.
        """ # nopep8
        return self._cards[1].get_value("k4")

    @k4.setter
    def k4(self, value: int) -> None:
        self._cards[1].set_value("k4", value)

    @property
    def k5(self) -> typing.Optional[int]:
        """Get or set the Fifth solid element ID of the set.
        """ # nopep8
        return self._cards[1].get_value("k5")

    @k5.setter
    def k5(self, value: int) -> None:
        self._cards[1].set_value("k5", value)

    @property
    def k6(self) -> typing.Optional[int]:
        """Get or set the Sixth solid element ID of the set.
        """ # nopep8
        return self._cards[1].get_value("k6")

    @k6.setter
    def k6(self, value: int) -> None:
        self._cards[1].set_value("k6", value)

    @property
    def k7(self) -> typing.Optional[int]:
        """Get or set the Seventh solid element ID of the set.
        """ # nopep8
        return self._cards[1].get_value("k7")

    @k7.setter
    def k7(self, value: int) -> None:
        self._cards[1].set_value("k7", value)

    @property
    def k8(self) -> typing.Optional[int]:
        """Get or set the Eighth solid element ID of the set.
        """ # nopep8
        return self._cards[1].get_value("k8")

    @k8.setter
    def k8(self, value: int) -> None:
        self._cards[1].set_value("k8", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

