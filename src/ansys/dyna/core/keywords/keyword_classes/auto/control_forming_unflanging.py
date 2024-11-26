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

class ControlFormingUnflanging(KeywordBase):
    """DYNA CONTROL_FORMING_UNFLANGING keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_UNFLANGING"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "noption",
                        int,
                        0,
                        10,
                        kwargs.get("noption")
                    ),
                    Field(
                        "dvid",
                        int,
                        10,
                        10,
                        kwargs.get("dvid")
                    ),
                    Field(
                        "nunbend",
                        int,
                        20,
                        10,
                        kwargs.get("nunbend")
                    ),
                    Field(
                        "stfbend",
                        float,
                        30,
                        10,
                        kwargs.get("stfbend")
                    ),
                    Field(
                        "stfcnt",
                        float,
                        40,
                        10,
                        kwargs.get("stfcnt")
                    ),
                    Field(
                        "iflimit",
                        int,
                        50,
                        10,
                        kwargs.get("iflimit")
                    ),
                    Field(
                        "dist",
                        float,
                        60,
                        10,
                        kwargs.get("dist")
                    ),
                    Field(
                        "ilinear",
                        int,
                        70,
                        10,
                        kwargs.get("ilinear", 2)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nb1",
                        int,
                        0,
                        10,
                        kwargs.get("nb1")
                    ),
                    Field(
                        "nb2",
                        int,
                        10,
                        10,
                        kwargs.get("nb2")
                    ),
                    Field(
                        "nb3",
                        int,
                        20,
                        10,
                        kwargs.get("nb3")
                    ),
                    Field(
                        "charlen",
                        float,
                        30,
                        10,
                        kwargs.get("charlen", 150.0)
                    ),
                    Field(
                        "ndouter",
                        int,
                        40,
                        10,
                        kwargs.get("ndouter")
                    ),
                ],
            ),
        ]

    @property
    def noption(self) -> typing.Optional[int]:
        """Get or set the Flag to turn on an unfolding simulation:
        EQ.1: Activate the unfolding simulation program.
        """ # nopep8
        return self._cards[0].get_value("noption")

    @noption.setter
    def noption(self, value: int) -> None:
        self._cards[0].set_value("noption", value)

    @property
    def dvid(self) -> typing.Optional[int]:
        """Get or set the This variable is currently not being used.
        """ # nopep8
        return self._cards[0].get_value("dvid")

    @dvid.setter
    def dvid(self, value: int) -> None:
        self._cards[0].set_value("dvid", value)

    @property
    def nunbend(self) -> typing.Optional[int]:
        """Get or set the Estimated number of unbending, ranging from 10 to 100.
        """ # nopep8
        return self._cards[0].get_value("nunbend")

    @nunbend.setter
    def nunbend(self, value: int) -> None:
        self._cards[0].set_value("nunbend", value)

    @property
    def stfbend(self) -> typing.Optional[float]:
        """Get or set the Unflanging stiffness, ranging from 0.1 to 10.0.
        """ # nopep8
        return self._cards[0].get_value("stfbend")

    @stfbend.setter
    def stfbend(self, value: float) -> None:
        self._cards[0].set_value("stfbend", value)

    @property
    def stfcnt(self) -> typing.Optional[float]:
        """Get or set the Normal stiffness, ranging from 0.1 to 10.0.
        """ # nopep8
        return self._cards[0].get_value("stfcnt")

    @stfcnt.setter
    def stfcnt(self, value: float) -> None:
        self._cards[0].set_value("stfcnt", value)

    @property
    def iflimit(self) -> typing.Optional[int]:
        """Get or set the Iteration limit for the first phase of unfolding, typically ranging from 11 to 400.
        """ # nopep8
        return self._cards[0].get_value("iflimit")

    @iflimit.setter
    def iflimit(self, value: int) -> None:
        self._cards[0].set_value("iflimit", value)

    @property
    def dist(self) -> typing.Optional[float]:
        """Get or set the Distance tolerance for auto-SPC along flange root.
        """ # nopep8
        return self._cards[0].get_value("dist")

    @dist.setter
    def dist(self, value: float) -> None:
        self._cards[0].set_value("dist", value)

    @property
    def ilinear(self) -> int:
        """Get or set the Unfolding algorithm selection flag:
        EQ.0: nonlinear unfolding.
        EQ.1: linear unfolding.
        EQ.2: a new method of initial unfolding followed by nonlinear iterations (recommended).
        """ # nopep8
        return self._cards[0].get_value("ilinear")

    @ilinear.setter
    def ilinear(self, value: int) -> None:
        if value not in [2, 0, 1]:
            raise Exception("""ilinear must be one of {2,0,1}""")
        self._cards[0].set_value("ilinear", value)

    @property
    def nb1(self) -> typing.Optional[int]:
        """Get or set the The start node ID on a flange root boundary For closed-loop flange root boundary, only this parameter needs to be defined; for open-loop flange root boundary, define this parameter as well as NB2 and NB3.
        """ # nopep8
        return self._cards[1].get_value("nb1")

    @nb1.setter
    def nb1(self, value: int) -> None:
        self._cards[1].set_value("nb1", value)

    @property
    def nb2(self) -> typing.Optional[int]:
        """Get or set the The ID of a node in the middle of the flange root boundary Define this parameter for open-loop flange root boundary.
        """ # nopep8
        return self._cards[1].get_value("nb2")

    @nb2.setter
    def nb2(self, value: int) -> None:
        self._cards[1].set_value("nb2", value)

    @property
    def nb3(self) -> typing.Optional[int]:
        """Get or set the The end node ID on a flange root boundary. Define this parameter for open-loop flange root boundary.
        """ # nopep8
        return self._cards[1].get_value("nb3")

    @nb3.setter
    def nb3(self, value: int) -> None:
        self._cards[1].set_value("nb3", value)

    @property
    def charlen(self) -> float:
        """Get or set the Maximum flange height to limit the search region for the boundary nodes along the flange root.
        """ # nopep8
        return self._cards[1].get_value("charlen")

    @charlen.setter
    def charlen(self, value: float) -> None:
        self._cards[1].set_value("charlen", value)

    @property
    def ndouter(self) -> typing.Optional[int]:
        """Get or set the A node ID on the outer flange boundary.This node helps search of nodes along the flange root, especially when holes are present in the flange area.
        """ # nopep8
        return self._cards[1].get_value("ndouter")

    @ndouter.setter
    def ndouter(self, value: int) -> None:
        self._cards[1].set_value("ndouter", value)

