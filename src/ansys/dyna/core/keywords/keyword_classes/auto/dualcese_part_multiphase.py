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

class DualcesePartMultiphase(KeywordBase):
    """DYNA DUALCESE_PART_MULTIPHASE keyword"""

    keyword = "DUALCESE"
    subkeyword = "PART_MULTIPHASE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "react_id",
                        int,
                        10,
                        10,
                        kwargs.get("react_id")
                    ),
                    Field(
                        "eossid",
                        int,
                        20,
                        10,
                        kwargs.get("eossid")
                    ),
                    Field(
                        "mid",
                        int,
                        30,
                        10,
                        kwargs.get("mid")
                    ),
                    Field(
                        "fsitype",
                        str,
                        40,
                        10,
                        kwargs.get("fsitype")
                    ),
                    Field(
                        "mmshid",
                        int,
                        50,
                        10,
                        kwargs.get("mmshid")
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID (must be different from any PID on a *DUALCESE_PART card)
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def react_id(self) -> typing.Optional[int]:
        """Get or set the ID of chemical reaction rate model (see *DUALCESE_REACTION_RATE_... cards)
        """ # nopep8
        return self._cards[0].get_value("react_id")

    @react_id.setter
    def react_id(self, value: int) -> None:
        self._cards[0].set_value("react_id", value)

    @property
    def eossid(self) -> typing.Optional[int]:
        """Get or set the Set ID of multiphase EOS set specification (see *DUALCESE_EOS_SET)
        """ # nopep8
        return self._cards[0].get_value("eossid")

    @eossid.setter
    def eossid(self, value: int) -> None:
        self._cards[0].set_value("eossid", value)

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material ID defined by a *DUALCESE_MAT_... card
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def fsitype(self) -> typing.Optional[str]:
        """Get or set the FSI type to use on this part:
        EQ.<BLANK>:	If left blank, no FSI is performed.
        EQ.IBM:	Immersed boundary FSI solver
        EQ.MOVMESH:	Moving mesh FSI solver(FSITYPE = MMM may also be used for the same effect)
        """ # nopep8
        return self._cards[0].get_value("fsitype")

    @fsitype.setter
    def fsitype(self, value: str) -> None:
        self._cards[0].set_value("fsitype", value)

    @property
    def mmshid(self) -> typing.Optional[int]:
        """Get or set the ID of the mesh motion algorithm to use for the moving mesh FSI solver on this part (region of the current dual CESE mesh).  This ID refers to a *DUALCESE_CONTROL_MESH_MOV card ID.
        """ # nopep8
        return self._cards[0].get_value("mmshid")

    @mmshid.setter
    def mmshid(self, value: int) -> None:
        self._cards[0].set_value("mmshid", value)

