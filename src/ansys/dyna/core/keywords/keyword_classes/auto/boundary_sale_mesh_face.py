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

class BoundarySaleMeshFace(KeywordBase):
    """DYNA BOUNDARY_SALE_MESH_FACE keyword"""

    keyword = "BOUNDARY"
    subkeyword = "SALE_MESH_FACE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "option",
                        str,
                        0,
                        10,
                        kwargs.get("option", "FIXED")
                    ),
                    Field(
                        "mshid",
                        int,
                        10,
                        10,
                        kwargs.get("mshid")
                    ),
                    Field(
                        "-x",
                        int,
                        20,
                        10,
                        kwargs.get("-x", 0)
                    ),
                    Field(
                        "+x",
                        int,
                        30,
                        10,
                        kwargs.get("+x", 0)
                    ),
                    Field(
                        "-y",
                        int,
                        40,
                        10,
                        kwargs.get("-y", 0)
                    ),
                    Field(
                        "+y",
                        int,
                        50,
                        10,
                        kwargs.get("+y", 0)
                    ),
                    Field(
                        "-z",
                        int,
                        60,
                        10,
                        kwargs.get("-z", 0)
                    ),
                    Field(
                        "-z",
                        int,
                        70,
                        10,
                        kwargs.get("-z", 0)
                    ),
                ],
            ),
        ]

    @property
    def option(self) -> str:
        """Get or set the There are 3 options.
        FIXED: All nodes at the face are fixed at all directions
        NOFLOW : No flow allowed through the face
        SYMM : The face is a symmetric plane(same as NOFLOW)
        NONREFL : Non - reflective boundary condition.
        """ # nopep8
        return self._cards[0].get_value("option")

    @option.setter
    def option(self, value: str) -> None:
        if value not in ["FIXED", "NOEFLOW", "SYMM", "NONREFL"]:
            raise Exception("""option must be one of {"FIXED","NOEFLOW","SYMM","NONREFL"}""")
        self._cards[0].set_value("option", value)

    @property
    def mshid(self) -> typing.Optional[int]:
        """Get or set the S-ALE Mesh ID
        """ # nopep8
        return self._cards[0].get_value("mshid")

    @mshid.setter
    def mshid(self, value: int) -> None:
        self._cards[0].set_value("mshid", value)

    @property
    def _x(self) -> int:
        """Get or set the Flags controlling ON/OFF at each S-ALE mesh face.
        EQ 0: OFF
        EQ 1 : ON
        """ # nopep8
        return self._cards[0].get_value("-x")

    @_x.setter
    def _x(self, value: int) -> None:
        self._cards[0].set_value("-x", value)

    @property
    def _x(self) -> int:
        """Get or set the Flags controlling ON/OFF at each S-ALE mesh face.
        EQ 0: OFF
        EQ 1 : ON
        """ # nopep8
        return self._cards[0].get_value("+x")

    @_x.setter
    def _x(self, value: int) -> None:
        self._cards[0].set_value("+x", value)

    @property
    def _y(self) -> int:
        """Get or set the Flags controlling ON/OFF at each S-ALE mesh face.
        EQ 0: OFF
        EQ 1 : ON
        """ # nopep8
        return self._cards[0].get_value("-y")

    @_y.setter
    def _y(self, value: int) -> None:
        self._cards[0].set_value("-y", value)

    @property
    def _y(self) -> int:
        """Get or set the Flags controlling ON/OFF at each S-ALE mesh face.
        EQ 0: OFF
        EQ 1 : ON
        """ # nopep8
        return self._cards[0].get_value("+y")

    @_y.setter
    def _y(self, value: int) -> None:
        self._cards[0].set_value("+y", value)

    @property
    def _z(self) -> int:
        """Get or set the Flags controlling ON/OFF at each S-ALE mesh face.
        EQ 0: OFF
        EQ 1 : ON
        """ # nopep8
        return self._cards[0].get_value("-z")

    @_z.setter
    def _z(self, value: int) -> None:
        self._cards[0].set_value("-z", value)

    @property
    def _z(self) -> int:
        """Get or set the Flags controlling ON/OFF at each S-ALE mesh face.
        EQ 0: OFF
        EQ 1 : ON
        """ # nopep8
        return self._cards[0].get_value("-z")

    @_z.setter
    def _z(self, value: int) -> None:
        self._cards[0].set_value("-z", value)

