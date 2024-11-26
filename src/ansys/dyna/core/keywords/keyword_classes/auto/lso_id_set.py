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

class LsoIdSet(KeywordBase):
    """DYNA LSO_ID_SET keyword"""

    keyword = "LSO"
    subkeyword = "ID_SET"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "setid",
                        int,
                        0,
                        10,
                        kwargs.get("setid")
                    ),
                    Field(
                        "type",
                        str,
                        10,
                        20,
                        kwargs.get("type", "SEG_SETS")
                    ),
                    Field(
                        "solver",
                        str,
                        20,
                        20,
                        kwargs.get("solver", "MECH")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "id1",
                        int,
                        0,
                        10,
                        kwargs.get("id1")
                    ),
                    Field(
                        "id2",
                        int,
                        10,
                        10,
                        kwargs.get("id2")
                    ),
                    Field(
                        "id3",
                        int,
                        20,
                        10,
                        kwargs.get("id3")
                    ),
                    Field(
                        "id4",
                        int,
                        30,
                        10,
                        kwargs.get("id4")
                    ),
                    Field(
                        "id5",
                        int,
                        40,
                        10,
                        kwargs.get("id5")
                    ),
                    Field(
                        "id6",
                        int,
                        50,
                        10,
                        kwargs.get("id6")
                    ),
                    Field(
                        "id7",
                        int,
                        60,
                        10,
                        kwargs.get("id7")
                    ),
                    Field(
                        "id8",
                        int,
                        70,
                        10,
                        kwargs.get("id8")
                    ),
                ],
            ),
        ]

    @property
    def setid(self) -> typing.Optional[int]:
        """Get or set the Identifier for this ID set.
        """ # nopep8
        return self._cards[0].get_value("setid")

    @setid.setter
    def setid(self, value: int) -> None:
        self._cards[0].set_value("setid", value)

    @property
    def type(self) -> str:
        """Get or set the The kind of IDs in this set:
        EQ.SEG_SETS: Each ID is a segment set connected with SOLVER.
        EQ.CIRCUIT: Each ID is a circuit ID (from *EM cards)
        EQ.SURF_PARTS: Each ID is a surface part number (See*MESH_SURFACE_ELEMENT)
        EQ.VOL_PARTS: Each ID is a volume part number (See *MESH_VOLUME)
        EQ.SURF_ELES: Each ID is a surface element number (See *MESH_SURFACE_ELEMENT)
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: str) -> None:
        if value not in ["SEG_SETS", "CIRCUIT", "SURF_PARTS", "VOL_PARTS", "SURF_ELES"]:
            raise Exception("""type must be one of {"SEG_SETS","CIRCUIT","SURF_PARTS","VOL_PARTS","SURF_ELES"}""")
        self._cards[0].set_value("type", value)

    @property
    def solver(self) -> str:
        """Get or set the Name of the solver (MECH, ICFD, CESE, EM,  ).
        """ # nopep8
        return self._cards[0].get_value("solver")

    @solver.setter
    def solver(self, value: str) -> None:
        if value not in ["MECH", "ICFD", "CESE", "EM"]:
            raise Exception("""solver must be one of {"MECH","ICFD","CESE","EM"}""")
        self._cards[0].set_value("solver", value)

    @property
    def id1(self) -> typing.Optional[int]:
        """Get or set the IDs of the TYPE kind.
        """ # nopep8
        return self._cards[1].get_value("id1")

    @id1.setter
    def id1(self, value: int) -> None:
        self._cards[1].set_value("id1", value)

    @property
    def id2(self) -> typing.Optional[int]:
        """Get or set the IDs of the TYPE kind.
        """ # nopep8
        return self._cards[1].get_value("id2")

    @id2.setter
    def id2(self, value: int) -> None:
        self._cards[1].set_value("id2", value)

    @property
    def id3(self) -> typing.Optional[int]:
        """Get or set the IDs of the TYPE kind.
        """ # nopep8
        return self._cards[1].get_value("id3")

    @id3.setter
    def id3(self, value: int) -> None:
        self._cards[1].set_value("id3", value)

    @property
    def id4(self) -> typing.Optional[int]:
        """Get or set the IDs of the TYPE kind.
        """ # nopep8
        return self._cards[1].get_value("id4")

    @id4.setter
    def id4(self, value: int) -> None:
        self._cards[1].set_value("id4", value)

    @property
    def id5(self) -> typing.Optional[int]:
        """Get or set the IDs of the TYPE kind.
        """ # nopep8
        return self._cards[1].get_value("id5")

    @id5.setter
    def id5(self, value: int) -> None:
        self._cards[1].set_value("id5", value)

    @property
    def id6(self) -> typing.Optional[int]:
        """Get or set the IDs of the TYPE kind.
        """ # nopep8
        return self._cards[1].get_value("id6")

    @id6.setter
    def id6(self, value: int) -> None:
        self._cards[1].set_value("id6", value)

    @property
    def id7(self) -> typing.Optional[int]:
        """Get or set the IDs of the TYPE kind.
        """ # nopep8
        return self._cards[1].get_value("id7")

    @id7.setter
    def id7(self, value: int) -> None:
        self._cards[1].set_value("id7", value)

    @property
    def id8(self) -> typing.Optional[int]:
        """Get or set the IDs of the TYPE kind.
        """ # nopep8
        return self._cards[1].get_value("id8")

    @id8.setter
    def id8(self, value: int) -> None:
        self._cards[1].set_value("id8", value)

