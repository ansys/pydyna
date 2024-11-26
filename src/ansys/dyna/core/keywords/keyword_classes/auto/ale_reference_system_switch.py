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

class AleReferenceSystemSwitch(KeywordBase):
    """DYNA ALE_REFERENCE_SYSTEM_SWITCH keyword"""

    keyword = "ALE"
    subkeyword = "REFERENCE_SYSTEM_SWITCH"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "id",
                        int,
                        0,
                        10,
                        kwargs.get("id")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "t1",
                        float,
                        0,
                        10,
                        kwargs.get("t1", 0.0)
                    ),
                    Field(
                        "t2",
                        float,
                        10,
                        10,
                        kwargs.get("t2", 0.0)
                    ),
                    Field(
                        "t3",
                        float,
                        20,
                        10,
                        kwargs.get("t3", 0.0)
                    ),
                    Field(
                        "t4",
                        float,
                        30,
                        10,
                        kwargs.get("t4", 0.0)
                    ),
                    Field(
                        "t5",
                        float,
                        40,
                        10,
                        kwargs.get("t5", 0.0)
                    ),
                    Field(
                        "t6",
                        float,
                        50,
                        10,
                        kwargs.get("t6", 0.0)
                    ),
                    Field(
                        "t7",
                        float,
                        60,
                        10,
                        kwargs.get("t7", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "type1",
                        int,
                        0,
                        10,
                        kwargs.get("type1", 0)
                    ),
                    Field(
                        "type2",
                        int,
                        10,
                        10,
                        kwargs.get("type2", 0)
                    ),
                    Field(
                        "type3",
                        int,
                        20,
                        10,
                        kwargs.get("type3", 0)
                    ),
                    Field(
                        "type4",
                        int,
                        30,
                        10,
                        kwargs.get("type4", 0)
                    ),
                    Field(
                        "type5",
                        int,
                        40,
                        10,
                        kwargs.get("type5", 0)
                    ),
                    Field(
                        "type6",
                        int,
                        50,
                        10,
                        kwargs.get("type6", 0)
                    ),
                    Field(
                        "type7",
                        int,
                        60,
                        10,
                        kwargs.get("type7", 0)
                    ),
                    Field(
                        "type8",
                        int,
                        70,
                        10,
                        kwargs.get("type8", 0)
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
    def id(self) -> typing.Optional[int]:
        """Get or set the Switch list ID, see *ALE_REFERENCE_SYSTEM_GROUP.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def t1(self) -> float:
        """Get or set the Time one for switching reference system type.
        """ # nopep8
        return self._cards[1].get_value("t1")

    @t1.setter
    def t1(self, value: float) -> None:
        self._cards[1].set_value("t1", value)

    @property
    def t2(self) -> float:
        """Get or set the Time two for switching reference system type.
        """ # nopep8
        return self._cards[1].get_value("t2")

    @t2.setter
    def t2(self, value: float) -> None:
        self._cards[1].set_value("t2", value)

    @property
    def t3(self) -> float:
        """Get or set the Time three for switching reference system type.
        """ # nopep8
        return self._cards[1].get_value("t3")

    @t3.setter
    def t3(self, value: float) -> None:
        self._cards[1].set_value("t3", value)

    @property
    def t4(self) -> float:
        """Get or set the Time four for switching reference system type.
        """ # nopep8
        return self._cards[1].get_value("t4")

    @t4.setter
    def t4(self, value: float) -> None:
        self._cards[1].set_value("t4", value)

    @property
    def t5(self) -> float:
        """Get or set the Time five for switching reference system type.
        """ # nopep8
        return self._cards[1].get_value("t5")

    @t5.setter
    def t5(self, value: float) -> None:
        self._cards[1].set_value("t5", value)

    @property
    def t6(self) -> float:
        """Get or set the Time six for switching reference system type.
        """ # nopep8
        return self._cards[1].get_value("t6")

    @t6.setter
    def t6(self, value: float) -> None:
        self._cards[1].set_value("t6", value)

    @property
    def t7(self) -> float:
        """Get or set the Time seven for switching reference system type.
        """ # nopep8
        return self._cards[1].get_value("t7")

    @t7.setter
    def t7(self, value: float) -> None:
        self._cards[1].set_value("t7", value)

    @property
    def type1(self) -> int:
        """Get or set the Reference system types:
        EQ.0: Eulerian,
        EQ.1: Lagrangian,
        EQ.2: Normal ALE mesh smoothing,
        EQ.3: Prescribed motion following load curves, see *ALE_REFERENCE_SYSTEM_CURVE,
        EQ.4: Automatic mesh motion following mass weighted average velocity in ALE mesh,
        EQ.5: Automatic mesh motion following coordinate system defined by three user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE,
        EQ.7: Automatic mesh expansion in order to enclose up to twelve user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE.
        """ # nopep8
        return self._cards[2].get_value("type1")

    @type1.setter
    def type1(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 7]:
            raise Exception("""type1 must be one of {0,1,2,3,4,5,7}""")
        self._cards[2].set_value("type1", value)

    @property
    def type2(self) -> int:
        """Get or set the Reference system types:
        EQ.0: Eulerian,
        EQ.1: Lagrangian,
        EQ.2: Normal ALE mesh smoothing,
        EQ.3: Prescribed motion following load curves, see *ALE_REFERENCE_SYSTEM_CURVE,
        EQ.4: Automatic mesh motion following mass weighted average velocity in ALE mesh,
        EQ.5: Automatic mesh motion following coordinate system defined by three user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE,
        EQ.7: Automatic mesh expansion in order to enclose up to twelve user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE.
        """ # nopep8
        return self._cards[2].get_value("type2")

    @type2.setter
    def type2(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 7]:
            raise Exception("""type2 must be one of {0,1,2,3,4,5,7}""")
        self._cards[2].set_value("type2", value)

    @property
    def type3(self) -> int:
        """Get or set the Reference system types:
        EQ.0: Eulerian,
        EQ.1: Lagrangian,
        EQ.2: Normal ALE mesh smoothing,
        EQ.3: Prescribed motion following load curves, see *ALE_REFERENCE_SYSTEM_CURVE,
        EQ.4: Automatic mesh motion following mass weighted average velocity in ALE mesh,
        EQ.5: Automatic mesh motion following coordinate system defined by three user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE,
        EQ.7: Automatic mesh expansion in order to enclose up to twelve user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE.
        """ # nopep8
        return self._cards[2].get_value("type3")

    @type3.setter
    def type3(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 7]:
            raise Exception("""type3 must be one of {0,1,2,3,4,5,7}""")
        self._cards[2].set_value("type3", value)

    @property
    def type4(self) -> int:
        """Get or set the Reference system types:
        EQ.0: Eulerian,
        EQ.1: Lagrangian,
        EQ.2: Normal ALE mesh smoothing,
        EQ.3: Prescribed motion following load curves, see *ALE_REFERENCE_SYSTEM_CURVE,
        EQ.4: Automatic mesh motion following mass weighted average velocity in ALE mesh,
        EQ.5: Automatic mesh motion following coordinate system defined by three user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE,
        EQ.7: Automatic mesh expansion in order to enclose up to twelve user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE.
        """ # nopep8
        return self._cards[2].get_value("type4")

    @type4.setter
    def type4(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 7]:
            raise Exception("""type4 must be one of {0,1,2,3,4,5,7}""")
        self._cards[2].set_value("type4", value)

    @property
    def type5(self) -> int:
        """Get or set the Reference system types:
        EQ.0: Eulerian,
        EQ.1: Lagrangian,
        EQ.2: Normal ALE mesh smoothing,
        EQ.3: Prescribed motion following load curves, see *ALE_REFERENCE_SYSTEM_CURVE,
        EQ.4: Automatic mesh motion following mass weighted average velocity in ALE mesh,
        EQ.5: Automatic mesh motion following coordinate system defined by three user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE,
        EQ.7: Automatic mesh expansion in order to enclose up to twelve user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE.
        """ # nopep8
        return self._cards[2].get_value("type5")

    @type5.setter
    def type5(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 7]:
            raise Exception("""type5 must be one of {0,1,2,3,4,5,7}""")
        self._cards[2].set_value("type5", value)

    @property
    def type6(self) -> int:
        """Get or set the Reference system types:
        EQ.0: Eulerian,
        EQ.1: Lagrangian,
        EQ.2: Normal ALE mesh smoothing,
        EQ.3: Prescribed motion following load curves, see *ALE_REFERENCE_SYSTEM_CURVE,
        EQ.4: Automatic mesh motion following mass weighted average velocity in ALE mesh,
        EQ.5: Automatic mesh motion following coordinate system defined by three user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE,
        EQ.7: Automatic mesh expansion in order to enclose up to twelve user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE.
        """ # nopep8
        return self._cards[2].get_value("type6")

    @type6.setter
    def type6(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 7]:
            raise Exception("""type6 must be one of {0,1,2,3,4,5,7}""")
        self._cards[2].set_value("type6", value)

    @property
    def type7(self) -> int:
        """Get or set the Reference system types:
        EQ.0: Eulerian,
        EQ.1: Lagrangian,
        EQ.2: Normal ALE mesh smoothing,
        EQ.3: Prescribed motion following load curves, see *ALE_REFERENCE_SYSTEM_CURVE,
        EQ.4: Automatic mesh motion following mass weighted average velocity in ALE mesh,
        EQ.5: Automatic mesh motion following coordinate system defined by three user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE,
        EQ.7: Automatic mesh expansion in order to enclose up to twelve user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE.
        """ # nopep8
        return self._cards[2].get_value("type7")

    @type7.setter
    def type7(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 7]:
            raise Exception("""type7 must be one of {0,1,2,3,4,5,7}""")
        self._cards[2].set_value("type7", value)

    @property
    def type8(self) -> int:
        """Get or set the Reference system types:
        EQ.0: Eulerian,
        EQ.1: Lagrangian,
        EQ.2: Normal ALE mesh smoothing,
        EQ.3: Prescribed motion following load curves, see *ALE_REFERENCE_SYSTEM_CURVE,
        EQ.4: Automatic mesh motion following mass weighted average velocity in ALE mesh,
        EQ.5: Automatic mesh motion following coordinate system defined by three user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE,
        EQ.7: Automatic mesh expansion in order to enclose up to twelve user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE.
        """ # nopep8
        return self._cards[2].get_value("type8")

    @type8.setter
    def type8(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 7]:
            raise Exception("""type8 must be one of {0,1,2,3,4,5,7}""")
        self._cards[2].set_value("type8", value)

    @property
    def id1(self) -> typing.Optional[int]:
        """Get or set the ID of node or curve group (PRTYPE 3, 5, or 7).
        """ # nopep8
        return self._cards[3].get_value("id1")

    @id1.setter
    def id1(self, value: int) -> None:
        self._cards[3].set_value("id1", value)

    @property
    def id2(self) -> typing.Optional[int]:
        """Get or set the ID of node or curve group (PRTYPE 3, 5, or 7).
        """ # nopep8
        return self._cards[3].get_value("id2")

    @id2.setter
    def id2(self, value: int) -> None:
        self._cards[3].set_value("id2", value)

    @property
    def id3(self) -> typing.Optional[int]:
        """Get or set the ID of node or curve group (PRTYPE 3, 5, or 7).
        """ # nopep8
        return self._cards[3].get_value("id3")

    @id3.setter
    def id3(self, value: int) -> None:
        self._cards[3].set_value("id3", value)

    @property
    def id4(self) -> typing.Optional[int]:
        """Get or set the ID of node or curve group (PRTYPE 3, 5, or 7).
        """ # nopep8
        return self._cards[3].get_value("id4")

    @id4.setter
    def id4(self, value: int) -> None:
        self._cards[3].set_value("id4", value)

    @property
    def id5(self) -> typing.Optional[int]:
        """Get or set the ID of node or curve group (PRTYPE 3, 5, or 7).
        """ # nopep8
        return self._cards[3].get_value("id5")

    @id5.setter
    def id5(self, value: int) -> None:
        self._cards[3].set_value("id5", value)

    @property
    def id6(self) -> typing.Optional[int]:
        """Get or set the ID of node or curve group (PRTYPE 3, 5, or 7).
        """ # nopep8
        return self._cards[3].get_value("id6")

    @id6.setter
    def id6(self, value: int) -> None:
        self._cards[3].set_value("id6", value)

    @property
    def id7(self) -> typing.Optional[int]:
        """Get or set the ID of node or curve group (PRTYPE 3, 5, or 7).
        """ # nopep8
        return self._cards[3].get_value("id7")

    @id7.setter
    def id7(self, value: int) -> None:
        self._cards[3].set_value("id7", value)

    @property
    def id8(self) -> typing.Optional[int]:
        """Get or set the ID of node or curve group (PRTYPE 3, 5, or 7).
        """ # nopep8
        return self._cards[3].get_value("id8")

    @id8.setter
    def id8(self, value: int) -> None:
        self._cards[3].set_value("id8", value)

