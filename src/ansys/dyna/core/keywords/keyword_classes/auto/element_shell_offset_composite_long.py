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

class ElementShellOffsetCompositeLong(KeywordBase):
    """DYNA ELEMENT_SHELL_OFFSET_COMPOSITE_LONG keyword"""

    keyword = "ELEMENT"
    subkeyword = "SHELL_OFFSET_COMPOSITE_LONG"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "eid",
                        int,
                        0,
                        8,
                        kwargs.get("eid")
                    ),
                    Field(
                        "pid",
                        int,
                        8,
                        8,
                        kwargs.get("pid")
                    ),
                    Field(
                        "n1",
                        int,
                        16,
                        8,
                        kwargs.get("n1")
                    ),
                    Field(
                        "n2",
                        int,
                        24,
                        8,
                        kwargs.get("n2")
                    ),
                    Field(
                        "n3",
                        int,
                        32,
                        8,
                        kwargs.get("n3")
                    ),
                    Field(
                        "n4",
                        int,
                        40,
                        8,
                        kwargs.get("n4")
                    ),
                    Field(
                        "n5",
                        int,
                        48,
                        8,
                        kwargs.get("n5")
                    ),
                    Field(
                        "n6",
                        int,
                        56,
                        8,
                        kwargs.get("n6")
                    ),
                    Field(
                        "n7",
                        int,
                        64,
                        8,
                        kwargs.get("n7")
                    ),
                    Field(
                        "n8",
                        int,
                        72,
                        8,
                        kwargs.get("n8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "offset",
                        float,
                        0,
                        16,
                        kwargs.get("offset", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "mid1",
                        int,
                        0,
                        10,
                        kwargs.get("mid1")
                    ),
                    Field(
                        "thick1",
                        float,
                        10,
                        10,
                        kwargs.get("thick1")
                    ),
                    Field(
                        "b1",
                        float,
                        20,
                        10,
                        kwargs.get("b1")
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "plyid1",
                        int,
                        40,
                        10,
                        kwargs.get("plyid1")
                    ),
                ],
            ),
        ]

    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Element ID. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        self._cards[0].set_value("eid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID, see *PART.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Nodal point 1.
        """ # nopep8
        return self._cards[0].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        self._cards[0].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Nodal point 2.
        """ # nopep8
        return self._cards[0].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        self._cards[0].set_value("n2", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Nodal point 3.
        """ # nopep8
        return self._cards[0].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        self._cards[0].set_value("n3", value)

    @property
    def n4(self) -> typing.Optional[int]:
        """Get or set the Nodal point 4.
        """ # nopep8
        return self._cards[0].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        self._cards[0].set_value("n4", value)

    @property
    def n5(self) -> typing.Optional[int]:
        """Get or set the Mid side nodal point 5.
        """ # nopep8
        return self._cards[0].get_value("n5")

    @n5.setter
    def n5(self, value: int) -> None:
        self._cards[0].set_value("n5", value)

    @property
    def n6(self) -> typing.Optional[int]:
        """Get or set the Mid side nodal point 6.
        """ # nopep8
        return self._cards[0].get_value("n6")

    @n6.setter
    def n6(self, value: int) -> None:
        self._cards[0].set_value("n6", value)

    @property
    def n7(self) -> typing.Optional[int]:
        """Get or set the Mid side nodal point 7.
        """ # nopep8
        return self._cards[0].get_value("n7")

    @n7.setter
    def n7(self, value: int) -> None:
        self._cards[0].set_value("n7", value)

    @property
    def n8(self) -> typing.Optional[int]:
        """Get or set the Mid side nodal point 8.
        """ # nopep8
        return self._cards[0].get_value("n8")

    @n8.setter
    def n8(self, value: int) -> None:
        self._cards[0].set_value("n8", value)

    @property
    def offset(self) -> float:
        """Get or set the The offset distance from the nodal points to the reference surface of the shell in the direction of the normal vector to the shell
        """ # nopep8
        return self._cards[1].get_value("offset")

    @offset.setter
    def offset(self, value: float) -> None:
        self._cards[1].set_value("offset", value)

    @property
    def mid1(self) -> typing.Optional[int]:
        """Get or set the Material ID of integration point i, see *MAT_ . Section.
        """ # nopep8
        return self._cards[2].get_value("mid1")

    @mid1.setter
    def mid1(self, value: int) -> None:
        self._cards[2].set_value("mid1", value)

    @property
    def thick1(self) -> typing.Optional[float]:
        """Get or set the Thickness of integration point i.
        """ # nopep8
        return self._cards[2].get_value("thick1")

    @thick1.setter
    def thick1(self, value: float) -> None:
        self._cards[2].set_value("thick1", value)

    @property
    def b1(self) -> typing.Optional[float]:
        """Get or set the Material angle of integration point i.
        """ # nopep8
        return self._cards[2].get_value("b1")

    @b1.setter
    def b1(self, value: float) -> None:
        self._cards[2].set_value("b1", value)

    @property
    def plyid1(self) -> typing.Optional[int]:
        """Get or set the Ply ID for integration point i (for post-processing purposes).
        """ # nopep8
        return self._cards[2].get_value("plyid1")

    @plyid1.setter
    def plyid1(self, value: int) -> None:
        self._cards[2].set_value("plyid1", value)
