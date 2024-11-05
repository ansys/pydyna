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

class LoadSegmentNonuniform(KeywordBase):
    """DYNA LOAD_SEGMENT_NONUNIFORM keyword"""

    keyword = "LOAD"
    subkeyword = "SEGMENT_NONUNIFORM"

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
                    Field(
                        "heading",
                        str,
                        10,
                        70,
                        kwargs.get("heading")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcid",
                        int,
                        0,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "sf",
                        float,
                        10,
                        10,
                        kwargs.get("sf", 1.0)
                    ),
                    Field(
                        "at",
                        float,
                        20,
                        10,
                        kwargs.get("at", 0.0)
                    ),
                    Field(
                        "dt",
                        float,
                        30,
                        10,
                        kwargs.get("dt", 1E+16)
                    ),
                    Field(
                        "cid",
                        int,
                        40,
                        10,
                        kwargs.get("cid")
                    ),
                    Field(
                        "v1",
                        float,
                        50,
                        10,
                        kwargs.get("v1")
                    ),
                    Field(
                        "v2",
                        float,
                        60,
                        10,
                        kwargs.get("v2")
                    ),
                    Field(
                        "v3",
                        float,
                        70,
                        10,
                        kwargs.get("v3")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "n1",
                        int,
                        0,
                        10,
                        kwargs.get("n1")
                    ),
                    Field(
                        "n2",
                        int,
                        10,
                        10,
                        kwargs.get("n2")
                    ),
                    Field(
                        "n3",
                        int,
                        20,
                        10,
                        kwargs.get("n3")
                    ),
                    Field(
                        "n4",
                        int,
                        30,
                        10,
                        kwargs.get("n4")
                    ),
                    Field(
                        "n5",
                        int,
                        40,
                        10,
                        kwargs.get("n5")
                    ),
                    Field(
                        "n6",
                        int,
                        50,
                        10,
                        kwargs.get("n6")
                    ),
                    Field(
                        "n7",
                        int,
                        60,
                        10,
                        kwargs.get("n7")
                    ),
                    Field(
                        "n8",
                        int,
                        70,
                        10,
                        kwargs.get("n8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "p1",
                        float,
                        0,
                        10,
                        kwargs.get("p1")
                    ),
                    Field(
                        "p2",
                        float,
                        10,
                        10,
                        kwargs.get("p2")
                    ),
                    Field(
                        "p3",
                        float,
                        20,
                        10,
                        kwargs.get("p3")
                    ),
                    Field(
                        "p4",
                        float,
                        30,
                        10,
                        kwargs.get("p4")
                    ),
                    Field(
                        "p5",
                        float,
                        40,
                        10,
                        kwargs.get("p5")
                    ),
                    Field(
                        "p6",
                        float,
                        50,
                        10,
                        kwargs.get("p6")
                    ),
                    Field(
                        "p7",
                        float,
                        60,
                        10,
                        kwargs.get("p7")
                    ),
                    Field(
                        "p8",
                        float,
                        70,
                        10,
                        kwargs.get("p8")
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the loading ID
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def heading(self) -> typing.Optional[str]:
        """Get or set the A description of the loading.
        """ # nopep8
        return self._cards[0].get_value("heading")

    @heading.setter
    def heading(self, value: str) -> None:
        self._cards[0].set_value("heading", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[1].set_value("lcid", value)

    @property
    def sf(self) -> float:
        """Get or set the Load curve scale factor.
        """ # nopep8
        return self._cards[1].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        self._cards[1].set_value("sf", value)

    @property
    def at(self) -> float:
        """Get or set the Arrival/birth time for the traction load.
        """ # nopep8
        return self._cards[1].get_value("at")

    @at.setter
    def at(self, value: float) -> None:
        self._cards[1].set_value("at", value)

    @property
    def dt(self) -> float:
        """Get or set the death time for the traction load.
        """ # nopep8
        return self._cards[1].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[1].set_value("dt", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Coordinate system id.
        """ # nopep8
        return self._cards[1].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[1].set_value("cid", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Vector direction cosines  referenced to coordinate system CID to define the direction of the traction loading
        """ # nopep8
        return self._cards[1].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[1].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Vector direction cosines  referenced to coordinate system CID to define the direction of the traction loading
        """ # nopep8
        return self._cards[1].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[1].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Vector direction cosines  referenced to coordinate system CID to define the direction of the traction loading
        """ # nopep8
        return self._cards[1].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[1].set_value("v3", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Node ID
        """ # nopep8
        return self._cards[2].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        self._cards[2].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Node ID
        """ # nopep8
        return self._cards[2].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        self._cards[2].set_value("n2", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Node ID.  Repeat N2 for two-dimensional geometries
        """ # nopep8
        return self._cards[2].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        self._cards[2].set_value("n3", value)

    @property
    def n4(self) -> typing.Optional[int]:
        """Get or set the Node ID.  Repeat N2 for two-dimensional geometries or repeat N3 for triangular segments
        """ # nopep8
        return self._cards[2].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        self._cards[2].set_value("n4", value)

    @property
    def n5(self) -> typing.Optional[int]:
        """Get or set the Optional mid-side node ID located between nodes 1 and 2.
        """ # nopep8
        return self._cards[2].get_value("n5")

    @n5.setter
    def n5(self, value: int) -> None:
        self._cards[2].set_value("n5", value)

    @property
    def n6(self) -> typing.Optional[int]:
        """Get or set the Optional mid-side node ID located between nodes 2 and 3
        """ # nopep8
        return self._cards[2].get_value("n6")

    @n6.setter
    def n6(self, value: int) -> None:
        self._cards[2].set_value("n6", value)

    @property
    def n7(self) -> typing.Optional[int]:
        """Get or set the Optional mid-side node ID located between nodes 3 and 4.
        """ # nopep8
        return self._cards[2].get_value("n7")

    @n7.setter
    def n7(self, value: int) -> None:
        self._cards[2].set_value("n7", value)

    @property
    def n8(self) -> typing.Optional[int]:
        """Get or set the Optional mid-side node ID located between nodes 4 and 1.   Do not define for six node quadratic surface segments.
        """ # nopep8
        return self._cards[2].get_value("n8")

    @n8.setter
    def n8(self, value: int) -> None:
        self._cards[2].set_value("n8", value)

    @property
    def p1(self) -> typing.Optional[float]:
        """Get or set the Scale factor at node ID, N1
        """ # nopep8
        return self._cards[3].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        self._cards[3].set_value("p1", value)

    @property
    def p2(self) -> typing.Optional[float]:
        """Get or set the Scale factor at node ID, N2
        """ # nopep8
        return self._cards[3].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        self._cards[3].set_value("p2", value)

    @property
    def p3(self) -> typing.Optional[float]:
        """Get or set the Scale factor at node ID, N3
        """ # nopep8
        return self._cards[3].get_value("p3")

    @p3.setter
    def p3(self, value: float) -> None:
        self._cards[3].set_value("p3", value)

    @property
    def p4(self) -> typing.Optional[float]:
        """Get or set the Scale factor at node ID, N4
        """ # nopep8
        return self._cards[3].get_value("p4")

    @p4.setter
    def p4(self, value: float) -> None:
        self._cards[3].set_value("p4", value)

    @property
    def p5(self) -> typing.Optional[float]:
        """Get or set the Scale factor at node ID, N5
        """ # nopep8
        return self._cards[3].get_value("p5")

    @p5.setter
    def p5(self, value: float) -> None:
        self._cards[3].set_value("p5", value)

    @property
    def p6(self) -> typing.Optional[float]:
        """Get or set the Scale factor at node ID, N6
        """ # nopep8
        return self._cards[3].get_value("p6")

    @p6.setter
    def p6(self, value: float) -> None:
        self._cards[3].set_value("p6", value)

    @property
    def p7(self) -> typing.Optional[float]:
        """Get or set the Scale factor at node ID, N7
        """ # nopep8
        return self._cards[3].get_value("p7")

    @p7.setter
    def p7(self, value: float) -> None:
        self._cards[3].set_value("p7", value)

    @property
    def p8(self) -> typing.Optional[float]:
        """Get or set the Scale factor at node ID, N8
        """ # nopep8
        return self._cards[3].get_value("p8")

    @p8.setter
    def p8(self, value: float) -> None:
        self._cards[3].set_value("p8", value)

