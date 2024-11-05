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

class BoundaryRadiationSegmentVfRead(KeywordBase):
    """DYNA BOUNDARY_RADIATION_SEGMENT_VF_READ keyword"""

    keyword = "BOUNDARY"
    subkeyword = "RADIATION_SEGMENT_VF_READ"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
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
                        "type",
                        int,
                        40,
                        10,
                        kwargs.get("type", 2)
                    ),
                    Field(
                        "block",
                        int,
                        50,
                        10,
                        kwargs.get("block", 0)
                    ),
                    Field(
                        "nint",
                        int,
                        60,
                        10,
                        kwargs.get("nint", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "selcid",
                        int,
                        0,
                        10,
                        kwargs.get("selcid", 0)
                    ),
                    Field(
                        "semult",
                        float,
                        10,
                        10,
                        kwargs.get("semult", 1.0)
                    ),
                ],
            ),
        ]

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the First node ID defining the segment.
        """ # nopep8
        return self._cards[0].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        self._cards[0].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Second node ID defining the segment.
        """ # nopep8
        return self._cards[0].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        self._cards[0].set_value("n2", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Third node ID defining the segment.
        """ # nopep8
        return self._cards[0].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        self._cards[0].set_value("n3", value)

    @property
    def n4(self) -> typing.Optional[int]:
        """Get or set the Fourth node ID defining the segment.
        """ # nopep8
        return self._cards[0].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        self._cards[0].set_value("n4", value)

    @property
    def type(self) -> int:
        """Get or set the Radiation type:
        EQ.2: Radiation within an enclosure.
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        self._cards[0].set_value("type", value)

    @property
    def block(self) -> int:
        """Get or set the Flag indicating if this surface blocks the view between any other 2 surfaces.
        EQ.0: no blocking (default)
        EQ.1: blocking.
        """ # nopep8
        return self._cards[0].get_value("block")

    @block.setter
    def block(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""block must be one of {0,1}""")
        self._cards[0].set_value("block", value)

    @property
    def nint(self) -> int:
        """Get or set the Number of integration points for viewfactor calculation.
        EQ.0: LS-DYNA determines the number of integration points based on the segment size and separation distance
        1 <= NINT <= 10: User specified number.
        """ # nopep8
        return self._cards[0].get_value("nint")

    @nint.setter
    def nint(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]:
            raise Exception("""nint must be one of {0,1,2,3,4,5,6,7,8,9,10}""")
        self._cards[0].set_value("nint", value)

    @property
    def selcid(self) -> int:
        """Get or set the Load curve ID for surface emissivity, see *DEFINE_CURVE.
        GT.0: function versus time,
        EQ.0: use constant multiplier value, SEMULT (default),
        LT.0: function versus temperature.
        """ # nopep8
        return self._cards[1].get_value("selcid")

    @selcid.setter
    def selcid(self, value: int) -> None:
        self._cards[1].set_value("selcid", value)

    @property
    def semult(self) -> float:
        """Get or set the Curve multiplier for surface emissivity, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("semult")

    @semult.setter
    def semult(self, value: float) -> None:
        self._cards[1].set_value("semult", value)

