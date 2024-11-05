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

class BoundaryConvectionSegment(KeywordBase):
    """DYNA BOUNDARY_CONVECTION_SEGMENT keyword"""

    keyword = "BOUNDARY"
    subkeyword = "CONVECTION_SEGMENT"

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
                ],
            ),
            Card(
                [
                    Field(
                        "hlcid",
                        int,
                        0,
                        10,
                        kwargs.get("hlcid")
                    ),
                    Field(
                        "hmult",
                        float,
                        10,
                        10,
                        kwargs.get("hmult", 1.0)
                    ),
                    Field(
                        "tlcid",
                        int,
                        20,
                        10,
                        kwargs.get("tlcid")
                    ),
                    Field(
                        "tmult",
                        float,
                        30,
                        10,
                        kwargs.get("tmult", 1.0)
                    ),
                    Field(
                        "loc",
                        int,
                        40,
                        10,
                        kwargs.get("loc", 0)
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
        """Get or set the Third Node ID's defining segment.
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
    def hlcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for heat transfer coefficient, h:
        GT.0: function versus time,
        EQ.0: use constant multiplier value, HMULT,
        LT.0: function versus temperature.
        """ # nopep8
        return self._cards[1].get_value("hlcid")

    @hlcid.setter
    def hlcid(self, value: int) -> None:
        self._cards[1].set_value("hlcid", value)

    @property
    def hmult(self) -> float:
        """Get or set the Curve multiplier for h.
        """ # nopep8
        return self._cards[1].get_value("hmult")

    @hmult.setter
    def hmult(self, value: float) -> None:
        self._cards[1].set_value("hmult", value)

    @property
    def tlcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for T-infinity versus time, see *DEFINE_CURVE:
        EQ.0: use constant multiplier value, TMULT.
        """ # nopep8
        return self._cards[1].get_value("tlcid")

    @tlcid.setter
    def tlcid(self, value: int) -> None:
        self._cards[1].set_value("tlcid", value)

    @property
    def tmult(self) -> float:
        """Get or set the Curve multiplier for T-infinity.
        """ # nopep8
        return self._cards[1].get_value("tmult")

    @tmult.setter
    def tmult(self, value: float) -> None:
        self._cards[1].set_value("tmult", value)

    @property
    def loc(self) -> int:
        """Get or set the LOC Application of surface for thermal shell elements, see paramter, TSHELL, in the *CONTROL_SHELL input::
        EQ.-1: lower surface of thermal shell element,
        EQ. 1: upper surface of thermal shell element.
        """ # nopep8
        return self._cards[1].get_value("loc")

    @loc.setter
    def loc(self, value: int) -> None:
        if value not in [0, -1, 1]:
            raise Exception("""loc must be one of {0,-1,1}""")
        self._cards[1].set_value("loc", value)

