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

"""Module providing the BoundaryRadiationSegment class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class BoundaryRadiationSegment(KeywordBase):
    """DYNA BOUNDARY_RADIATION_SEGMENT keyword"""

    keyword = "BOUNDARY"
    subkeyword = "RADIATION_SEGMENT"

    def __init__(self, **kwargs):
        """Initialize the BoundaryRadiationSegment class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "n1",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "n2",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "n3",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "n4",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "type",
                        int,
                        40,
                        10,
                        1,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "rflcid",
                        int,
                        0,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "rfmult",
                        float,
                        10,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "tilcid",
                        int,
                        20,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "timult",
                        float,
                        30,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "loc",
                        int,
                        40,
                        10,
                        0,
                        **kwargs,
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
        """Set the n1 property."""
        self._cards[0].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Second node ID defining the segment.
        """ # nopep8
        return self._cards[0].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        """Set the n2 property."""
        self._cards[0].set_value("n2", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Third node ID defining the segment.
        """ # nopep8
        return self._cards[0].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        """Set the n3 property."""
        self._cards[0].set_value("n3", value)

    @property
    def n4(self) -> typing.Optional[int]:
        """Get or set the Fourth node ID defining the segment.
        """ # nopep8
        return self._cards[0].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        """Set the n4 property."""
        self._cards[0].set_value("n4", value)

    @property
    def type(self) -> int:
        """Get or set the Radiation type:
        EQ.1: radiation boundary to environment
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        """Set the type property."""
        self._cards[0].set_value("type", value)

    @property
    def rflcid(self) -> int:
        """Get or set the Load curve ID for radiation factor f, see *DEFINE_CURVE.
        GT.0: function versus time,
        EQ.0: use constant multiplier value, RFMULT (default),
        LT.0: function versus temperature.
        """ # nopep8
        return self._cards[1].get_value("rflcid")

    @rflcid.setter
    def rflcid(self, value: int) -> None:
        """Set the rflcid property."""
        self._cards[1].set_value("rflcid", value)

    @property
    def rfmult(self) -> float:
        """Get or set the Curve multiplier for f, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("rfmult")

    @rfmult.setter
    def rfmult(self, value: float) -> None:
        """Set the rfmult property."""
        self._cards[1].set_value("rfmult", value)

    @property
    def tilcid(self) -> int:
        """Get or set the Load curve ID for T-infinity versus time, see *DEFINE_CURVE.
        EQ.0: use constant multiplier, TIMULT (default).
        """ # nopep8
        return self._cards[1].get_value("tilcid")

    @tilcid.setter
    def tilcid(self, value: int) -> None:
        """Set the tilcid property."""
        self._cards[1].set_value("tilcid", value)

    @property
    def timult(self) -> float:
        """Get or set the Curve multiplier for T-infinity.
        """ # nopep8
        return self._cards[1].get_value("timult")

    @timult.setter
    def timult(self, value: float) -> None:
        """Set the timult property."""
        self._cards[1].set_value("timult", value)

    @property
    def loc(self) -> int:
        """Get or set the Application of surface for thermal shell elements, see paramter, TSHELL, in the *CONTROL_SHELL input:
        EQ.-1: lower surface of thermal shell element,
        EQ. 1: upper surface of thermal shell element
        """ # nopep8
        return self._cards[1].get_value("loc")

    @loc.setter
    def loc(self, value: int) -> None:
        """Set the loc property."""
        if value not in [0, -1, 1, None]:
            raise Exception("""loc must be `None` or one of {0,-1,1}.""")
        self._cards[1].set_value("loc", value)

