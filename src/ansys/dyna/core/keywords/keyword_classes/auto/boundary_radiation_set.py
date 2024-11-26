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

class BoundaryRadiationSet(KeywordBase):
    """DYNA BOUNDARY_RADIATION_SET keyword"""

    keyword = "BOUNDARY"
    subkeyword = "RADIATION_SET"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ssid",
                        int,
                        0,
                        10,
                        kwargs.get("ssid")
                    ),
                    Field(
                        "type",
                        int,
                        10,
                        10,
                        kwargs.get("type", 1)
                    ),
                    Field(
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        50,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "pserod",
                        int,
                        60,
                        10,
                        kwargs.get("pserod")
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
                        kwargs.get("rflcid", 0)
                    ),
                    Field(
                        "rfmult",
                        float,
                        10,
                        10,
                        kwargs.get("rfmult", 1.0)
                    ),
                    Field(
                        "tilcid",
                        int,
                        20,
                        10,
                        kwargs.get("tilcid", 0)
                    ),
                    Field(
                        "timult",
                        float,
                        30,
                        10,
                        kwargs.get("timult", 1.0)
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
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID, see also *SET_SEGMENT.
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        self._cards[0].set_value("ssid", value)

    @property
    def type(self) -> int:
        """Get or set the Radiation type:
        EQ.1: radiation boundary to environment
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        self._cards[0].set_value("type", value)

    @property
    def pserod(self) -> typing.Optional[int]:
        """Get or set the Part set ID for updating boundary segments exposed to the environment as solid elements erode
        """ # nopep8
        return self._cards[0].get_value("pserod")

    @pserod.setter
    def pserod(self, value: int) -> None:
        self._cards[0].set_value("pserod", value)

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
        self._cards[1].set_value("rflcid", value)

    @property
    def rfmult(self) -> float:
        """Get or set the Curve multiplier for f, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("rfmult")

    @rfmult.setter
    def rfmult(self, value: float) -> None:
        self._cards[1].set_value("rfmult", value)

    @property
    def tilcid(self) -> int:
        """Get or set the Load curve ID for T-infinity versus time, see *DEFINE_CURVE.
        EQ.0: use constant multiplier, TIMULT (default).
        """ # nopep8
        return self._cards[1].get_value("tilcid")

    @tilcid.setter
    def tilcid(self, value: int) -> None:
        self._cards[1].set_value("tilcid", value)

    @property
    def timult(self) -> float:
        """Get or set the Curve multiplier for T-infinity.
        """ # nopep8
        return self._cards[1].get_value("timult")

    @timult.setter
    def timult(self, value: float) -> None:
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
        if value not in [0, -1, 1]:
            raise Exception("""loc must be one of {0,-1,1}""")
        self._cards[1].set_value("loc", value)

