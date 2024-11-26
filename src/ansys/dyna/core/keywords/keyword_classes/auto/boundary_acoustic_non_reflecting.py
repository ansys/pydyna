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

class BoundaryAcousticNonReflecting(KeywordBase):
    """DYNA BOUNDARY_ACOUSTIC_NON_REFLECTING keyword"""

    keyword = "BOUNDARY"
    subkeyword = "ACOUSTIC_NON_REFLECTING"

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
                        "nrbtyp",
                        int,
                        10,
                        10,
                        kwargs.get("nrbtyp")
                    ),
                    Field(
                        "crvopt",
                        int,
                        20,
                        10,
                        kwargs.get("crvopt")
                    ),
                    Field(
                        "data1",
                        float,
                        30,
                        10,
                        kwargs.get("data1")
                    ),
                    Field(
                        "data2",
                        float,
                        40,
                        10,
                        kwargs.get("data2")
                    ),
                    Field(
                        "data3",
                        float,
                        50,
                        10,
                        kwargs.get("data3")
                    ),
                ],
            ),
        ]

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID of an acoustic surface.
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        self._cards[0].set_value("ssid", value)

    @property
    def nrbtyp(self) -> typing.Optional[int]:
        """Get or set the Absorbing boundary type:
        EQ.1:	Plane wave absorbing boundary
        EQ.2 : Curved wave absorbing boundary(see CRVOPT).
        """ # nopep8
        return self._cards[0].get_value("nrbtyp")

    @nrbtyp.setter
    def nrbtyp(self, value: int) -> None:
        self._cards[0].set_value("nrbtyp", value)

    @property
    def crvopt(self) -> typing.Optional[int]:
        """Get or set the Curvature specification option for NRBTYP = 2:
        EQ.1:	Provide average curvature, 1/R
        EQ.2 : Provide coordinates of the center of curvature, (Xc,Yc,Zc).
        """ # nopep8
        return self._cards[0].get_value("crvopt")

    @crvopt.setter
    def crvopt(self, value: int) -> None:
        self._cards[0].set_value("crvopt", value)

    @property
    def data1(self) -> typing.Optional[float]:
        """Get or set the CRVOPT.EQ.1:	Average curvature, 1/R
        CRVOPT.EQ.2:	Coordinate Xc.
        """ # nopep8
        return self._cards[0].get_value("data1")

    @data1.setter
    def data1(self, value: float) -> None:
        self._cards[0].set_value("data1", value)

    @property
    def data2(self) -> typing.Optional[float]:
        """Get or set the Coordinate Yc for CRVOPT = 2.
        """ # nopep8
        return self._cards[0].get_value("data2")

    @data2.setter
    def data2(self, value: float) -> None:
        self._cards[0].set_value("data2", value)

    @property
    def data3(self) -> typing.Optional[float]:
        """Get or set the Coordinate Zc for CRVOPT = 2.
        """ # nopep8
        return self._cards[0].get_value("data3")

    @data3.setter
    def data3(self, value: float) -> None:
        self._cards[0].set_value("data3", value)

