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

class ControlFormingOutput(KeywordBase):
    """DYNA CONTROL_FORMING_OUTPUT keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_OUTPUT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "cid",
                        int,
                        0,
                        10,
                        kwargs.get("cid")
                    ),
                    Field(
                        "nout",
                        int,
                        10,
                        10,
                        kwargs.get("nout")
                    ),
                    Field(
                        "tbeg",
                        float,
                        20,
                        10,
                        kwargs.get("tbeg")
                    ),
                    Field(
                        "tend",
                        float,
                        30,
                        10,
                        kwargs.get("tend")
                    ),
                    Field(
                        "y1/lcid",
                        float,
                        40,
                        10,
                        kwargs.get("y1/lcid")
                    ),
                    Field(
                        "y2/lcid",
                        float,
                        50,
                        10,
                        kwargs.get("y2/lcid")
                    ),
                    Field(
                        "y3",
                        float,
                        60,
                        10,
                        kwargs.get("y3")
                    ),
                    Field(
                        "y4",
                        float,
                        70,
                        10,
                        kwargs.get("y4")
                    ),
                ],
            ),
        ]

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the ID of a tooling kinematics curve, as defined by *DEFINE_CURVE and used by *BOUNDARY_PRESCRIBED_MOTION_RIGID.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[0].set_value("cid", value)

    @property
    def nout(self) -> typing.Optional[int]:
        """Get or set the Total number of D3PLOT outputs for the tooling kinematics curve, excluding the beginning and final time, see figures below.
        """ # nopep8
        return self._cards[0].get_value("nout")

    @nout.setter
    def nout(self, value: int) -> None:
        self._cards[0].set_value("nout", value)

    @property
    def tbeg(self) -> typing.Optional[float]:
        """Get or set the Start time of the curve.
        """ # nopep8
        return self._cards[0].get_value("tbeg")

    @tbeg.setter
    def tbeg(self, value: float) -> None:
        self._cards[0].set_value("tbeg", value)

    @property
    def tend(self) -> typing.Optional[float]:
        """Get or set the End time of the curve.
        """ # nopep8
        return self._cards[0].get_value("tend")

    @tend.setter
    def tend(self, value: float) -> None:
        self._cards[0].set_value("tend", value)

    @property
    def y1_lcid(self) -> typing.Optional[float]:
        """Get or set the GT.0:	All four variables (Y1, Y2, Y3, Y4) are taken to be the distances from the punch home, where d3plot files will be output
        LT.0:	The absolute value of Y1/LCID (must be an integer) is taken as a load curve ID (see *DEFINE_CURVE).
        Only the abscissas in the load curve, which are the distances to punch home, are used.
        These distances specify the states that are written to the d3plot files.  Ordinates of the curve are ignored.
        This case accommodates more states than is possible with the four variables Y1, Y2, Y3, Y4.  Furthermore, when Y1/LCID < 0, Y2, Y3, and Y4 are ignored.
        Available starting from Dev Revision 112604, the output will be skipped for any negative abscissa in the load curve.
        Note a curve with only negative abscissas is not allowed..
        """ # nopep8
        return self._cards[0].get_value("y1/lcid")

    @y1_lcid.setter
    def y1_lcid(self, value: float) -> None:
        self._cards[0].set_value("y1/lcid", value)

    @property
    def y2_lcid(self) -> typing.Optional[float]:
        """Get or set the GT.0:	The input is taken as the distance from the punch home, where a d3plot file will be output
        LT.0:	The absolute value of Y2/CIDT (must be an integer) is taken as a load curve ID (see *DEFINE_CURVE).
        Only the abscissas in the load curve, which are the simulation times, are used.
        These times specify the states that are written to the d3plot files.  Ordinates of the curve are ignored.
        Note this time-dependent load curve will output additional d3plot files on top of the d3plot files already written in case Y1/LCID < 0 (if specified).
        Furthermore, when Y2/CIDT < 0, Y3 and Y4 are ignored.  See the example Using CIDT below.
        """ # nopep8
        return self._cards[0].get_value("y2/lcid")

    @y2_lcid.setter
    def y2_lcid(self, value: float) -> None:
        self._cards[0].set_value("y2/lcid", value)

    @property
    def y3(self) -> typing.Optional[float]:
        """Get or set the Distances to tooling home, where D3PLOT files will be output.
        """ # nopep8
        return self._cards[0].get_value("y3")

    @y3.setter
    def y3(self, value: float) -> None:
        self._cards[0].set_value("y3", value)

    @property
    def y4(self) -> typing.Optional[float]:
        """Get or set the Distances to tooling home, where D3PLOT files will be output.
        """ # nopep8
        return self._cards[0].get_value("y4")

    @y4.setter
    def y4(self, value: float) -> None:
        self._cards[0].set_value("y4", value)

