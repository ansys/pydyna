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

class InterfaceCompensationNewMultiSteps(KeywordBase):
    """DYNA INTERFACE_COMPENSATION_NEW_MULTI_STEPS keyword"""

    keyword = "INTERFACE"
    subkeyword = "COMPENSATION_NEW_MULTI_STEPS"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "method",
                        int,
                        0,
                        10,
                        kwargs.get("method", 6)
                    ),
                    Field(
                        "sl",
                        float,
                        10,
                        10,
                        kwargs.get("sl", 5.0)
                    ),
                    Field(
                        "sf",
                        float,
                        20,
                        10,
                        kwargs.get("sf", 0.75)
                    ),
                    Field(
                        "elref",
                        int,
                        30,
                        10,
                        kwargs.get("elref", 1)
                    ),
                    Field(
                        "psidm",
                        float,
                        40,
                        10,
                        kwargs.get("psidm")
                    ),
                    Field(
                        "undct",
                        float,
                        50,
                        10,
                        kwargs.get("undct", 0)
                    ),
                    Field(
                        "angle",
                        float,
                        60,
                        10,
                        kwargs.get("angle", 0.0)
                    ),
                    Field(
                        "nlinea",
                        int,
                        70,
                        10,
                        kwargs.get("nlinea", 1)
                    ),
                ],
            ),
        ]

    @property
    def method(self) -> int:
        """Get or set the There are several extrapolation methods for the addendum and binder outside of trim lines, see Remarks
        """ # nopep8
        return self._cards[0].get_value("method")

    @method.setter
    def method(self, value: int) -> None:
        self._cards[0].set_value("method", value)

    @property
    def sl(self) -> float:
        """Get or set the The smooth level parameter controls the smoothness of the modified surfaces. A large value makes the surface smoother. The commonly used value is between 5 and 10.  If springback is large, the transition region is expected to be large.  However, by using a smaller value of SL, the region of transition can be reduced
        """ # nopep8
        return self._cards[0].get_value("sl")

    @sl.setter
    def sl(self, value: float) -> None:
        self._cards[0].set_value("sl", value)

    @property
    def sf(self) -> float:
        """Get or set the This scales how much of the shape deviation is compensated. For example, if 10 mm springback is predicted, and the scale factor is chosen as 0.75, then the compensation in the opposite direction will only be 7.5 mm.Through many parameter studies, it is found that the best scale factor is case dependent. For some cases, a scale factor of 0.75 is best, while for others, larger values are better. Sometimes, the best value can be larger than 1.1.	Since it is impossible to choose the best value for each application it is suggested that for a new application, the initial trial is 0.75.  If the springback cannot be effectively compensated, more iterations must be used to compensate the remaining shape deviation.For channel with twisting, the scale factor is more important. It was found that a small change of the tool shape might change the twisting mode.  If this occurs, using a small value (<0.5) is suggested.
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        self._cards[0].set_value("sf", value)

    @property
    def elref(self) -> int:
        """Get or set the EQ.1: special element refinement is used with the tool elements (default)
        EQ.2: special element refinement is turned off
        """ # nopep8
        return self._cards[0].get_value("elref")

    @elref.setter
    def elref(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""elref must be one of {1,2}""")
        self._cards[0].set_value("elref", value)

    @property
    def psidm(self) -> typing.Optional[float]:
        """Get or set the Define the Part set ID for master parts.  It is important to properly choose the parts for the master side.  Usually, only one side (master side) of the tool will be chosen as the master side, and the modification of the other side (slave side) depends solely on the change, which occurs in the master side.  In this way, the two sides are coupled and a constant gap between the two sides is maintained. If both sides are chosen as master side, the gap between the two sides might change and the gap might become inhomogeneous.	The choice of Master side will have effect on the final result for method 7 for three-piece draw. At this time, when the punch and binder are chosen as the master side, the binder region will not be changed. Otherwise, when the die is chosen as Master side the binder will be changed, since the changes extend to the edges of the Master tool.
        """ # nopep8
        return self._cards[0].get_value("psidm")

    @psidm.setter
    def psidm(self, value: float) -> None:
        self._cards[0].set_value("psidm", value)

    @property
    def undct(self) -> float:
        """Get or set the EQ.0: Default	EQ.1: Check and fix undercut.
        """ # nopep8
        return self._cards[0].get_value("undct")

    @undct.setter
    def undct(self, value: float) -> None:
        if value not in [0, 1]:
            raise Exception("""undct must be one of {0,1}""")
        self._cards[0].set_value("undct", value)

    @property
    def angle(self) -> float:
        """Get or set the An angle defining the undercut
        """ # nopep8
        return self._cards[0].get_value("angle")

    @angle.setter
    def angle(self, value: float) -> None:
        self._cards[0].set_value("angle", value)

    @property
    def nlinea(self) -> int:
        """Get or set the Activate nonlinear extrapolation.
        """ # nopep8
        return self._cards[0].get_value("nlinea")

    @nlinea.setter
    def nlinea(self, value: int) -> None:
        self._cards[0].set_value("nlinea", value)

