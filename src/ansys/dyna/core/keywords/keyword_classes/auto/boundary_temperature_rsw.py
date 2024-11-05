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

class BoundaryTemperatureRsw(KeywordBase):
    """DYNA BOUNDARY_TEMPERATURE_RSW keyword"""

    keyword = "BOUNDARY"
    subkeyword = "TEMPERATURE_RSW"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "sid",
                        int,
                        0,
                        10,
                        kwargs.get("sid")
                    ),
                    Field(
                        "option",
                        int,
                        10,
                        10,
                        kwargs.get("option", 0)
                    ),
                    Field(
                        "nid1",
                        int,
                        20,
                        10,
                        kwargs.get("nid1")
                    ),
                    Field(
                        "nid2",
                        int,
                        30,
                        10,
                        kwargs.get("nid2")
                    ),
                    Field(
                        "tdeath",
                        float,
                        40,
                        10,
                        kwargs.get("tdeath", 1.e20)
                    ),
                    Field(
                        "tbirth",
                        float,
                        50,
                        10,
                        kwargs.get("tbirth", 0.)
                    ),
                    Field(
                        "loc",
                        int,
                        60,
                        10,
                        kwargs.get("loc", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dist",
                        float,
                        0,
                        10,
                        kwargs.get("dist", 0)
                    ),
                    Field(
                        "h1",
                        float,
                        10,
                        10,
                        kwargs.get("h1", 0.0)
                    ),
                    Field(
                        "h2",
                        float,
                        20,
                        10,
                        kwargs.get("h2", 0.0)
                    ),
                    Field(
                        "r",
                        float,
                        30,
                        10,
                        kwargs.get("r", 0.0)
                    ),
                    Field(
                        "tempc",
                        float,
                        40,
                        10,
                        kwargs.get("tempc", 0.0)
                    ),
                    Field(
                        "tempb",
                        float,
                        50,
                        10,
                        kwargs.get("tempb", 0.0)
                    ),
                    Field(
                        "lcidt",
                        int,
                        60,
                        10,
                        kwargs.get("lcidt")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "hz1",
                        float,
                        0,
                        10,
                        kwargs.get("hz1")
                    ),
                    Field(
                        "hz2",
                        float,
                        10,
                        10,
                        kwargs.get("hz2", 0.0)
                    ),
                    Field(
                        "rz",
                        float,
                        20,
                        10,
                        kwargs.get("rz", 0.0)
                    ),
                    Field(
                        "tempzb",
                        float,
                        30,
                        10,
                        kwargs.get("tempzb", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Node Set ID; see *SET_‌NODE_‌OPTION. Nodes in the set will be checked to see if they are in the nugget or heat affected zone. If they are, the boundary condition will be applied. The boundary condition will not be applied to nodes in these regions if they are not included in the set..
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[0].set_value("sid", value)

    @property
    def option(self) -> int:
        """Get or set the Option for heat affected zone around the weld nugget:
        EQ.0: no heat affected zone
        EQ.1: ellipsoidal region considered
        """ # nopep8
        return self._cards[0].get_value("option")

    @option.setter
    def option(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""option must be one of {0,1}""")
        self._cards[0].set_value("option", value)

    @property
    def nid1(self) -> typing.Optional[int]:
        """Get or set the Node defining the tail of the orientation vector (axis of rotation of
        the ellipsoidal region) and the base for positioning of the nugget.
        See Remarks 1 and 2.
        """ # nopep8
        return self._cards[0].get_value("nid1")

    @nid1.setter
    def nid1(self, value: int) -> None:
        self._cards[0].set_value("nid1", value)

    @property
    def nid2(self) -> typing.Optional[int]:
        """Get or set the Node defining the head of the orientation vector (axis of rotation
        of the ellipsoidal region). See Remarks 1 and 2.
        """ # nopep8
        return self._cards[0].get_value("nid2")

    @nid2.setter
    def nid2(self, value: int) -> None:
        self._cards[0].set_value("nid2", value)

    @property
    def tdeath(self) -> float:
        """Get or set the Deactivation time for temperature boundary condition. At this
        point in time the temperature constraint is removed.
        """ # nopep8
        return self._cards[0].get_value("tdeath")

    @tdeath.setter
    def tdeath(self, value: float) -> None:
        self._cards[0].set_value("tdeath", value)

    @property
    def tbirth(self) -> float:
        """Get or set the Activation time for temperature boundary condition. Before this
        point in time the temperature constraint is ignored
        """ # nopep8
        return self._cards[0].get_value("tbirth")

    @tbirth.setter
    def tbirth(self, value: float) -> None:
        self._cards[0].set_value("tbirth", value)

    @property
    def loc(self) -> int:
        """Get or set the Application of surface for thermal shell elements, see parameter,
        THSHEL, in the *CONTROL_SHELL input:
        EQ.-1: lower surface of thermal shell element
        EQ.0: middle surface of thermal shell element
        EQ.1: upper surface of thermal shell element.
        """ # nopep8
        return self._cards[0].get_value("loc")

    @loc.setter
    def loc(self, value: int) -> None:
        if value not in [0, -1, 1]:
            raise Exception("""loc must be one of {0,-1,1}""")
        self._cards[0].set_value("loc", value)

    @property
    def dist(self) -> float:
        """Get or set the Position of center of nugget on the axis of rotation. Parameter
        defines the distance to NID1 along the orientation vector. See	Remark 1..
        """ # nopep8
        return self._cards[1].get_value("dist")

    @dist.setter
    def dist(self, value: float) -> None:
        self._cards[1].set_value("dist", value)

    @property
    def h1(self) -> float:
        """Get or set the Half width h1 of nugget in the lower half, i.e. in direction to NID1.	See Remark 2.
        """ # nopep8
        return self._cards[1].get_value("h1")

    @h1.setter
    def h1(self, value: float) -> None:
        self._cards[1].set_value("h1", value)

    @property
    def h2(self) -> float:
        """Get or set the Half width h2 of nugget in the upper half, i.e. in direction to NID2. See Remark 2.
        """ # nopep8
        return self._cards[1].get_value("h2")

    @h2.setter
    def h2(self, value: float) -> None:
        self._cards[1].set_value("h2", value)

    @property
    def r(self) -> float:
        """Get or set the Radius rweld of the nugget in surface normal to orientation vector. See Remark 2.
        """ # nopep8
        return self._cards[1].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        self._cards[1].set_value("r", value)

    @property
    def tempc(self) -> float:
        """Get or set the Base temperature at the center of the nugget. See Remark 3.
        """ # nopep8
        return self._cards[1].get_value("tempc")

    @tempc.setter
    def tempc(self, value: float) -> None:
        self._cards[1].set_value("tempc", value)

    @property
    def tempb(self) -> float:
        """Get or set the Base temperature at the boundary of the nugget. See Remark 3.
        """ # nopep8
        return self._cards[1].get_value("tempb")

    @tempb.setter
    def tempb(self, value: float) -> None:
        self._cards[1].set_value("tempb", value)

    @property
    def lcidt(self) -> typing.Optional[int]:
        """Get or set the |LCIDT| refers to the load curve ID prescribing the temperature evolution in the nugget as a function of time. The abscissa of the load curve will be normalized between the birth and death times of the boundary condition.
        GT.0:	The ordinate values of the load curve scale the respective base temperature of a particular point.
        EQ.0:	No temperature evolution. Base temperatures are used.
        LT.0:	The ordinate values of the load curve are used to define a linear combination between the temperature at the birth time and the base temperature of a particular point.Load curve ordinate values should range between 0.0 and 1.0.We recommend LCIDT < 0 to ensure a smooth temperature evolution.
        """ # nopep8
        return self._cards[1].get_value("lcidt")

    @lcidt.setter
    def lcidt(self, value: int) -> None:
        self._cards[1].set_value("lcidt", value)

    @property
    def hz1(self) -> typing.Optional[float]:
        """Get or set the Half width hz1 of heat affected zone in the lower half, meaning in
        direction to NID1. Only active for OPTION = 1. See Remark 4.
        """ # nopep8
        return self._cards[2].get_value("hz1")

    @hz1.setter
    def hz1(self, value: float) -> None:
        self._cards[2].set_value("hz1", value)

    @property
    def hz2(self) -> float:
        """Get or set the Half width hz2 of heat affected zone in the upper half, meaning in
        direction to NID1. Only active for OPTION = 1. See Remark 4.
        """ # nopep8
        return self._cards[2].get_value("hz2")

    @hz2.setter
    def hz2(self, value: float) -> None:
        self._cards[2].set_value("hz2", value)

    @property
    def rz(self) -> float:
        """Get or set the Radius Rhaz of the heat affected zone in surface normal to
        orientation vector. See Remark 4.
        """ # nopep8
        return self._cards[2].get_value("rz")

    @rz.setter
    def rz(self, value: float) -> None:
        self._cards[2].set_value("rz", value)

    @property
    def tempzb(self) -> float:
        """Get or set the Base temperature at the boundary of the heat affected zone
        for OPTION = 1. See Remark 4.
        """ # nopep8
        return self._cards[2].get_value("tempzb")

    @tempzb.setter
    def tempzb(self, value: float) -> None:
        self._cards[2].set_value("tempzb", value)

