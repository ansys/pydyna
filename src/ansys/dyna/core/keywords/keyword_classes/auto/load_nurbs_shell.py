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

class LoadNurbsShell(KeywordBase):
    """DYNA LOAD_NURBS_SHELL keyword"""

    keyword = "LOAD"
    subkeyword = "NURBS_SHELL"

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
                        "ssid",
                        int,
                        0,
                        10,
                        kwargs.get("ssid")
                    ),
                    Field(
                        "lcid",
                        int,
                        10,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "sf",
                        float,
                        20,
                        10,
                        kwargs.get("sf", 1.0)
                    ),
                    Field(
                        "at",
                        float,
                        30,
                        10,
                        kwargs.get("at", 0.0)
                    ),
                    Field(
                        "dt",
                        float,
                        40,
                        10,
                        kwargs.get("dt", 1.0e+16)
                    ),
                    Field(
                        "ltype",
                        str,
                        50,
                        10,
                        kwargs.get("ltype", "PRESS")
                    ),
                    Field(
                        "regdef",
                        str,
                        60,
                        10,
                        kwargs.get("regdef", "RS")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cid",
                        int,
                        0,
                        10,
                        kwargs.get("cid", 0)
                    ),
                    Field(
                        "v1",
                        float,
                        10,
                        10,
                        kwargs.get("v1")
                    ),
                    Field(
                        "v2",
                        float,
                        20,
                        10,
                        kwargs.get("v2")
                    ),
                    Field(
                        "v3",
                        float,
                        30,
                        10,
                        kwargs.get("v3")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "rmin",
                        float,
                        0,
                        10,
                        kwargs.get("rmin")
                    ),
                    Field(
                        "smin",
                        float,
                        10,
                        10,
                        kwargs.get("smin")
                    ),
                    Field(
                        "rmax",
                        float,
                        20,
                        10,
                        kwargs.get("rmax")
                    ),
                    Field(
                        "smax",
                        float,
                        30,
                        10,
                        kwargs.get("smax")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ne1",
                        int,
                        0,
                        10,
                        kwargs.get("ne1")
                    ),
                    Field(
                        "ne2",
                        int,
                        10,
                        10,
                        kwargs.get("ne2")
                    ),
                    Field(
                        "ne3",
                        int,
                        20,
                        10,
                        kwargs.get("ne3")
                    ),
                    Field(
                        "ne4",
                        int,
                        30,
                        10,
                        kwargs.get("ne4")
                    ),
                    Field(
                        "ne5",
                        int,
                        40,
                        10,
                        kwargs.get("ne5")
                    ),
                    Field(
                        "ne6",
                        int,
                        50,
                        10,
                        kwargs.get("ne6")
                    ),
                    Field(
                        "ne7",
                        int,
                        60,
                        10,
                        kwargs.get("ne7")
                    ),
                    Field(
                        "ne8",
                        int,
                        70,
                        10,
                        kwargs.get("ne8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nt1",
                        int,
                        0,
                        10,
                        kwargs.get("nt1")
                    ),
                    Field(
                        "nt2",
                        int,
                        10,
                        10,
                        kwargs.get("nt2")
                    ),
                    Field(
                        "nt3",
                        int,
                        20,
                        10,
                        kwargs.get("nt3")
                    ),
                    Field(
                        "nt4",
                        int,
                        30,
                        10,
                        kwargs.get("nt4")
                    ),
                    Field(
                        "nte",
                        int,
                        40,
                        10,
                        kwargs.get("nte")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "r1",
                        float,
                        0,
                        10,
                        kwargs.get("r1")
                    ),
                    Field(
                        "s1",
                        float,
                        10,
                        10,
                        kwargs.get("s1")
                    ),
                    Field(
                        "r2",
                        float,
                        20,
                        10,
                        kwargs.get("r2")
                    ),
                    Field(
                        "s2",
                        float,
                        30,
                        10,
                        kwargs.get("s2")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nc1",
                        int,
                        0,
                        10,
                        kwargs.get("nc1")
                    ),
                    Field(
                        "nc2",
                        int,
                        10,
                        10,
                        kwargs.get("nc2")
                    ),
                    Field(
                        "nce",
                        int,
                        20,
                        10,
                        kwargs.get("nce")
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
    def ssid(self) -> typing.Optional[int]:
        """Get or set the NURBS shell patch ID.
        """ # nopep8
        return self._cards[1].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        self._cards[1].set_value("ssid", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID.
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
        """Get or set the Arrival/birth time for load.
        """ # nopep8
        return self._cards[1].get_value("at")

    @at.setter
    def at(self, value: float) -> None:
        self._cards[1].set_value("at", value)

    @property
    def dt(self) -> float:
        """Get or set the Death time for load.
        """ # nopep8
        return self._cards[1].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[1].set_value("dt", value)

    @property
    def ltype(self) -> str:
        """Get or set the Load type.
        EQ.PRESS:Surface traction is applied on a region on a NURBS patch, along the opposite direction to the NURBS patch surface normal.
        EQ.CRV:Loading is applied on a curve on a NURBS patch,along (V1,V2,V3) of CID coordinate system.The loading dimension is force per unit length along the curve.
        EQ.CRVS:Loading, force per unit length, is applied on a curve on the surface of a NURBS patch,along the local shear direction, CS direction
        EQ.CRVT:Loading, force per unit length, is applied on a curve on the surface of a NURBS patch, along the local transverse direction, CT direction.EQ.CRVN:Loading, force per unit length, is applied on a curve on the surface of a NURBS patch, along the local normal direction, CN direction.
        EQ.TRACT:Surface traction, force per unit area, is applied on a region on a NURBS patch, along (V1,V2,V3) of the coordinate system CID.
        """ # nopep8
        return self._cards[1].get_value("ltype")

    @ltype.setter
    def ltype(self, value: str) -> None:
        if value not in ["PRESS", "CRV", "CRVS", "CRVT", "CRVN", "TRACT"]:
            raise Exception("""ltype must be one of {"PRESS","CRV","CRVS","CRVT","CRVN","TRACT"}""")
        self._cards[1].set_value("ltype", value)

    @property
    def regdef(self) -> str:
        """Get or set the The method of defining the region of a NURBS shell patch on which the loading is applied.
        """ # nopep8
        return self._cards[1].get_value("regdef")

    @regdef.setter
    def regdef(self, value: str) -> None:
        if value not in ["RS", "NBEW", "NBEP"]:
            raise Exception("""regdef must be one of {"RS","NBEW","NBEP"}""")
        self._cards[1].set_value("regdef", value)

    @property
    def cid(self) -> int:
        """Get or set the Coordinate system ID.
        """ # nopep8
        return self._cards[2].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[2].set_value("cid", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Vector direction cosines defining the direction of the traction loading.
        """ # nopep8
        return self._cards[2].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[2].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Vector direction cosines defining the direction of the traction loading.
        """ # nopep8
        return self._cards[2].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[2].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Vector direction cosines defining the direction of the traction loading.
        """ # nopep8
        return self._cards[2].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[2].set_value("v3", value)

    @property
    def rmin(self) -> typing.Optional[float]:
        """Get or set the The minimum and maximum values of the univariate knot vector in local r and s-directions of the area on which the pressure loading is applied.
        """ # nopep8
        return self._cards[3].get_value("rmin")

    @rmin.setter
    def rmin(self, value: float) -> None:
        self._cards[3].set_value("rmin", value)

    @property
    def smin(self) -> typing.Optional[float]:
        """Get or set the The minimum and maximum values of the univariate knot vector in local r and s-directions of the area on which the pressure loading is applied.
        """ # nopep8
        return self._cards[3].get_value("smin")

    @smin.setter
    def smin(self, value: float) -> None:
        self._cards[3].set_value("smin", value)

    @property
    def rmax(self) -> typing.Optional[float]:
        """Get or set the The minimum and maximum values of the univariate knot vector in local r and s-directions of the area on which the pressure loading is applied.
        """ # nopep8
        return self._cards[3].get_value("rmax")

    @rmax.setter
    def rmax(self, value: float) -> None:
        self._cards[3].set_value("rmax", value)

    @property
    def smax(self) -> typing.Optional[float]:
        """Get or set the The minimum and maximum values of the univariate knot vector in local r and s-directions of the area on which the pressure loading is applied.
        """ # nopep8
        return self._cards[3].get_value("smax")

    @smax.setter
    def smax(self, value: float) -> None:
        self._cards[3].set_value("smax", value)

    @property
    def ne1(self) -> typing.Optional[int]:
        """Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
        """ # nopep8
        return self._cards[4].get_value("ne1")

    @ne1.setter
    def ne1(self, value: int) -> None:
        self._cards[4].set_value("ne1", value)

    @property
    def ne2(self) -> typing.Optional[int]:
        """Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
        """ # nopep8
        return self._cards[4].get_value("ne2")

    @ne2.setter
    def ne2(self, value: int) -> None:
        self._cards[4].set_value("ne2", value)

    @property
    def ne3(self) -> typing.Optional[int]:
        """Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
        """ # nopep8
        return self._cards[4].get_value("ne3")

    @ne3.setter
    def ne3(self, value: int) -> None:
        self._cards[4].set_value("ne3", value)

    @property
    def ne4(self) -> typing.Optional[int]:
        """Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
        """ # nopep8
        return self._cards[4].get_value("ne4")

    @ne4.setter
    def ne4(self, value: int) -> None:
        self._cards[4].set_value("ne4", value)

    @property
    def ne5(self) -> typing.Optional[int]:
        """Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
        """ # nopep8
        return self._cards[4].get_value("ne5")

    @ne5.setter
    def ne5(self, value: int) -> None:
        self._cards[4].set_value("ne5", value)

    @property
    def ne6(self) -> typing.Optional[int]:
        """Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
        """ # nopep8
        return self._cards[4].get_value("ne6")

    @ne6.setter
    def ne6(self, value: int) -> None:
        self._cards[4].set_value("ne6", value)

    @property
    def ne7(self) -> typing.Optional[int]:
        """Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
        """ # nopep8
        return self._cards[4].get_value("ne7")

    @ne7.setter
    def ne7(self, value: int) -> None:
        self._cards[4].set_value("ne7", value)

    @property
    def ne8(self) -> typing.Optional[int]:
        """Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
        """ # nopep8
        return self._cards[4].get_value("ne8")

    @ne8.setter
    def ne8(self, value: int) -> None:
        self._cards[4].set_value("ne8", value)

    @property
    def nt1(self) -> typing.Optional[int]:
        """Get or set the Nodes defining an area on the surface of a NURBS element where loading is applied.
        """ # nopep8
        return self._cards[5].get_value("nt1")

    @nt1.setter
    def nt1(self, value: int) -> None:
        self._cards[5].set_value("nt1", value)

    @property
    def nt2(self) -> typing.Optional[int]:
        """Get or set the Nodes defining an area on the surface of a NURBS element where loading is applied.
        """ # nopep8
        return self._cards[5].get_value("nt2")

    @nt2.setter
    def nt2(self, value: int) -> None:
        self._cards[5].set_value("nt2", value)

    @property
    def nt3(self) -> typing.Optional[int]:
        """Get or set the Nodes defining an area on the surface of a NURBS element where loading is applied.
        """ # nopep8
        return self._cards[5].get_value("nt3")

    @nt3.setter
    def nt3(self, value: int) -> None:
        self._cards[5].set_value("nt3", value)

    @property
    def nt4(self) -> typing.Optional[int]:
        """Get or set the Nodes defining an area on the surface of a NURBS element where loading is applied.
        """ # nopep8
        return self._cards[5].get_value("nt4")

    @nt4.setter
    def nt4(self, value: int) -> None:
        self._cards[5].set_value("nt4", value)

    @property
    def nte(self) -> typing.Optional[int]:
        """Get or set the Optional node used to identify the NURBS element on which the load application area defined by the NTi's is located.
        """ # nopep8
        return self._cards[5].get_value("nte")

    @nte.setter
    def nte(self, value: int) -> None:
        self._cards[5].set_value("nte", value)

    @property
    def r1(self) -> typing.Optional[float]:
        """Get or set the The univariate knot vector in local r and s-directions of the starting and ending points of the curve on which the curve loading is applied.
        """ # nopep8
        return self._cards[6].get_value("r1")

    @r1.setter
    def r1(self, value: float) -> None:
        self._cards[6].set_value("r1", value)

    @property
    def s1(self) -> typing.Optional[float]:
        """Get or set the The univariate knot vector in local r and s-directions of the starting and ending points of the curve on which the curve loading is applied.
        """ # nopep8
        return self._cards[6].get_value("s1")

    @s1.setter
    def s1(self, value: float) -> None:
        self._cards[6].set_value("s1", value)

    @property
    def r2(self) -> typing.Optional[float]:
        """Get or set the The univariate knot vector in local r and s-directions of the starting and ending points of the curve on which the curve loading is applied.
        """ # nopep8
        return self._cards[6].get_value("r2")

    @r2.setter
    def r2(self, value: float) -> None:
        self._cards[6].set_value("r2", value)

    @property
    def s2(self) -> typing.Optional[float]:
        """Get or set the The univariate knot vector in local r and s-directions of the starting and ending points of the curve on which the curve loading is applied.
        """ # nopep8
        return self._cards[6].get_value("s2")

    @s2.setter
    def s2(self, value: float) -> None:
        self._cards[6].set_value("s2", value)

    @property
    def nc1(self) -> typing.Optional[int]:
        """Get or set the Nodes defining a curve on the surface of a NURBS element where loading is applied.
        """ # nopep8
        return self._cards[7].get_value("nc1")

    @nc1.setter
    def nc1(self, value: int) -> None:
        self._cards[7].set_value("nc1", value)

    @property
    def nc2(self) -> typing.Optional[int]:
        """Get or set the Nodes defining a curve on the surface of a NURBS element where loading is applied.
        """ # nopep8
        return self._cards[7].get_value("nc2")

    @nc2.setter
    def nc2(self, value: int) -> None:
        self._cards[7].set_value("nc2", value)

    @property
    def nce(self) -> typing.Optional[int]:
        """Get or set the Optional node used to identify the NURBS element on which the load application curve defined by the NTi's is located.
        """ # nopep8
        return self._cards[7].get_value("nce")

    @nce.setter
    def nce(self, value: int) -> None:
        self._cards[7].set_value("nce", value)

