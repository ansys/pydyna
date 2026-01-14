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

"""Module providing the LoadNurbsShell class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_LOADNURBSSHELL_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("heading", str, 10, 70, None),
)

_LOADNURBSSHELL_CARD1 = (
    FieldSchema("ssid", int, 0, 10, None),
    FieldSchema("lcid", int, 10, 10, None),
    FieldSchema("sf", float, 20, 10, 1.0),
    FieldSchema("at", float, 30, 10, 0.0),
    FieldSchema("dt", float, 40, 10, 1e+16),
    FieldSchema("ltype", str, 50, 10, "PRESS"),
    FieldSchema("regdef", str, 60, 10, "RS"),
)

_LOADNURBSSHELL_CARD2 = (
    FieldSchema("cid", int, 0, 10, 0),
    FieldSchema("v1", float, 10, 10, None),
    FieldSchema("v2", float, 20, 10, None),
    FieldSchema("v3", float, 30, 10, None),
)

_LOADNURBSSHELL_CARD3 = (
    FieldSchema("rmin", float, 0, 10, None),
    FieldSchema("smin", float, 10, 10, None),
    FieldSchema("rmax", float, 20, 10, None),
    FieldSchema("smax", float, 30, 10, None),
)

_LOADNURBSSHELL_CARD4 = (
    FieldSchema("ne1", int, 0, 10, None),
    FieldSchema("ne2", int, 10, 10, None),
    FieldSchema("ne3", int, 20, 10, None),
    FieldSchema("ne4", int, 30, 10, None),
    FieldSchema("ne5", int, 40, 10, None),
    FieldSchema("ne6", int, 50, 10, None),
    FieldSchema("ne7", int, 60, 10, None),
    FieldSchema("ne8", int, 70, 10, None),
)

_LOADNURBSSHELL_CARD5 = (
    FieldSchema("nt1", int, 0, 10, None),
    FieldSchema("nt2", int, 10, 10, None),
    FieldSchema("nt3", int, 20, 10, None),
    FieldSchema("nt4", int, 30, 10, None),
    FieldSchema("nte", int, 40, 10, None),
)

_LOADNURBSSHELL_CARD6 = (
    FieldSchema("r1", float, 0, 10, None),
    FieldSchema("s1", float, 10, 10, None),
    FieldSchema("r2", float, 20, 10, None),
    FieldSchema("s2", float, 30, 10, None),
)

_LOADNURBSSHELL_CARD7 = (
    FieldSchema("nc1", int, 0, 10, None),
    FieldSchema("nc2", int, 10, 10, None),
    FieldSchema("nce", int, 20, 10, None),
)

class LoadNurbsShell(KeywordBase):
    """DYNA LOAD_NURBS_SHELL keyword"""

    keyword = "LOAD"
    subkeyword = "NURBS_SHELL"

    def __init__(self, **kwargs):
        """Initialize the LoadNurbsShell class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADNURBSSHELL_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADNURBSSHELL_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADNURBSSHELL_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADNURBSSHELL_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADNURBSSHELL_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADNURBSSHELL_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADNURBSSHELL_CARD6,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADNURBSSHELL_CARD7,
                **kwargs,
            ),        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the loading ID
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def heading(self) -> typing.Optional[str]:
        """Get or set the A description of the loading.
        """ # nopep8
        return self._cards[0].get_value("heading")

    @heading.setter
    def heading(self, value: str) -> None:
        """Set the heading property."""
        self._cards[0].set_value("heading", value)

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the NURBS shell patch ID.
        """ # nopep8
        return self._cards[1].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[1].set_value("ssid", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[1].set_value("lcid", value)

    @property
    def sf(self) -> float:
        """Get or set the Load curve scale factor.
        """ # nopep8
        return self._cards[1].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[1].set_value("sf", value)

    @property
    def at(self) -> float:
        """Get or set the Arrival/birth time for load.
        """ # nopep8
        return self._cards[1].get_value("at")

    @at.setter
    def at(self, value: float) -> None:
        """Set the at property."""
        self._cards[1].set_value("at", value)

    @property
    def dt(self) -> float:
        """Get or set the Death time for load.
        """ # nopep8
        return self._cards[1].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        """Set the dt property."""
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
        """Set the ltype property."""
        if value not in ["PRESS", "CRV", "CRVS", "CRVT", "CRVN", "TRACT", None]:
            raise Exception("""ltype must be `None` or one of {"PRESS","CRV","CRVS","CRVT","CRVN","TRACT"}.""")
        self._cards[1].set_value("ltype", value)

    @property
    def regdef(self) -> str:
        """Get or set the The method of defining the region of a NURBS shell patch on which the loading is applied.
        """ # nopep8
        return self._cards[1].get_value("regdef")

    @regdef.setter
    def regdef(self, value: str) -> None:
        """Set the regdef property."""
        if value not in ["RS", "NBEW", "NBEP", None]:
            raise Exception("""regdef must be `None` or one of {"RS","NBEW","NBEP"}.""")
        self._cards[1].set_value("regdef", value)

    @property
    def cid(self) -> int:
        """Get or set the Coordinate system ID.
        """ # nopep8
        return self._cards[2].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[2].set_value("cid", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Vector direction cosines defining the direction of the traction loading.
        """ # nopep8
        return self._cards[2].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        """Set the v1 property."""
        self._cards[2].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Vector direction cosines defining the direction of the traction loading.
        """ # nopep8
        return self._cards[2].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[2].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Vector direction cosines defining the direction of the traction loading.
        """ # nopep8
        return self._cards[2].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[2].set_value("v3", value)

    @property
    def rmin(self) -> typing.Optional[float]:
        """Get or set the The minimum and maximum values of the univariate knot vector in local r and s-directions of the area on which the pressure loading is applied.
        """ # nopep8
        return self._cards[3].get_value("rmin")

    @rmin.setter
    def rmin(self, value: float) -> None:
        """Set the rmin property."""
        self._cards[3].set_value("rmin", value)

    @property
    def smin(self) -> typing.Optional[float]:
        """Get or set the The minimum and maximum values of the univariate knot vector in local r and s-directions of the area on which the pressure loading is applied.
        """ # nopep8
        return self._cards[3].get_value("smin")

    @smin.setter
    def smin(self, value: float) -> None:
        """Set the smin property."""
        self._cards[3].set_value("smin", value)

    @property
    def rmax(self) -> typing.Optional[float]:
        """Get or set the The minimum and maximum values of the univariate knot vector in local r and s-directions of the area on which the pressure loading is applied.
        """ # nopep8
        return self._cards[3].get_value("rmax")

    @rmax.setter
    def rmax(self, value: float) -> None:
        """Set the rmax property."""
        self._cards[3].set_value("rmax", value)

    @property
    def smax(self) -> typing.Optional[float]:
        """Get or set the The minimum and maximum values of the univariate knot vector in local r and s-directions of the area on which the pressure loading is applied.
        """ # nopep8
        return self._cards[3].get_value("smax")

    @smax.setter
    def smax(self, value: float) -> None:
        """Set the smax property."""
        self._cards[3].set_value("smax", value)

    @property
    def ne1(self) -> typing.Optional[int]:
        """Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
        """ # nopep8
        return self._cards[4].get_value("ne1")

    @ne1.setter
    def ne1(self, value: int) -> None:
        """Set the ne1 property."""
        self._cards[4].set_value("ne1", value)

    @property
    def ne2(self) -> typing.Optional[int]:
        """Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
        """ # nopep8
        return self._cards[4].get_value("ne2")

    @ne2.setter
    def ne2(self, value: int) -> None:
        """Set the ne2 property."""
        self._cards[4].set_value("ne2", value)

    @property
    def ne3(self) -> typing.Optional[int]:
        """Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
        """ # nopep8
        return self._cards[4].get_value("ne3")

    @ne3.setter
    def ne3(self, value: int) -> None:
        """Set the ne3 property."""
        self._cards[4].set_value("ne3", value)

    @property
    def ne4(self) -> typing.Optional[int]:
        """Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
        """ # nopep8
        return self._cards[4].get_value("ne4")

    @ne4.setter
    def ne4(self, value: int) -> None:
        """Set the ne4 property."""
        self._cards[4].set_value("ne4", value)

    @property
    def ne5(self) -> typing.Optional[int]:
        """Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
        """ # nopep8
        return self._cards[4].get_value("ne5")

    @ne5.setter
    def ne5(self, value: int) -> None:
        """Set the ne5 property."""
        self._cards[4].set_value("ne5", value)

    @property
    def ne6(self) -> typing.Optional[int]:
        """Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
        """ # nopep8
        return self._cards[4].get_value("ne6")

    @ne6.setter
    def ne6(self, value: int) -> None:
        """Set the ne6 property."""
        self._cards[4].set_value("ne6", value)

    @property
    def ne7(self) -> typing.Optional[int]:
        """Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
        """ # nopep8
        return self._cards[4].get_value("ne7")

    @ne7.setter
    def ne7(self, value: int) -> None:
        """Set the ne7 property."""
        self._cards[4].set_value("ne7", value)

    @property
    def ne8(self) -> typing.Optional[int]:
        """Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
        """ # nopep8
        return self._cards[4].get_value("ne8")

    @ne8.setter
    def ne8(self, value: int) -> None:
        """Set the ne8 property."""
        self._cards[4].set_value("ne8", value)

    @property
    def nt1(self) -> typing.Optional[int]:
        """Get or set the Nodes defining an area on the surface of a NURBS element where loading is applied.
        """ # nopep8
        return self._cards[5].get_value("nt1")

    @nt1.setter
    def nt1(self, value: int) -> None:
        """Set the nt1 property."""
        self._cards[5].set_value("nt1", value)

    @property
    def nt2(self) -> typing.Optional[int]:
        """Get or set the Nodes defining an area on the surface of a NURBS element where loading is applied.
        """ # nopep8
        return self._cards[5].get_value("nt2")

    @nt2.setter
    def nt2(self, value: int) -> None:
        """Set the nt2 property."""
        self._cards[5].set_value("nt2", value)

    @property
    def nt3(self) -> typing.Optional[int]:
        """Get or set the Nodes defining an area on the surface of a NURBS element where loading is applied.
        """ # nopep8
        return self._cards[5].get_value("nt3")

    @nt3.setter
    def nt3(self, value: int) -> None:
        """Set the nt3 property."""
        self._cards[5].set_value("nt3", value)

    @property
    def nt4(self) -> typing.Optional[int]:
        """Get or set the Nodes defining an area on the surface of a NURBS element where loading is applied.
        """ # nopep8
        return self._cards[5].get_value("nt4")

    @nt4.setter
    def nt4(self, value: int) -> None:
        """Set the nt4 property."""
        self._cards[5].set_value("nt4", value)

    @property
    def nte(self) -> typing.Optional[int]:
        """Get or set the Optional node used to identify the NURBS element on which the load application area defined by the NTi's is located.
        """ # nopep8
        return self._cards[5].get_value("nte")

    @nte.setter
    def nte(self, value: int) -> None:
        """Set the nte property."""
        self._cards[5].set_value("nte", value)

    @property
    def r1(self) -> typing.Optional[float]:
        """Get or set the The univariate knot vector in local r and s-directions of the starting and ending points of the curve on which the curve loading is applied.
        """ # nopep8
        return self._cards[6].get_value("r1")

    @r1.setter
    def r1(self, value: float) -> None:
        """Set the r1 property."""
        self._cards[6].set_value("r1", value)

    @property
    def s1(self) -> typing.Optional[float]:
        """Get or set the The univariate knot vector in local r and s-directions of the starting and ending points of the curve on which the curve loading is applied.
        """ # nopep8
        return self._cards[6].get_value("s1")

    @s1.setter
    def s1(self, value: float) -> None:
        """Set the s1 property."""
        self._cards[6].set_value("s1", value)

    @property
    def r2(self) -> typing.Optional[float]:
        """Get or set the The univariate knot vector in local r and s-directions of the starting and ending points of the curve on which the curve loading is applied.
        """ # nopep8
        return self._cards[6].get_value("r2")

    @r2.setter
    def r2(self, value: float) -> None:
        """Set the r2 property."""
        self._cards[6].set_value("r2", value)

    @property
    def s2(self) -> typing.Optional[float]:
        """Get or set the The univariate knot vector in local r and s-directions of the starting and ending points of the curve on which the curve loading is applied.
        """ # nopep8
        return self._cards[6].get_value("s2")

    @s2.setter
    def s2(self, value: float) -> None:
        """Set the s2 property."""
        self._cards[6].set_value("s2", value)

    @property
    def nc1(self) -> typing.Optional[int]:
        """Get or set the Nodes defining a curve on the surface of a NURBS element where loading is applied.
        """ # nopep8
        return self._cards[7].get_value("nc1")

    @nc1.setter
    def nc1(self, value: int) -> None:
        """Set the nc1 property."""
        self._cards[7].set_value("nc1", value)

    @property
    def nc2(self) -> typing.Optional[int]:
        """Get or set the Nodes defining a curve on the surface of a NURBS element where loading is applied.
        """ # nopep8
        return self._cards[7].get_value("nc2")

    @nc2.setter
    def nc2(self, value: int) -> None:
        """Set the nc2 property."""
        self._cards[7].set_value("nc2", value)

    @property
    def nce(self) -> typing.Optional[int]:
        """Get or set the Optional node used to identify the NURBS element on which the load application curve defined by the NTi's is located.
        """ # nopep8
        return self._cards[7].get_value("nce")

    @nce.setter
    def nce(self, value: int) -> None:
        """Set the nce property."""
        self._cards[7].set_value("nce", value)

