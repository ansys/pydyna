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

"""Module providing the InitialVolumeFractionGeometry class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_INITIALVOLUMEFRACTIONGEOMETRY_CARD0 = (
    FieldSchema("fmsid", int, 0, 10, None),
    FieldSchema("fmidtyp", int, 10, 10, 0),
    FieldSchema("bammg", int, 20, 10, 0),
    FieldSchema("ntrace", int, 30, 10, 3),
)

_INITIALVOLUMEFRACTIONGEOMETRY_CARD1 = (
    FieldSchema("conttyp", int, 0, 10, 1),
    FieldSchema("fillopt", int, 10, 10, 0),
    FieldSchema("fammg", int, 20, 10, None),
    FieldSchema("vx", float, 30, 10, None),
    FieldSchema("vy", float, 40, 10, None),
    FieldSchema("vz", float, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

_INITIALVOLUMEFRACTIONGEOMETRY_CARD2 = (
    FieldSchema("sid", int, 0, 10, None),
    FieldSchema("stype", int, 10, 10, 0),
    FieldSchema("normdir", int, 20, 10, None),
    FieldSchema("xoffset", float, 30, 10, 0.0),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

_INITIALVOLUMEFRACTIONGEOMETRY_CARD3 = (
    FieldSchema("sgsid ", int, 0, 10, None),
    FieldSchema("normdir", int, 10, 10, None),
    FieldSchema("xoffset", float, 30, 10, 0.0),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

_INITIALVOLUMEFRACTIONGEOMETRY_CARD4 = (
    FieldSchema("x0", float, 0, 10, None),
    FieldSchema("y0", float, 10, 10, None),
    FieldSchema("z0", float, 20, 10, None),
    FieldSchema("xcos", float, 30, 10, None),
    FieldSchema("ycos", float, 40, 10, None),
    FieldSchema("zcos", float, 50, 10, None),
    FieldSchema("unused", float, 60, 10, None),
    FieldSchema("unused", float, 70, 10, None),
)

_INITIALVOLUMEFRACTIONGEOMETRY_CARD5 = (
    FieldSchema("x0", float, 0, 10, None),
    FieldSchema("y0", float, 10, 10, None),
    FieldSchema("z0", float, 20, 10, None),
    FieldSchema("x1", float, 30, 10, None),
    FieldSchema("y1", float, 40, 10, None),
    FieldSchema("z1", float, 50, 10, None),
    FieldSchema("r1", float, 60, 10, None),
    FieldSchema("r2", float, 70, 10, None),
)

_INITIALVOLUMEFRACTIONGEOMETRY_CARD6 = (
    FieldSchema("x0", float, 0, 10, None),
    FieldSchema("y0", float, 10, 10, None),
    FieldSchema("z0", float, 20, 10, None),
    FieldSchema("x1", float, 30, 10, None),
    FieldSchema("y1", float, 40, 10, None),
    FieldSchema("z1", float, 50, 10, None),
    FieldSchema("lcsid", int, 60, 10, None),
    FieldSchema("unused", float, 70, 10, None),
)

_INITIALVOLUMEFRACTIONGEOMETRY_CARD7 = (
    FieldSchema("x0", float, 0, 10, None),
    FieldSchema("y0", float, 10, 10, None),
    FieldSchema("z0", float, 20, 10, None),
    FieldSchema("r0", float, 30, 10, None),
    FieldSchema("unused", float, 40, 10, None),
    FieldSchema("unused", float, 50, 10, None),
    FieldSchema("unused", float, 60, 10, None),
    FieldSchema("unused", float, 70, 10, None),
)

class InitialVolumeFractionGeometry(KeywordBase):
    """DYNA INITIAL_VOLUME_FRACTION_GEOMETRY keyword"""

    keyword = "INITIAL"
    subkeyword = "VOLUME_FRACTION_GEOMETRY"

    def __init__(self, **kwargs):
        """Initialize the InitialVolumeFractionGeometry class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INITIALVOLUMEFRACTIONGEOMETRY_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INITIALVOLUMEFRACTIONGEOMETRY_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INITIALVOLUMEFRACTIONGEOMETRY_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INITIALVOLUMEFRACTIONGEOMETRY_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INITIALVOLUMEFRACTIONGEOMETRY_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INITIALVOLUMEFRACTIONGEOMETRY_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INITIALVOLUMEFRACTIONGEOMETRY_CARD6,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INITIALVOLUMEFRACTIONGEOMETRY_CARD7,
                **kwargs,
            ),        ]
    @property
    def fmsid(self) -> typing.Optional[int]:
        """Get or set the Background ALE (fluid) mesh SID to be initialized or filled with various AMMGs.  This set ID refers to one or more ALE parts
        """ # nopep8
        return self._cards[0].get_value("fmsid")

    @fmsid.setter
    def fmsid(self, value: int) -> None:
        """Set the fmsid property."""
        self._cards[0].set_value("fmsid", value)

    @property
    def fmidtyp(self) -> int:
        """Get or set the ALE mesh set ID type:
        EQ.0:  FMSID is an ALE part set ID (PSID).
        EQ.1:  FMSID is an ALE part ID (PID).
        """ # nopep8
        return self._cards[0].get_value("fmidtyp")

    @fmidtyp.setter
    def fmidtyp(self, value: int) -> None:
        """Set the fmidtyp property."""
        if value not in [0, 1, None]:
            raise Exception("""fmidtyp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("fmidtyp", value)

    @property
    def bammg(self) -> int:
        """Get or set the The background fluid group ID or ALE Multi-Material group ID (AMMGID) that initially fills the entire ALE mesh region defined by FMSID.For S-ALE, AMMG name (AMMGNM) could be also used in place of AMMGID
        """ # nopep8
        return self._cards[0].get_value("bammg")

    @bammg.setter
    def bammg(self, value: int) -> None:
        """Set the bammg property."""
        self._cards[0].set_value("bammg", value)

    @property
    def ntrace(self) -> int:
        """Get or set the Number of sampling points for volume filling detection.  Typically NTRACE ranges from 3 to maybe 10 (or more).  The higher it is, the finer the ALE element is divided so that small gaps between 2 Lagrangian shells may be filled in.
        """ # nopep8
        return self._cards[0].get_value("ntrace")

    @ntrace.setter
    def ntrace(self, value: int) -> None:
        """Set the ntrace property."""
        self._cards[0].set_value("ntrace", value)

    @property
    def conttyp(self) -> int:
        """Get or set the A "container" defines a Lagrangian surface boundary of a spatial region, inside (or outside) of which, an AMMG would fill up.  CONTTYP defines the container geometry type of this surface boundary (or shell structure).
        EQ.1: The container geometry is defined by a part ID (PID) or a part set ID (PSID), where the parts should be defined by shell elements (see *PART or *SET_PART).
        EQ.2: The container geometry is defined by a segment set (SGSID).
        EQ.3: The container geometry is defined by a plane: a point and a normal vector.
        EQ.4: The container geometry is defined by a conical surface: 2 end points and 2 corresponding radii.
        EQ.5: The container geometry is defined by a cuboid or rectangular box: 2 opposing end points, minimum to maximum coordinates.
        EQ.6: The container geometry is defined by a sphere: 1 center point, and a radius
        EQ.7:	The container geometry is defined by a user-defined function implemented in *DEFINE_FUNCTION.
        The arguments of the function should be the coordinates of a point (x,y,z).
        The function should return 1.0 if the point is inside the geometry
        """ # nopep8
        return self._cards[1].get_value("conttyp")

    @conttyp.setter
    def conttyp(self, value: int) -> None:
        """Set the conttyp property."""
        if value not in [1, 2, 3, 4, 5, 6, 7, None]:
            raise Exception("""conttyp must be `None` or one of {1,2,3,4,5,6,7}.""")
        self._cards[1].set_value("conttyp", value)

    @property
    def fillopt(self) -> int:
        """Get or set the A flag to indicate which side of the container surface the AMMG is supposed to fill.  CNTTYP = 1, 2, and 3, the “head” side of a container surface/segment is defined as the side pointed to by the heads of the normal vectors of the segments (“tail” side refers to opposite direction to “head”).  See Remark 5. Note that for CNTTYP = 1 and 2, the fluid interface can be offset from the container walls with XOFFST. XOFFST does not apply to the other container geometries.
        EQ.0:	The “head” side of the geometry defined above will be filled with fluid(default).For CNTTYP = 4, 5, 6,and 7, the inside of the container is filled.
        EQ.1 : The “tail” side of the geometry defined above will be filled with fluid.For CNTTYP = 4, 5, 6,and 7, the outside of the container is filled.
        """ # nopep8
        return self._cards[1].get_value("fillopt")

    @fillopt.setter
    def fillopt(self, value: int) -> None:
        """Set the fillopt property."""
        if value not in [0, 1, None]:
            raise Exception("""fillopt must be `None` or one of {0,1}.""")
        self._cards[1].set_value("fillopt", value)

    @property
    def fammg(self) -> typing.Optional[int]:
        """Get or set the This defines the fluid group ID or ALE Multi-Material group ID (AMMGID) which will fill up the interior (or exterior) of the space defined by the “container”. The order of AMMGIDs is determined by the order in which they are listed under *ALE_MULTI-MATERIAL_GROUP card.  For example, the first data card under the *ALE_MULTI-MATERIAL_GROUP keyword defines the multi-material group with ID (AMMGID) 1, the second data card defined AMMGID = 2, and so on. In case of S-ALE, AMMG name (AMMGNM) could be also used in place of AMMGID. See Remark 8.
        LT.0: | FAMMG | is a * SET_MULTI - MATERIAL_GROUP_LIST ID listing pairs of group IDs.For each pair, the 2nd group replaces the first one in the “container”.
        """ # nopep8
        return self._cards[1].get_value("fammg")

    @fammg.setter
    def fammg(self, value: int) -> None:
        """Set the fammg property."""
        self._cards[1].set_value("fammg", value)

    @property
    def vx(self) -> typing.Optional[float]:
        """Get or set the A uniform initial X-velocity applied to the filled material group.
        """ # nopep8
        return self._cards[1].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        """Set the vx property."""
        self._cards[1].set_value("vx", value)

    @property
    def vy(self) -> typing.Optional[float]:
        """Get or set the A uniform initial Y-velocity applied to the filled material group.
        """ # nopep8
        return self._cards[1].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        """Set the vy property."""
        self._cards[1].set_value("vy", value)

    @property
    def vz(self) -> typing.Optional[float]:
        """Get or set the A uniform initial Z-velocity applied to the filled material group.
        """ # nopep8
        return self._cards[1].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        """Set the vz property."""
        self._cards[1].set_value("vz", value)

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the A Set ID pointing to a part ID (PID) or part set ID (PSID) of the Lagrangian shell element structure defining the "container" geometry to be filled (see *PART or *SET_PART)
        """ # nopep8
        return self._cards[2].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[2].set_value("sid", value)

    @property
    def stype(self) -> int:
        """Get or set the Set ID type:
        EQ.0:  Container SID is a Lagrangian part set ID (PSID).
        EQ.1:  Container SID is a Lagrangian part ID (PID).
        """ # nopep8
        return self._cards[2].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        """Set the stype property."""
        if value not in [0, 1, None]:
            raise Exception("""stype must be `None` or one of {0,1}.""")
        self._cards[2].set_value("stype", value)

    @property
    def normdir(self) -> typing.Optional[int]:
        """Get or set the Obsolete
        """ # nopep8
        return self._cards[2].get_value("normdir")

    @normdir.setter
    def normdir(self, value: int) -> None:
        """Set the normdir property."""
        self._cards[2].set_value("normdir", value)

    @property
    def xoffset(self) -> float:
        """Get or set the |XOFFST| is the absolute length for offsetting the fluid interface from the nominal fluid interface LS-DYNA would otherwise define by default.  The sign of XOFFST determines which direction the interface is offset. It is based on the normal vectors of the segments associated with the container.
        XOFFST.GT.0:	Interface is offset along the positive direction of the segments of the container.
        XOFFST.LT.0 : Interface is offset in the negative direction of the normal vectors of the segments of the container.
        This is applicable to cases in which high pressure fluid is contained within a container.The offset allows LS - DYNA time to prevent leakage.In general, this may be set to roughly 5 - 10 % of the ALE element width.It may be important only for when ILEAK is turned ON to give the code time to catch the leakage(see * CONSTRAINED_LAGRANGE_IN_SOLID).If ILEAK is not ON, this may not be necessary
        """ # nopep8
        return self._cards[2].get_value("xoffset")

    @xoffset.setter
    def xoffset(self, value: float) -> None:
        """Set the xoffset property."""
        self._cards[2].set_value("xoffset", value)

    @property
    def sgsid_(self) -> typing.Optional[int]:
        """Get or set the Segment Set ID defining the "container", see *SET_SEGMENT
        """ # nopep8
        return self._cards[3].get_value("sgsid ")

    @sgsid_.setter
    def sgsid_(self, value: int) -> None:
        """Set the sgsid_ property."""
        self._cards[3].set_value("sgsid ", value)

    @property
    def normdir(self) -> typing.Optional[int]:
        """Get or set the Obsolete
        """ # nopep8
        return self._cards[3].get_value("normdir")

    @normdir.setter
    def normdir(self, value: int) -> None:
        """Set the normdir property."""
        self._cards[3].set_value("normdir", value)

    @property
    def xoffset(self) -> float:
        """Get or set the |XOFFST| is the absolute length for offsetting the fluid interface from the nominal fluid interface LS-DYNA would otherwise define by default.  The sign of XOFFST determines which direction the interface is offset. It is based on the normal vectors of the segments associated with the container.
        XOFFST.GT.0:	Interface is offset along the positive direction of the segments of the container.
        XOFFST.LT.0 : Interface is offset in the negative direction of the normal vectors of the segments of the container.
        This is applicable to cases in which high pressure fluid is contained within a container.The offset allows LS - DYNA time to prevent leakage.In general, this may be set to roughly 5 - 10 % of the ALE element width.It may be important only for when ILEAK is turned ON to give the code time to catch the leakage(see * CONSTRAINED_LAGRANGE_IN_SOLID).If ILEAK is not ON, this may not be necessary
        """ # nopep8
        return self._cards[3].get_value("xoffset")

    @xoffset.setter
    def xoffset(self, value: float) -> None:
        """Set the xoffset property."""
        self._cards[3].set_value("xoffset", value)

    @property
    def x0(self) -> typing.Optional[float]:
        """Get or set the X-coordinate of a spatial point  on  the plane
        """ # nopep8
        return self._cards[4].get_value("x0")

    @x0.setter
    def x0(self, value: float) -> None:
        """Set the x0 property."""
        self._cards[4].set_value("x0", value)

    @property
    def y0(self) -> typing.Optional[float]:
        """Get or set the Y-coordinate of a spatial point  on  the plane
        """ # nopep8
        return self._cards[4].get_value("y0")

    @y0.setter
    def y0(self, value: float) -> None:
        """Set the y0 property."""
        self._cards[4].set_value("y0", value)

    @property
    def z0(self) -> typing.Optional[float]:
        """Get or set the Z-coordinate of a spatial point  on  the plane
        """ # nopep8
        return self._cards[4].get_value("z0")

    @z0.setter
    def z0(self, value: float) -> None:
        """Set the z0 property."""
        self._cards[4].set_value("z0", value)

    @property
    def xcos(self) -> typing.Optional[float]:
        """Get or set the X-direction cosines of the plane normal vector
        """ # nopep8
        return self._cards[4].get_value("xcos")

    @xcos.setter
    def xcos(self, value: float) -> None:
        """Set the xcos property."""
        self._cards[4].set_value("xcos", value)

    @property
    def ycos(self) -> typing.Optional[float]:
        """Get or set the Y-direction cosines of the plane normal vector
        """ # nopep8
        return self._cards[4].get_value("ycos")

    @ycos.setter
    def ycos(self, value: float) -> None:
        """Set the ycos property."""
        self._cards[4].set_value("ycos", value)

    @property
    def zcos(self) -> typing.Optional[float]:
        """Get or set the Z-direction cosines of the plane normal vector
        """ # nopep8
        return self._cards[4].get_value("zcos")

    @zcos.setter
    def zcos(self, value: float) -> None:
        """Set the zcos property."""
        self._cards[4].set_value("zcos", value)

    @property
    def x0(self) -> typing.Optional[float]:
        """Get or set the X-coordinate of the center of the lower  base of the cone
        """ # nopep8
        return self._cards[5].get_value("x0")

    @x0.setter
    def x0(self, value: float) -> None:
        """Set the x0 property."""
        self._cards[5].set_value("x0", value)

    @property
    def y0(self) -> typing.Optional[float]:
        """Get or set the Y-coordinate of the center of the lower  base of the cone
        """ # nopep8
        return self._cards[5].get_value("y0")

    @y0.setter
    def y0(self, value: float) -> None:
        """Set the y0 property."""
        self._cards[5].set_value("y0", value)

    @property
    def z0(self) -> typing.Optional[float]:
        """Get or set the Z-coordinate of the center of the lower  base of the cone
        """ # nopep8
        return self._cards[5].get_value("z0")

    @z0.setter
    def z0(self, value: float) -> None:
        """Set the z0 property."""
        self._cards[5].set_value("z0", value)

    @property
    def x1(self) -> typing.Optional[float]:
        """Get or set the X-coordinate of the center of the upper base of the cone
        """ # nopep8
        return self._cards[5].get_value("x1")

    @x1.setter
    def x1(self, value: float) -> None:
        """Set the x1 property."""
        self._cards[5].set_value("x1", value)

    @property
    def y1(self) -> typing.Optional[float]:
        """Get or set the Y-coordinate of the center of the upper base of the cone
        """ # nopep8
        return self._cards[5].get_value("y1")

    @y1.setter
    def y1(self, value: float) -> None:
        """Set the y1 property."""
        self._cards[5].set_value("y1", value)

    @property
    def z1(self) -> typing.Optional[float]:
        """Get or set the Z-coordinate of the center of the upper base of the cone
        """ # nopep8
        return self._cards[5].get_value("z1")

    @z1.setter
    def z1(self, value: float) -> None:
        """Set the z1 property."""
        self._cards[5].set_value("z1", value)

    @property
    def r1(self) -> typing.Optional[float]:
        """Get or set the Radius of the lower base
        """ # nopep8
        return self._cards[5].get_value("r1")

    @r1.setter
    def r1(self, value: float) -> None:
        """Set the r1 property."""
        self._cards[5].set_value("r1", value)

    @property
    def r2(self) -> typing.Optional[float]:
        """Get or set the Radius of the upper base
        """ # nopep8
        return self._cards[5].get_value("r2")

    @r2.setter
    def r2(self, value: float) -> None:
        """Set the r2 property."""
        self._cards[5].set_value("r2", value)

    @property
    def x0(self) -> typing.Optional[float]:
        """Get or set the X_max coordinate of the box
        """ # nopep8
        return self._cards[6].get_value("x0")

    @x0.setter
    def x0(self, value: float) -> None:
        """Set the x0 property."""
        self._cards[6].set_value("x0", value)

    @property
    def y0(self) -> typing.Optional[float]:
        """Get or set the Y_max coordinate of the box
        """ # nopep8
        return self._cards[6].get_value("y0")

    @y0.setter
    def y0(self, value: float) -> None:
        """Set the y0 property."""
        self._cards[6].set_value("y0", value)

    @property
    def z0(self) -> typing.Optional[float]:
        """Get or set the Z_max coordinate of the box
        """ # nopep8
        return self._cards[6].get_value("z0")

    @z0.setter
    def z0(self, value: float) -> None:
        """Set the z0 property."""
        self._cards[6].set_value("z0", value)

    @property
    def x1(self) -> typing.Optional[float]:
        """Get or set the X_min coordinate of the box
        """ # nopep8
        return self._cards[6].get_value("x1")

    @x1.setter
    def x1(self, value: float) -> None:
        """Set the x1 property."""
        self._cards[6].set_value("x1", value)

    @property
    def y1(self) -> typing.Optional[float]:
        """Get or set the Y_min coordinate of the box
        """ # nopep8
        return self._cards[6].get_value("y1")

    @y1.setter
    def y1(self, value: float) -> None:
        """Set the y1 property."""
        self._cards[6].set_value("y1", value)

    @property
    def z1(self) -> typing.Optional[float]:
        """Get or set the Z_min coordinate of the box
        """ # nopep8
        return self._cards[6].get_value("z1")

    @z1.setter
    def z1(self, value: float) -> None:
        """Set the z1 property."""
        self._cards[6].set_value("z1", value)

    @property
    def lcsid(self) -> typing.Optional[int]:
        """Get or set the Local coordinate system ID, if defined, the box is aligned with the
        local coordinate system instead of global coordinate system.
        Please see *DEFINE_COORDINATE_ for details
        """ # nopep8
        return self._cards[6].get_value("lcsid")

    @lcsid.setter
    def lcsid(self, value: int) -> None:
        """Set the lcsid property."""
        self._cards[6].set_value("lcsid", value)

    @property
    def x0(self) -> typing.Optional[float]:
        """Get or set the X-coordinate of the center of the sphere
        """ # nopep8
        return self._cards[7].get_value("x0")

    @x0.setter
    def x0(self, value: float) -> None:
        """Set the x0 property."""
        self._cards[7].set_value("x0", value)

    @property
    def y0(self) -> typing.Optional[float]:
        """Get or set the Y-coordinate of the center of the sphere
        """ # nopep8
        return self._cards[7].get_value("y0")

    @y0.setter
    def y0(self, value: float) -> None:
        """Set the y0 property."""
        self._cards[7].set_value("y0", value)

    @property
    def z0(self) -> typing.Optional[float]:
        """Get or set the Z-coordinate of the center of the sphere
        """ # nopep8
        return self._cards[7].get_value("z0")

    @z0.setter
    def z0(self, value: float) -> None:
        """Set the z0 property."""
        self._cards[7].set_value("z0", value)

    @property
    def r0(self) -> typing.Optional[float]:
        """Get or set the Radius of the sphere
        """ # nopep8
        return self._cards[7].get_value("r0")

    @r0.setter
    def r0(self, value: float) -> None:
        """Set the r0 property."""
        self._cards[7].set_value("r0", value)

