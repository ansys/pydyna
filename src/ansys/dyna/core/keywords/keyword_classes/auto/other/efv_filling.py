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

"""Module providing the EfvFilling class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_EFVFILLING_CARD0 = (
    FieldSchema("psid", int, 0, 10, None),
)

_EFVFILLING_CARD1 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("inid", int, 20, 10, None),
)

_EFVFILLING_CARD2 = (
    FieldSchema("geom", str, 0, 10, " "),
    FieldSchema("e1", float, 10, 10, None),
    FieldSchema("e2", float, 20, 10, None),
    FieldSchema("e3", float, 30, 10, None),
    FieldSchema("e4", float, 40, 10, None),
    FieldSchema("e5", float, 50, 10, None),
    FieldSchema("e6", float, 60, 10, None),
)

class EfvFilling(KeywordBase):
    """DYNA EFV_FILLING keyword"""

    keyword = "EFV"
    subkeyword = "FILLING"
    _link_fields = {
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the EfvFilling class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVFILLING_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVFILLING_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVFILLING_CARD2,
                **kwargs,
            ),
        ]
    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part set ID defined in *EFV_BOX_MESH,*EFV_CYLINDRICAL_MESH, or *EFV_STRUCTURED_MESH
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[0].set_value("psid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of the material part filling GEOM.
        """ # nopep8
        return self._cards[1].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[1].set_value("pid", value)

    @property
    def inid(self) -> typing.Optional[int]:
        """Get or set the ID of *EFV_INITIAL to initialize the material part (densities, internal energies, velocities,).
        """ # nopep8
        return self._cards[1].get_value("inid")

    @inid.setter
    def inid(self, value: int) -> None:
        """Set the inid property."""
        self._cards[1].set_value("inid", value)

    @property
    def geom(self) -> str:
        """Get or set the Geometry types.See the table below for more details:
        The "GEOM� column in the table below enumerates the allowed values for the geometry variable as well as describing E1, �, E6 for each geometry type.  Each of the following operations accepts up to 6 arguments but may take fewer.  Values of En left unspecified are ignored.
        CPT Mesh region defined by control points.E1 is the minimum control point along the x - axis.E2 is the maximum control point along the x - axis.E3 is the minimum control point along the y - axis.E4 is the maximum control point along the y - axis.E5 is the minimum control point along the z - axis.E6 is the maximum control point along the z - axis.
        REPLACE Fill a part in an Eulerian mesh region with the material from a part that has a dummy mesh superimposed on the Eulerian mesh.The dummy mesh specifies the Eulerian mesh region.The dummy mesh has part ID PID.The dummy mesh should be made of shells in 2D and solids in 3D. * SECTION_EFV with ELFORM = 9 and *EFV_MAT should specify the part section and material for PID, respectively.E1 is the material part in the Eulerian mesh region to be replaced by the material in PID.
        BOX* DEFINE_BOX ID that defines a box to fill with the material from PID.*SECTION_EFV with ELFORM = 9 and *EFV_MAT should specify the part section and material for PID, respectively.E2 is the material part in the box to be replaced by the material in PID.
        CYLINDER Define a cylinder to fill with the material from PID.*SECTION_EFV with ELFORM = 9 and *EFV_MAT should specify the part section and material for PID, respectively.E1and E2 are the node IDs that locate the base centers.E3and E4 are the base radii.E5 is the number of nodes along the base circumferences.E6 is the material part in the cylinder to be replaced by the material in PID.
        SPHERE Define a sphere or ellipsoid to fill with the material from PID. * SECTION_EFV with ELFORM = 9 and *EFV_MAT should specify the part section and material for PID, respectively.E1 is the node ID giving the center of the sphere or ellipsoid.E6 is the ID of a local coordinate system(see * DEFINE_COORDINATE_SYSTEM).E2, E3,and E4 are the radii in each direction of this coordinate system.They can be different to form an ellipsoid.E5 is the number of nodes along the circumference.E6 is the material part in the sphere / ellipsoid to be replaced by the material in PID.
        SEGSET Use segments to specify the boundary of a geometry to be filled with the material from PID. * SECTION_EFV with ELFORM = 9 and *EFV_MAT should specify the part section and material for PID, respectively.E1 is the set ID of the segments(see * SET_SEGMENT) that define the boundary of the geometry to fill.E2 is the material part in the geometry to be replaced by the material in PID.
        RECTANGLE In 2D, fill a rectangle defined between 2 corners with the smallest coordinates(XMIN, YMIN) and largest coordinates(XMAX, YMAX).E1 is a switching flag to determine whether to fill the interior or exterior of the rectangle.E1 = 0 means fill the interior while E1 = 1 means fill the exterior.E2, E3, E4,and E5 are XMIN, XMAX, YMIN,and YMAX, respectively.
        QUAD In 2D, fill a quadrilateral defined by 4 nodes: E2, E3, E4,and E5.The node numbering of the quadrilateral must be counterclockwise around the boundary.E1 is a switching flag to determine whether to fill the interior or exterior of the rectangle.E1 = 0 means fill the interior while E1 = 1 means fill the exterior.
        ELLIPSE In 2D, fill an ellipse centered at the point(XC, YC) with the semi - axis along the X - axis DX and the semi - axis along the Y - axis DY.E1 is a switching flag to flip the side to fill(by default E1 = 0, if E1 = 1 fill the other side).E2 = XC, E3 = YC, E4 = DX, E5 = DY.
        PARABOLA In 2D, fill a side of a parabola defined by X = AY ** 2 + BY + C.E1 is a flag setting which side of the parabola to fill.For E1 = 0, the side of the parabola with convex geometry is filled, meaning any two points in region can be connected with a line segment that remains in the region.For E1 = 1, concave geometry side of the parabola is filled.See Figure 0 - 1. Here, E2, E3,and E4 specify A, B,and C, respectively.
        HALFSPACE In 2D, fill the region on one side of a line defined by the points(E2, E3) and (E4, E5) in x - y space.E1 determines the region filled.When E1 = 0, the region on the left side of the line is filled when going along the line from the point(E2, E3) to the point(E4, E5).When E1 = 0, the region on the right side of the line is filled.
        UNUSED Mesh region defined by control points where the elements are unused.E1 is the minimum control point along the x - axis.E2 is the maximum control point along the x - axis.E3 is the minimum control point along the y - axis.E4 is the maximum control point along the y - axis.E5 is the minimum control point along the z - axis.E6 is the maximum control point along the z - axis.PID is ignored for this case.Materials occupying the elements are removed.
        """ # nopep8
        return self._cards[2].get_value("geom")

    @geom.setter
    def geom(self, value: str) -> None:
        """Set the geom property."""
        if value not in ["CPT", "REPLACE", "BOX", "CYLINDER", "SPHERE", "SEGSET", "RECTANGLE", "QUAD", "ELLIPSE", "PARABOLA", "HALFSPACE", "UNUSED", None]:
            raise Exception("""geom must be `None` or one of {"CPT","REPLACE","BOX","CYLINDER","SPHERE","SEGSET","RECTANGLE","QUAD","ELLIPSE","PARABOLA","HALFSPACE","UNUSED"}.""")
        self._cards[2].set_value("geom", value)

    @property
    def e1(self) -> typing.Optional[float]:
        """Get or set the These values have different definitions for different options.  See the table below for details.
        """ # nopep8
        return self._cards[2].get_value("e1")

    @e1.setter
    def e1(self, value: float) -> None:
        """Set the e1 property."""
        self._cards[2].set_value("e1", value)

    @property
    def e2(self) -> typing.Optional[float]:
        """Get or set the These values have different definitions for different options.  See the table below for details.
        """ # nopep8
        return self._cards[2].get_value("e2")

    @e2.setter
    def e2(self, value: float) -> None:
        """Set the e2 property."""
        self._cards[2].set_value("e2", value)

    @property
    def e3(self) -> typing.Optional[float]:
        """Get or set the These values have different definitions for different options.  See the table below for details.
        """ # nopep8
        return self._cards[2].get_value("e3")

    @e3.setter
    def e3(self, value: float) -> None:
        """Set the e3 property."""
        self._cards[2].set_value("e3", value)

    @property
    def e4(self) -> typing.Optional[float]:
        """Get or set the These values have different definitions for different options.  See the table below for details.
        """ # nopep8
        return self._cards[2].get_value("e4")

    @e4.setter
    def e4(self, value: float) -> None:
        """Set the e4 property."""
        self._cards[2].set_value("e4", value)

    @property
    def e5(self) -> typing.Optional[float]:
        """Get or set the These values have different definitions for different options.  See the table below for details.
        """ # nopep8
        return self._cards[2].get_value("e5")

    @e5.setter
    def e5(self, value: float) -> None:
        """Set the e5 property."""
        self._cards[2].set_value("e5", value)

    @property
    def e6(self) -> typing.Optional[float]:
        """Get or set the These values have different definitions for different options.  See the table below for details.
        """ # nopep8
        return self._cards[2].get_value("e6")

    @e6.setter
    def e6(self, value: float) -> None:
        """Set the e6 property."""
        self._cards[2].set_value("e6", value)

    @property
    def pid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

