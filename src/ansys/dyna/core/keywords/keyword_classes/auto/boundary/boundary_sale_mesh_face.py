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

"""Module providing the BoundarySaleMeshFace class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BOUNDARYSALEMESHFACE_CARD0 = (
    FieldSchema("bctype", str, 0, 10, "FIXED"),
    FieldSchema("mshid", int, 10, 10, None),
    FieldSchema("negx", int, 20, 10, 0),
    FieldSchema("posx", int, 30, 10, 0),
    FieldSchema("negy", int, 40, 10, 0),
    FieldSchema("posy", int, 50, 10, 0),
    FieldSchema("negz", int, 60, 10, 0),
    FieldSchema("posz", int, 70, 10, 0),
)

class BoundarySaleMeshFace(KeywordBase):
    """DYNA BOUNDARY_SALE_MESH_FACE keyword"""

    keyword = "BOUNDARY"
    subkeyword = "SALE_MESH_FACE"

    def __init__(self, **kwargs):
        """Initialize the BoundarySaleMeshFace class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYSALEMESHFACE_CARD0,
                **kwargs,
            ),
        ]
    @property
    def bctype(self) -> str:
        """Get or set the Available boundary conditions:
        EQ.FIXED: All nodes at the face are fixed in all directions. (Applied with with *BOUNDARY_SPC_SET)
        EQ.NOFLOW: No flow allowed through the face.                     (Applied with * BOUNDARY_SPC_SET)
        EQ.SYM: The face is a symmetric plane(same as NOFLOW). (Applied with * BOUNDARY_SPC_SET)
        EQ.NONREFL: Non - reflective boundary condition.                (Applied with * BOUNDARY_NON_REFLECTING)
        EQ.FLOWVEL: Nodes constrained by time - dependent velocities. (Applied with * BOUNDARY_PRESCRIBED_MOTION)
        EQ.PRES: Faces loaded by time - dependent pressures.    (Applied with * LOAD_SEGMENT_SET)
        EQ.AMBIENT: Ambient elements attached to the faces.See Remarks 1 and 2.                                           (Applied with * BOUNDARY_AMBIENT)
        """ # nopep8
        return self._cards[0].get_value("bctype")

    @bctype.setter
    def bctype(self, value: str) -> None:
        """Set the bctype property."""
        if value not in ["FIXED", "NOFLOW", "SYM", "NONREFL", "FLOWVEL", "PRES", "AMBIENT", None]:
            raise Exception("""bctype must be `None` or one of {"FIXED","NOFLOW","SYM","NONREFL","FLOWVEL","PRES","AMBIENT"}.""")
        self._cards[0].set_value("bctype", value)

    @property
    def mshid(self) -> typing.Optional[int]:
        """Get or set the S-ALE Mesh ID
        """ # nopep8
        return self._cards[0].get_value("mshid")

    @mshid.setter
    def mshid(self, value: int) -> None:
        """Set the mshid property."""
        self._cards[0].set_value("mshid", value)

    @property
    def negx(self) -> int:
        """Get or set the Determine where the boundary condition is applied to the mesh. NEGX, POSX, NEGY, POSY, NEGZ, or POSZ means the mesh faces with an outward normal vector in the local -x, +x, -y, +y, -z, or +z-directions, respectively.
        For BCTYPE != PRES, FLOWVEL, or AMBIENT:
        EQ.0: The boundary condition is not applied to faces with this outward normal.
        EQ.1: The boundary condition is applied to faces with this outward normal.
        For BCTYPE = PRES or FLOWVEL :
        EQ.0: The boundary condition is not applied to faces with this outward normal.
        GT.0: The boundary condition is applied to faces with this outward normal and is controlled by a time - dependent curve(*DEFINE_CURVE), whose ID is referred to with this
        For BCTYPE = AMBIENT:
        EQ.0: The boundary condition is not applied to the elements connected to the faces with this outward normal.
        GT.0: The boundary condition is applied to the elements connected to the faces with this outward normal.The value sets the reservoir material as described in Remark???? 1.
        """ # nopep8
        return self._cards[0].get_value("negx")

    @negx.setter
    def negx(self, value: int) -> None:
        """Set the negx property."""
        self._cards[0].set_value("negx", value)

    @property
    def posx(self) -> int:
        """Get or set the Determine where the boundary condition is applied to the mesh. NEGX, POSX, NEGY, POSY, NEGZ, or POSZ means the mesh faces with an outward normal vector in the local -x, +x, -y, +y, -z, or +z-directions, respectively.
        For BCTYPE != PRES, FLOWVEL, or AMBIENT:
        EQ.0: The boundary condition is not applied to faces with this outward normal.
        EQ.1: The boundary condition is applied to faces with this outward normal.
        For BCTYPE = PRES or FLOWVEL :
        EQ.0: The boundary condition is not applied to faces with this outward normal.
        GT.0: The boundary condition is applied to faces with this outward normal and is controlled by a time - dependent curve(*DEFINE_CURVE), whose ID is referred to with this
        For BCTYPE = AMBIENT:
        EQ.0: The boundary condition is not applied to the elements connected to the faces with this outward normal.
        GT.0: The boundary condition is applied to the elements connected to the faces with this outward normal.The value sets the reservoir material as described in Remark???? 1.
        """ # nopep8
        return self._cards[0].get_value("posx")

    @posx.setter
    def posx(self, value: int) -> None:
        """Set the posx property."""
        self._cards[0].set_value("posx", value)

    @property
    def negy(self) -> int:
        """Get or set the Determine where the boundary condition is applied to the mesh. NEGX, POSX, NEGY, POSY, NEGZ, or POSZ means the mesh faces with an outward normal vector in the local -x, +x, -y, +y, -z, or +z-directions, respectively.
        For BCTYPE != PRES, FLOWVEL, or AMBIENT:
        EQ.0: The boundary condition is not applied to faces with this outward normal.
        EQ.1: The boundary condition is applied to faces with this outward normal.
        For BCTYPE = PRES or FLOWVEL :
        EQ.0: The boundary condition is not applied to faces with this outward normal.
        GT.0: The boundary condition is applied to faces with this outward normal and is controlled by a time - dependent curve(*DEFINE_CURVE), whose ID is referred to with this
        For BCTYPE = AMBIENT:
        EQ.0: The boundary condition is not applied to the elements connected to the faces with this outward normal.
        GT.0: The boundary condition is applied to the elements connected to the faces with this outward normal.The value sets the reservoir material as described in Remark???? 1.
        """ # nopep8
        return self._cards[0].get_value("negy")

    @negy.setter
    def negy(self, value: int) -> None:
        """Set the negy property."""
        self._cards[0].set_value("negy", value)

    @property
    def posy(self) -> int:
        """Get or set the Determine where the boundary condition is applied to the mesh. NEGX, POSX, NEGY, POSY, NEGZ, or POSZ means the mesh faces with an outward normal vector in the local -x, +x, -y, +y, -z, or +z-directions, respectively.
        For BCTYPE != PRES, FLOWVEL, or AMBIENT:
        EQ.0: The boundary condition is not applied to faces with this outward normal.
        EQ.1: The boundary condition is applied to faces with this outward normal.
        For BCTYPE = PRES or FLOWVEL :
        EQ.0: The boundary condition is not applied to faces with this outward normal.
        GT.0: The boundary condition is applied to faces with this outward normal and is controlled by a time - dependent curve(*DEFINE_CURVE), whose ID is referred to with this
        For BCTYPE = AMBIENT:
        EQ.0: The boundary condition is not applied to the elements connected to the faces with this outward normal.
        GT.0: The boundary condition is applied to the elements connected to the faces with this outward normal.The value sets the reservoir material as described in Remark???? 1.
        """ # nopep8
        return self._cards[0].get_value("posy")

    @posy.setter
    def posy(self, value: int) -> None:
        """Set the posy property."""
        self._cards[0].set_value("posy", value)

    @property
    def negz(self) -> int:
        """Get or set the Determine where the boundary condition is applied to the mesh. NEGX, POSX, NEGY, POSY, NEGZ, or POSZ means the mesh faces with an outward normal vector in the local -x, +x, -y, +y, -z, or +z-directions, respectively.
        For BCTYPE != PRES, FLOWVEL, or AMBIENT:
        EQ.0: The boundary condition is not applied to faces with this outward normal.
        EQ.1: The boundary condition is applied to faces with this outward normal.
        For BCTYPE = PRES or FLOWVEL :
        EQ.0: The boundary condition is not applied to faces with this outward normal.
        GT.0: The boundary condition is applied to faces with this outward normal and is controlled by a time - dependent curve(*DEFINE_CURVE), whose ID is referred to with this
        For BCTYPE = AMBIENT:
        EQ.0: The boundary condition is not applied to the elements connected to the faces with this outward normal.
        GT.0: The boundary condition is applied to the elements connected to the faces with this outward normal.The value sets the reservoir material as described in Remark???? 1.
        """ # nopep8
        return self._cards[0].get_value("negz")

    @negz.setter
    def negz(self, value: int) -> None:
        """Set the negz property."""
        self._cards[0].set_value("negz", value)

    @property
    def posz(self) -> int:
        """Get or set the Determine where the boundary condition is applied to the mesh. NEGX, POSX, NEGY, POSY, NEGZ, or POSZ means the mesh faces with an outward normal vector in the local -x, +x, -y, +y, -z, or +z-directions, respectively.
        For BCTYPE != PRES, FLOWVEL, or AMBIENT:
        EQ.0: The boundary condition is not applied to faces with this outward normal.
        EQ.1: The boundary condition is applied to faces with this outward normal.
        For BCTYPE = PRES or FLOWVEL :
        EQ.0: The boundary condition is not applied to faces with this outward normal.
        GT.0: The boundary condition is applied to faces with this outward normal and is controlled by a time - dependent curve(*DEFINE_CURVE), whose ID is referred to with this
        For BCTYPE = AMBIENT:
        EQ.0: The boundary condition is not applied to the elements connected to the faces with this outward normal.
        GT.0: The boundary condition is applied to the elements connected to the faces with this outward normal.The value sets the reservoir material as described in Remark???? 1.
        """ # nopep8
        return self._cards[0].get_value("posz")

    @posz.setter
    def posz(self, value: int) -> None:
        """Set the posz property."""
        self._cards[0].set_value("posz", value)

