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

"""Module providing the AleStructuredMeshRefineRegion class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ALESTRUCTUREDMESHREFINEREGION_CARD0 = (
    FieldSchema("mshid", int, 0, 10, 0),
    FieldSchema("ifx", int, 10, 10, 1),
    FieldSchema("ify", int, 20, 10, 1),
    FieldSchema("ifz", int, 30, 10, 1),
)

_ALESTRUCTUREDMESHREFINEREGION_CARD1 = (
    FieldSchema("imin", int, 0, 10, None),
    FieldSchema("imax", int, 10, 10, None),
    FieldSchema("jmin", int, 20, 10, None),
    FieldSchema("jmax", int, 30, 10, None),
    FieldSchema("kmin", int, 40, 10, None),
    FieldSchema("kmax", int, 50, 10, None),
)

class AleStructuredMeshRefineRegion(KeywordBase):
    """DYNA ALE_STRUCTURED_MESH_REFINE_REGION keyword"""

    keyword = "ALE"
    subkeyword = "STRUCTURED_MESH_REFINE_REGION"

    def __init__(self, **kwargs):
        """Initialize the AleStructuredMeshRefineRegion class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ALESTRUCTUREDMESHREFINEREGION_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ALESTRUCTUREDMESHREFINEREGION_CARD1,
                **kwargs,
            ),
        ]
    @property
    def mshid(self) -> int:
        """Get or set the S-ALE mesh ID for S-ALE mesh to be refined.
        """ # nopep8
        return self._cards[0].get_value("mshid")

    @mshid.setter
    def mshid(self, value: int) -> None:
        """Set the mshid property."""
        self._cards[0].set_value("mshid", value)

    @property
    def ifx(self) -> int:
        """Get or set the Refinement factor for each local direction.  The number of elements in each direction of the new mesh is the refinement factor for that direction multiplied by the current number of elements in that direction.  They must be integers..
        """ # nopep8
        return self._cards[0].get_value("ifx")

    @ifx.setter
    def ifx(self, value: int) -> None:
        """Set the ifx property."""
        self._cards[0].set_value("ifx", value)

    @property
    def ify(self) -> int:
        """Get or set the Refinement factor for each local direction.  The number of elements in each direction of the new mesh is the refinement factor for that direction multiplied by the current number of elements in that direction.  They must be integers..
        """ # nopep8
        return self._cards[0].get_value("ify")

    @ify.setter
    def ify(self, value: int) -> None:
        """Set the ify property."""
        self._cards[0].set_value("ify", value)

    @property
    def ifz(self) -> int:
        """Get or set the Refinement factor for each local direction.  The number of elements in each direction of the new mesh is the refinement factor for that direction multiplied by the current number of elements in that direction.  They must be integers..
        """ # nopep8
        return self._cards[0].get_value("ifz")

    @ifz.setter
    def ifz(self, value: int) -> None:
        """Set the ifz property."""
        self._cards[0].set_value("ifz", value)

    @property
    def imin(self) -> typing.Optional[int]:
        """Get or set the Minimum and maximum nodal indices (or control points) along each direction of the S-ALE mesh that defines a region to refine. These integers should be between the smallest and largest control points of *ALE_STRUCTURED_MESH_CONTROL_POINTS keywords called by *ALE_STRUCTURED_MESH to create the mesh with the specified MSHID.Note that for 2D S-ALE, KMIN and KMAX are ignored.
        """ # nopep8
        return self._cards[1].get_value("imin")

    @imin.setter
    def imin(self, value: int) -> None:
        """Set the imin property."""
        self._cards[1].set_value("imin", value)

    @property
    def imax(self) -> typing.Optional[int]:
        """Get or set the Minimum and maximum nodal indices (or control points) along each direction of the S-ALE mesh that defines a region to refine. These integers should be between the smallest and largest control points of *ALE_STRUCTURED_MESH_CONTROL_POINTS keywords called by *ALE_STRUCTURED_MESH to create the mesh with the specified MSHID.Note that for 2D S-ALE, KMIN and KMAX are ignored.
        """ # nopep8
        return self._cards[1].get_value("imax")

    @imax.setter
    def imax(self, value: int) -> None:
        """Set the imax property."""
        self._cards[1].set_value("imax", value)

    @property
    def jmin(self) -> typing.Optional[int]:
        """Get or set the Minimum and maximum nodal indices (or control points) along each direction of the S-ALE mesh that defines a region to refine. These integers should be between the smallest and largest control points of *ALE_STRUCTURED_MESH_CONTROL_POINTS keywords called by *ALE_STRUCTURED_MESH to create the mesh with the specified MSHID.Note that for 2D S-ALE, KMIN and KMAX are ignored.
        """ # nopep8
        return self._cards[1].get_value("jmin")

    @jmin.setter
    def jmin(self, value: int) -> None:
        """Set the jmin property."""
        self._cards[1].set_value("jmin", value)

    @property
    def jmax(self) -> typing.Optional[int]:
        """Get or set the Minimum and maximum nodal indices (or control points) along each direction of the S-ALE mesh that defines a region to refine. These integers should be between the smallest and largest control points of *ALE_STRUCTURED_MESH_CONTROL_POINTS keywords called by *ALE_STRUCTURED_MESH to create the mesh with the specified MSHID.Note that for 2D S-ALE, KMIN and KMAX are ignored.
        """ # nopep8
        return self._cards[1].get_value("jmax")

    @jmax.setter
    def jmax(self, value: int) -> None:
        """Set the jmax property."""
        self._cards[1].set_value("jmax", value)

    @property
    def kmin(self) -> typing.Optional[int]:
        """Get or set the Minimum and maximum nodal indices (or control points) along each direction of the S-ALE mesh that defines a region to refine. These integers should be between the smallest and largest control points of *ALE_STRUCTURED_MESH_CONTROL_POINTS keywords called by *ALE_STRUCTURED_MESH to create the mesh with the specified MSHID.Note that for 2D S-ALE, KMIN and KMAX are ignored.
        """ # nopep8
        return self._cards[1].get_value("kmin")

    @kmin.setter
    def kmin(self, value: int) -> None:
        """Set the kmin property."""
        self._cards[1].set_value("kmin", value)

    @property
    def kmax(self) -> typing.Optional[int]:
        """Get or set the Minimum and maximum nodal indices (or control points) along each direction of the S-ALE mesh that defines a region to refine. These integers should be between the smallest and largest control points of *ALE_STRUCTURED_MESH_CONTROL_POINTS keywords called by *ALE_STRUCTURED_MESH to create the mesh with the specified MSHID.Note that for 2D S-ALE, KMIN and KMAX are ignored.
        """ # nopep8
        return self._cards[1].get_value("kmax")

    @kmax.setter
    def kmax(self, value: int) -> None:
        """Set the kmax property."""
        self._cards[1].set_value("kmax", value)

