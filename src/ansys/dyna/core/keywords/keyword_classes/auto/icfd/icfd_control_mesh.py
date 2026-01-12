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

"""Module providing the IcfdControlMesh class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDCONTROLMESH_CARD0 = (
    FieldSchema("mgsf", float, 0, 10, 1.41),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("mstrat", int, 20, 10, 0),
    FieldSchema("2dstruc", int, 30, 10, 0),
    FieldSchema("nrmsh", int, 40, 10, 0),
)

_ICFDCONTROLMESH_CARD1 = (
    FieldSchema("aver", int, 0, 10, 14),
)

class IcfdControlMesh(KeywordBase):
    """DYNA ICFD_CONTROL_MESH keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_MESH"

    def __init__(self, **kwargs):
        """Initialize the IcfdControlMesh class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLMESH_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLMESH_CARD1,
                **kwargs,
            ),        ]
    @property
    def mgsf(self) -> float:
        """Get or set the Mesh Growth Scale Factor : Specifies the maximum mesh size that the volume mesher is allowed to use when generating the volume mesh based on the mesh surface element sizes defined in *MESH_SURFACE_ELEMENT. Values between 1 and 2 are allowed. Values closer to 1 will result in a finer volume mesh (1 means the volume mesh is not allowed to be coarser than the element size from the closest surface meshes) and val# ues closer to 2 will result in a coarser volume mesh (2 means the volume can use elements as much as twice as coarse as those from the closest surface mesh).
        """ # nopep8
        return self._cards[0].get_value("mgsf")

    @mgsf.setter
    def mgsf(self, value: float) -> None:
        """Set the mgsf property."""
        self._cards[0].set_value("mgsf", value)

    @property
    def mstrat(self) -> int:
        """Get or set the Mesh generation strategy:
        EQ.0: Mesh generation based on Delaunay criteria.
        EQ.1: Mesh generation based on octree (See Remark 2).
        """ # nopep8
        return self._cards[0].get_value("mstrat")

    @mstrat.setter
    def mstrat(self, value: int) -> None:
        """Set the mstrat property."""
        if value not in [0, 1, None]:
            raise Exception("""mstrat must be `None` or one of {0,1}.""")
        self._cards[0].set_value("mstrat", value)

    @property
    def _2dstruc(self) -> int:
        """Get or set the Flag to decide between a unstructured mesh generation strategy in 2D or a structured mesh strategy: EQ.0: Structured mesh.
        EQ.1: Unstructured mesh.
        """ # nopep8
        return self._cards[0].get_value("2dstruc")

    @_2dstruc.setter
    def _2dstruc(self, value: int) -> None:
        """Set the _2dstruc property."""
        if value not in [0, 1, None]:
            raise Exception("""_2dstruc must be `None` or one of {0,1}.""")
        self._cards[0].set_value("2dstruc", value)

    @property
    def nrmsh(self) -> int:
        """Get or set the Flag to turn off any remeshing:
        EQ.0:Remeshing possible
        EQ.1:Remeshing impossible
        """ # nopep8
        return self._cards[0].get_value("nrmsh")

    @nrmsh.setter
    def nrmsh(self, value: int) -> None:
        """Set the nrmsh property."""
        if value not in [0, 1, None]:
            raise Exception("""nrmsh must be `None` or one of {0,1}.""")
        self._cards[0].set_value("nrmsh", value)

    @property
    def aver(self) -> int:
        """Get or set the Automatic Volume Mesher version :
        EQ.14 :	Version 14.
        EQ.16 : Version 16. (See Remark 4)
        """ # nopep8
        return self._cards[1].get_value("aver")

    @aver.setter
    def aver(self, value: int) -> None:
        """Set the aver property."""
        self._cards[1].set_value("aver", value)

