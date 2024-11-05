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

class IcfdControlMesh(KeywordBase):
    """DYNA ICFD_CONTROL_MESH keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_MESH"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "mgsf",
                        float,
                        0,
                        10,
                        kwargs.get("mgsf", 1.41)
                    ),
                    Field(
                        "unused",
                        int,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "mstrat",
                        int,
                        20,
                        10,
                        kwargs.get("mstrat", 0)
                    ),
                    Field(
                        "2dstruc",
                        int,
                        30,
                        10,
                        kwargs.get("2dstruc", 0)
                    ),
                    Field(
                        "nrmsh",
                        int,
                        40,
                        10,
                        kwargs.get("nrmsh", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "aver",
                        int,
                        0,
                        10,
                        kwargs.get("aver", 14)
                    ),
                ],
            ),
        ]

    @property
    def mgsf(self) -> float:
        """Get or set the Mesh Growth Scale Factor : Specifies the maximum mesh size that the volume mesher is allowed to use when generating the volume mesh based on the mesh surface element sizes defined in *MESH_SURFACE_ELEMENT. Values between 1 and 2 are allowed. Values closer to 1 will result in a finer volume mesh (1 means the volume mesh is not allowed to be coarser than the element size from the closest surface meshes) and val# ues closer to 2 will result in a coarser volume mesh (2 means the volume can use elements as much as twice as coarse as those from the closest surface mesh).
        """ # nopep8
        return self._cards[0].get_value("mgsf")

    @mgsf.setter
    def mgsf(self, value: float) -> None:
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
        if value not in [0, 1]:
            raise Exception("""mstrat must be one of {0,1}""")
        self._cards[0].set_value("mstrat", value)

    @property
    def _2dstruc(self) -> int:
        """Get or set the Flag to decide between a unstructured mesh generation strategy in 2D or a structured mesh strategy: EQ.0: Structured mesh.
        EQ.1: Unstructured mesh.
        """ # nopep8
        return self._cards[0].get_value("2dstruc")

    @_2dstruc.setter
    def _2dstruc(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""_2dstruc must be one of {0,1}""")
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
        if value not in [0, 1]:
            raise Exception("""nrmsh must be one of {0,1}""")
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
        self._cards[1].set_value("aver", value)

