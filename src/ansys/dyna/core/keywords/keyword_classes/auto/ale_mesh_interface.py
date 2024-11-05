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

class AleMeshInterface(KeywordBase):
    """DYNA ALE_MESH_INTERFACE keyword"""

    keyword = "ALE"
    subkeyword = "MESH_INTERFACE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "mmgset",
                        int,
                        0,
                        10,
                        kwargs.get("mmgset")
                    ),
                    Field(
                        "nowrt",
                        int,
                        10,
                        10,
                        kwargs.get("nowrt", 0)
                    ),
                    Field(
                        "volrat",
                        float,
                        20,
                        10,
                        kwargs.get("volrat", 0.0)
                    ),
                    Field(
                        "interp",
                        int,
                        30,
                        10,
                        kwargs.get("interp", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "edgmin",
                        float,
                        0,
                        10,
                        kwargs.get("edgmin", 0.0)
                    ),
                    Field(
                        "edgmax",
                        float,
                        10,
                        10,
                        kwargs.get("edgmax", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def mmgset(self) -> typing.Optional[int]:
        """Get or set the Multi-Material Set ID (see *SET_MULTI-MATERIAL_GROUP_LIST).
        The materials (or ALE groups) in this set are selected to be meshed.
        """ # nopep8
        return self._cards[0].get_value("mmgset")

    @mmgset.setter
    def mmgset(self, value: int) -> None:
        self._cards[0].set_value("mmgset", value)

    @property
    def nowrt(self) -> int:
        """Get or set the Three digit flag to deselect which file to output:
        EQ.__0:	Write a first try of the triangular meshes for the material interfaces(see Remark 1).The mesh is output in a keyword file called alemeshmatint.k.
        EQ.__1 : Do not output alemeshmatint.k.
        EQ._0_ : Write triangular meshes of the material interfaces, after  their remeshing(see Remark 2), in a keyword file called aleremeshmatint.k.
        EQ._1_ : Do not output aleremeshmatint.k.
        EQ.0__ : Write tetrahedral meshes of the material volumes in a keyword file called alemeshmatvol.k.
        EQ.1__ : Do not output alemeshmatvol.k.
        """ # nopep8
        return self._cards[0].get_value("nowrt")

    @nowrt.setter
    def nowrt(self, value: int) -> None:
        self._cards[0].set_value("nowrt", value)

    @property
    def volrat(self) -> float:
        """Get or set the Mesh volume ratio beyond which the mesh is output (see Remark 3)
        """ # nopep8
        return self._cards[0].get_value("volrat")

    @volrat.setter
    def volrat(self, value: float) -> None:
        self._cards[0].set_value("volrat", value)

    @property
    def interp(self) -> int:
        """Get or set the Interpolating method :
        EQ.0‌:     The ALE hexahedron data are interpolated at the Lagrangian tetrahedron centers.
        EQ.1‌ : The intersection volumes between ALE hexahedra and Lagrangian tetrahedra are computed and the ALE data are mapped to the Lagrangian elements with a volume - averaged method.
        """ # nopep8
        return self._cards[0].get_value("interp")

    @interp.setter
    def interp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""interp must be one of {0,1}""")
        self._cards[0].set_value("interp", value)

    @property
    def edgmin(self) -> float:
        """Get or set the Minimum triangle edge applied during remeshing (see Remark 2).
        """ # nopep8
        return self._cards[1].get_value("edgmin")

    @edgmin.setter
    def edgmin(self, value: float) -> None:
        self._cards[1].set_value("edgmin", value)

    @property
    def edgmax(self) -> float:
        """Get or set the Maximum triangle edge applied during remeshing (see Remark 2).
        """ # nopep8
        return self._cards[1].get_value("edgmax")

    @edgmax.setter
    def edgmax(self, value: float) -> None:
        self._cards[1].set_value("edgmax", value)

