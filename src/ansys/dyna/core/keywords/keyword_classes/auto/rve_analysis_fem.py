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

class RveAnalysisFem(KeywordBase):
    """DYNA RVE_ANALYSIS_FEM keyword"""

    keyword = "RVE"
    subkeyword = "ANALYSIS_FEM"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "filename",
                        str,
                        0,
                        80,
                        kwargs.get("filename")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "inpt",
                        int,
                        0,
                        10,
                        kwargs.get("inpt", 0)
                    ),
                    Field(
                        "oupt",
                        int,
                        10,
                        10,
                        kwargs.get("oupt", 1)
                    ),
                    Field(
                        "lcid",
                        int,
                        20,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "idof",
                        int,
                        30,
                        10,
                        kwargs.get("idof")
                    ),
                    Field(
                        "bc",
                        int,
                        40,
                        10,
                        kwargs.get("bc", 0)
                    ),
                    Field(
                        "imatch",
                        int,
                        50,
                        10,
                        kwargs.get("imatch", 1)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "h11",
                        float,
                        0,
                        10,
                        kwargs.get("h11")
                    ),
                    Field(
                        "h22",
                        float,
                        10,
                        10,
                        kwargs.get("h22")
                    ),
                    Field(
                        "h33",
                        float,
                        20,
                        10,
                        kwargs.get("h33")
                    ),
                    Field(
                        "h12",
                        float,
                        30,
                        10,
                        kwargs.get("h12")
                    ),
                    Field(
                        "h23",
                        float,
                        40,
                        10,
                        kwargs.get("h23")
                    ),
                    Field(
                        "h13",
                        float,
                        50,
                        10,
                        kwargs.get("h13")
                    ),
                ],
            ),
        ]

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the Name of an input file that contains the mesh information (nodal coordinates, element connectivity) of the RVE model.
        Note that this keyword variable should be of the format "XXX.k", where file name extension ".k" is included.
        The finite element mesh given in this file is used for the spatial discretization of the material microstructures,
        and it does not involve any special 'control nodes' or 'control elements'
        """ # nopep8
        return self._cards[0].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        self._cards[0].set_value("filename", value)

    @property
    def inpt(self) -> int:
        """Get or set the Type of input:
        EQ.0: RVE boundary conditions are fully defined by two factors: (1) the parameter "BC" of this input card,
        and (2) the mesh information in the file [MESHFILE]. When running an RVE simulation, LS-DYNA automatically
        creates a file named "rve_[MESHFILE].k",
        which contains all the necessary information (e.g., control nodes, displacement constraints, etc.) for boundary condition enforcement.
        EQ.1: Users provide a file named  rve_[MESHFILE].k  to define the boundary condition keywords
        (e.g. *CONSTRAINED_MULTIPLE_GLOBAL, *BOUNDARY_SPC_NODE, *BOUNDARY_MOTION_NODE, etc.) and control nodes for
        enforcing RVE boundary conditions. Note that, it is usually non-trivial to manually define all the keywords for RVE boundary conditions.
        If the file "rve_[MESHFILE].k" is not given when running RVE simulations, then the option INPT=1 will be ignored, and
        LS-DYNA will create "rve_[MESHFILE].k" based on the parameter "BC" of this input card and the mesh information in the file [MESHFILE].
        """ # nopep8
        return self._cards[1].get_value("inpt")

    @inpt.setter
    def inpt(self, value: int) -> None:
        self._cards[1].set_value("inpt", value)

    @property
    def oupt(self) -> int:
        """Get or set the =1: RVE homogenization results will be output to a database file "rveout". Please refer to the keyword *DATABASE_RVE
        """ # nopep8
        return self._cards[1].get_value("oupt")

    @oupt.setter
    def oupt(self, value: int) -> None:
        self._cards[1].set_value("oupt", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the ID of the loading curve. To perform RVE analysis, a loading curve defined by the keyword *DEFINE_CURVE
        is required to specify the loading history. There are two columns in the loading curve, where the first column
        is adopted as a scaling factor for the user-defined macroscopic deformation measure (H11, H22,   H13, which are defined in CARD3 of this *RVE_ANALYSIS_FEM keyword),
        and the second column provides the corresponding scaling factor for the loading time (1.0 in the second column denotes the end of the loading).
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[1].set_value("lcid", value)

    @property
    def idof(self) -> typing.Optional[int]:
        """Get or set the Dimension of the RVE.
        EQ.2: 2D geometry.
        EQ.3: 3D geometry.
        """ # nopep8
        return self._cards[1].get_value("idof")

    @idof.setter
    def idof(self, value: int) -> None:
        self._cards[1].set_value("idof", value)

    @property
    def bc(self) -> int:
        """Get or set the Type of the RVE boundary condition:
        EQ. 0: Periodic Displacement Boundary Condition (PDBC).
        EQ. 1: Linear Displacement Boundary Condition (LDBC).
        """ # nopep8
        return self._cards[1].get_value("bc")

    @bc.setter
    def bc(self, value: int) -> None:
        self._cards[1].set_value("bc", value)

    @property
    def imatch(self) -> int:
        """Get or set the Type of the given RVE mesh:
        EQ. 0: The mesh is non-matching for PDBC.
        EQ. 1: The mesh is PDBC-matching. This variable is effective only when the user chooses to impose PDBC by setting BC=0.
        When the mesh is PDBC-matching, the nodal distributions on the RVEs opposite sides match well with each other.
        For instance, let us consider two opposite surfaces (surface A, and surface B) that are both perpendicular to the X-axis,
        for any FEM node on surface A, if we draw a straight line that is parallel to the X-axis, then the intersection
        point of this line with surface B must also be an FEM node. For such PDBC-matching meshes, an efficient
        direct nearest neighbor search algorithm can be used for the PDBC imposition, so a PDBC-matching mesh is preferred if
        users would like to impose the periodic displacement boundary condition for RVE analysis. However, it is not always straightforward
        to create PDBC-matching meshes for RVE models if very complex material micro-structures exist. In this case,
        IMATCH=0 can be chosen to impose PDBC on a non-matching mesh by employing a projection-based constraint imposition method.
        """ # nopep8
        return self._cards[1].get_value("imatch")

    @imatch.setter
    def imatch(self, value: int) -> None:
        self._cards[1].set_value("imatch", value)

    @property
    def h11(self) -> typing.Optional[float]:
        """Get or set the Component 11 of the prescribed macroscopic displacement gradient.
        """ # nopep8
        return self._cards[2].get_value("h11")

    @h11.setter
    def h11(self, value: float) -> None:
        self._cards[2].set_value("h11", value)

    @property
    def h22(self) -> typing.Optional[float]:
        """Get or set the Component 22 of the prescribed macroscopic displacement gradient.
        """ # nopep8
        return self._cards[2].get_value("h22")

    @h22.setter
    def h22(self, value: float) -> None:
        self._cards[2].set_value("h22", value)

    @property
    def h33(self) -> typing.Optional[float]:
        """Get or set the Component 33 of the prescribed macroscopic displacement gradient.
        """ # nopep8
        return self._cards[2].get_value("h33")

    @h33.setter
    def h33(self, value: float) -> None:
        self._cards[2].set_value("h33", value)

    @property
    def h12(self) -> typing.Optional[float]:
        """Get or set the Component 12 of the prescribed macroscopic displacement gradient.
        """ # nopep8
        return self._cards[2].get_value("h12")

    @h12.setter
    def h12(self, value: float) -> None:
        self._cards[2].set_value("h12", value)

    @property
    def h23(self) -> typing.Optional[float]:
        """Get or set the Component 23 of the prescribed macroscopic displacement gradient.
        """ # nopep8
        return self._cards[2].get_value("h23")

    @h23.setter
    def h23(self, value: float) -> None:
        self._cards[2].set_value("h23", value)

    @property
    def h13(self) -> typing.Optional[float]:
        """Get or set the Component 13 of the prescribed macroscopic displacement gradient.
        """ # nopep8
        return self._cards[2].get_value("h13")

    @h13.setter
    def h13(self, value: float) -> None:
        self._cards[2].set_value("h13", value)

