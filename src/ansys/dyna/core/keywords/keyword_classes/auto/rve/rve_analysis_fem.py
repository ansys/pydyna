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

"""Module providing the RveAnalysisFem class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_RVEANALYSISFEM_CARD0 = (
    FieldSchema("filename", str, 0, 80, None),
)

_RVEANALYSISFEM_CARD1 = (
    FieldSchema("inpt", int, 0, 10, 0),
    FieldSchema("oupt", int, 10, 10, 1),
    FieldSchema("lcid", int, 20, 10, None),
    FieldSchema("idof", int, 30, 10, None),
    FieldSchema("bc", int, 40, 10, 0),
    FieldSchema("imatch", int, 50, 10, 1),
    FieldSchema("image", int, 60, 10, 1),
)

_RVEANALYSISFEM_CARD2 = (
    FieldSchema("h11", float, 0, 10, None),
    FieldSchema("h22", float, 10, 10, None),
    FieldSchema("h33", float, 20, 10, None),
    FieldSchema("h12", float, 30, 10, None),
    FieldSchema("h23", float, 40, 10, None),
    FieldSchema("h13", float, 50, 10, None),
)

_RVEANALYSISFEM_CARD3 = (
    FieldSchema("px", float, 0, 10, None),
    FieldSchema("py", float, 10, 10, None),
    FieldSchema("pz", float, 20, 10, None),
)

class RveAnalysisFem(KeywordBase):
    """DYNA RVE_ANALYSIS_FEM keyword"""

    keyword = "RVE"
    subkeyword = "ANALYSIS_FEM"
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the RveAnalysisFem class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _RVEANALYSISFEM_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _RVEANALYSISFEM_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _RVEANALYSISFEM_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _RVEANALYSISFEM_CARD3,
                **kwargs,
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
        """Set the filename property."""
        self._cards[0].set_value("filename", value)

    @property
    def inpt(self) -> int:
        """Get or set the Type of input:
        EQ.0: RVE boundary conditions are fully defined by two factors: (1) the parameter BC of this input card,and (2) the mesh information in the file specified in MESHFILE.When running an RVE simulation, the solver automatically creates a file named rve_[basename].k.This file contains all the necessary information(e.g., dummy nodes, displacement constraints, etc.) for boundary condition enforcement.
        EQ.1: You provide a file named rve_[basename].k to specify the boundary condition keywords(e.g. *CONSTRAINED_MULTIPLE_GLOBAL, *BOUNDARY_SPC_NODE, etc.) and dummy nodes for enforcing RVE boundary conditions.We do not recommend this option since it is usually nontrivial to manually define all the keywords for RVE boundary conditions.If the file rve_[basename].k is not found when running RVE simulations, then the solver creates rve_[basename].k based on the parameter BC and the mesh information in the file specified with MESHFILE.
        """ # nopep8
        return self._cards[1].get_value("inpt")

    @inpt.setter
    def inpt(self, value: int) -> None:
        """Set the inpt property."""
        self._cards[1].set_value("inpt", value)

    @property
    def oupt(self) -> int:
        """Get or set the Type of output:
        EQ.1: RVE homogenization results will be written out to a file named rveout.Please refer to the keyword *DATABASE_RVE.
        """ # nopep8
        return self._cards[1].get_value("oupt")

    @oupt.setter
    def oupt(self, value: int) -> None:
        """Set the oupt property."""
        self._cards[1].set_value("oupt", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for specifying loading history (see *DEFINE_CURVE) or flag to indicate that each component of the macroscopic deformation measure (H11, H22, ..., H13 on Card 3) is input as a load curve:
        GT.0: Load curve ID.This curve gives a scale factor for the user - defined macroscopic deformation measure(H11, H22, ..., H13 input on Card 3) as a function of loading time.The scale factor is the ordinate(second column) while the loading time is the abscissa(first column).
        LT.0: Flag to indicate that each component of the macroscopic dispalcement gradient, Hij, is input as a load curve on Card 3.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
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
        """Set the idof property."""
        self._cards[1].set_value("idof", value)

    @property
    def bc(self) -> int:
        """Get or set the Type of RVE boundary condition:
        EQ. 0: Periodic Displacement Boundary Condition (PDBC).
        EQ. 1: Linear Displacement Boundary Condition (LDBC).
        EQ.2: Partially Periodic Displacement Boundary Condition (PDBC). Card 4 is required.
        EQ.3:	Periodic displacement boundary condition for RVEs with zero-thickness cohesive interface elements. It is intended for material interface debonding prediction.
        """ # nopep8
        return self._cards[1].get_value("bc")

    @bc.setter
    def bc(self, value: int) -> None:
        """Set the bc property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""bc must be `None` or one of {0,1,2,3}.""")
        self._cards[1].set_value("bc", value)

    @property
    def imatch(self) -> int:
        """Get or set the Type of the given RVE mesh (ignored unless BC = 0 or 2);:
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
        """Set the imatch property."""
        self._cards[1].set_value("imatch", value)

    @property
    def image(self) -> int:
        """Get or set the Create image RVE in specified directions. This feature is only available for IMATCH = 1. See Remark 11 and Figure 0-2.
        EQ.110: Create image RVE in x direction
        EQ.120: Create image RVE in y direction
        EQ.130: Create image RVE in z direction
        EQ.212: Create image RVE in both the x and y directions
        EQ.213: Create image RVE in both the x and z directions
        EQ.232: Create image RVE in both the y and z directions
        EQ.300: Create image RVE in the x, yand z directions
        """ # nopep8
        return self._cards[1].get_value("image")

    @image.setter
    def image(self, value: int) -> None:
        """Set the image property."""
        self._cards[1].set_value("image", value)

    @property
    def h11(self) -> typing.Optional[float]:
        """Get or set the Component 11 of the prescribed macroscopic displacement gradient, H. H is assumed to be symmetric (see Remark 4). To not impose the corresponding constraints on the RVE, leave the component HIJ empty (instead of setting it to be zero). If LCID < 0 on Card 2, the input value is assumed to be a load curve ID for a load curve giving the component as a function of time. Please refer to Remarks 2 and 8.
        """ # nopep8
        return self._cards[2].get_value("h11")

    @h11.setter
    def h11(self, value: float) -> None:
        """Set the h11 property."""
        self._cards[2].set_value("h11", value)

    @property
    def h22(self) -> typing.Optional[float]:
        """Get or set the Component 22 of the prescribed macroscopic displacement gradient, H. H is assumed to be symmetric (see Remark 4). To not impose the corresponding constraints on the RVE, leave the component HIJ empty (instead of setting it to be zero). If LCID < 0 on Card 2, the input value is assumed to be a load curve ID for a load curve giving the component as a function of time. Please refer to Remarks 2 and 8.
        """ # nopep8
        return self._cards[2].get_value("h22")

    @h22.setter
    def h22(self, value: float) -> None:
        """Set the h22 property."""
        self._cards[2].set_value("h22", value)

    @property
    def h33(self) -> typing.Optional[float]:
        """Get or set the Component 33 of the prescribed macroscopic displacement gradient, H. H is assumed to be symmetric (see Remark 4). To not impose the corresponding constraints on the RVE, leave the component HIJ empty (instead of setting it to be zero). If LCID < 0 on Card 2, the input value is assumed to be a load curve ID for a load curve giving the component as a function of time. Please refer to Remarks 2 and 8.
        """ # nopep8
        return self._cards[2].get_value("h33")

    @h33.setter
    def h33(self, value: float) -> None:
        """Set the h33 property."""
        self._cards[2].set_value("h33", value)

    @property
    def h12(self) -> typing.Optional[float]:
        """Get or set the Component 12 of the prescribed macroscopic displacement gradient, H. H is assumed to be symmetric (see Remark 4). To not impose the corresponding constraints on the RVE, leave the component HIJ empty (instead of setting it to be zero). If LCID < 0 on Card 2, the input value is assumed to be a load curve ID for a load curve giving the component as a function of time. Please refer to Remarks 2 and 8.
        """ # nopep8
        return self._cards[2].get_value("h12")

    @h12.setter
    def h12(self, value: float) -> None:
        """Set the h12 property."""
        self._cards[2].set_value("h12", value)

    @property
    def h23(self) -> typing.Optional[float]:
        """Get or set the Component 23 of the prescribed macroscopic displacement gradient, H. H is assumed to be symmetric (see Remark 4). To not impose the corresponding constraints on the RVE, leave the component HIJ empty (instead of setting it to be zero). If LCID < 0 on Card 2, the input value is assumed to be a load curve ID for a load curve giving the component as a function of time. Please refer to Remarks 2 and 8.
        """ # nopep8
        return self._cards[2].get_value("h23")

    @h23.setter
    def h23(self, value: float) -> None:
        """Set the h23 property."""
        self._cards[2].set_value("h23", value)

    @property
    def h13(self) -> typing.Optional[float]:
        """Get or set the Component 13 of the prescribed macroscopic displacement gradient, H. H is assumed to be symmetric (see Remark 4). To not impose the corresponding constraints on the RVE, leave the component HIJ empty (instead of setting it to be zero). If LCID < 0 on Card 2, the input value is assumed to be a load curve ID for a load curve giving the component as a function of time. Please refer to Remarks 2 and 8.
        """ # nopep8
        return self._cards[2].get_value("h13")

    @h13.setter
    def h13(self, value: float) -> None:
        """Set the h13 property."""
        self._cards[2].set_value("h13", value)

    @property
    def px(self) -> typing.Optional[float]:
        """Get or set the Flag to apply the periodic boundary condition in the ith direction (i = x, y, or z) for PDBC:
        EQ.0: Periodic boundary condition is not applied for this direction.
        EQ.1: Periodic boundary condition is applied for this direction.
        """ # nopep8
        return self._cards[3].get_value("px")

    @px.setter
    def px(self, value: float) -> None:
        """Set the px property."""
        self._cards[3].set_value("px", value)

    @property
    def py(self) -> typing.Optional[float]:
        """Get or set the Flag to apply the periodic boundary condition in the ith direction (i = x, y, or z) for PDBC:
        EQ.0: Periodic boundary condition is not applied for this direction.
        EQ.1: Periodic boundary condition is applied for this direction.
        """ # nopep8
        return self._cards[3].get_value("py")

    @py.setter
    def py(self, value: float) -> None:
        """Set the py property."""
        self._cards[3].set_value("py", value)

    @property
    def pz(self) -> typing.Optional[float]:
        """Get or set the Flag to apply the periodic boundary condition in the ith direction (i = x, y, or z) for PDBC:
        EQ.0: Periodic boundary condition is not applied for this direction.
        EQ.1: Periodic boundary condition is applied for this direction.
        """ # nopep8
        return self._cards[3].get_value("pz")

    @pz.setter
    def pz(self, value: float) -> None:
        """Set the pz property."""
        self._cards[3].set_value("pz", value)

    @property
    def lcid_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid:
                return kwd
        return None

    @lcid_link.setter
    def lcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid."""
        self.lcid = value.lcid

