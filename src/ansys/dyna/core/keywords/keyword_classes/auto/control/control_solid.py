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

"""Module providing the ControlSolid class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_CONTROLSOLID_CARD0 = (
    FieldSchema("esort", int, 0, 10, 0),
    FieldSchema("fmatrx", int, 10, 10, 0),
    FieldSchema("niptets", int, 20, 10, 4),
    FieldSchema("swlocl", int, 30, 10, 1),
    FieldSchema("psfail", int, 40, 10, 0),
    FieldSchema("t10jtol", float, 50, 10, 0.0),
    FieldSchema("icoh", int, 60, 10, 0),
    FieldSchema("tet13k", int, 70, 10, 0),
)

_CONTROLSOLID_CARD1 = (
    FieldSchema("pm1", int, 0, 8, None),
    FieldSchema("pm2", int, 8, 8, None),
    FieldSchema("pm3", int, 16, 8, None),
    FieldSchema("pm4", int, 24, 8, None),
    FieldSchema("pm5", int, 32, 8, None),
    FieldSchema("pm6", int, 40, 8, None),
    FieldSchema("pm7", int, 48, 8, None),
    FieldSchema("pm8", int, 56, 8, None),
    FieldSchema("pm9", int, 64, 8, None),
    FieldSchema("pm10", int, 72, 8, None),
)

_CONTROLSOLID_CARD2 = (
    FieldSchema("tet13v", int, 0, 10, 0),
    FieldSchema("rinrt", int, 10, 10, 0),
    FieldSchema("coheqc", int, 20, 10, 0),
)

class ControlSolid(KeywordBase):
    """DYNA CONTROL_SOLID keyword"""

    keyword = "CONTROL"
    subkeyword = "SOLID"
    _link_fields = {
        "psfail": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ControlSolid class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLSOLID_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLSOLID_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLSOLID_CARD2,
                **kwargs,
            ),
        ]
    @property
    def esort(self) -> int:
        """Get or set the Automatic sorting of tetrahedron and pentahedron elements to avoid using degenerate formulation for thee shapes. See *SECTION_SOLID.
        EQ.0: no sorting(default).
        EQ.1: sort tetrahedron to type 10, pentahedron to type 15.
        EQ.2: sort tetrahedron to type 10, 1-point integrated pentahedron to type 115, fully integrated pentahedron to type 15.
        EQ.3: same as EQ.1 but also print switched elements in message file.
        EQ.4: same as EQ.2 but also print switched elements in message file
        EQ.11: Same as 1 except sort tetrahedron to type 13.
        EQ.12: Same as 2 except sort tetrahedron to type 13
        EQ.13: Same as 3 except sort tetrahedron to type 13
        EQ.14: Same as 4 except sort tetrahedron to type 13
        """ # nopep8
        return self._cards[0].get_value("esort")

    @esort.setter
    def esort(self, value: int) -> None:
        """Set the esort property."""
        if value not in [0, 1, 2, 3, 4, 11, 12, 13, 14, None]:
            raise Exception("""esort must be `None` or one of {0,1,2,3,4,11,12,13,14}.""")
        self._cards[0].set_value("esort", value)

    @property
    def fmatrx(self) -> int:
        """Get or set the Default method used in the calculation of the defomation gradient matrix.
        EQ.1: Update incrementally in time. This is the default for explicit.
        EQ.2: Directly compute F. This is the default for implicit and implicit/explicit switching.
        """ # nopep8
        return self._cards[0].get_value("fmatrx")

    @fmatrx.setter
    def fmatrx(self, value: int) -> None:
        """Set the fmatrx property."""
        self._cards[0].set_value("fmatrx", value)

    @property
    def niptets(self) -> int:
        """Get or set the Number of integration points used in the quadratic tetrahedron elements. Either 4 or 5 can be specified. This option applies to the type 4 and type 16 tetrahedron elements.
        """ # nopep8
        return self._cards[0].get_value("niptets")

    @niptets.setter
    def niptets(self, value: int) -> None:
        """Set the niptets property."""
        self._cards[0].set_value("niptets", value)

    @property
    def swlocl(self) -> int:
        """Get or set the Output option for stresses in solid elements used as spot welds with material *MAT_SPOTWELD.
        EQ.1: Stresses in the global coordinate system (default),
        EQ.2: Stresses in the element coordinate system
        """ # nopep8
        return self._cards[0].get_value("swlocl")

    @swlocl.setter
    def swlocl(self, value: int) -> None:
        """Set the swlocl property."""
        if value not in [1, 2, None]:
            raise Exception("""swlocl must be `None` or one of {1,2}.""")
        self._cards[0].set_value("swlocl", value)

    @property
    def psfail(self) -> int:
        """Get or set the Solid element erosion from negative volume is limited only to solid elements in the part set indicated by PSFAIL.  This is similar to setting ERODE = 1 in *CONTROL_TIMESTEP, except that it is not global.
        In other words, when PSFAIL is nonzero, the time - step - based criterion for erosion(TSMIN) applies to all solid elements(except formulations 11 and 12),and the negative volume criterion for erosion applies only to solids in part set PSFAIL.
        """ # nopep8
        return self._cards[0].get_value("psfail")

    @psfail.setter
    def psfail(self, value: int) -> None:
        """Set the psfail property."""
        self._cards[0].set_value("psfail", value)

    @property
    def t10jtol(self) -> float:
        """Get or set the Tolerance for jacobian in 4-point 10-noded quadratic tetrahedra (type 16).If the quotient between the minimum and maximum jacobian value falls below this tolerance, a warning message is issued in the messag file. This is useful for tracking badly shaped elements in implicit analysis that deteriorates convergence, a value of 1.0 indicates a perfectly shaped element.
        """ # nopep8
        return self._cards[0].get_value("t10jtol")

    @t10jtol.setter
    def t10jtol(self, value: float) -> None:
        """Set the t10jtol property."""
        self._cards[0].set_value("t10jtol", value)

    @property
    def icoh(self) -> int:
        """Get or set the Flag for cohesive elements to control deletion, the time step estimate, and the element type used in implicit and explicit. Breaking LS-DYNA convention, ICOH is interpreted digit-wise, namely as,
        ICOH = [MLK] = K + 10�L + 100�M .
        The first digit(in the one�s place), K, is interpreted as follows:
        K.EQ.0: No cohesive element deletion due to neighbor failure.
        K.EQ.1: Solid elements having ELFORM = 19 - 22 (or ELFORM = 1, 2, 15 being used with *MAT_169) are eroded when neighboring shell or solid elements fail.This works for nodewise connected partsand tied contacts.
        The second digit(in the ten�s place), L, controls how the mass used to estimate the element time step is computed.L defaults to zero if ICOH is less than 10 (having a single digit).See Remark 1.
        L.EQ.0: The mass is the sum of the element nodes.Each node is assumed to be shared between two elements.
        L.EQ.1: The mass is computed by integrating the cohesive density.
        L.EQ.2: Same as the default (0), but each node is assumed to be shared by four elements.Compared to the default, the time step is reduced by a factor of 1 / ?2.
        L.EQ.3: The element connectivity is used to compute how much(nodal) mass is associated with each element, meaning no assumption regarding connectivity is made.This setting is recommended.
        The third digit(in the hundred�s place), M, is interpreted as follows:
        M.EQ.0: Explicit element variants used in explicit calculations and higher - accuracy implicit element variants used in implicit calculations.
        M.EQ.1: Higher - accuracy implicit element variants used in both implicit and explicit calculations.
        """ # nopep8
        return self._cards[0].get_value("icoh")

    @icoh.setter
    def icoh(self, value: int) -> None:
        """Set the icoh property."""
        self._cards[0].set_value("icoh", value)

    @property
    def tet13k(self) -> int:
        """Get or set the Set to 1 to invoke a consistent tangent stiffness matrix for the pressure-averaged tetrahedron (type 13).  This feature is available only for the implicit integrator.  This element type averages the volumetric strain over adjacent elements to alleviate volumetric locking; therefore, the corresponding material tangent stiffness should be treated accordingly.  In contrast to a hexahedral mesh, where a node usually connects to fewer than eight elements, tetrahedral meshes offer no such regularity.  Consequently, matrix assembly is computationally expensive for nonlinear implicit analysis, so this option is recommended only for linear or eigenvalue analysis to exploit the stiffness characteristics of the type 13 tetrahedron.
        """ # nopep8
        return self._cards[0].get_value("tet13k")

    @tet13k.setter
    def tet13k(self, value: int) -> None:
        """Set the tet13k property."""
        if value not in [0, 1, None]:
            raise Exception("""tet13k must be `None` or one of {0,1}.""")
        self._cards[0].set_value("tet13k", value)

    @property
    def pm1(self) -> typing.Optional[int]:
        """Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
        """ # nopep8
        return self._cards[1].get_value("pm1")

    @pm1.setter
    def pm1(self, value: int) -> None:
        """Set the pm1 property."""
        self._cards[1].set_value("pm1", value)

    @property
    def pm2(self) -> typing.Optional[int]:
        """Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
        """ # nopep8
        return self._cards[1].get_value("pm2")

    @pm2.setter
    def pm2(self, value: int) -> None:
        """Set the pm2 property."""
        self._cards[1].set_value("pm2", value)

    @property
    def pm3(self) -> typing.Optional[int]:
        """Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
        """ # nopep8
        return self._cards[1].get_value("pm3")

    @pm3.setter
    def pm3(self, value: int) -> None:
        """Set the pm3 property."""
        self._cards[1].set_value("pm3", value)

    @property
    def pm4(self) -> typing.Optional[int]:
        """Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
        """ # nopep8
        return self._cards[1].get_value("pm4")

    @pm4.setter
    def pm4(self, value: int) -> None:
        """Set the pm4 property."""
        self._cards[1].set_value("pm4", value)

    @property
    def pm5(self) -> typing.Optional[int]:
        """Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
        """ # nopep8
        return self._cards[1].get_value("pm5")

    @pm5.setter
    def pm5(self, value: int) -> None:
        """Set the pm5 property."""
        self._cards[1].set_value("pm5", value)

    @property
    def pm6(self) -> typing.Optional[int]:
        """Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
        """ # nopep8
        return self._cards[1].get_value("pm6")

    @pm6.setter
    def pm6(self, value: int) -> None:
        """Set the pm6 property."""
        self._cards[1].set_value("pm6", value)

    @property
    def pm7(self) -> typing.Optional[int]:
        """Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
        """ # nopep8
        return self._cards[1].get_value("pm7")

    @pm7.setter
    def pm7(self, value: int) -> None:
        """Set the pm7 property."""
        self._cards[1].set_value("pm7", value)

    @property
    def pm8(self) -> typing.Optional[int]:
        """Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
        """ # nopep8
        return self._cards[1].get_value("pm8")

    @pm8.setter
    def pm8(self, value: int) -> None:
        """Set the pm8 property."""
        self._cards[1].set_value("pm8", value)

    @property
    def pm9(self) -> typing.Optional[int]:
        """Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
        """ # nopep8
        return self._cards[1].get_value("pm9")

    @pm9.setter
    def pm9(self, value: int) -> None:
        """Set the pm9 property."""
        self._cards[1].set_value("pm9", value)

    @property
    def pm10(self) -> typing.Optional[int]:
        """Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
        """ # nopep8
        return self._cards[1].get_value("pm10")

    @pm10.setter
    def pm10(self, value: int) -> None:
        """Set the pm10 property."""
        self._cards[1].set_value("pm10", value)

    @property
    def tet13v(self) -> int:
        """Get or set the Choice of implementation for solid element types 10 and 13: If EXACC on *CONTROL_ACCURAY is greater than zero, TET13V is set to one.
        EQ.0:	Efficient version for type 10 and more accurate version for type 13 (default).With the single precision version, a little noise could be observed in the solution for elements moving long distances with rigid body motion.
        EQ.1 : More accurate version for both types 10 and 13(smoother results) with an additional cost of about 15 % .
        EQ.2 : Efficient version for both types 10 and 13
        """ # nopep8
        return self._cards[2].get_value("tet13v")

    @tet13v.setter
    def tet13v(self, value: int) -> None:
        """Set the tet13v property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""tet13v must be `None` or one of {0,1,2}.""")
        self._cards[2].set_value("tet13v", value)

    @property
    def rinrt(self) -> int:
        """Get or set the Option to compute rotational inertia for the nodes of solid elements. This ensures consistent results if the applied constraints assume rotational degrees of freedom, as with tied contacts using the option SHELL_EDGE_TO_SURFACE.
        EQ.0: By default, an average of the existing rotational inertia from the shelland beam elements in the model is distributed to the nodes of the solid elements.This method is sufficient in most situations but might lead to inconsistencies between different model assemblies in the case of rotational motion.
        EQ.1: Compute rotational inertia for each solid element based on its dimensionsand mass density to ensure consistency.
        """ # nopep8
        return self._cards[2].get_value("rinrt")

    @rinrt.setter
    def rinrt(self, value: int) -> None:
        """Set the rinrt property."""
        if value not in [0, 1, None]:
            raise Exception("""rinrt must be `None` or one of {0,1}.""")
        self._cards[2].set_value("rinrt", value)

    @property
    def coheqc(self) -> int:
        """Get or set the Flag for cohesive elements quality check. Note that this check is only performed for cohesive elements with materials that have ROFLG = 0.COHEQC is interpreted digit-wise, namely as,
        COHEQC = [LK] = K + 10xL
        The first digit, K, controls the behavior of LS - DYNA when cohesive elements with poor quality are detected at t = 0:
        K.EQ.0: Error termination with a list of elements(default).
        K.EQ.1: Issue a warning and continue.
        K.EQ.2: Same as 1 but delete the concerned elements.
        The second digit, L, controls the behavior of LS - DYNA when inconsistent cohesive elements connectivity is detected.
        L.EQ.0: Error termination with a list of elements(default).
        L.EQ.1: Issue a warning and continue.
        """ # nopep8
        return self._cards[2].get_value("coheqc")

    @coheqc.setter
    def coheqc(self, value: int) -> None:
        """Set the coheqc property."""
        self._cards[2].set_value("coheqc", value)

    @property
    def psfail_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for psfail."""
        return self._get_set_link("PART", self.psfail)

    @psfail_link.setter
    def psfail_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psfail."""
        self.psfail = value.sid

