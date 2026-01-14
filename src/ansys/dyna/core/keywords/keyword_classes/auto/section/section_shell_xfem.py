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

"""Module providing the SectionShellXfem class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_SECTIONSHELLXFEM_CARD0 = (
    FieldSchema("secid", int, 0, 10, None),
    FieldSchema("elform", int, 10, 10, 2),
    FieldSchema("shrf", float, 20, 10, 1.0),
    FieldSchema("nip", int, 30, 10, 2),
    FieldSchema("propt", float, 40, 10, 1.0),
    FieldSchema("qr_irid", int, 50, 10, 0, "qr/irid"),
    FieldSchema("icomp", int, 60, 10, 0),
    FieldSchema("setyp", int, 70, 10, 1),
)

_SECTIONSHELLXFEM_CARD1 = (
    FieldSchema("t1", float, 0, 10, 0.0),
    FieldSchema("t2", float, 10, 10, 0.0),
    FieldSchema("t3", float, 20, 10, 0.0),
    FieldSchema("t4", float, 30, 10, 0.0),
    FieldSchema("nloc", float, 40, 10, 0.0),
    FieldSchema("marea", float, 50, 10, 0.0),
)

_SECTIONSHELLXFEM_CARD2 = (
    FieldSchema("cmid", int, 0, 10, None),
    FieldSchema("baselm", int, 10, 10, None),
    FieldSchema("domint", int, 20, 10, 0),
    FieldSchema("failcr", int, 30, 10, 1),
    FieldSchema("propcr", int, 40, 10, None),
    FieldSchema("fs", float, 50, 10, 0.0),
    FieldSchema("ls_fs1", float, 60, 10, 0.0, "ls/fs1"),
    FieldSchema("nc_cl", float, 70, 10, None, "nc/cl"),
)

_SECTIONSHELLXFEM_CARD3 = (
    FieldSchema("nipp", int, 0, 10, 0),
    FieldSchema("nxdof", int, 10, 10, 0),
    FieldSchema("iunf", int, 20, 10, 0),
    FieldSchema("ihgf", int, 30, 10, 0),
    FieldSchema("itaj", int, 40, 10, 0),
    FieldSchema("lmc", int, 50, 10, 0),
    FieldSchema("nhsv", int, 60, 10, 0),
    FieldSchema("iloc", int, 70, 10, 0),
)

_SECTIONSHELLXFEM_CARD4 = (
    FieldSchema("xi", float, 0, 10, None),
    FieldSchema("eta", float, 10, 10, None),
    FieldSchema("wgt", float, 20, 10, None),
)

_SECTIONSHELLXFEM_CARD5 = (
    FieldSchema("bi", float, 0, 10, None),
    FieldSchema("bi", float, 10, 10, None),
    FieldSchema("bi", float, 20, 10, None),
    FieldSchema("bi", float, 30, 10, None),
    FieldSchema("bi", float, 40, 10, None),
    FieldSchema("bi", float, 50, 10, None),
    FieldSchema("bi", float, 60, 10, None),
    FieldSchema("bi", float, 70, 10, None),
)

_SECTIONSHELLXFEM_CARD6 = (
    FieldSchema("pi", float, 0, 10, 0.0),
    FieldSchema("pi", float, 10, 10, 0.0),
    FieldSchema("pi", float, 20, 10, 0.0),
    FieldSchema("pi", float, 30, 10, 0.0),
    FieldSchema("pi", float, 40, 10, 0.0),
    FieldSchema("pi", float, 50, 10, 0.0),
    FieldSchema("pi", float, 60, 10, 0.0),
    FieldSchema("pi", float, 70, 10, 0.0),
)

_SECTIONSHELLXFEM_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SectionShellXfem(KeywordBase):
    """DYNA SECTION_SHELL_XFEM keyword"""

    keyword = "SECTION"
    subkeyword = "SHELL_XFEM"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the SectionShellXfem class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SECTIONSHELLXFEM_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SECTIONSHELLXFEM_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SECTIONSHELLXFEM_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SECTIONSHELLXFEM_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SECTIONSHELLXFEM_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SECTIONSHELLXFEM_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SECTIONSHELLXFEM_CARD6,
                **kwargs,
            ),            OptionCardSet(
                option_spec = SectionShellXfem.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SECTIONSHELLXFEM_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def secid(self) -> typing.Optional[int]:
        """Get or set the Section ID. SECID is referenced on the *PART card and must be unique.
        """ # nopep8
        return self._cards[0].get_value("secid")

    @secid.setter
    def secid(self, value: int) -> None:
        """Set the secid property."""
        self._cards[0].set_value("secid", value)

    @property
    def elform(self) -> int:
        """Get or set the ELFORM Element formulation options, see Remarks 1 and 3:
        EQ.1: Hughes-Liu,
        EQ.2: Belytschko-Tsay (default),
        EQ.3: BCIZ triangular shell,
        EQ.4: C0 triangular shell,
        EQ.5: Belytschko-Tsay membrane,
        EQ.6: S/R Hughes-Liu,
        EQ.7: S/R co-rotational Hughes-Liu,
        EQ.8: Belytschko-Leviathan shell,
        EQ.9: Fully integrated Belytschko-Tsay membrane,
        EQ.10: Belytschko-Wong-Chiang,
        EQ.11: Fast (co-rotational) Hughes-Liu,
        EQ.12: Plane stress (x-y plane),
        EQ.13: Plane strain (x-y plane),
        EQ.14: Axisymmetric solid (xy-plane, y-axis of symmetry) - area weighted (see Remark 11),
        EQ.15: Axisymmetric solid (y-axis of symmetry) - volume weighted,
        EQ.16: Fully integrated shell element (very fast),
        EQ.-16: Fully integrated shell element modified for higher accuracy,
        EQ.17 Fully integrated DKT, triangular shell element.  See Remark 10,
        EQ.18: Fully Integrated linear DK qaudrilateral/triangular shell, See Remarks 2 and 3.
        EQ.20: Fully integrated linear assumed strain C0 shell, See Remark 3.
        EQ.21: Fully integrated linear strain C0 shell (5DOF)
        EQ.22: Linear shear panel element (3 DOF per node), See Remark 4.
        EQ.23: 8-node quadrilateral shell
        EQ.24: 6-node quadratic triangular shell
        EQ.25: Belytschko-Tsay shell with thickness stretch.
        EQ.26: Fully integrated shell with thickness stretch.
        EQ.27: C0 triangular shell with thickness stretch.
        EQ.29: Cohesive shell element for edge-to-edge connection of shells.  See Remark 13.
        EQ.-29:Cohesive shell element for edge-to-edge connection of shells (more suitable for pure shear).  See Remark 13.
        EQ.30:	Fast fully integrated element with 2 in-plane integration points based on ELFORM 16
        EQ.31: 1 point eulerian Navier-Stokes,
        EQ.32: 8 point Eulerian Navier-Stokes,
        EQ.33: CVFEM Eulerian Navier-Stokes.EQ.
        EQ.41: Mesh-free (EFG) shell local approach. (more suitable for crashworthiness analysis)
        EQ.42: Mesh-free (EFG) shell global approach. (more suitable for metal forming analysis)
        EQ.43: Mesh-free (EFG) plane strain formulation (x-y plane).
        EQ.44: Mesh-free (EFG) axisymmetric solid formulation (x-y plane, y-axis of symmetry).
        46: Cohesive element for two-dimensional plane strain, plane stress, and area-weighted axisymmetric problems (type 14 shells).
        EQ.47: Cohesive element for two-dimensional volume-weighted axisymmetric problems (use with type 15 shells).
        EQ.52:	Plane strain (xy-plane) XFEM, base element type 13 with full integration. See Remark 9.EQ.54:	Shell XFEM, base element type defined by BASELM(default 2).See Remark 9.
        EQ.55 : 8 - node singular plane strain(xy - plane) finite element.See Remark 12.
        EQ.98 : Interpolation shell
        EQ.99 : Simplified linear element for time - domain vibration studies.See Remark 5.
        EQ.101 : User defined shell
        EQ.102 : User defined shell
        EQ.103 : User defined shell
        EQ.104 : User defined shell
        EQ.105 : User defined shell
        EQ.201 : Isogeometric shells with NURBS.See * ELEMENT_SHELL_NURBS_PATCH.
        GE.1000 : Generalized shell element formulation(user defined).See * DEFINE_ELEMENT_GENERALIZED_SHELL.
        Note that the 2D and 3D element types must not be mixed,and different types of 2D elements must not be used together.For example,
        two - dimensional axisymmetric calculations can use either element types 14 or 15, but these element types must not be mixed together.Likewise,
        the plane strain element type must not be used with either the plane stress element or the axisymmetric element types.
        In three dimensions, the different shell elements types, i.e., 1 - 11 and 16, can be freely mixed together.
        """ # nopep8
        return self._cards[0].get_value("elform")

    @elform.setter
    def elform(self, value: int) -> None:
        """Set the elform property."""
        self._cards[0].set_value("elform", value)

    @property
    def shrf(self) -> float:
        """Get or set the Shear factor which scales the transverse shear stress (default =1.0).
        A suggested value is 5/6.
        """ # nopep8
        return self._cards[0].get_value("shrf")

    @shrf.setter
    def shrf(self, value: float) -> None:
        """Set the shrf property."""
        self._cards[0].set_value("shrf", value)

    @property
    def nip(self) -> int:
        """Get or set the Number of through shell thickness integration points. Default is set to 2.
        Through thickness integration for the two-dimensional elements (options 11-15 above) is not meaningful; consequently, the default is equal to 1 integration point.  Fully integrated two-dimensional elements are available for options 13 and 15 by setting NIP equal to a value of 4 corresponding to a 2x2 Gaussian quadrature.
        """ # nopep8
        return self._cards[0].get_value("nip")

    @nip.setter
    def nip(self, value: int) -> None:
        """Set the nip property."""
        self._cards[0].set_value("nip", value)

    @property
    def propt(self) -> float:
        """Get or set the Printout option:
        EQ.1: average resultants and fiber lengths (default),
        EQ.2: resultants at plan points and fiber lengths,
        EQ.3: resultants, stresses at all points, fiber lengths.
        """ # nopep8
        return self._cards[0].get_value("propt")

    @propt.setter
    def propt(self, value: float) -> None:
        """Set the propt property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""propt must be `None` or one of {1,2,3}.""")
        self._cards[0].set_value("propt", value)

    @property
    def qr_irid(self) -> int:
        """Get or set the Quadrature rule or Integration rule ID, see *INTEGRATION_SHELL:
        LT.0: absolute value is specified rule number,
        EQ.0: Gauss (up to ten points are permitted),
        EQ.1: trapezoidal, not recommend for accuracy reasons.
        """ # nopep8
        return self._cards[0].get_value("qr_irid")

    @qr_irid.setter
    def qr_irid(self, value: int) -> None:
        """Set the qr_irid property."""
        self._cards[0].set_value("qr_irid", value)

    @property
    def icomp(self) -> int:
        """Get or set the Flag for orthotropic/anisotropic layered composite material model. This option applies to material types 22, 23, 33, 34, 36, 40, 41-50, 54-56, 58, 59, 103, 116 and 194:
        EQ.0: Flag is tuned off (default),
        EQ.1: a material angle in degrees is defined for each through thickness integration point. Thus, each layer has one integration point.
        """ # nopep8
        return self._cards[0].get_value("icomp")

    @icomp.setter
    def icomp(self, value: int) -> None:
        """Set the icomp property."""
        if value not in [0, 1, None]:
            raise Exception("""icomp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("icomp", value)

    @property
    def setyp(self) -> int:
        """Get or set the 2D solid element type: Defined for ELFORM 13, 14, and 15:
        EQ.1: Lagrangian,
        EQ.2: Eulerian (single material with voids),
        EQ.3: ALE
        """ # nopep8
        return self._cards[0].get_value("setyp")

    @setyp.setter
    def setyp(self, value: int) -> None:
        """Set the setyp property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""setyp must be `None` or one of {1,2,3}.""")
        self._cards[0].set_value("setyp", value)

    @property
    def t1(self) -> float:
        """Get or set the Shell thickness at node n1 , unless the thickness is defined on the *ELEMENT_SHELL_OPTION card.
        """ # nopep8
        return self._cards[1].get_value("t1")

    @t1.setter
    def t1(self, value: float) -> None:
        """Set the t1 property."""
        self._cards[1].set_value("t1", value)

    @property
    def t2(self) -> float:
        """Get or set the Shell thickness at node n2, unless the thickness is defined on the *ELEMENT_SHELL_OPTION card.
        """ # nopep8
        return self._cards[1].get_value("t2")

    @t2.setter
    def t2(self, value: float) -> None:
        """Set the t2 property."""
        self._cards[1].set_value("t2", value)

    @property
    def t3(self) -> float:
        """Get or set the Shell thickness at node n3, unless the thickness is defined on the *ELEMENT_SHELL_OPTION card.
        """ # nopep8
        return self._cards[1].get_value("t3")

    @t3.setter
    def t3(self, value: float) -> None:
        """Set the t3 property."""
        self._cards[1].set_value("t3", value)

    @property
    def t4(self) -> float:
        """Get or set the Shell thickness at node n4, unless the thickness is defined on the *ELEMENT_SHELL_OPTION card.
        """ # nopep8
        return self._cards[1].get_value("t4")

    @t4.setter
    def t4(self, value: float) -> None:
        """Set the t4 property."""
        self._cards[1].set_value("t4", value)

    @property
    def nloc(self) -> float:
        """Get or set the Location of reference surface for three dimensional shell elements. If nonzero, the offset distance from the plane of the nodal points to the reference surface of the shell in the direction of the shell normal vector is a value offset = -0.50*NLOC*(average shell thickness). This offset is not considered in the contact subroutines unless CNTCO is set to 1 in *CONTROL_SHELL. Alternatively, the offset can be specified by using the OFFSET option in the *ELEMENT_SHELL input section.
        EQ.1.0: top surface,
        EQ.0.0: mid-surface (default),
        EQ.-1.0: bottom surface.For nonzero offset distances, the time step size is reduced to prevent instabilities. See NLOCDT in *CONTROL_SHELL.
        """ # nopep8
        return self._cards[1].get_value("nloc")

    @nloc.setter
    def nloc(self, value: float) -> None:
        """Set the nloc property."""
        self._cards[1].set_value("nloc", value)

    @property
    def marea(self) -> float:
        """Get or set the Non-structural mass per unit area.  This is additional mass which comes from materials, such as carpeting.
        This mass is not directly included in the time step calculation.
        Another and often more convenient alternative for defining distributed mass is with *ELEMENT_MASS_PART,
        which allows additional non-structural mass to be distributed by an area weighted distribution to all nodes of a given part ID.
        """ # nopep8
        return self._cards[1].get_value("marea")

    @marea.setter
    def marea(self, value: float) -> None:
        """Set the marea property."""
        self._cards[1].set_value("marea", value)

    @property
    def cmid(self) -> typing.Optional[int]:
        """Get or set the Cohesive material ID. *MAT_185 is available for both brittle and ductile fracture and *MAT_240 is available for ductile material.
        """ # nopep8
        return self._cards[2].get_value("cmid")

    @cmid.setter
    def cmid(self, value: int) -> None:
        """Set the cmid property."""
        self._cards[2].set_value("cmid", value)

    @property
    def baselm(self) -> typing.Optional[int]:
        """Get or set the Base element type for XFEM (type 13 for 2D, types 2, 16 or 17 for shell)
        """ # nopep8
        return self._cards[2].get_value("baselm")

    @baselm.setter
    def baselm(self, value: int) -> None:
        """Set the baselm property."""
        self._cards[2].set_value("baselm", value)

    @property
    def domint(self) -> int:
        """Get or set the Option for domain integration in XFEM:
        EQ.0: Phantom element integration
        EQ.1: Subdomain integration with triangular local boundary integration (available in 2D only)
        """ # nopep8
        return self._cards[2].get_value("domint")

    @domint.setter
    def domint(self, value: int) -> None:
        """Set the domint property."""
        if value not in [0, 1, None]:
            raise Exception("""domint must be `None` or one of {0,1}.""")
        self._cards[2].set_value("domint", value)

    @property
    def failcr(self) -> int:
        """Get or set the Option for different failure criteria:
        EQ.1: Maximum tensile stress.
        EQ.2: Maximum shear stress.
        EQ.-1:	effective plastic strain (EPS).
        EQ.-2:	crack length dependent EPS.  See Remark 15
        EQ.-4: Stress triaxiality based failure plastic strain.
        """ # nopep8
        return self._cards[2].get_value("failcr")

    @failcr.setter
    def failcr(self, value: int) -> None:
        """Set the failcr property."""
        self._cards[2].set_value("failcr", value)

    @property
    def propcr(self) -> typing.Optional[int]:
        """Get or set the PROPCR is interpreted digit-wise:
        PROPCR = [IP] = P + 10×I
        P determines the crack propagation direction :
        P.EQ.0 : first principal strain direction if FAILCR < 0 (default for ductile fracture), first principal stress if FAILCR = 1, or maximum shear stress if FAILCR = 2. P = 0 is the only option for brittle fracture(FAILCR > 0).
        P.EQ.2 : center of effective plastic strain
        P.EQ.3 : directional center of effective plastic strain
        I determines crack initiation :
        I.EQ.0 : crack initiates at boundary(default)
        I.EQ.3 : crack initiates anywhere
        """ # nopep8
        return self._cards[2].get_value("propcr")

    @propcr.setter
    def propcr(self, value: int) -> None:
        """Set the propcr property."""
        self._cards[2].set_value("propcr", value)

    @property
    def fs(self) -> float:
        """Get or set the Failure value for FAILCR = -1 or -2:
        FAILCR.EQ. - 1:	Failure strain / failure critical value
        FAILCR.EQ. - 2 : Initial failure plastic strain
        Curve ID or Table ID for stress triaxiality based failure plastic strain for FAILCR = -4.
        """ # nopep8
        return self._cards[2].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        """Set the fs property."""
        self._cards[2].set_value("fs", value)

    @property
    def ls_fs1(self) -> float:
        """Get or set the LS: Length scale for strain regularization (FAILCR = -1 only)
        FS1: Final failure plastic strain (FAILCR = -2 only)
        """ # nopep8
        return self._cards[2].get_value("ls_fs1")

    @ls_fs1.setter
    def ls_fs1(self, value: float) -> None:
        """Set the ls_fs1 property."""
        self._cards[2].set_value("ls_fs1", value)

    @property
    def nc_cl(self) -> typing.Optional[float]:
        """Get or set the NC is the number of cracks allowed in the part FAILCR .NE. -2
        CL:Crack length at which the failure strain is FS1 (FAILCR = -2 only)
        """ # nopep8
        return self._cards[2].get_value("nc_cl")

    @nc_cl.setter
    def nc_cl(self, value: float) -> None:
        """Set the nc_cl property."""
        self._cards[2].set_value("nc_cl", value)

    @property
    def nipp(self) -> int:
        """Get or set the Number of in-plane integration points for user-defined shell (0 if resultant/discrete element).
        """ # nopep8
        return self._cards[3].get_value("nipp")

    @nipp.setter
    def nipp(self, value: int) -> None:
        """Set the nipp property."""
        self._cards[3].set_value("nipp", value)

    @property
    def nxdof(self) -> int:
        """Get or set the Number of extra degrees of freedom per node for user-defined shell.
        """ # nopep8
        return self._cards[3].get_value("nxdof")

    @nxdof.setter
    def nxdof(self, value: int) -> None:
        """Set the nxdof property."""
        self._cards[3].set_value("nxdof", value)

    @property
    def iunf(self) -> int:
        """Get or set the Flag for using nodal fiber vectors in user-defined shell.
        """ # nopep8
        return self._cards[3].get_value("iunf")

    @iunf.setter
    def iunf(self, value: int) -> None:
        """Set the iunf property."""
        if value not in [0, 1, None]:
            raise Exception("""iunf must be `None` or one of {0,1}.""")
        self._cards[3].set_value("iunf", value)

    @property
    def ihgf(self) -> int:
        """Get or set the Flag for using hourglass stabilization (NIPP.GT.0).
        """ # nopep8
        return self._cards[3].get_value("ihgf")

    @ihgf.setter
    def ihgf(self, value: int) -> None:
        """Set the ihgf property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""ihgf must be `None` or one of {0,1,2,3}.""")
        self._cards[3].set_value("ihgf", value)

    @property
    def itaj(self) -> int:
        """Get or set the Flag for setting up finite element matrices (NIPP.GT.0).
        """ # nopep8
        return self._cards[3].get_value("itaj")

    @itaj.setter
    def itaj(self, value: int) -> None:
        """Set the itaj property."""
        if value not in [0, 1, None]:
            raise Exception("""itaj must be `None` or one of {0,1}.""")
        self._cards[3].set_value("itaj", value)

    @property
    def lmc(self) -> int:
        """Get or set the Number of property parameters.
        """ # nopep8
        return self._cards[3].get_value("lmc")

    @lmc.setter
    def lmc(self, value: int) -> None:
        """Set the lmc property."""
        self._cards[3].set_value("lmc", value)

    @property
    def nhsv(self) -> int:
        """Get or set the Number of history variables.
        """ # nopep8
        return self._cards[3].get_value("nhsv")

    @nhsv.setter
    def nhsv(self, value: int) -> None:
        """Set the nhsv property."""
        self._cards[3].set_value("nhsv", value)

    @property
    def iloc(self) -> int:
        """Get or set the Coordinate system option.
        """ # nopep8
        return self._cards[3].get_value("iloc")

    @iloc.setter
    def iloc(self, value: int) -> None:
        """Set the iloc property."""
        if value not in [0, 1, None]:
            raise Exception("""iloc must be `None` or one of {0,1}.""")
        self._cards[3].set_value("iloc", value)

    @property
    def xi(self) -> typing.Optional[float]:
        """Get or set the First isoparametric coordinate.
        """ # nopep8
        return self._cards[4].get_value("xi")

    @xi.setter
    def xi(self, value: float) -> None:
        """Set the xi property."""
        self._cards[4].set_value("xi", value)

    @property
    def eta(self) -> typing.Optional[float]:
        """Get or set the Second isoparametric coordinate.
        """ # nopep8
        return self._cards[4].get_value("eta")

    @eta.setter
    def eta(self, value: float) -> None:
        """Set the eta property."""
        self._cards[4].set_value("eta", value)

    @property
    def wgt(self) -> typing.Optional[float]:
        """Get or set the Isoparametric weight.
        """ # nopep8
        return self._cards[4].get_value("wgt")

    @wgt.setter
    def wgt(self, value: float) -> None:
        """Set the wgt property."""
        self._cards[4].set_value("wgt", value)

    @property
    def bi(self) -> typing.Optional[float]:
        """Get or set the beta-1, material angle at ith-integration point.
        """ # nopep8
        return self._cards[5].get_value("bi")

    @bi.setter
    def bi(self, value: float) -> None:
        """Set the bi property."""
        self._cards[5].set_value("bi", value)

    @property
    def bi(self) -> typing.Optional[float]:
        """Get or set the beta-2, material angle at ith-integration point.
        """ # nopep8
        return self._cards[5].get_value("bi")

    @bi.setter
    def bi(self, value: float) -> None:
        """Set the bi property."""
        self._cards[5].set_value("bi", value)

    @property
    def bi(self) -> typing.Optional[float]:
        """Get or set the beta-3, material angle at ith-integration point.
        """ # nopep8
        return self._cards[5].get_value("bi")

    @bi.setter
    def bi(self, value: float) -> None:
        """Set the bi property."""
        self._cards[5].set_value("bi", value)

    @property
    def bi(self) -> typing.Optional[float]:
        """Get or set the beta-4, material angle at ith-integration point.
        """ # nopep8
        return self._cards[5].get_value("bi")

    @bi.setter
    def bi(self, value: float) -> None:
        """Set the bi property."""
        self._cards[5].set_value("bi", value)

    @property
    def bi(self) -> typing.Optional[float]:
        """Get or set the beta-5, material angle at ith-integration point.
        """ # nopep8
        return self._cards[5].get_value("bi")

    @bi.setter
    def bi(self, value: float) -> None:
        """Set the bi property."""
        self._cards[5].set_value("bi", value)

    @property
    def bi(self) -> typing.Optional[float]:
        """Get or set the beta-6, material angle at ith-integration point.
        """ # nopep8
        return self._cards[5].get_value("bi")

    @bi.setter
    def bi(self, value: float) -> None:
        """Set the bi property."""
        self._cards[5].set_value("bi", value)

    @property
    def bi(self) -> typing.Optional[float]:
        """Get or set the beta-7, material angle at ith-integration point.
        """ # nopep8
        return self._cards[5].get_value("bi")

    @bi.setter
    def bi(self, value: float) -> None:
        """Set the bi property."""
        self._cards[5].set_value("bi", value)

    @property
    def bi(self) -> typing.Optional[float]:
        """Get or set the beta-8, material angle at ith-integration point.
        """ # nopep8
        return self._cards[5].get_value("bi")

    @bi.setter
    def bi(self, value: float) -> None:
        """Set the bi property."""
        self._cards[5].set_value("bi", value)

    @property
    def pi(self) -> float:
        """Get or set the Ith property parameter.
        """ # nopep8
        return self._cards[6].get_value("pi")

    @pi.setter
    def pi(self, value: float) -> None:
        """Set the pi property."""
        self._cards[6].set_value("pi", value)

    @property
    def pi(self) -> float:
        """Get or set the Ith property parameter.
        """ # nopep8
        return self._cards[6].get_value("pi")

    @pi.setter
    def pi(self, value: float) -> None:
        """Set the pi property."""
        self._cards[6].set_value("pi", value)

    @property
    def pi(self) -> float:
        """Get or set the Ith property parameter.
        """ # nopep8
        return self._cards[6].get_value("pi")

    @pi.setter
    def pi(self, value: float) -> None:
        """Set the pi property."""
        self._cards[6].set_value("pi", value)

    @property
    def pi(self) -> float:
        """Get or set the Ith property parameter.
        """ # nopep8
        return self._cards[6].get_value("pi")

    @pi.setter
    def pi(self, value: float) -> None:
        """Set the pi property."""
        self._cards[6].set_value("pi", value)

    @property
    def pi(self) -> float:
        """Get or set the Ith property parameter.
        """ # nopep8
        return self._cards[6].get_value("pi")

    @pi.setter
    def pi(self, value: float) -> None:
        """Set the pi property."""
        self._cards[6].set_value("pi", value)

    @property
    def pi(self) -> float:
        """Get or set the Ith property parameter.
        """ # nopep8
        return self._cards[6].get_value("pi")

    @pi.setter
    def pi(self, value: float) -> None:
        """Set the pi property."""
        self._cards[6].set_value("pi", value)

    @property
    def pi(self) -> float:
        """Get or set the Ith property parameter.
        """ # nopep8
        return self._cards[6].get_value("pi")

    @pi.setter
    def pi(self, value: float) -> None:
        """Set the pi property."""
        self._cards[6].set_value("pi", value)

    @property
    def pi(self) -> float:
        """Get or set the Ith property parameter.
        """ # nopep8
        return self._cards[6].get_value("pi")

    @pi.setter
    def pi(self, value: float) -> None:
        """Set the pi property."""
        self._cards[6].set_value("pi", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[7].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[7].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

