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

"""Module providing the SectionShell class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.series_card import SeriesCard
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class SectionShell(KeywordBase):
    """DYNA SECTION_SHELL keyword"""

    keyword = "SECTION"
    subkeyword = "SHELL"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the SectionShell class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "secid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "elform",
                        int,
                        10,
                        10,
                        2,
                        **kwargs,
                    ),
                    Field(
                        "shrf",
                        float,
                        20,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "nip",
                        int,
                        30,
                        10,
                        2,
                        **kwargs,
                    ),
                    Field(
                        "propt",
                        float,
                        40,
                        10,
                        1,
                        **kwargs,
                    ),
                    Field(
                        "qr/irid",
                        int,
                        50,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "icomp",
                        int,
                        60,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "setyp",
                        int,
                        70,
                        10,
                        1,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "t1",
                        float,
                        0,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "t2",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "t3",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "t4",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "nloc",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "marea",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "idof",
                        float,
                        60,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "edgset",
                        int,
                        70,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
            ),
            SeriesCard(
                "angle",
                8,
                10,
                float,
                lambda: self.nip,
                lambda: self.icomp == 1,
                data = kwargs.get("angle")),
            Card(
                [
                    Field(
                        "nipp",
                        int,
                        0,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "nxdof",
                        int,
                        10,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "iunf",
                        int,
                        20,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "ihgf",
                        int,
                        30,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "itaj",
                        int,
                        40,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "lmc",
                        int,
                        50,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "nhsv",
                        int,
                        60,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "iloc",
                        int,
                        70,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
                lambda: self.elform in [101, 102, 103, 104, 105],
            ),
            TableCard(
                [
                    Field("xi", float, 0, 10, None),
                    Field("eta", float, 10, 10, None),
                    Field("wgt", float, 20, 10, None),
                ],
                lambda: self.nipp,
                lambda: self.elform in [101, 102, 103, 104, 105],
                name="integration_points",
                **kwargs,
            ),
            SeriesCard(
                "pi",
                8,
                10,
                float,
                lambda: self.lmc,
                lambda: self.elform in [101, 102, 103, 104, 105],
                data = kwargs.get("pi")),
            OptionCardSet(
                option_spec = SectionShell.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
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
        return self._cards[0].get_value("qr/irid")

    @qr_irid.setter
    def qr_irid(self, value: int) -> None:
        """Set the qr_irid property."""
        self._cards[0].set_value("qr/irid", value)

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
    def idof(self) -> float:
        """Get or set the Treatment of through thickness strain (see Remark 7).
        LT.0:	Same as IDOF = 3 but the contact pressure is averaged over a time - IDOF in order to reduce noise and thus improve stability.
        EQ.1: The thickness field is continous across the element edges for metalforming applications. This is the default.
        EQ.2: The thickness field is discontinous across the element edges. This is necessary for crashworthiness applications due to shell intersections, sharp included angles, and non-smooth deformations.This option applies to element types 25, 26 and 27. This is the default for these element types.
        EQ.3:The thickness strain is governed by the stress from contact and pressure loads, meaning that the strain is adjusted for the through thickness stress to equilibrate the contact and load pressure. Please note that it is assumed that the pressure is positive, i.e., acting towards the shell surface. Thus, vacuum loads cannot be used with this option. This option applies to element types 2, 4, and ±16
        EQ.11 : Same as IDOF = 1 but the through thickness strain is simplified in the kinematics in order to reduce locking effects which were observed for considerable thickness changes.This option applies to element type 25 only.
        EQ.12 : Same as IDOF = 2 but the through thickness strain is simplified in the kinematics in order to reduce locking effects which were observed for considerable thickness changes.This option applies to element type 25 only.
        """ # nopep8
        return self._cards[1].get_value("idof")

    @idof.setter
    def idof(self, value: float) -> None:
        """Set the idof property."""
        self._cards[1].set_value("idof", value)

    @property
    def edgset(self) -> int:
        """Get or set the Edge node set required for shell type seatbelts.  See Remark 8.
        See Figure Error! Reference source not found. in *ELEMENT_SEATBELT for additional clarification.
        """ # nopep8
        return self._cards[1].get_value("edgset")

    @edgset.setter
    def edgset(self, value: int) -> None:
        """Set the edgset property."""
        self._cards[1].set_value("edgset", value)

    @property
    def angle(self) -> SeriesCard:
        """dynamic array of beta-i: material angle at ith-integration point.."""
        return self._cards[2]

    @angle.setter
    def angle(self, value: typing.List) -> None:
        self._cards[2].data = value

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
    def integration_points(self):
        """Get the table of integration_points."""
        return self._cards[4].table

    @integration_points.setter
    def integration_points(self, df):
        """Set integration_points from the dataframe df"""
        self._cards[4].table = df

    @property
    def pi(self) -> SeriesCard:
        """dynamic array of LMC property parameters.."""
        return self._cards[5]

    @pi.setter
    def pi(self, value: typing.List) -> None:
        self._cards[5].data = value

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[6].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[6].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

