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
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class SectionSolidEfg(KeywordBase):
    """DYNA SECTION_SOLID_EFG keyword"""

    keyword = "SECTION"
    subkeyword = "SOLID_EFG"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
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
                        kwargs.get("secid")
                    ),
                    Field(
                        "elform",
                        int,
                        10,
                        10,
                        kwargs.get("elform", 1)
                    ),
                    Field(
                        "aet",
                        int,
                        20,
                        10,
                        kwargs.get("aet", 0)
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        50,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "cohoff",
                        float,
                        60,
                        10,
                        kwargs.get("cohoff")
                    ),
                    Field(
                        "gaskeit",
                        float,
                        70,
                        10,
                        kwargs.get("gaskeit")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dx",
                        float,
                        0,
                        10,
                        kwargs.get("dx", 1.01)
                    ),
                    Field(
                        "dy",
                        float,
                        10,
                        10,
                        kwargs.get("dy", 1.01)
                    ),
                    Field(
                        "dz",
                        float,
                        20,
                        10,
                        kwargs.get("dz", 1.01)
                    ),
                    Field(
                        "ispline",
                        int,
                        30,
                        10,
                        kwargs.get("ispline", 0)
                    ),
                    Field(
                        "idila",
                        int,
                        40,
                        10,
                        kwargs.get("idila", 0)
                    ),
                    Field(
                        "iebt",
                        int,
                        50,
                        10,
                        kwargs.get("iebt", 3)
                    ),
                    Field(
                        "idim",
                        int,
                        60,
                        10,
                        kwargs.get("idim", 2)
                    ),
                    Field(
                        "toldef",
                        float,
                        70,
                        10,
                        kwargs.get("toldef", 0.01)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ips",
                        int,
                        0,
                        10,
                        kwargs.get("ips", 0)
                    ),
                    Field(
                        "stime",
                        float,
                        10,
                        10,
                        kwargs.get("stime", 1e+20)
                    ),
                    Field(
                        "iken",
                        int,
                        20,
                        10,
                        kwargs.get("iken", 0)
                    ),
                    Field(
                        "sf",
                        float,
                        30,
                        10,
                        kwargs.get("sf", 0.0)
                    ),
                    Field(
                        "cmid",
                        int,
                        40,
                        10,
                        kwargs.get("cmid")
                    ),
                    Field(
                        "ibr",
                        int,
                        50,
                        10,
                        kwargs.get("ibr", 1)
                    ),
                    Field(
                        "ds",
                        float,
                        60,
                        10,
                        kwargs.get("ds", 0.01)
                    ),
                    Field(
                        "ecut",
                        float,
                        70,
                        10,
                        kwargs.get("ecut", 0.01)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nip",
                        int,
                        0,
                        10,
                        kwargs.get("nip")
                    ),
                    Field(
                        "nxdof",
                        int,
                        10,
                        10,
                        kwargs.get("nxdof")
                    ),
                    Field(
                        "ihgf",
                        int,
                        20,
                        10,
                        kwargs.get("ihgf", 0)
                    ),
                    Field(
                        "itaj",
                        int,
                        30,
                        10,
                        kwargs.get("itaj", 0)
                    ),
                    Field(
                        "lmc",
                        int,
                        40,
                        10,
                        kwargs.get("lmc")
                    ),
                    Field(
                        "nhsv",
                        int,
                        50,
                        10,
                        kwargs.get("nhsv")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xi",
                        float,
                        0,
                        10,
                        kwargs.get("xi")
                    ),
                    Field(
                        "eta",
                        float,
                        10,
                        10,
                        kwargs.get("eta")
                    ),
                    Field(
                        "zeta",
                        float,
                        20,
                        10,
                        kwargs.get("zeta")
                    ),
                    Field(
                        "wgt",
                        float,
                        30,
                        10,
                        kwargs.get("wgt")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "p1",
                        float,
                        0,
                        10,
                        kwargs.get("p1")
                    ),
                    Field(
                        "p2",
                        float,
                        10,
                        10,
                        kwargs.get("p2")
                    ),
                    Field(
                        "p3",
                        float,
                        20,
                        10,
                        kwargs.get("p3")
                    ),
                    Field(
                        "p4",
                        float,
                        30,
                        10,
                        kwargs.get("p4")
                    ),
                    Field(
                        "p5",
                        float,
                        40,
                        10,
                        kwargs.get("p5")
                    ),
                    Field(
                        "p6",
                        float,
                        50,
                        10,
                        kwargs.get("p6")
                    ),
                    Field(
                        "p7",
                        float,
                        60,
                        10,
                        kwargs.get("p7")
                    ),
                    Field(
                        "p8",
                        float,
                        70,
                        10,
                        kwargs.get("p8")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = SectionSolidEfg.option_specs[0],
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
        self._cards[0].set_value("secid", value)

    @property
    def elform(self) -> int:
        """Get or set the Element formulation options.  Remark 2 enumerates the element formulations available for implicit calculations:
        EQ. -18: 8 point enhanced strain solid element with 13 incompatible modes(see Remarks 4 and 22)
        EQ. -2: 8 point hexahedron intended for elements with poor aspect ratios, accurate formulation(see Remark 15)
        EQ. -1: 8 point hexahedron intended for elements with poor aspect ratios, efficient formulation(see Remark 15)
        EQ. 0: 1 point corotational for *MAT_MODIFIED_HONEYCOMB(see Remark 3)
        EQ.1: Constant stress solid element : default element type.By specifying hourglass type 10 with this element, a Cosserat Point  Element is invoked; see *CONTROL_HOURGLASS.
        EQ.2: 8 point hexahedron(see Remark 4)
        EQ.3: Fully integrated quadratic 8 node element with nodal rotations
        EQ.4: S/R quadratic tetrahedron element with nodal rotations
        EQ.5 : 1 point ALE
        EQ.6 : 1 point Eulerian
        EQ.7 : 1 point Eulerian ambient
        EQ.8 : Acoustic
        EQ.9 : 1 point corotational for *MAT_MODIFIED_HONEYCOMB(see Remark 3)
        EQ.10 : 1 point tetrahedron(see Remark 1)
        EQ.11 : 1 point ALE multi - material element
        EQ.12 : 1 point integration with single material and void
        EQ.13 : 1 point nodal pressure tetrahedron(see Remark 14)
        EQ.14 : 8 point acoustic
        EQ.15 : 2 point pentahedron element(see Remark 1)
        EQ.16 : 4 or 5 point 10 - noded tetrahedron(see Remark 13).By specifying hourglass type 10 with this element, a Cosserat Point Element is invoked; see *CONTROL_HOURGLASS.
        EQ.17: 10 - noded composite tetrahedron(see Remark 13)
        EQ.18 : 9 point enhanced strain solid element with 12 incompatible modes(implicit only; see Remarks 4 and 22)
        EQ.19 : 8 - noded, 4 point cohesive element(see Remarks 1 and 6)
        EQ.20 : 8 - noded, 4 point cohesive element with offsets for use with shells(see Remarks 1, 6,and 8)
        EQ.21 : 6 - noded, 1 point pentahedron cohesive element(see Remarks 1 and 7)
        EQ.22 : 6 - noded, 1 point pentahedron cohesive element with offsets for use with shells(see Remarks 1, 7,and 8)
        EQ.23 : 20 - node solid formulation
        EQ.24 : 27 - noded, fully integrated S / R quadratic solid element(see Remark 21)
        EQ.25 : 21 - noded, quadratic pentahedron(see Remark 21)
        EQ.26 : 15 - noded, quadratic tetrahedron(see Remark 21)
        EQ.27 : 20 - noded, cubic tetrahedron(see Remark 21)
        EQ.28 : 40 - noded, cubic pentrahedron(see Remark 21)
        EQ.29 : 64 - noded, cubic hexahedron(see Remark 21)
        EQ.41 : Mesh - free(EFG) solid formulation(see Remark 16)
        EQ.42 : Adaptive 4 - noded mesh - free(EFG) solid formulation(see Remark 16)
        EQ.43 : Mesh - free enriched finite element
        EQ.45 : Tied mesh - free enriched finite element
        EQ.47 : Smoothed Particle Galerkin(SPG) method(see Remark 17)
        EQ.60 : 1 point tetrahedron(see Remark 19)
        EQ.62:	8 point brick with incompatible modes by assumed strain
        EQ.98 : Interpolation solid
        EQ.99 : Simplified linear element for time - domain vibration studies(See Remark 5)
        EQ.101 : User defined solid
        EQ.102 : User defined solid
        EQ.103 : User defined solid
        EQ.104 : User defined solid
        EQ.105 : User defined solid
        EQ.115 : 1 point pentahedron element with hourglass control
        GE.201 : Isogeometric solids with NURBS. (see *ELEMENT_SOLID_NURBS_PATCH)
        GE.1000 : Generalized user - defined solid element formulation(see *DEFINE_ELEMENT_GENERALIZED_SOLID)
        """ # nopep8
        return self._cards[0].get_value("elform")

    @elform.setter
    def elform(self, value: int) -> None:
        if value not in [1, -1, -2, -18, 0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 41, 42, 43, 45, 47, 60, 62, 98, 99, 101, 102, 103, 104, 105, 115, 201, 1000]:
            raise Exception("""elform must be one of {1,-1,-2,-18,0,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,41,42,43,45,47,60,62,98,99,101,102,103,104,105,115,201,1000}""")
        self._cards[0].set_value("elform", value)

    @property
    def aet(self) -> int:
        """Get or set the Ambient Element type: Can be defined for ELFORM 7, 11 and 12.
        EQ.1: temperature (not currently available),
        EQ.2: pressure and temperature (not currently available),
        EQ.3: pressure outflow,
        EQ.4: pressure inflow (default for ELFORM 7).
        EQ.5: receptor for blast load (see *LOAD_BLAST_ENHANCED, available only for ELFORM=11).
        """ # nopep8
        return self._cards[0].get_value("aet")

    @aet.setter
    def aet(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5]:
            raise Exception("""aet must be one of {0,1,2,3,4,5}""")
        self._cards[0].set_value("aet", value)

    @property
    def cohoff(self) -> typing.Optional[float]:
        """Get or set the Applies to cohesive solid elements 20 and 22. COHOFF specifies the relative location of the cohesive layer. It must be a number between -1 and 1. A value of -1 will place it on the bottom face of the cohesive element, while a value of +1 will place it on the top face. This parameter is preferably used when the cohesive element is used for connecting shells with different thicknesses. In this case the cohesive layer should not be located exactly between the bottom and top layer which is the default location
        """ # nopep8
        return self._cards[0].get_value("cohoff")

    @cohoff.setter
    def cohoff(self, value: float) -> None:
        self._cards[0].set_value("cohoff", value)

    @property
    def gaskeit(self) -> typing.Optional[float]:
        """Get or set the Gasket thickness for converting ELFORM 19, 20, 21 and 22 to gasket elements and use with *MAT_COHESIVE_GASKET
        """ # nopep8
        return self._cards[0].get_value("gaskeit")

    @gaskeit.setter
    def gaskeit(self, value: float) -> None:
        self._cards[0].set_value("gaskeit", value)

    @property
    def dx(self) -> float:
        """Get or set the Normalized dilation parameters of the kernel function in X, Y and Z directions. The normalized dilation parameters of the kernel function are introduced to provide the smoothness and compact support properties on the construction of the mesh-free shape functions. Values between 1.0 and 1.5 are recommended. Values smaller than 1.0 are not allowed. Larger values will increase the computation time and will sometimes result in a divergence problem.
        """ # nopep8
        return self._cards[1].get_value("dx")

    @dx.setter
    def dx(self, value: float) -> None:
        self._cards[1].set_value("dx", value)

    @property
    def dy(self) -> float:
        """Get or set the Normalized dilation parameters of the kernel function in X, Y and Z directions. The normalized dilation parameters of the kernel function are introduced to provide the smoothness and compact support properties on the construction of the mesh-free shape functions. Values between 1.0 and 1.5 are recommended. Values smaller than 1.0 are not allowed. Larger values will increase the computation time and will sometimes result in a divergence problem.
        """ # nopep8
        return self._cards[1].get_value("dy")

    @dy.setter
    def dy(self, value: float) -> None:
        self._cards[1].set_value("dy", value)

    @property
    def dz(self) -> float:
        """Get or set the Normalized dilation parameters of the kernel function in X, Y and Z directions. The normalized dilation parameters of the kernel function are introduced to provide the smoothness and compact support properties on the construction of the mesh-free shape functions. Values between 1.0 and 1.5 are recommended. Values smaller than 1.0 are not allowed. Larger values will increase the computation time and will sometimes result in a divergence problem.
        """ # nopep8
        return self._cards[1].get_value("dz")

    @dz.setter
    def dz(self, value: float) -> None:
        self._cards[1].set_value("dz", value)

    @property
    def ispline(self) -> int:
        """Get or set the Replace the choice for the EFG kernel functions definition in *CONTROL_EFG. This allows users to define different ISPLINE in different sections.
        EQ.0: Cubic spline function (default)
        EQ.1: Quadratic spline function
        EQ.2: Cubic spline function with circular shape.
        """ # nopep8
        return self._cards[1].get_value("ispline")

    @ispline.setter
    def ispline(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""ispline must be one of {0,1,2}""")
        self._cards[1].set_value("ispline", value)

    @property
    def idila(self) -> int:
        """Get or set the Replace the choice for the normalized dilation parameter definition in *CONTROL_EFG. This allows users to define different IDILA in different sections.
        EQ.0: Maximum distance based on the background elements,
        EQ.1: Maximum distance based on surrounding nodes.
        """ # nopep8
        return self._cards[1].get_value("idila")

    @idila.setter
    def idila(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""idila must be one of {0,1}""")
        self._cards[1].set_value("idila", value)

    @property
    def iebt(self) -> int:
        """Get or set the Essential boundary condition treatment.
        EQ.1: Full transformation method (default)
        EQ.-1: (w/o transformation),
        EQ.2: Mixed transformation method,
        EQ.3: Coupled FEM/EFG method,
        EQ.4: Fast transformation method,
        EQ.-4: (w/o transformation),
        EQ.5: Fluid particle method for E.O.S and *MAT_ELASTIC_FLUID materials.
        EQ.7:	Maximum entropy approximation
        """ # nopep8
        return self._cards[1].get_value("iebt")

    @iebt.setter
    def iebt(self, value: int) -> None:
        if value not in [3, -1, 1, 2, 4, -4, 5, 7]:
            raise Exception("""iebt must be one of {3,-1,1,2,4,-4,5,7}""")
        self._cards[1].set_value("iebt", value)

    @property
    def idim(self) -> int:
        """Get or set the Domain integration method.
        EQ.1: Local boundary integration,
        EQ.2: Two-point Gauss integration(default),
        EQ.3: Improved Gauss integration for IEBT=4 or -4.
        EQ.-1:Stabilized EFG integration method (apply to 6-noded cell, 8-noded cell or combination of these two)
        EQ.-2:EFG fracture method (apply to 4-noded cell and SMP only)
        """ # nopep8
        return self._cards[1].get_value("idim")

    @idim.setter
    def idim(self, value: int) -> None:
        if value not in [2, 1, 3, -1, -2]:
            raise Exception("""idim must be one of {2,1,3,-1,-2}""")
        self._cards[1].set_value("idim", value)

    @property
    def toldef(self) -> float:
        """Get or set the Deformation tolerance for the activation of adaptive EFG Semi-Lagrangian and Eulerian kernel.
        EQ.0.0: Lagrangian kernel,
        GT.0.0: Semi-Lagrangian kernel,
        LT.0.0: Eulerian kernel.
        """ # nopep8
        return self._cards[1].get_value("toldef")

    @toldef.setter
    def toldef(self, value: float) -> None:
        self._cards[1].set_value("toldef", value)

    @property
    def ips(self) -> int:
        """Get or set the EQ.0: No pressure smoothing (default)
        EQ.1: Moving-least squared pressure recovery.
        """ # nopep8
        return self._cards[2].get_value("ips")

    @ips.setter
    def ips(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ips must be one of {0,1}""")
        self._cards[2].set_value("ips", value)

    @property
    def stime(self) -> float:
        """Get or set the Time to switch from stabilized EFG to standard EFG formulation.
        """ # nopep8
        return self._cards[2].get_value("stime")

    @stime.setter
    def stime(self, value: float) -> None:
        self._cards[2].set_value("stime", value)

    @property
    def iken(self) -> int:
        """Get or set the EQ.0: Moving-least-square approximation (default, recommended)
        EQ.1: Maximum Entropy approximation.
        """ # nopep8
        return self._cards[2].get_value("iken")

    @iken.setter
    def iken(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""iken must be one of {0,1}""")
        self._cards[2].set_value("iken", value)

    @property
    def sf(self) -> float:
        """Get or set the Failure strain, recommended as an extra condition for the crack initiation
        under slow loading besides the stress-based cohesive law.
        """ # nopep8
        return self._cards[2].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        self._cards[2].set_value("sf", value)

    @property
    def cmid(self) -> typing.Optional[int]:
        """Get or set the Cohesive material ID for EFG fracture analysis (only Mode I crack is
        considered and only *MAT_COHESIVE_TH is available).
        """ # nopep8
        return self._cards[2].get_value("cmid")

    @cmid.setter
    def cmid(self, value: int) -> None:
        self._cards[2].set_value("cmid", value)

    @property
    def ibr(self) -> int:
        """Get or set the EQ.1: No branching allowed
        EQ.2: Branching is allowed.
        """ # nopep8
        return self._cards[2].get_value("ibr")

    @ibr.setter
    def ibr(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""ibr must be one of {1,2}""")
        self._cards[2].set_value("ibr", value)

    @property
    def ds(self) -> float:
        """Get or set the Normalized support defined for computing the displacement jump in fracture analysis.
        """ # nopep8
        return self._cards[2].get_value("ds")

    @ds.setter
    def ds(self, value: float) -> None:
        self._cards[2].set_value("ds", value)

    @property
    def ecut(self) -> float:
        """Get or set the Define the minimum distance to the node that a crack surface can cut to the edge.
        """ # nopep8
        return self._cards[2].get_value("ecut")

    @ecut.setter
    def ecut(self, value: float) -> None:
        self._cards[2].set_value("ecut", value)

    @property
    def nip(self) -> typing.Optional[int]:
        """Get or set the Number of integration points for user-defined solid (0 if resultant/discrete element).
        """ # nopep8
        return self._cards[3].get_value("nip")

    @nip.setter
    def nip(self, value: int) -> None:
        self._cards[3].set_value("nip", value)

    @property
    def nxdof(self) -> typing.Optional[int]:
        """Get or set the Number of extra degrees of freedom per node for user-defined solid.
        """ # nopep8
        return self._cards[3].get_value("nxdof")

    @nxdof.setter
    def nxdof(self, value: int) -> None:
        self._cards[3].set_value("nxdof", value)

    @property
    def ihgf(self) -> int:
        """Get or set the Flag for using hourglass stabilization (NIP.GT.0).
        """ # nopep8
        return self._cards[3].get_value("ihgf")

    @ihgf.setter
    def ihgf(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""ihgf must be one of {0,1,2,3}""")
        self._cards[3].set_value("ihgf", value)

    @property
    def itaj(self) -> int:
        """Get or set the Flag for setting up finite element matrices (NIP.GT.0).
        """ # nopep8
        return self._cards[3].get_value("itaj")

    @itaj.setter
    def itaj(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""itaj must be one of {0,1}""")
        self._cards[3].set_value("itaj", value)

    @property
    def lmc(self) -> typing.Optional[int]:
        """Get or set the Number of property parameters.
        """ # nopep8
        return self._cards[3].get_value("lmc")

    @lmc.setter
    def lmc(self, value: int) -> None:
        self._cards[3].set_value("lmc", value)

    @property
    def nhsv(self) -> typing.Optional[int]:
        """Get or set the Number of history variables.
        """ # nopep8
        return self._cards[3].get_value("nhsv")

    @nhsv.setter
    def nhsv(self, value: int) -> None:
        self._cards[3].set_value("nhsv", value)

    @property
    def xi(self) -> typing.Optional[float]:
        """Get or set the First isoparametric coordinate.
        """ # nopep8
        return self._cards[4].get_value("xi")

    @xi.setter
    def xi(self, value: float) -> None:
        self._cards[4].set_value("xi", value)

    @property
    def eta(self) -> typing.Optional[float]:
        """Get or set the Second isoparametric coordinate.
        """ # nopep8
        return self._cards[4].get_value("eta")

    @eta.setter
    def eta(self, value: float) -> None:
        self._cards[4].set_value("eta", value)

    @property
    def zeta(self) -> typing.Optional[float]:
        """Get or set the Third isoparametric coordinate.
        """ # nopep8
        return self._cards[4].get_value("zeta")

    @zeta.setter
    def zeta(self, value: float) -> None:
        self._cards[4].set_value("zeta", value)

    @property
    def wgt(self) -> typing.Optional[float]:
        """Get or set the Isoparametric weight.
        """ # nopep8
        return self._cards[4].get_value("wgt")

    @wgt.setter
    def wgt(self, value: float) -> None:
        self._cards[4].set_value("wgt", value)

    @property
    def p1(self) -> typing.Optional[float]:
        """Get or set the property parameter.
        """ # nopep8
        return self._cards[5].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        self._cards[5].set_value("p1", value)

    @property
    def p2(self) -> typing.Optional[float]:
        """Get or set the property parameter.
        """ # nopep8
        return self._cards[5].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        self._cards[5].set_value("p2", value)

    @property
    def p3(self) -> typing.Optional[float]:
        """Get or set the property parameter.
        """ # nopep8
        return self._cards[5].get_value("p3")

    @p3.setter
    def p3(self, value: float) -> None:
        self._cards[5].set_value("p3", value)

    @property
    def p4(self) -> typing.Optional[float]:
        """Get or set the property parameter.
        """ # nopep8
        return self._cards[5].get_value("p4")

    @p4.setter
    def p4(self, value: float) -> None:
        self._cards[5].set_value("p4", value)

    @property
    def p5(self) -> typing.Optional[float]:
        """Get or set the property parameter.
        """ # nopep8
        return self._cards[5].get_value("p5")

    @p5.setter
    def p5(self, value: float) -> None:
        self._cards[5].set_value("p5", value)

    @property
    def p6(self) -> typing.Optional[float]:
        """Get or set the property parameter.
        """ # nopep8
        return self._cards[5].get_value("p6")

    @p6.setter
    def p6(self, value: float) -> None:
        self._cards[5].set_value("p6", value)

    @property
    def p7(self) -> typing.Optional[float]:
        """Get or set the property parameter.
        """ # nopep8
        return self._cards[5].get_value("p7")

    @p7.setter
    def p7(self, value: float) -> None:
        self._cards[5].set_value("p7", value)

    @property
    def p8(self) -> typing.Optional[float]:
        """Get or set the property parameter.
        """ # nopep8
        return self._cards[5].get_value("p8")

    @p8.setter
    def p8(self, value: float) -> None:
        self._cards[5].set_value("p8", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[6].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[6].cards[0].set_value("title", value)

