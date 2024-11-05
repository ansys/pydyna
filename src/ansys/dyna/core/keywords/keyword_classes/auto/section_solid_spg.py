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

class SectionSolidSpg(KeywordBase):
    """DYNA SECTION_SOLID_SPG keyword"""

    keyword = "SECTION"
    subkeyword = "SOLID_SPG"
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
                        kwargs.get("dx")
                    ),
                    Field(
                        "dy",
                        float,
                        10,
                        10,
                        kwargs.get("dy")
                    ),
                    Field(
                        "dz",
                        float,
                        20,
                        10,
                        kwargs.get("dz")
                    ),
                    Field(
                        "ispline",
                        int,
                        30,
                        10,
                        kwargs.get("ispline", 0)
                    ),
                    Field(
                        "kernel",
                        int,
                        40,
                        10,
                        kwargs.get("kernel", 0)
                    ),
                    Field(
                        "unused",
                        float,
                        50,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "smstep",
                        int,
                        60,
                        10,
                        kwargs.get("smstep")
                    ),
                    Field(
                        "msc",
                        float,
                        70,
                        10,
                        kwargs.get("msc")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "idam",
                        int,
                        0,
                        10,
                        kwargs.get("idam", 1)
                    ),
                    Field(
                        "fs",
                        float,
                        10,
                        10,
                        kwargs.get("fs")
                    ),
                    Field(
                        "stretch",
                        float,
                        20,
                        10,
                        kwargs.get("stretch", 1.0E10)
                    ),
                    Field(
                        "itb",
                        int,
                        30,
                        10,
                        kwargs.get("itb")
                    ),
                    Field(
                        "msfac",
                        float,
                        40,
                        10,
                        kwargs.get("msfac")
                    ),
                    Field(
                        "isc",
                        float,
                        50,
                        10,
                        kwargs.get("isc")
                    ),
                    Field(
                        "boxid",
                        int,
                        60,
                        10,
                        kwargs.get("boxid")
                    ),
                    Field(
                        "pdamp",
                        float,
                        70,
                        10,
                        kwargs.get("pdamp", -0.001)
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
                option_spec = SectionSolidSpg.option_specs[0],
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
    def dx(self) -> typing.Optional[float]:
        """Get or set the Normalized dilation parameters of the kernel function in x, y and z directions, respectively.  The normalized dilation parameters of the kernel function are introduced to provide the smoothness and locality on the construction of the mesh-free shape functions.  Values between 1.4 and 1.8 are recommended.  Values smaller than 1.0 are not allowed.  Larger values will increase the computation time and will sometimes result in divergence of the solution.
        """ # nopep8
        return self._cards[1].get_value("dx")

    @dx.setter
    def dx(self, value: float) -> None:
        self._cards[1].set_value("dx", value)

    @property
    def dy(self) -> typing.Optional[float]:
        """Get or set the Normalized dilation parameters of the kernel function in x, y and z directions, respectively.  The normalized dilation parameters of the kernel function are introduced to provide the smoothness and locality on the construction of the mesh-free shape functions.  Values between 1.4 and 1.8 are recommended.  Values smaller than 1.0 are not allowed.  Larger values will increase the computation time and will sometimes result in divergence of the solution.
        """ # nopep8
        return self._cards[1].get_value("dy")

    @dy.setter
    def dy(self, value: float) -> None:
        self._cards[1].set_value("dy", value)

    @property
    def dz(self) -> typing.Optional[float]:
        """Get or set the Normalized dilation parameters of the kernel function in x, y and z directions, respectively.  The normalized dilation parameters of the kernel function are introduced to provide the smoothness and locality on the construction of the mesh-free shape functions.  Values between 1.4 and 1.8 are recommended.  Values smaller than 1.0 are not allowed.  Larger values will increase the computation time and will sometimes result in divergence of the solution.
        """ # nopep8
        return self._cards[1].get_value("dz")

    @dz.setter
    def dz(self, value: float) -> None:
        self._cards[1].set_value("dz", value)

    @property
    def ispline(self) -> int:
        """Get or set the Type of kernel function:
        EQ.0:	Cubic spline function with cubical support(default)
        EQ.1 : Quadratic spline function with cubical support
        EQ.2 : Cubic spline function with spherical support
        """ # nopep8
        return self._cards[1].get_value("ispline")

    @ispline.setter
    def ispline(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""ispline must be one of {0,1,2}""")
        self._cards[1].set_value("ispline", value)

    @property
    def kernel(self) -> int:
        """Get or set the Type of kernel support update scheme:
        EQ.0:	Updated Lagrangian, failure or no failure analysis, tension dominant problem
        EQ.1 : Eulerian, failure analysis, global extreme deformation
        EQ.2 : Pseudo Lagrangian, failure analysis, local extreme deformation
        """ # nopep8
        return self._cards[1].get_value("kernel")

    @kernel.setter
    def kernel(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""kernel must be one of {0,1,2}""")
        self._cards[1].set_value("kernel", value)

    @property
    def smstep(self) -> typing.Optional[int]:
        """Get or set the Interval of time steps to conduct displacement smoothing
        Default: 15, if KERNEL = 0
        5, if KERNEL = 1
        30, if KERNEL = 2
        Code will determine according to KERNEL if SMSTEP = 0 from input
        """ # nopep8
        return self._cards[1].get_value("smstep")

    @smstep.setter
    def smstep(self, value: int) -> None:
        self._cards[1].set_value("smstep", value)

    @property
    def msc(self) -> typing.Optional[float]:
        """Get or set the Smoothing scheme for momentum consistent SPG only (ITB=3 on Card2b)
        EQ.0:  Regular smoothing scheme
        EQ.1: New smoothing scheme for very low speed deformation, with better controls of the low energy modes than the regular smoothing scheme
        """ # nopep8
        return self._cards[1].get_value("msc")

    @msc.setter
    def msc(self, value: float) -> None:
        self._cards[1].set_value("msc", value)

    @property
    def idam(self) -> int:
        """Get or set the Option of bond failure mechanism
        EQ.1:	Effective plastic strain(phenomenological strain damage, default)
        EQ.2 : Maximum principal stress
        EQ.3 : Maximum shear strain
        EQ.4 : Minimum principal strain(input must be positive)
        EQ.5 : Effective plastic strain and maximum shear strain
        EQ.7 : Anisotropic damage for honeycomb modeled with* MAT_126 only.We recommend only using this with ITB = 3. This feature is available starting with R13.
        EQ.11 : Pre - damage model for brittle material failure(with crack propagation).It includes both bond failureand stress degradation.It is available as of R13.We recommend using this with ITB = 3.
        EQ.13 : Pre - damage model for ductile material failure.It includes stress degradation but not bond failure.It is available as of R13.We recommend using this with ITB = 3.
        """ # nopep8
        return self._cards[2].get_value("idam")

    @idam.setter
    def idam(self, value: int) -> None:
        self._cards[2].set_value("idam", value)

    @property
    def fs(self) -> typing.Optional[float]:
        """Get or set the Critical value of the quantity indicated by IDAM for bond failure triggering. Default: 1.0E+10, i.e., no failure analysis
        For * MAT_3 and *MAT_24, “FS” on material cards overwrites this value.
        When FS is defined on material cards, not only bond failure will occur when it is reached, but also the stress will be set to zero according to material law.
        If FS is defined on SPG card, only bond failure will occur without setting  stress to zero
        """ # nopep8
        return self._cards[2].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        self._cards[2].set_value("fs", value)

    @property
    def stretch(self) -> float:
        """Get or set the Critical relative deformation (stretching or compression ratio) between the two nodes forming the bond for bond failure
        """ # nopep8
        return self._cards[2].get_value("stretch")

    @stretch.setter
    def stretch(self, value: float) -> None:
        self._cards[2].set_value("stretch", value)

    @property
    def itb(self) -> typing.Optional[int]:
        """Get or set the Option of stabilization:
        EQ.1:	Fluid particle approximation(accurate but slow), used with KERNEL = 0 or 1
        EQ.2 : Simplified fluid particle approximation(efficient and robust), used with KERNEL = 2
        EQ.3 : Momentum consistent SPG(MCSPG) formulation(latest beta or R14 is recommended).MCSPG can be applied for large deformation, tension dominant problems.For coupled thermal mechanical problems, MCSPG is the only option.KERNEL = 1 is recommended for MCSPG.
        Default : 1, if KERNEL = 0 or 1
        2, if KERNEL = 2
        """ # nopep8
        return self._cards[2].get_value("itb")

    @itb.setter
    def itb(self, value: int) -> None:
        self._cards[2].set_value("itb", value)

    @property
    def msfac(self) -> typing.Optional[float]:
        """Get or set the For momentum consistent SPG invoked with ITB = 3 only, quadrature factor for surface nodes to suppress shear locking in thin structures. We recommend using the latest beta version or R14. The default for a regular solid structure is 1.00 while for a thin structure is 0.75.
        """ # nopep8
        return self._cards[2].get_value("msfac")

    @msfac.setter
    def msfac(self, value: float) -> None:
        self._cards[2].set_value("msfac", value)

    @property
    def isc(self) -> typing.Optional[float]:
        """Get or set the Self-contact indicator:
        EQ.0:	No self - contact between the bond - failed particles in the same part.
        EQ.1 : Self - contact is defined between the bond - failed particles in the same part.The penalty factor in the self - contact is between 0.01 to 0.1 × Young’s modulus.This option is available for SMP only.
        """ # nopep8
        return self._cards[2].get_value("isc")

    @isc.setter
    def isc(self, value: float) -> None:
        self._cards[2].set_value("isc", value)

    @property
    def boxid(self) -> typing.Optional[int]:
        """Get or set the ID of a box defining the active SPG region. Outside this region, the particles are not included in the SPG calculation. See *DEFINE_BOX
        """ # nopep8
        return self._cards[2].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        self._cards[2].set_value("boxid", value)

    @property
    def pdamp(self) -> float:
        """Get or set the Particle-to-particle damping coefficient. It is used for momentum consistent SPG (ITB = 3) only. The recommended range of values is -0.01 to -0.001. A positive value is not recommended
        """ # nopep8
        return self._cards[2].get_value("pdamp")

    @pdamp.setter
    def pdamp(self, value: float) -> None:
        self._cards[2].set_value("pdamp", value)

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

