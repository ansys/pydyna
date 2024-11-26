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

class Mat103(KeywordBase):
    """DYNA MAT_103 keyword"""

    keyword = "MAT"
    subkeyword = "103"
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
                        "mid",
                        int,
                        0,
                        10,
                        kwargs.get("mid")
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        kwargs.get("ro")
                    ),
                    Field(
                        "e",
                        float,
                        20,
                        10,
                        kwargs.get("e")
                    ),
                    Field(
                        "pr",
                        float,
                        30,
                        10,
                        kwargs.get("pr")
                    ),
                    Field(
                        "sigy",
                        float,
                        40,
                        10,
                        kwargs.get("sigy")
                    ),
                    Field(
                        "flag",
                        int,
                        50,
                        10,
                        kwargs.get("flag", 0)
                    ),
                    Field(
                        "lcss",
                        int,
                        60,
                        10,
                        kwargs.get("lcss")
                    ),
                    Field(
                        "alpha",
                        float,
                        70,
                        10,
                        kwargs.get("alpha")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "qr1",
                        float,
                        0,
                        10,
                        kwargs.get("qr1")
                    ),
                    Field(
                        "cr1",
                        float,
                        10,
                        10,
                        kwargs.get("cr1")
                    ),
                    Field(
                        "qr2",
                        float,
                        20,
                        10,
                        kwargs.get("qr2")
                    ),
                    Field(
                        "cr2",
                        float,
                        30,
                        10,
                        kwargs.get("cr2")
                    ),
                    Field(
                        "qx1",
                        float,
                        40,
                        10,
                        kwargs.get("qx1")
                    ),
                    Field(
                        "cx1",
                        float,
                        50,
                        10,
                        kwargs.get("cx1")
                    ),
                    Field(
                        "qx2",
                        float,
                        60,
                        10,
                        kwargs.get("qx2")
                    ),
                    Field(
                        "cx2",
                        float,
                        70,
                        10,
                        kwargs.get("cx2")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "vk",
                        float,
                        0,
                        10,
                        kwargs.get("vk")
                    ),
                    Field(
                        "vm",
                        float,
                        10,
                        10,
                        kwargs.get("vm")
                    ),
                    Field(
                        "r00/f",
                        float,
                        20,
                        10,
                        kwargs.get("r00/f")
                    ),
                    Field(
                        "r45/g",
                        float,
                        30,
                        10,
                        kwargs.get("r45/g")
                    ),
                    Field(
                        "r90/h",
                        float,
                        40,
                        10,
                        kwargs.get("r90/h")
                    ),
                    Field(
                        "l",
                        float,
                        50,
                        10,
                        kwargs.get("l")
                    ),
                    Field(
                        "m",
                        float,
                        60,
                        10,
                        kwargs.get("m")
                    ),
                    Field(
                        "n",
                        float,
                        70,
                        10,
                        kwargs.get("n")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "aopt",
                        float,
                        0,
                        10,
                        kwargs.get("aopt")
                    ),
                    Field(
                        "fail",
                        float,
                        10,
                        10,
                        kwargs.get("fail")
                    ),
                    Field(
                        "numint",
                        float,
                        20,
                        10,
                        kwargs.get("numint")
                    ),
                    Field(
                        "macf",
                        int,
                        30,
                        10,
                        kwargs.get("macf", 1)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xp",
                        float,
                        0,
                        10,
                        kwargs.get("xp")
                    ),
                    Field(
                        "yp",
                        float,
                        10,
                        10,
                        kwargs.get("yp")
                    ),
                    Field(
                        "zp",
                        float,
                        20,
                        10,
                        kwargs.get("zp")
                    ),
                    Field(
                        "a1",
                        float,
                        30,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "a2",
                        float,
                        40,
                        10,
                        kwargs.get("a2")
                    ),
                    Field(
                        "a3",
                        float,
                        50,
                        10,
                        kwargs.get("a3")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "v1",
                        float,
                        0,
                        10,
                        kwargs.get("v1")
                    ),
                    Field(
                        "v2",
                        float,
                        10,
                        10,
                        kwargs.get("v2")
                    ),
                    Field(
                        "v3",
                        float,
                        20,
                        10,
                        kwargs.get("v3")
                    ),
                    Field(
                        "d1",
                        float,
                        30,
                        10,
                        kwargs.get("d1")
                    ),
                    Field(
                        "d2",
                        float,
                        40,
                        10,
                        kwargs.get("d2")
                    ),
                    Field(
                        "d3",
                        float,
                        50,
                        10,
                        kwargs.get("d3")
                    ),
                    Field(
                        "beta",
                        float,
                        60,
                        10,
                        kwargs.get("beta")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat103.option_specs[0],
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
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def sigy(self) -> typing.Optional[float]:
        """Get or set the Initial yield stress.
        """ # nopep8
        return self._cards[0].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        self._cards[0].set_value("sigy", value)

    @property
    def flag(self) -> int:
        """Get or set the Flag
        EQ.0 Give all material parameters (default),
        EQ.1 Material parameters are fit in LS-DYNA to Load curve. The parameters Qr1, Cr1, Qr2, and Cr2 for isotropic hardening are determined by the fit and those for kinematic hardening are found by scaling those for isotropic hardening by (1-α) where α is defined.
        EQ.2: Use load curve directly, i.e., no fitting is required for the parameters Q-r1, C-r1, Q-r2, and C-r2.EQ.4: Use table definition directly, no fitting is required and the values for Qr1, Cr1, Qr2, Cr2, Vk and Vm are ignored. Only
        isotropic hardening is implemented, and this option is only available for solids
        """ # nopep8
        return self._cards[0].get_value("flag")

    @flag.setter
    def flag(self, value: int) -> None:
        if value not in [0, 1, 2, 4]:
            raise Exception("""flag must be one of {0,1,2,4}""")
        self._cards[0].set_value("flag", value)

    @property
    def lcss(self) -> typing.Optional[int]:
        """Get or set the Load curve ID or Table ID. The load curve ID defines effective stress versus effective plastic strain. Card 2 is ignored with this option. The table ID, see Figure 20.7, defines for each strain rate value a load curve ID giving the stress versus effectiveplastic strain for that rate. If the load curve only is used, then the coefficients Vk and Vm must be given if viscoplastice behavior is desired. If a Table ID is given these coefficients are determined internally during initialization.
        """ # nopep8
        return self._cards[0].get_value("lcss")

    @lcss.setter
    def lcss(self, value: int) -> None:
        self._cards[0].set_value("lcss", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Distribution of hardening used in the curve-fitting. ALPHA = 0 provides pure kinematic hardening, ALPHA = 1 provides pure isotropic hardening.
        """ # nopep8
        return self._cards[0].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        self._cards[0].set_value("alpha", value)

    @property
    def qr1(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter Qr1.
        """ # nopep8
        return self._cards[1].get_value("qr1")

    @qr1.setter
    def qr1(self, value: float) -> None:
        self._cards[1].set_value("qr1", value)

    @property
    def cr1(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter Cr1.
        """ # nopep8
        return self._cards[1].get_value("cr1")

    @cr1.setter
    def cr1(self, value: float) -> None:
        self._cards[1].set_value("cr1", value)

    @property
    def qr2(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter Qr2.
        """ # nopep8
        return self._cards[1].get_value("qr2")

    @qr2.setter
    def qr2(self, value: float) -> None:
        self._cards[1].set_value("qr2", value)

    @property
    def cr2(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter Cr2.
        """ # nopep8
        return self._cards[1].get_value("cr2")

    @cr2.setter
    def cr2(self, value: float) -> None:
        self._cards[1].set_value("cr2", value)

    @property
    def qx1(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening parameter Qc1.
        """ # nopep8
        return self._cards[1].get_value("qx1")

    @qx1.setter
    def qx1(self, value: float) -> None:
        self._cards[1].set_value("qx1", value)

    @property
    def cx1(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening parameter Cc1.
        """ # nopep8
        return self._cards[1].get_value("cx1")

    @cx1.setter
    def cx1(self, value: float) -> None:
        self._cards[1].set_value("cx1", value)

    @property
    def qx2(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening parameter Qc2.
        """ # nopep8
        return self._cards[1].get_value("qx2")

    @qx2.setter
    def qx2(self, value: float) -> None:
        self._cards[1].set_value("qx2", value)

    @property
    def cx2(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening parameter Cc2.
        """ # nopep8
        return self._cards[1].get_value("cx2")

    @cx2.setter
    def cx2(self, value: float) -> None:
        self._cards[1].set_value("cx2", value)

    @property
    def vk(self) -> typing.Optional[float]:
        """Get or set the Viscous material parameter Vk.
        """ # nopep8
        return self._cards[2].get_value("vk")

    @vk.setter
    def vk(self, value: float) -> None:
        self._cards[2].set_value("vk", value)

    @property
    def vm(self) -> typing.Optional[float]:
        """Get or set the Viscous material parameter Vm.
        """ # nopep8
        return self._cards[2].get_value("vm")

    @vm.setter
    def vm(self, value: float) -> None:
        self._cards[2].set_value("vm", value)

    @property
    def r00_f(self) -> typing.Optional[float]:
        """Get or set the R00 for shell, default=1.0, F for brick default =1/2.
        """ # nopep8
        return self._cards[2].get_value("r00/f")

    @r00_f.setter
    def r00_f(self, value: float) -> None:
        self._cards[2].set_value("r00/f", value)

    @property
    def r45_g(self) -> typing.Optional[float]:
        """Get or set the R45 for shell, default=1.0, G for brick default =1/2.
        """ # nopep8
        return self._cards[2].get_value("r45/g")

    @r45_g.setter
    def r45_g(self, value: float) -> None:
        self._cards[2].set_value("r45/g", value)

    @property
    def r90_h(self) -> typing.Optional[float]:
        """Get or set the R90 for shell, default=1.0, H for brick default =1/2.
        """ # nopep8
        return self._cards[2].get_value("r90/h")

    @r90_h.setter
    def r90_h(self, value: float) -> None:
        self._cards[2].set_value("r90/h", value)

    @property
    def l(self) -> typing.Optional[float]:
        """Get or set the L for brick, default =3/2.
        """ # nopep8
        return self._cards[2].get_value("l")

    @l.setter
    def l(self, value: float) -> None:
        self._cards[2].set_value("l", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the M for brick, default =3/2.
        """ # nopep8
        return self._cards[2].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        self._cards[2].set_value("m", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the N for brick, default =3/2.
        """ # nopep8
        return self._cards[2].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        self._cards[2].set_value("n", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
        EQ.0.0:	Locally orthotropic with material axes determined by element nodes 1, 2,and 4, as with* DEFINE_COORDINATE_NODES.For shells only, the material axes are then rotated about the normal vector to the surface of the shell by the angle BETA.
        EQ.1.0 : Locally orthotropic with material axes determined by a point, P, in spaceand the global location of the element center; this is the a - direction.This option is for solid elements only.
        EQ.2.0:	Globally orthotropic with material axes determined by vectors defined below, as with* DEFINE_COORDINATE_VECTOR
        EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element.The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.Thus, for solid elements, AOPT = 3 is only available for hexahedrons.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle BETA.BETA may be set in the keyword input for the element or in the input for this keyword.Note that for solids, the material axes may be switched depending on the choice of MACF.The switch may occur before or after applying BETA depending on the value of MACF.
        EQ.4.0 : Locally orthotropic in a cylindrical coordinate system with the material axes determined by a vector v,and an originating point, P, which define the centerline axis.This option is for solid elements only.
        LT.0.0 : The absolute value of AOPT is a coordinate system ID number(CID on * DEFINE_COORDINATE_OPTION).
        """ # nopep8
        return self._cards[3].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[3].set_value("aopt", value)

    @property
    def fail(self) -> typing.Optional[float]:
        """Get or set the Failure flag.
        LT.0.0: User defined failure subroutine is called to determine failure. This is subroutine named, MATUSR_103, in DYN21.F.
        EQ.0.0: Failure is not considered. This option is recommended if failure is not of interest since many calculations will be saved.
        GT.0.0: Plastic strain to failure. When the plastic strain reaches this value, the element is deleted from the calculation.
        """ # nopep8
        return self._cards[3].get_value("fail")

    @fail.setter
    def fail(self, value: float) -> None:
        self._cards[3].set_value("fail", value)

    @property
    def numint(self) -> typing.Optional[float]:
        """Get or set the Number of integration points which must fail before element deletion. If zero, all points must fail. This option applies to shell elements only. For the case of one point shells, NUMINT should be set to a value that is less than the number of through thickness integration points. Nonphysical stretching can sometimes appear in the results if all integration points have failed except for one point away from the midsurface. This is due to the fact that unconstrained nodal rotations will prevent strains from developing at the remaining integration point. In fully integrated shells, similar problems can occur.
        """ # nopep8
        return self._cards[3].get_value("numint")

    @numint.setter
    def numint(self, value: float) -> None:
        self._cards[3].set_value("numint", value)

    @property
    def macf(self) -> int:
        """Get or set the Material axes change flag for solid elements:
        EQ.1 : No change, default
        EQ.2 : Switch material axes a and b after BETA rotation
        EQ.3 : Switch material axes a and c after BETA rotation
        EQ.4 : Switch material axes b and c after BETA rotation
        EQ. - 4 : Switch material axes b and c before BETA rotation
        EQ. - 3 : Switch material axes a and c before BETA rotation
        EQ. - 2 : Switch material axes a and b before BETA rotation
        Figure Error!Reference source not found.indicates when LS - DYNA applies MACF during the process to obtain the final material axes.If BETA on * ELEMENT_SOLID_{OPTION} is defined, then that BETA is used for the rotation for all AOPT options.Otherwise, if AOPT = 3, the BETA input on Card 3 rotates the axes.For all other values of AOPT, the material axes will be switched as specified by MACF, but no BETA rotation will be performed.
        """ # nopep8
        return self._cards[3].get_value("macf")

    @macf.setter
    def macf(self, value: int) -> None:
        if value not in [1, 2, 3, 4, -4, -3, -2]:
            raise Exception("""macf must be one of {1,2,3,4,-4,-3,-2}""")
        self._cards[3].set_value("macf", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the x-coordinates of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[4].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[4].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the y-coordinates of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[4].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[4].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the z-coordinates of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[4].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[4].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[4].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[4].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[4].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the component of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[5].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[5].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the component of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[5].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[5].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the component of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[5].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[5].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[5].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[5].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[5].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle for AOPT = 3, which may be overridden on the element card, see *ELEMENT_SHELL.
        """ # nopep8
        return self._cards[5].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[5].set_value("beta", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[6].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[6].cards[0].set_value("title", value)

