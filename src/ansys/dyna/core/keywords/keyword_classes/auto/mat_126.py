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

class Mat126(KeywordBase):
    """DYNA MAT_126 keyword"""

    keyword = "MAT"
    subkeyword = "126"
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
                        "vf",
                        float,
                        50,
                        10,
                        kwargs.get("vf")
                    ),
                    Field(
                        "mu",
                        float,
                        60,
                        10,
                        kwargs.get("mu", 5.0E-02)
                    ),
                    Field(
                        "bulk",
                        float,
                        70,
                        10,
                        kwargs.get("bulk", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lca",
                        int,
                        0,
                        10,
                        kwargs.get("lca")
                    ),
                    Field(
                        "lcb",
                        int,
                        10,
                        10,
                        kwargs.get("lcb", 0)
                    ),
                    Field(
                        "lcc",
                        int,
                        20,
                        10,
                        kwargs.get("lcc", 0)
                    ),
                    Field(
                        "lcs",
                        int,
                        30,
                        10,
                        kwargs.get("lcs", 0)
                    ),
                    Field(
                        "lcab",
                        int,
                        40,
                        10,
                        kwargs.get("lcab", 0)
                    ),
                    Field(
                        "lcbc",
                        int,
                        50,
                        10,
                        kwargs.get("lcbc", 0)
                    ),
                    Field(
                        "lcca",
                        int,
                        60,
                        10,
                        kwargs.get("lcca", 0)
                    ),
                    Field(
                        "lcsr",
                        int,
                        70,
                        10,
                        kwargs.get("lcsr", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "eaau",
                        float,
                        0,
                        10,
                        kwargs.get("eaau")
                    ),
                    Field(
                        "ebbu",
                        float,
                        10,
                        10,
                        kwargs.get("ebbu")
                    ),
                    Field(
                        "eccu",
                        float,
                        20,
                        10,
                        kwargs.get("eccu")
                    ),
                    Field(
                        "gabu",
                        float,
                        30,
                        10,
                        kwargs.get("gabu")
                    ),
                    Field(
                        "gbcu",
                        float,
                        40,
                        10,
                        kwargs.get("gbcu")
                    ),
                    Field(
                        "gcau",
                        float,
                        50,
                        10,
                        kwargs.get("gcau")
                    ),
                    Field(
                        "aopt",
                        float,
                        60,
                        10,
                        kwargs.get("aopt")
                    ),
                    Field(
                        "macf",
                        int,
                        70,
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
                        "d1",
                        float,
                        0,
                        10,
                        kwargs.get("d1")
                    ),
                    Field(
                        "d2",
                        float,
                        10,
                        10,
                        kwargs.get("d2")
                    ),
                    Field(
                        "d3",
                        float,
                        20,
                        10,
                        kwargs.get("d3")
                    ),
                    Field(
                        "tsef",
                        float,
                        30,
                        10,
                        kwargs.get("tsef")
                    ),
                    Field(
                        "ssef",
                        float,
                        40,
                        10,
                        kwargs.get("ssef")
                    ),
                    Field(
                        "vref",
                        float,
                        50,
                        10,
                        kwargs.get("vref")
                    ),
                    Field(
                        "tref",
                        float,
                        60,
                        10,
                        kwargs.get("tref")
                    ),
                    Field(
                        "shdflg",
                        float,
                        70,
                        10,
                        kwargs.get("shdflg", 0.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat126.option_specs[0],
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
        """Get or set the Young's modulus for compacted hot_plate_rolling material.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio for compacted modified_honeycomb material.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def sigy(self) -> typing.Optional[float]:
        """Get or set the Yield stress for fully compacted modified_honeycomb.
        """ # nopep8
        return self._cards[0].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        self._cards[0].set_value("sigy", value)

    @property
    def vf(self) -> typing.Optional[float]:
        """Get or set the Relative volume at which the modified_honeycomb is fully compacted.
        """ # nopep8
        return self._cards[0].get_value("vf")

    @vf.setter
    def vf(self, value: float) -> None:
        self._cards[0].set_value("vf", value)

    @property
    def mu(self) -> float:
        """Get or set the Material viscosity coefficient (default = 0.05).
        """ # nopep8
        return self._cards[0].get_value("mu")

    @mu.setter
    def mu(self, value: float) -> None:
        self._cards[0].set_value("mu", value)

    @property
    def bulk(self) -> float:
        """Get or set the Bulk viscosity flag:
        EQ.0.0: bulk viscosity is not used (default),
        EQ.1.0: bulk viscosity is active and MU=0
        """ # nopep8
        return self._cards[0].get_value("bulk")

    @bulk.setter
    def bulk(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""bulk must be one of {0.0,1.0}""")
        self._cards[0].set_value("bulk", value)

    @property
    def lca(self) -> typing.Optional[int]:
        """Get or set the Load curve ID, see *DEFINE_CURVE,
        LCA<0:Yield stress as a function of the angle off the material axis in degrees.
        LCA>0:sigma-aa versus normal strain component aa. For the corotational solid elements, types 0 and 9, engineering strain is expected, but for all other solid element formulations a logarithmic strain is expected.
        """ # nopep8
        return self._cards[1].get_value("lca")

    @lca.setter
    def lca(self, value: int) -> None:
        self._cards[1].set_value("lca", value)

    @property
    def lcb(self) -> int:
        """Get or set the Load curve ID, see *DEFINE_CURVE,
        LCA<0:Strong axis hardening stress as a function of the volumetric strain.
        LCA>0:sigma-bb versus normal strain component bb. For the cor otational solid elements, types 0 and 9, engineering strain is expected, but for all other solid element formulations a logarithmic strain is expected(default: LCB=LCA).
        """ # nopep8
        return self._cards[1].get_value("lcb")

    @lcb.setter
    def lcb(self, value: int) -> None:
        self._cards[1].set_value("lcb", value)

    @property
    def lcc(self) -> int:
        """Get or set the Load curve ID, see *DEFINE_CURVE,
        LCA<0:Weak axis hardening stress as a function of the volumetric strain.
        LCA>0:sigma-cc versus normal strain component cc. For the cor otational solid elements, types 0 and 9, engineering strain is expected, but for all other solid element formulations a logarithmic strain is expected(default: LCC=LCA).
        """ # nopep8
        return self._cards[1].get_value("lcc")

    @lcc.setter
    def lcc(self, value: int) -> None:
        self._cards[1].set_value("lcc", value)

    @property
    def lcs(self) -> int:
        """Get or set the Load curve ID, see *DEFINE_CURVE,
        LCA<0:Damage curve giving shear stress multiplier as function of the shear strain component. This curve definition is optional and may be used if damage is desired. if SHDFLG=0 (the dafault), the damage value multiplies the stress every time step and the stress is updated incrementally. The damage curve should be set to unity until failure begins. After failure the value should drop to 0.999 or 0.99 or any number between zero and one depending on how many steps are needed to zero the stress. Alternatively, if SHDFLG=1.0, the damage value is treated as a factor that scales the shear stress component to the undamaged value.
        LCA>0:shear stress versus shear strain. For the corotational solid elements, types 0 and 9, engineering strain is expected, but for all other solid element formulations a shear strain based on the deformed configuration is used. (default LCS=LCA). Each component of shear stress may have its own load curve.
        """ # nopep8
        return self._cards[1].get_value("lcs")

    @lcs.setter
    def lcs(self, value: int) -> None:
        self._cards[1].set_value("lcs", value)

    @property
    def lcab(self) -> int:
        """Get or set the Load curve ID, see *DEFINE_CURVE,
        LCA<0:Damage curve giving ab-stress multiplier as a function of the ab-shear strain component. This curve definition is optional and may be used if damage is desired. See LCS above.
        LCA>0:sigma-ab versus shear strain-ab. For the corotational solid elements, types 0 and 9, engineering strain is expected, but for all other solid element formulations a shear strain based on the deformed configuration is used. (default LCAB=LCS).
        """ # nopep8
        return self._cards[1].get_value("lcab")

    @lcab.setter
    def lcab(self, value: int) -> None:
        self._cards[1].set_value("lcab", value)

    @property
    def lcbc(self) -> int:
        """Get or set the Load curve ID, see *DEFINE_CURVE,
        LCA<0:Damage curve giving bc-shear stress multiplier as a function of the bc-shear strain component. This curve definition is optional and may be used if damage is desired. See LCS above.
        LCA>0:sigma-bc versus shear strain-bc. For the corotational solid elements, types 0 and 9, engineering strain is expected, but for all other solid element formulations a shear strain based on the deformed configuration is used. (default LCBC=LCS).
        """ # nopep8
        return self._cards[1].get_value("lcbc")

    @lcbc.setter
    def lcbc(self, value: int) -> None:
        self._cards[1].set_value("lcbc", value)

    @property
    def lcca(self) -> int:
        """Get or set the Load curve ID, see *DEFINE_CURVE,
        LCA<0:Damage curve giving ca-shear stress multiplier as a function of the ca-shear strain component. This curve definition is optional and may be used if damage is desired. See LCS above.
        LCA>0:sigma-ca versus shear strain-ca. For the corotational solid elements, types 0 and 9, engineering strain is expected, but for all other solid element formulations a shear strain based on the deformed configuration is used. (default LCCA=LCS).
        """ # nopep8
        return self._cards[1].get_value("lcca")

    @lcca.setter
    def lcca(self, value: int) -> None:
        self._cards[1].set_value("lcca", value)

    @property
    def lcsr(self) -> int:
        """Get or set the Load curve ID, see *DEFINE_CURVE, for strain-rate effects defining the scale factor versus strain rate (optional).The curves defined above are scaled using this curve.
        """ # nopep8
        return self._cards[1].get_value("lcsr")

    @lcsr.setter
    def lcsr(self, value: int) -> None:
        self._cards[1].set_value("lcsr", value)

    @property
    def eaau(self) -> typing.Optional[float]:
        """Get or set the Elastic modulus Eaau in uncompressed configuration.
        """ # nopep8
        return self._cards[2].get_value("eaau")

    @eaau.setter
    def eaau(self, value: float) -> None:
        self._cards[2].set_value("eaau", value)

    @property
    def ebbu(self) -> typing.Optional[float]:
        """Get or set the Elastic modulus Ebbu in uncompressed configuration.
        """ # nopep8
        return self._cards[2].get_value("ebbu")

    @ebbu.setter
    def ebbu(self, value: float) -> None:
        self._cards[2].set_value("ebbu", value)

    @property
    def eccu(self) -> typing.Optional[float]:
        """Get or set the Elastic modulus Eccu in uncompressed configuration.
        """ # nopep8
        return self._cards[2].get_value("eccu")

    @eccu.setter
    def eccu(self, value: float) -> None:
        self._cards[2].set_value("eccu", value)

    @property
    def gabu(self) -> typing.Optional[float]:
        """Get or set the Shear modulus Gabu in uncompressed configuration.
        """ # nopep8
        return self._cards[2].get_value("gabu")

    @gabu.setter
    def gabu(self, value: float) -> None:
        self._cards[2].set_value("gabu", value)

    @property
    def gbcu(self) -> typing.Optional[float]:
        """Get or set the Shear modulus Gbcu in uncompressed configuration.
        """ # nopep8
        return self._cards[2].get_value("gbcu")

    @gbcu.setter
    def gbcu(self, value: float) -> None:
        self._cards[2].set_value("gbcu", value)

    @property
    def gcau(self) -> typing.Optional[float]:
        """Get or set the Shear modulus Gcau in uncompressed configuration.
        """ # nopep8
        return self._cards[2].get_value("gcau")

    @gcau.setter
    def gcau(self, value: float) -> None:
        self._cards[2].set_value("gcau", value)

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
        return self._cards[2].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[2].set_value("aopt", value)

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
        return self._cards[2].get_value("macf")

    @macf.setter
    def macf(self, value: int) -> None:
        if value not in [1, 2, 3, 4, -4, -3, -2]:
            raise Exception("""macf must be one of {1,2,3,4,-4,-3,-2}""")
        self._cards[2].set_value("macf", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the x-coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[3].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[3].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the y-coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[3].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[3].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the z-coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[3].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[3].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[3].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[3].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[3].set_value("a3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[4].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[4].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[4].set_value("d3", value)

    @property
    def tsef(self) -> typing.Optional[float]:
        """Get or set the Tensile strain at element failure (element will erode).
        """ # nopep8
        return self._cards[4].get_value("tsef")

    @tsef.setter
    def tsef(self, value: float) -> None:
        self._cards[4].set_value("tsef", value)

    @property
    def ssef(self) -> typing.Optional[float]:
        """Get or set the Shear strain at element failure (element will erode).
        """ # nopep8
        return self._cards[4].get_value("ssef")

    @ssef.setter
    def ssef(self, value: float) -> None:
        self._cards[4].set_value("ssef", value)

    @property
    def vref(self) -> typing.Optional[float]:
        """Get or set the This is an optional input parameter for solid elements types 1, 2, 3, 4, and 10. Relative volume at which the reference geometry is stored. At this time the element behaves like a nonlinear spring. The TREF, below, is reached first then VREF will have no effect.
        """ # nopep8
        return self._cards[4].get_value("vref")

    @vref.setter
    def vref(self, value: float) -> None:
        self._cards[4].set_value("vref", value)

    @property
    def tref(self) -> typing.Optional[float]:
        """Get or set the This is an optional input parameter for solid elements types 1, 2, 3, 4, and 10. Element time step size at which the reference geometry is stored. When this time step size is reached the element behaves like a nonlinear spring. If VREF, above, is reached first then TREF will have no effect.
        """ # nopep8
        return self._cards[4].get_value("tref")

    @tref.setter
    def tref(self, value: float) -> None:
        self._cards[4].set_value("tref", value)

    @property
    def shdflg(self) -> float:
        """Get or set the Flag defining treatment of damage from curves LCS, LCAB, LCBC and LCCA (relevant only when LCA<0)
        EQ.0.0: Damage reduces shear stress every time step,
        EQ.1.0: Damage=(shear stress)/(undsamged shear stress)
        """ # nopep8
        return self._cards[4].get_value("shdflg")

    @shdflg.setter
    def shdflg(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""shdflg must be one of {0.0,1.0}""")
        self._cards[4].set_value("shdflg", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[5].cards[0].set_value("title", value)

