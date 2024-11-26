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

class MatCompositeMscDmg(KeywordBase):
    """DYNA MAT_COMPOSITE_MSC_DMG keyword"""

    keyword = "MAT"
    subkeyword = "COMPOSITE_MSC_DMG"
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
                        "ea",
                        float,
                        20,
                        10,
                        kwargs.get("ea")
                    ),
                    Field(
                        "eb",
                        float,
                        30,
                        10,
                        kwargs.get("eb")
                    ),
                    Field(
                        "ec",
                        float,
                        40,
                        10,
                        kwargs.get("ec")
                    ),
                    Field(
                        "prba",
                        float,
                        50,
                        10,
                        kwargs.get("prba")
                    ),
                    Field(
                        "prca",
                        float,
                        60,
                        10,
                        kwargs.get("prca")
                    ),
                    Field(
                        "prcb",
                        float,
                        70,
                        10,
                        kwargs.get("prcb")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "gab",
                        float,
                        0,
                        10,
                        kwargs.get("gab")
                    ),
                    Field(
                        "gbc",
                        float,
                        10,
                        10,
                        kwargs.get("gbc")
                    ),
                    Field(
                        "gca",
                        float,
                        20,
                        10,
                        kwargs.get("gca")
                    ),
                    Field(
                        "aopt",
                        float,
                        30,
                        10,
                        kwargs.get("aopt")
                    ),
                    Field(
                        "macf",
                        int,
                        40,
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
            Card(
                [
                    Field(
                        "sat",
                        float,
                        0,
                        10,
                        kwargs.get("sat")
                    ),
                    Field(
                        "sac",
                        float,
                        10,
                        10,
                        kwargs.get("sac")
                    ),
                    Field(
                        "sbt",
                        float,
                        20,
                        10,
                        kwargs.get("sbt")
                    ),
                    Field(
                        "sbc",
                        float,
                        30,
                        10,
                        kwargs.get("sbc")
                    ),
                    Field(
                        "sct",
                        float,
                        40,
                        10,
                        kwargs.get("sct")
                    ),
                    Field(
                        "sfc",
                        float,
                        50,
                        10,
                        kwargs.get("sfc")
                    ),
                    Field(
                        "sfs",
                        float,
                        60,
                        10,
                        kwargs.get("sfs")
                    ),
                    Field(
                        "sab",
                        float,
                        70,
                        10,
                        kwargs.get("sab")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sbc",
                        float,
                        0,
                        10,
                        kwargs.get("sbc")
                    ),
                    Field(
                        "sca",
                        float,
                        10,
                        10,
                        kwargs.get("sca")
                    ),
                    Field(
                        "sffc",
                        float,
                        20,
                        10,
                        kwargs.get("sffc")
                    ),
                    Field(
                        "amodel",
                        int,
                        30,
                        10,
                        kwargs.get("amodel", 1)
                    ),
                    Field(
                        "phic",
                        float,
                        40,
                        10,
                        kwargs.get("phic")
                    ),
                    Field(
                        "e_limt",
                        float,
                        50,
                        10,
                        kwargs.get("e_limt")
                    ),
                    Field(
                        "s_delm",
                        float,
                        60,
                        10,
                        kwargs.get("s_delm")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "omgmx",
                        float,
                        0,
                        10,
                        kwargs.get("omgmx")
                    ),
                    Field(
                        "ecrsh",
                        float,
                        10,
                        10,
                        kwargs.get("ecrsh")
                    ),
                    Field(
                        "eexpn",
                        float,
                        20,
                        10,
                        kwargs.get("eexpn")
                    ),
                    Field(
                        "cerate1",
                        float,
                        30,
                        10,
                        kwargs.get("cerate1")
                    ),
                    Field(
                        "am1",
                        float,
                        40,
                        10,
                        kwargs.get("am1")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "am2",
                        float,
                        0,
                        10,
                        kwargs.get("am2")
                    ),
                    Field(
                        "am3",
                        float,
                        10,
                        10,
                        kwargs.get("am3")
                    ),
                    Field(
                        "am4",
                        float,
                        20,
                        10,
                        kwargs.get("am4")
                    ),
                    Field(
                        "cerate2",
                        float,
                        30,
                        10,
                        kwargs.get("cerate2")
                    ),
                    Field(
                        "cerate3",
                        float,
                        40,
                        10,
                        kwargs.get("cerate3")
                    ),
                    Field(
                        "cerate4",
                        float,
                        50,
                        10,
                        kwargs.get("cerate4")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatCompositeMscDmg.option_specs[0],
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
    def ea(self) -> typing.Optional[float]:
        """Get or set the Ea, Young's modulus - longitudinal direction
        """ # nopep8
        return self._cards[0].get_value("ea")

    @ea.setter
    def ea(self, value: float) -> None:
        self._cards[0].set_value("ea", value)

    @property
    def eb(self) -> typing.Optional[float]:
        """Get or set the Eb, Young's modulus - transverse direction
        """ # nopep8
        return self._cards[0].get_value("eb")

    @eb.setter
    def eb(self, value: float) -> None:
        self._cards[0].set_value("eb", value)

    @property
    def ec(self) -> typing.Optional[float]:
        """Get or set the Ec, Young's modulus - through thickness direction
        """ # nopep8
        return self._cards[0].get_value("ec")

    @ec.setter
    def ec(self, value: float) -> None:
        self._cards[0].set_value("ec", value)

    @property
    def prba(self) -> typing.Optional[float]:
        """Get or set the Vba , Poisson's ratio ba
        """ # nopep8
        return self._cards[0].get_value("prba")

    @prba.setter
    def prba(self, value: float) -> None:
        self._cards[0].set_value("prba", value)

    @property
    def prca(self) -> typing.Optional[float]:
        """Get or set the Vca , Poisson's ratio ca
        """ # nopep8
        return self._cards[0].get_value("prca")

    @prca.setter
    def prca(self, value: float) -> None:
        self._cards[0].set_value("prca", value)

    @property
    def prcb(self) -> typing.Optional[float]:
        """Get or set the Vab , Poisson's ratio cb
        """ # nopep8
        return self._cards[0].get_value("prcb")

    @prcb.setter
    def prcb(self, value: float) -> None:
        self._cards[0].set_value("prcb", value)

    @property
    def gab(self) -> typing.Optional[float]:
        """Get or set the Gab, shear modulus ab
        """ # nopep8
        return self._cards[1].get_value("gab")

    @gab.setter
    def gab(self, value: float) -> None:
        self._cards[1].set_value("gab", value)

    @property
    def gbc(self) -> typing.Optional[float]:
        """Get or set the Gbc, shear modulus bc
        """ # nopep8
        return self._cards[1].get_value("gbc")

    @gbc.setter
    def gbc(self, value: float) -> None:
        self._cards[1].set_value("gbc", value)

    @property
    def gca(self) -> typing.Optional[float]:
        """Get or set the Gca, shear modulus ca
        """ # nopep8
        return self._cards[1].get_value("gca")

    @gca.setter
    def gca(self, value: float) -> None:
        self._cards[1].set_value("gca", value)

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
        return self._cards[1].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[1].set_value("aopt", value)

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
        return self._cards[1].get_value("macf")

    @macf.setter
    def macf(self, value: int) -> None:
        if value not in [1, 2, 3, 4, -4, -3, -2]:
            raise Exception("""macf must be one of {1,2,3,4,-4,-3,-2}""")
        self._cards[1].set_value("macf", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Define X coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[2].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[2].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Define Y coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[2].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[2].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Define Z coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[2].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[2].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Define x-component of vector A for AOPT = 2.
        """ # nopep8
        return self._cards[2].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[2].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Define y-component of vector A for AOPT = 2.
        """ # nopep8
        return self._cards[2].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[2].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Define z-component of vector A for AOPT = 2.
        """ # nopep8
        return self._cards[2].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[2].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Define x-component of vector V for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[3].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Define y-component of vector V for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[3].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Define z-component of vector V for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[3].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Define x-component of vector D for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[3].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Define y-component of vector D for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[3].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Define z-component of vector D for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[3].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Layer in-plane rotational angle in degrees.
        """ # nopep8
        return self._cards[3].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[3].set_value("beta", value)

    @property
    def sat(self) -> typing.Optional[float]:
        """Get or set the Longitudinal tensile strength
        """ # nopep8
        return self._cards[4].get_value("sat")

    @sat.setter
    def sat(self, value: float) -> None:
        self._cards[4].set_value("sat", value)

    @property
    def sac(self) -> typing.Optional[float]:
        """Get or set the Longitudinal compressive strength
        """ # nopep8
        return self._cards[4].get_value("sac")

    @sac.setter
    def sac(self, value: float) -> None:
        self._cards[4].set_value("sac", value)

    @property
    def sbt(self) -> typing.Optional[float]:
        """Get or set the Transverse tensile strength
        """ # nopep8
        return self._cards[4].get_value("sbt")

    @sbt.setter
    def sbt(self, value: float) -> None:
        self._cards[4].set_value("sbt", value)

    @property
    def sbc(self) -> typing.Optional[float]:
        """Get or set the Transverse compressive strength
        """ # nopep8
        return self._cards[4].get_value("sbc")

    @sbc.setter
    def sbc(self, value: float) -> None:
        self._cards[4].set_value("sbc", value)

    @property
    def sct(self) -> typing.Optional[float]:
        """Get or set the Through thickness tensile strength
        """ # nopep8
        return self._cards[4].get_value("sct")

    @sct.setter
    def sct(self, value: float) -> None:
        self._cards[4].set_value("sct", value)

    @property
    def sfc(self) -> typing.Optional[float]:
        """Get or set the Crush strength
        """ # nopep8
        return self._cards[4].get_value("sfc")

    @sfc.setter
    def sfc(self, value: float) -> None:
        self._cards[4].set_value("sfc", value)

    @property
    def sfs(self) -> typing.Optional[float]:
        """Get or set the Fiber mode shear strength
        """ # nopep8
        return self._cards[4].get_value("sfs")

    @sfs.setter
    def sfs(self, value: float) -> None:
        self._cards[4].set_value("sfs", value)

    @property
    def sab(self) -> typing.Optional[float]:
        """Get or set the Matrix mode shear strength, ab plane, see below.
        """ # nopep8
        return self._cards[4].get_value("sab")

    @sab.setter
    def sab(self, value: float) -> None:
        self._cards[4].set_value("sab", value)

    @property
    def sbc(self) -> typing.Optional[float]:
        """Get or set the Matrix mode shear strength, bc plane, see below.
        """ # nopep8
        return self._cards[5].get_value("sbc")

    @sbc.setter
    def sbc(self, value: float) -> None:
        self._cards[5].set_value("sbc", value)

    @property
    def sca(self) -> typing.Optional[float]:
        """Get or set the Matrix mode shear strength, ca plane, see below.
        """ # nopep8
        return self._cards[5].get_value("sca")

    @sca.setter
    def sca(self, value: float) -> None:
        self._cards[5].set_value("sca", value)

    @property
    def sffc(self) -> typing.Optional[float]:
        """Get or set the Scale factor for residual compressive strength
        """ # nopep8
        return self._cards[5].get_value("sffc")

    @sffc.setter
    def sffc(self, value: float) -> None:
        self._cards[5].set_value("sffc", value)

    @property
    def amodel(self) -> int:
        """Get or set the Material models:
        EQ. 1: Unidirectional layer model
        EQ. 2: Fabric layer model
        """ # nopep8
        return self._cards[5].get_value("amodel")

    @amodel.setter
    def amodel(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""amodel must be one of {1,2}""")
        self._cards[5].set_value("amodel", value)

    @property
    def phic(self) -> typing.Optional[float]:
        """Get or set the Coulomb friction angle for matrix and delamination failure
        """ # nopep8
        return self._cards[5].get_value("phic")

    @phic.setter
    def phic(self, value: float) -> None:
        self._cards[5].set_value("phic", value)

    @property
    def e_limt(self) -> typing.Optional[float]:
        """Get or set the Element eroding axial strain
        """ # nopep8
        return self._cards[5].get_value("e_limt")

    @e_limt.setter
    def e_limt(self, value: float) -> None:
        self._cards[5].set_value("e_limt", value)

    @property
    def s_delm(self) -> typing.Optional[float]:
        """Get or set the Scale factor for delamination criterion
        """ # nopep8
        return self._cards[5].get_value("s_delm")

    @s_delm.setter
    def s_delm(self, value: float) -> None:
        self._cards[5].set_value("s_delm", value)

    @property
    def omgmx(self) -> typing.Optional[float]:
        """Get or set the Limit damage parameter for elastic modulus reduction
        """ # nopep8
        return self._cards[6].get_value("omgmx")

    @omgmx.setter
    def omgmx(self, value: float) -> None:
        self._cards[6].set_value("omgmx", value)

    @property
    def ecrsh(self) -> typing.Optional[float]:
        """Get or set the Limit compressive volume strain for element eroding
        """ # nopep8
        return self._cards[6].get_value("ecrsh")

    @ecrsh.setter
    def ecrsh(self, value: float) -> None:
        self._cards[6].set_value("ecrsh", value)

    @property
    def eexpn(self) -> typing.Optional[float]:
        """Get or set the Limit tensile volume strain for element eroding
        """ # nopep8
        return self._cards[6].get_value("eexpn")

    @eexpn.setter
    def eexpn(self, value: float) -> None:
        self._cards[6].set_value("eexpn", value)

    @property
    def cerate1(self) -> typing.Optional[float]:
        """Get or set the Coefficient for strain rate dependent strength properties.
        """ # nopep8
        return self._cards[6].get_value("cerate1")

    @cerate1.setter
    def cerate1(self, value: float) -> None:
        self._cards[6].set_value("cerate1", value)

    @property
    def am1(self) -> typing.Optional[float]:
        """Get or set the Coefficient for strain rate softening property for fiber damage in a direction.
        """ # nopep8
        return self._cards[6].get_value("am1")

    @am1.setter
    def am1(self, value: float) -> None:
        self._cards[6].set_value("am1", value)

    @property
    def am2(self) -> typing.Optional[float]:
        """Get or set the Coefficient for strain rate softening property for fiber damage in b direction.
        """ # nopep8
        return self._cards[7].get_value("am2")

    @am2.setter
    def am2(self, value: float) -> None:
        self._cards[7].set_value("am2", value)

    @property
    def am3(self) -> typing.Optional[float]:
        """Get or set the Coefficient for strain rate softening property for fiber crush and punch shear damage.
        """ # nopep8
        return self._cards[7].get_value("am3")

    @am3.setter
    def am3(self, value: float) -> None:
        self._cards[7].set_value("am3", value)

    @property
    def am4(self) -> typing.Optional[float]:
        """Get or set the Coefficient for strain rate softening property for matrix and delamination damage.
        """ # nopep8
        return self._cards[7].get_value("am4")

    @am4.setter
    def am4(self, value: float) -> None:
        self._cards[7].set_value("am4", value)

    @property
    def cerate2(self) -> typing.Optional[float]:
        """Get or set the Coefficient for strain rate dependent axial moduli.
        """ # nopep8
        return self._cards[7].get_value("cerate2")

    @cerate2.setter
    def cerate2(self, value: float) -> None:
        self._cards[7].set_value("cerate2", value)

    @property
    def cerate3(self) -> typing.Optional[float]:
        """Get or set the Coefficient for strain rate dependent shear moduli.
        """ # nopep8
        return self._cards[7].get_value("cerate3")

    @cerate3.setter
    def cerate3(self, value: float) -> None:
        self._cards[7].set_value("cerate3", value)

    @property
    def cerate4(self) -> typing.Optional[float]:
        """Get or set the Coefficient for strain rate dependent transverse moduli.
        """ # nopep8
        return self._cards[7].get_value("cerate4")

    @cerate4.setter
    def cerate4(self, value: float) -> None:
        self._cards[7].set_value("cerate4", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[8].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[8].cards[0].set_value("title", value)

