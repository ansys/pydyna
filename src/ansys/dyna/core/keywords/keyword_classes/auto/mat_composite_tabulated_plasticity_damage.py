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

class MatCompositeTabulatedPlasticityDamage(KeywordBase):
    """DYNA MAT_COMPOSITE_TABULATED_PLASTICITY_DAMAGE keyword"""

    keyword = "MAT"
    subkeyword = "COMPOSITE_TABULATED_PLASTICITY_DAMAGE"
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
                        "ptol",
                        float,
                        30,
                        10,
                        kwargs.get("ptol", 10e-6)
                    ),
                    Field(
                        "aopt",
                        float,
                        40,
                        10,
                        kwargs.get("aopt", 0.0)
                    ),
                    Field(
                        "macf",
                        int,
                        50,
                        10,
                        kwargs.get("macf", 1)
                    ),
                    Field(
                        "filt",
                        float,
                        60,
                        10,
                        kwargs.get("filt", 0.0)
                    ),
                    Field(
                        "vevp",
                        int,
                        70,
                        10,
                        kwargs.get("vevp", 0)
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
                        kwargs.get("d3", 0)
                    ),
                    Field(
                        "beta",
                        float,
                        60,
                        10,
                        kwargs.get("beta", 0)
                    ),
                    Field(
                        "tcsym",
                        int,
                        70,
                        10,
                        kwargs.get("tcsym", 0)
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
                    Field(
                        "h44",
                        float,
                        60,
                        10,
                        kwargs.get("h44", 3.0)
                    ),
                    Field(
                        "h55",
                        float,
                        70,
                        10,
                        kwargs.get("h55", 3.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "h66",
                        float,
                        0,
                        10,
                        kwargs.get("h66", 3.0)
                    ),
                    Field(
                        "lt1",
                        int,
                        10,
                        10,
                        kwargs.get("lt1")
                    ),
                    Field(
                        "lt2",
                        int,
                        20,
                        10,
                        kwargs.get("lt2")
                    ),
                    Field(
                        "lt3",
                        int,
                        30,
                        10,
                        kwargs.get("lt3")
                    ),
                    Field(
                        "lt4",
                        int,
                        40,
                        10,
                        kwargs.get("lt4")
                    ),
                    Field(
                        "lt5",
                        int,
                        50,
                        10,
                        kwargs.get("lt5")
                    ),
                    Field(
                        "lt6",
                        int,
                        60,
                        10,
                        kwargs.get("lt6")
                    ),
                    Field(
                        "lt7",
                        int,
                        70,
                        10,
                        kwargs.get("lt7")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lt8",
                        int,
                        0,
                        10,
                        kwargs.get("lt8")
                    ),
                    Field(
                        "lt9",
                        int,
                        10,
                        10,
                        kwargs.get("lt9")
                    ),
                    Field(
                        "lt10",
                        int,
                        20,
                        10,
                        kwargs.get("lt10")
                    ),
                    Field(
                        "lt11",
                        int,
                        30,
                        10,
                        kwargs.get("lt11")
                    ),
                    Field(
                        "lt12",
                        int,
                        40,
                        10,
                        kwargs.get("lt12")
                    ),
                    Field(
                        "ysc",
                        int,
                        50,
                        10,
                        kwargs.get("ysc")
                    ),
                    Field(
                        "dflag",
                        int,
                        60,
                        10,
                        kwargs.get("dflag", 0)
                    ),
                    Field(
                        "dc",
                        int,
                        70,
                        10,
                        kwargs.get("dc")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ftype",
                        int,
                        0,
                        10,
                        kwargs.get("ftype", 0)
                    ),
                    Field(
                        "unused",
                        int,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
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
                        "unused",
                        int,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        70,
                        10,
                        kwargs.get("unused")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "unused",
                        int,
                        0,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
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
                        "unused",
                        int,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        70,
                        10,
                        kwargs.get("unused")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "beta11",
                        float,
                        0,
                        10,
                        kwargs.get("beta11", 0.001)
                    ),
                    Field(
                        "beta22",
                        float,
                        10,
                        10,
                        kwargs.get("beta22", 0.001)
                    ),
                    Field(
                        "beta33",
                        float,
                        20,
                        10,
                        kwargs.get("beta33", 0.001)
                    ),
                    Field(
                        "beta44",
                        float,
                        30,
                        10,
                        kwargs.get("beta44", 0.001)
                    ),
                    Field(
                        "beta55",
                        float,
                        40,
                        10,
                        kwargs.get("beta55", 0.001)
                    ),
                    Field(
                        "beta66",
                        float,
                        50,
                        10,
                        kwargs.get("beta66", 0.001)
                    ),
                    Field(
                        "beta12",
                        float,
                        60,
                        10,
                        kwargs.get("beta12")
                    ),
                    Field(
                        "beta23",
                        float,
                        70,
                        10,
                        kwargs.get("beta23")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "beta13",
                        float,
                        0,
                        10,
                        kwargs.get("beta13")
                    ),
                    Field(
                        "cp",
                        float,
                        10,
                        10,
                        kwargs.get("cp")
                    ),
                    Field(
                        "tqc",
                        float,
                        20,
                        10,
                        kwargs.get("tqc")
                    ),
                    Field(
                        "temp",
                        float,
                        30,
                        10,
                        kwargs.get("temp")
                    ),
                    Field(
                        "pmacc",
                        float,
                        40,
                        10,
                        kwargs.get("pmacc")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatCompositeTabulatedPlasticityDamage.option_specs[0],
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
        """Get or set the Material identification. A unique number or label must be specified.
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
        """Get or set the Ea, Young's modulus in a-direction.
        """ # nopep8
        return self._cards[0].get_value("ea")

    @ea.setter
    def ea(self, value: float) -> None:
        self._cards[0].set_value("ea", value)

    @property
    def eb(self) -> typing.Optional[float]:
        """Get or set the Ea, Young's modulus in b-direction.
        """ # nopep8
        return self._cards[0].get_value("eb")

    @eb.setter
    def eb(self, value: float) -> None:
        self._cards[0].set_value("eb", value)

    @property
    def ec(self) -> typing.Optional[float]:
        """Get or set the Ea, Young's modulus in c-direction.
        """ # nopep8
        return self._cards[0].get_value("ec")

    @ec.setter
    def ec(self, value: float) -> None:
        self._cards[0].set_value("ec", value)

    @property
    def prba(self) -> typing.Optional[float]:
        """Get or set the (elastic) Poisson's ratio, ba.
        """ # nopep8
        return self._cards[0].get_value("prba")

    @prba.setter
    def prba(self, value: float) -> None:
        self._cards[0].set_value("prba", value)

    @property
    def prca(self) -> typing.Optional[float]:
        """Get or set the (elastic) Poisson's ratio, ca.
        """ # nopep8
        return self._cards[0].get_value("prca")

    @prca.setter
    def prca(self, value: float) -> None:
        self._cards[0].set_value("prca", value)

    @property
    def prcb(self) -> typing.Optional[float]:
        """Get or set the (elastic) Poisson's ratio, cb.
        """ # nopep8
        return self._cards[0].get_value("prcb")

    @prcb.setter
    def prcb(self, value: float) -> None:
        self._cards[0].set_value("prcb", value)

    @property
    def gab(self) -> typing.Optional[float]:
        """Get or set the Shear modulus, ab.
        """ # nopep8
        return self._cards[1].get_value("gab")

    @gab.setter
    def gab(self, value: float) -> None:
        self._cards[1].set_value("gab", value)

    @property
    def gbc(self) -> typing.Optional[float]:
        """Get or set the Shear modulus, bc.
        """ # nopep8
        return self._cards[1].get_value("gbc")

    @gbc.setter
    def gbc(self, value: float) -> None:
        self._cards[1].set_value("gbc", value)

    @property
    def gca(self) -> typing.Optional[float]:
        """Get or set the Shear modulus, ca.
        """ # nopep8
        return self._cards[1].get_value("gca")

    @gca.setter
    def gca(self, value: float) -> None:
        self._cards[1].set_value("gca", value)

    @property
    def ptol(self) -> float:
        """Get or set the Yield function tolerance used during plastic multiplier calculations.
        """ # nopep8
        return self._cards[1].get_value("ptol")

    @ptol.setter
    def ptol(self, value: float) -> None:
        self._cards[1].set_value("ptol", value)

    @property
    def aopt(self) -> float:
        """Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC for a more complete description):
        EQ.0.0:	Locally orthotropic with material axes determined by element nodes.For shells only, the material axes are then rotated about the normal vector to the surface of the shell by the angle BETA.
        EQ.1.0 : Locally orthotropic with material axes determined by a point, P, in spaceand the global location of the element center.This option is for solid elements only.
        EQ.2.0 : Globally orthotropic with material axes determined by vectors defined below
        EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element.The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.Thus, for solid elements, AOPT = 3 is only available for hexahedrons.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle BETA.BETA may be set in the keyword input for the element or in the input for this keyword.Note that for solids, the material axes may be switched depending on the choice of MACF.The switch may occur before or after applying BETA depending on the value of MACF.
        EQ.4.0 : Locally orthotropic in cylindrical coordinate system with the material axes determined by a vector, v,and an originating point, P, defining the centerline axis.This option is for solid elements only.
        LT.0.0 : The absolute value of AOPT is a coordinate system ID number(CID on * DEFINE_COORDINATE_NODES, *DEFINE_COORDINATE_SYSTEM or *DEFINE - _COORDINATE_VECTOR).
        """ # nopep8
        return self._cards[1].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[1].set_value("aopt", value)

    @property
    def macf(self) -> int:
        """Get or set the Material axes change flag for brick elements:

        EQ. - 4:	Switch material axes b and c before BETA rotation
        EQ. - 3 : Switch material axes a and c before BETA rotation
        EQ. - 2 : Switch material axes a and b before BETA rotation
        EQ.1 : No change, default
        EQ.2 : Switch material axes a and b after BETA rotation
        EQ.3 : Switch material axes a and c after BETA rotation
        EQ.4 : Switch material axes b and c after BETA rotation
        Figure Error!Reference source not found.indicates when LS - DYNA applies MACF during the process to obtain the final material axes.If BETA on * ELEMENT_SOLID_{ OPTION } is defined, then that BETA is used for the rotation for all AOPT options.Otherwise, if AOPT‌ = 3, the BETA input on Card 3 rotates the axes.For all other values of AOPT, the material axes will be switched as specified by MACF, but no BETA rotation will be performed.
        """ # nopep8
        return self._cards[1].get_value("macf")

    @macf.setter
    def macf(self, value: int) -> None:
        if value not in [1, 2, 3, 4, -2, -3, -4]:
            raise Exception("""macf must be one of {1,2,3,4,-2,-3,-4}""")
        self._cards[1].set_value("macf", value)

    @property
    def filt(self) -> float:
        """Get or set the Factor for strain rate filtering
        """ # nopep8
        return self._cards[1].get_value("filt")

    @filt.setter
    def filt(self, value: float) -> None:
        self._cards[1].set_value("filt", value)

    @property
    def vevp(self) -> int:
        """Get or set the Flag to control viscoelastic, viscoplastic behavior:
        EQ.0:	Viscoplastic only with no rate effects in elastic region(default)
        EQ.1 : Viscoelastic, viscoplastic
        EQ.2 : Viscoelastic only.
        """ # nopep8
        return self._cards[1].get_value("vevp")

    @vevp.setter
    def vevp(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""vevp must be one of {0,1,2}""")
        self._cards[1].set_value("vevp", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[2].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[2].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[2].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[2].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[2].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[2].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[2].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[2].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[2].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[2].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[2].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[2].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[3].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[3].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[3].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[3].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[3].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[3].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[3].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[3].set_value("d2", value)

    @property
    def d3(self) -> float:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[3].set_value("d3", value)

    @property
    def beta(self) -> float:
        """Get or set the Angle in degrees of a material rotation about the c-axis, available for AOPT = 0 (shells only) and AOPT = 3 (all element types).  This angle may be overridden on the element card; see *ELEMENT_‌SHELL_‌BETA and *ELEMENT_‌SOLID_‌ORTHO.
        """ # nopep8
        return self._cards[3].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[3].set_value("beta", value)

    @property
    def tcsym(self) -> int:
        """Get or set the Flag for handling tension-compression asymmetry in all three material directions:
        EQ.0:	Do not adjust user - defined data.
        EQ.1 : Compute and use average of tension and compression elastic moduli in adjusting the stress - strain curve.See Remark 7.
        EQ.2 : Use compression modulus as user - defined tension modulus in adjusting the stress - strain curve.See Remark 7.
        EQ.3 : Use tension modulus as user - defined compression modulus in adjusting the stress - strain curve.See Remark 7.
        EQ.4 : Use user - defined tensile curve as the compressive curve overriding the user - defined compressive curve.This implies that the normal stress - strain curves are symmetric including yield values.
        EQ.5 : Use user - defined compressive curve as the tensile curve overriding the user - defined tensile curve.This implies that the normal stress - strain curves are symmetric including yield values.
        """ # nopep8
        return self._cards[3].get_value("tcsym")

    @tcsym.setter
    def tcsym(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5]:
            raise Exception("""tcsym must be one of {0,1,2,3,4,5}""")
        self._cards[3].set_value("tcsym", value)

    @property
    def h11(self) -> typing.Optional[float]:
        """Get or set the Plastic flow rule coefficients. See Remark (1).
        """ # nopep8
        return self._cards[4].get_value("h11")

    @h11.setter
    def h11(self, value: float) -> None:
        self._cards[4].set_value("h11", value)

    @property
    def h22(self) -> typing.Optional[float]:
        """Get or set the Plastic flow rule coefficients. See Remark (1).
        """ # nopep8
        return self._cards[4].get_value("h22")

    @h22.setter
    def h22(self, value: float) -> None:
        self._cards[4].set_value("h22", value)

    @property
    def h33(self) -> typing.Optional[float]:
        """Get or set the Plastic flow rule coefficients. See Remark (1).
        """ # nopep8
        return self._cards[4].get_value("h33")

    @h33.setter
    def h33(self, value: float) -> None:
        self._cards[4].set_value("h33", value)

    @property
    def h12(self) -> typing.Optional[float]:
        """Get or set the Plastic flow rule coefficients. See Remark (1).
        """ # nopep8
        return self._cards[4].get_value("h12")

    @h12.setter
    def h12(self, value: float) -> None:
        self._cards[4].set_value("h12", value)

    @property
    def h23(self) -> typing.Optional[float]:
        """Get or set the Plastic flow rule coefficients. See Remark (1).
        """ # nopep8
        return self._cards[4].get_value("h23")

    @h23.setter
    def h23(self, value: float) -> None:
        self._cards[4].set_value("h23", value)

    @property
    def h13(self) -> typing.Optional[float]:
        """Get or set the Plastic flow rule coefficients. See Remark (1).
        """ # nopep8
        return self._cards[4].get_value("h13")

    @h13.setter
    def h13(self, value: float) -> None:
        self._cards[4].set_value("h13", value)

    @property
    def h44(self) -> float:
        """Get or set the Plastic flow rule coefficients. See Remark (1).
        """ # nopep8
        return self._cards[4].get_value("h44")

    @h44.setter
    def h44(self, value: float) -> None:
        self._cards[4].set_value("h44", value)

    @property
    def h55(self) -> float:
        """Get or set the Plastic flow rule coefficients. See Remark (1).
        """ # nopep8
        return self._cards[4].get_value("h55")

    @h55.setter
    def h55(self, value: float) -> None:
        self._cards[4].set_value("h55", value)

    @property
    def h66(self) -> float:
        """Get or set the Plastic flow rule coefficients. See Remark (1).
        """ # nopep8
        return self._cards[5].get_value("h66")

    @h66.setter
    def h66(self, value: float) -> None:
        self._cards[5].set_value("h66", value)

    @property
    def lt1(self) -> typing.Optional[int]:
        """Get or set the TABLE_3D ID's containing temperature and strain rate dependent stress-strain input curves for the 12
        separate tests (LT: 3D Load Tables). LT1-3: Tension 1,2,3 directions
        """ # nopep8
        return self._cards[5].get_value("lt1")

    @lt1.setter
    def lt1(self, value: int) -> None:
        self._cards[5].set_value("lt1", value)

    @property
    def lt2(self) -> typing.Optional[int]:
        """Get or set the TABLE_3D ID's containing temperature and strain rate dependent stress-strain input curves for the 12
        separate tests (LT: 3D Load Tables). LT1-3: Tension 1,2,3 directions.
        """ # nopep8
        return self._cards[5].get_value("lt2")

    @lt2.setter
    def lt2(self, value: int) -> None:
        self._cards[5].set_value("lt2", value)

    @property
    def lt3(self) -> typing.Optional[int]:
        """Get or set the TABLE_3D ID's containing temperature and strain rate dependent stress-strain input curves for the 12
        separate tests (LT: 3D Load Tables). LT1-3: Tension 1,2,3 directions.
        """ # nopep8
        return self._cards[5].get_value("lt3")

    @lt3.setter
    def lt3(self, value: int) -> None:
        self._cards[5].set_value("lt3", value)

    @property
    def lt4(self) -> typing.Optional[int]:
        """Get or set the TABLE_3D ID's containing temperature and strain rate dependent stress-strain input curves for the 12
        separate tests (LT: 3D Load Tables). LT4-6: Compression  1,2,3 directions.
        """ # nopep8
        return self._cards[5].get_value("lt4")

    @lt4.setter
    def lt4(self, value: int) -> None:
        self._cards[5].set_value("lt4", value)

    @property
    def lt5(self) -> typing.Optional[int]:
        """Get or set the TABLE_3D ID's containing temperature and strain rate dependent stress-strain input curves for the 12
        separate tests (LT: 3D Load Tables). LT4-6: Compression  1,2,3 directions.
        """ # nopep8
        return self._cards[5].get_value("lt5")

    @lt5.setter
    def lt5(self, value: int) -> None:
        self._cards[5].set_value("lt5", value)

    @property
    def lt6(self) -> typing.Optional[int]:
        """Get or set the TABLE_3D ID's containing temperature and strain rate dependent stress-strain input curves for the 12
        separate tests (LT: 3D Load Tables). LT4-6: Compression  1,2,3 directions.
        """ # nopep8
        return self._cards[5].get_value("lt6")

    @lt6.setter
    def lt6(self, value: int) -> None:
        self._cards[5].set_value("lt6", value)

    @property
    def lt7(self) -> typing.Optional[int]:
        """Get or set the TABLE_3D ID's containing temperature and strain rate dependent stress-strain input curves for the 12
        separate tests (LT: 3D Load Tables). LT7: Shear 1-2 plane.
        """ # nopep8
        return self._cards[5].get_value("lt7")

    @lt7.setter
    def lt7(self, value: int) -> None:
        self._cards[5].set_value("lt7", value)

    @property
    def lt8(self) -> typing.Optional[int]:
        """Get or set the TABLE_3D ID's containing temperature and strain rate dependent stress-strain input curves for the 12
        separate tests (LT: 3D Load Tables). LT8: Shear 2-3 plane.
        """ # nopep8
        return self._cards[6].get_value("lt8")

    @lt8.setter
    def lt8(self, value: int) -> None:
        self._cards[6].set_value("lt8", value)

    @property
    def lt9(self) -> typing.Optional[int]:
        """Get or set the TABLE_3D ID's containing temperature and strain rate dependent stress-strain input curves for the 12
        separate tests (LT: 3D Load Tables). LT9: Shear 1-3 plane.
        """ # nopep8
        return self._cards[6].get_value("lt9")

    @lt9.setter
    def lt9(self, value: int) -> None:
        self._cards[6].set_value("lt9", value)

    @property
    def lt10(self) -> typing.Optional[int]:
        """Get or set the TABLE_3D ID's containing temperature and strain rate dependent stress-strain input curves for the 12
        separate tests (LT: 3D Load Tables). LT10: 45 degree Off-axis 1-2 plane TensionorCompression.
        """ # nopep8
        return self._cards[6].get_value("lt10")

    @lt10.setter
    def lt10(self, value: int) -> None:
        self._cards[6].set_value("lt10", value)

    @property
    def lt11(self) -> typing.Optional[int]:
        """Get or set the TABLE_3D ID's containing temperature and strain rate dependent stress-strain input curves for the 12
        separate tests (LT: 3D Load Tables). LT10: 45 degree Off-axis 2-3 plane TensionorCompression.
        """ # nopep8
        return self._cards[6].get_value("lt11")

    @lt11.setter
    def lt11(self, value: int) -> None:
        self._cards[6].set_value("lt11", value)

    @property
    def lt12(self) -> typing.Optional[int]:
        """Get or set the TABLE_3D ID's containing temperature and strain rate dependent stress-strain input curves for the 12
        separate tests (LT: 3D Load Tables). LT10: 45 degree Off-axis 1-3 plane TensionorCompression.
        """ # nopep8
        return self._cards[6].get_value("lt12")

    @lt12.setter
    def lt12(self, value: int) -> None:
        self._cards[6].set_value("lt12", value)

    @property
    def ysc(self) -> typing.Optional[int]:
        """Get or set the Load curve ID containing the stress-strain curve ID's and associated initial yield strain values. See Remark (3).
        """ # nopep8
        return self._cards[6].get_value("ysc")

    @ysc.setter
    def ysc(self, value: int) -> None:
        self._cards[6].set_value("ysc", value)

    @property
    def dflag(self) -> int:
        """Get or set the Damage formulation flag:
        EQ.0:	Based on effective stress(default)
        EQ.1 : Based on corrected plastic strain.
        """ # nopep8
        return self._cards[6].get_value("dflag")

    @dflag.setter
    def dflag(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""dflag must be one of {0,1}""")
        self._cards[6].set_value("dflag", value)

    @property
    def dc(self) -> typing.Optional[int]:
        """Get or set the Curve ID that specifies which components of the damage model are active.
        It contains the damage parameter ID and the corresponding damage versus total strain ID.
        Set this value to zero if damage should not be included in the analysis. See Remark (4).
        """ # nopep8
        return self._cards[6].get_value("dc")

    @dc.setter
    def dc(self, value: int) -> None:
        self._cards[6].set_value("dc", value)

    @property
    def ftype(self) -> int:
        """Get or set the Failure criterion type (see Remarks 5 and 6):
        EQ.0:	No failure considered(default)
        EQ.1 : Puck Failure Criterion(PFC) (solid elements only)
        EQ.2 : Tsai - Wu Failure Criterion(TWFC) (solid elements only)
        EQ.3 : Generalized Tabulated Failure Criterion(GTFC)
        """ # nopep8
        return self._cards[7].get_value("ftype")

    @ftype.setter
    def ftype(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""ftype must be one of {0,1,2,3}""")
        self._cards[7].set_value("ftype", value)

    @property
    def beta11(self) -> float:
        """Get or set the Decay constant for the relaxation matrix of the visco-elastic law in 1-direction (default = 0.001)
        """ # nopep8
        return self._cards[9].get_value("beta11")

    @beta11.setter
    def beta11(self, value: float) -> None:
        self._cards[9].set_value("beta11", value)

    @property
    def beta22(self) -> float:
        """Get or set the Decay constant for the relaxation matrix of the visco-elastic law in 2-direction (default = 0.001).
        Value must be greater than or equal to zero
        """ # nopep8
        return self._cards[9].get_value("beta22")

    @beta22.setter
    def beta22(self, value: float) -> None:
        self._cards[9].set_value("beta22", value)

    @property
    def beta33(self) -> float:
        """Get or set the Decay constant for the relaxation matrix of the visco-elastic law in 3-direction (default = 0.001)
        NOT required for shell element.
        Value must be greater than or equal to zero.
        """ # nopep8
        return self._cards[9].get_value("beta33")

    @beta33.setter
    def beta33(self, value: float) -> None:
        self._cards[9].set_value("beta33", value)

    @property
    def beta44(self) -> float:
        """Get or set the Decay constant for the relaxation matrix of the visco-elastic law in 12-shear (default = 0.001).
        Value must be greater than or equal to zero.
        """ # nopep8
        return self._cards[9].get_value("beta44")

    @beta44.setter
    def beta44(self, value: float) -> None:
        self._cards[9].set_value("beta44", value)

    @property
    def beta55(self) -> float:
        """Get or set the Decay constant for the relaxation matrix of the visco-elastic law in 23-shear (default = 0.001)
        NOT required for shell element.
        Value must be greater than or equal to zero.
        """ # nopep8
        return self._cards[9].get_value("beta55")

    @beta55.setter
    def beta55(self, value: float) -> None:
        self._cards[9].set_value("beta55", value)

    @property
    def beta66(self) -> float:
        """Get or set the Decay constant for the relaxation matrix of the visco-elastic law in 13-shear (default = 0.001)
        NOT required for shell element
        """ # nopep8
        return self._cards[9].get_value("beta66")

    @beta66.setter
    def beta66(self, value: float) -> None:
        self._cards[9].set_value("beta66", value)

    @property
    def beta12(self) -> typing.Optional[float]:
        """Get or set the Decay constant for the relaxation matrix of the visco-elastic law 12-coupling (default = (BETA11 + BETA22)/2).
        Value must be greater than or equal to zero.
        """ # nopep8
        return self._cards[9].get_value("beta12")

    @beta12.setter
    def beta12(self, value: float) -> None:
        self._cards[9].set_value("beta12", value)

    @property
    def beta23(self) -> typing.Optional[float]:
        """Get or set the Decay constant for the relaxation matrix of the visco-elastic law 23-coupling (default = (BETA22 + BETA33)/2)
        NOT required for shell element.
        Value must be greater than or equal to zero
        """ # nopep8
        return self._cards[9].get_value("beta23")

    @beta23.setter
    def beta23(self, value: float) -> None:
        self._cards[9].set_value("beta23", value)

    @property
    def beta13(self) -> typing.Optional[float]:
        """Get or set the Decay constant for the relaxation matrix of the visco-elastic law 13-coupling (default = (BETA11 + BETA33)/2)
        NOT required for shell element.
        Value must be greater than or equal to zero.
        """ # nopep8
        return self._cards[10].get_value("beta13")

    @beta13.setter
    def beta13(self, value: float) -> None:
        self._cards[10].set_value("beta13", value)

    @property
    def cp(self) -> typing.Optional[float]:
        """Get or set the Specific heat capacity (per unit mass)
        """ # nopep8
        return self._cards[10].get_value("cp")

    @cp.setter
    def cp(self, value: float) -> None:
        self._cards[10].set_value("cp", value)

    @property
    def tqc(self) -> typing.Optional[float]:
        """Get or set the Taylor-Quinney Coefficient
        """ # nopep8
        return self._cards[10].get_value("tqc")

    @tqc.setter
    def tqc(self, value: float) -> None:
        self._cards[10].set_value("tqc", value)

    @property
    def temp(self) -> typing.Optional[float]:
        """Get or set the This is the reference (or, initial) temperature used to obtain the corresponding stress-strain curves
        """ # nopep8
        return self._cards[10].get_value("temp")

    @temp.setter
    def temp(self, value: float) -> None:
        self._cards[10].set_value("temp", value)

    @property
    def pmacc(self) -> typing.Optional[float]:
        """Get or set the Plastic multiplier computational accuracy
        EQ. 0: Use up to a maximum of 1000 increments(default)
        EQ.N : Specify a positive value N greater than 1 as the maximum number of increments.An error message is issued if a converged solution cannot be found
        """ # nopep8
        return self._cards[10].get_value("pmacc")

    @pmacc.setter
    def pmacc(self, value: float) -> None:
        self._cards[10].set_value("pmacc", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[11].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[11].cards[0].set_value("title", value)

