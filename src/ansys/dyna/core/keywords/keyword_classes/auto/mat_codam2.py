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

class MatCodam2(KeywordBase):
    """DYNA MAT_CODAM2 keyword"""

    keyword = "MAT"
    subkeyword = "CODAM2"
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
                        "unused",
                        float,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "prba",
                        float,
                        50,
                        10,
                        kwargs.get("prba")
                    ),
                    Field(
                        "unused",
                        float,
                        60,
                        10,
                        kwargs.get("unused")
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
                        "unused",
                        float,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        float,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "nlayer",
                        int,
                        30,
                        10,
                        kwargs.get("nlayer", 0)
                    ),
                    Field(
                        "r1",
                        float,
                        40,
                        10,
                        kwargs.get("r1")
                    ),
                    Field(
                        "r2",
                        float,
                        50,
                        10,
                        kwargs.get("r2")
                    ),
                    Field(
                        "nfreq",
                        int,
                        60,
                        10,
                        kwargs.get("nfreq", 0)
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
                    Field(
                        "aopt",
                        float,
                        60,
                        10,
                        kwargs.get("aopt")
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
                    Field(
                        "macf",
                        float,
                        70,
                        10,
                        kwargs.get("macf", 1)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "angle1",
                        float,
                        0,
                        10,
                        kwargs.get("angle1")
                    ),
                    Field(
                        "angle2",
                        float,
                        10,
                        10,
                        kwargs.get("angle2")
                    ),
                    Field(
                        "angle3",
                        float,
                        20,
                        10,
                        kwargs.get("angle3")
                    ),
                    Field(
                        "angle4",
                        float,
                        30,
                        10,
                        kwargs.get("angle4")
                    ),
                    Field(
                        "angle5",
                        float,
                        40,
                        10,
                        kwargs.get("angle5")
                    ),
                    Field(
                        "angle6",
                        float,
                        50,
                        10,
                        kwargs.get("angle6")
                    ),
                    Field(
                        "angle7",
                        float,
                        60,
                        10,
                        kwargs.get("angle7")
                    ),
                    Field(
                        "angle8",
                        float,
                        70,
                        10,
                        kwargs.get("angle8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "imatt",
                        float,
                        0,
                        10,
                        kwargs.get("imatt")
                    ),
                    Field(
                        "ifibt",
                        float,
                        10,
                        10,
                        kwargs.get("ifibt")
                    ),
                    Field(
                        "iloct",
                        float,
                        20,
                        10,
                        kwargs.get("iloct")
                    ),
                    Field(
                        "idelt",
                        float,
                        30,
                        10,
                        kwargs.get("idelt")
                    ),
                    Field(
                        "smatt",
                        float,
                        40,
                        10,
                        kwargs.get("smatt")
                    ),
                    Field(
                        "sfibt",
                        float,
                        50,
                        10,
                        kwargs.get("sfibt")
                    ),
                    Field(
                        "sloct",
                        float,
                        60,
                        10,
                        kwargs.get("sloct")
                    ),
                    Field(
                        "sdelt",
                        float,
                        70,
                        10,
                        kwargs.get("sdelt")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "imatc",
                        float,
                        0,
                        10,
                        kwargs.get("imatc")
                    ),
                    Field(
                        "ifibc",
                        float,
                        10,
                        10,
                        kwargs.get("ifibc")
                    ),
                    Field(
                        "ilocc",
                        float,
                        20,
                        10,
                        kwargs.get("ilocc")
                    ),
                    Field(
                        "idelc",
                        float,
                        30,
                        10,
                        kwargs.get("idelc")
                    ),
                    Field(
                        "smatc",
                        float,
                        40,
                        10,
                        kwargs.get("smatc")
                    ),
                    Field(
                        "sfibc",
                        float,
                        50,
                        10,
                        kwargs.get("sfibc")
                    ),
                    Field(
                        "slocc",
                        float,
                        60,
                        10,
                        kwargs.get("slocc")
                    ),
                    Field(
                        "sdelc",
                        float,
                        70,
                        10,
                        kwargs.get("sdelc")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "erode",
                        int,
                        0,
                        10,
                        kwargs.get("erode", 0)
                    ),
                    Field(
                        "erpar1",
                        float,
                        10,
                        10,
                        kwargs.get("erpar1")
                    ),
                    Field(
                        "erpar2",
                        float,
                        20,
                        10,
                        kwargs.get("erpar2")
                    ),
                    Field(
                        "resids",
                        float,
                        30,
                        10,
                        kwargs.get("resids")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatCodam2.option_specs[0],
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
        """Get or set the Young's modulus in a-direction = Modulus along the direction of fibers.
        """ # nopep8
        return self._cards[0].get_value("ea")

    @ea.setter
    def ea(self, value: float) -> None:
        self._cards[0].set_value("ea", value)

    @property
    def eb(self) -> typing.Optional[float]:
        """Get or set the Young's modulus in b-direction = Modulus transverse to fibers.
        """ # nopep8
        return self._cards[0].get_value("eb")

    @eb.setter
    def eb(self, value: float) -> None:
        self._cards[0].set_value("eb", value)

    @property
    def prba(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio, ba (minor in-plane Poisson's ratio).
        """ # nopep8
        return self._cards[0].get_value("prba")

    @prba.setter
    def prba(self, value: float) -> None:
        self._cards[0].set_value("prba", value)

    @property
    def prcb(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio, cb (Poisson's ratio in the plane of isotropy).
        """ # nopep8
        return self._cards[0].get_value("prcb")

    @prcb.setter
    def prcb(self, value: float) -> None:
        self._cards[0].set_value("prcb", value)

    @property
    def gab(self) -> typing.Optional[float]:
        """Get or set the Shear modulus, ab (in-plane shear modulus).
        """ # nopep8
        return self._cards[1].get_value("gab")

    @gab.setter
    def gab(self, value: float) -> None:
        self._cards[1].set_value("gab", value)

    @property
    def nlayer(self) -> int:
        """Get or set the Number of layers in the sub-laminate excluding symmetry.
        """ # nopep8
        return self._cards[1].get_value("nlayer")

    @nlayer.setter
    def nlayer(self, value: int) -> None:
        self._cards[1].set_value("nlayer", value)

    @property
    def r1(self) -> typing.Optional[float]:
        """Get or set the Non-local averaging radius.
        """ # nopep8
        return self._cards[1].get_value("r1")

    @r1.setter
    def r1(self, value: float) -> None:
        self._cards[1].set_value("r1", value)

    @property
    def r2(self) -> typing.Optional[float]:
        """Get or set the Currently not used.
        """ # nopep8
        return self._cards[1].get_value("r2")

    @r2.setter
    def r2(self, value: float) -> None:
        self._cards[1].set_value("r2", value)

    @property
    def nfreq(self) -> int:
        """Get or set the Number of time steps between update of neighbor list for nonlocal smoothing.
        EQ.0: Do only one search at the start of the calculation.
        """ # nopep8
        return self._cards[1].get_value("nfreq")

    @nfreq.setter
    def nfreq(self, value: int) -> None:
        self._cards[1].set_value("nfreq", value)

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
    def d3(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[3].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_SOLID_ORTHO.
        """ # nopep8
        return self._cards[3].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[3].set_value("beta", value)

    @property
    def macf(self) -> float:
        """Get or set the Material axes change flag for solid elements:
        EQ. - 4:	Switch material axes b and c before BETA rotation
        EQ. - 3 : Switch material axes a and c before BETA rotation
        EQ. - 2 : Switch material axes a and b before BETA rotation
        EQ.1 : No change, default
        EQ.2 : Switch material axes a and b after BETA rotation
        EQ.3 : Switch material axes a and c after BETA rotation
        EQ.4 : Switch material axes b and c after BETA rotation
        Figure Error!Reference source not found.indicates when LS - DYNA applies MACF during the process to obtain the final material axes.If BETA on * ELEMENT_SOLID_{OPTION} is defined, then that BETA is used for the rotation for all AOPT options.Otherwise, if AOPT = 3, the BETA input on Card 6 rotates the axes.For all other values of AOPT, the material axes will be switched as specified by MACF, but no BETA rotation will be performed.
        """ # nopep8
        return self._cards[3].get_value("macf")

    @macf.setter
    def macf(self, value: float) -> None:
        if value not in [1, 2, 3, 4, -2, -3, -4]:
            raise Exception("""macf must be one of {1,2,3,4,-2,-3,-4}""")
        self._cards[3].set_value("macf", value)

    @property
    def angle1(self) -> typing.Optional[float]:
        """Get or set the Rotation angle in degrees of layers with respect to the material axes. Input one for each layer.
        """ # nopep8
        return self._cards[4].get_value("angle1")

    @angle1.setter
    def angle1(self, value: float) -> None:
        self._cards[4].set_value("angle1", value)

    @property
    def angle2(self) -> typing.Optional[float]:
        """Get or set the Rotation angle in degrees of layers with respect to the material axes. Input one for each layer.
        """ # nopep8
        return self._cards[4].get_value("angle2")

    @angle2.setter
    def angle2(self, value: float) -> None:
        self._cards[4].set_value("angle2", value)

    @property
    def angle3(self) -> typing.Optional[float]:
        """Get or set the Rotation angle in degrees of layers with respect to the material axes. Input one for each layer.
        """ # nopep8
        return self._cards[4].get_value("angle3")

    @angle3.setter
    def angle3(self, value: float) -> None:
        self._cards[4].set_value("angle3", value)

    @property
    def angle4(self) -> typing.Optional[float]:
        """Get or set the Rotation angle in degrees of layers with respect to the material axes. Input one for each layer.
        """ # nopep8
        return self._cards[4].get_value("angle4")

    @angle4.setter
    def angle4(self, value: float) -> None:
        self._cards[4].set_value("angle4", value)

    @property
    def angle5(self) -> typing.Optional[float]:
        """Get or set the Rotation angle in degrees of layers with respect to the material axes. Input one for each layer.
        """ # nopep8
        return self._cards[4].get_value("angle5")

    @angle5.setter
    def angle5(self, value: float) -> None:
        self._cards[4].set_value("angle5", value)

    @property
    def angle6(self) -> typing.Optional[float]:
        """Get or set the Rotation angle in degrees of layers with respect to the material axes. Input one for each layer.
        """ # nopep8
        return self._cards[4].get_value("angle6")

    @angle6.setter
    def angle6(self, value: float) -> None:
        self._cards[4].set_value("angle6", value)

    @property
    def angle7(self) -> typing.Optional[float]:
        """Get or set the Rotation angle in degrees of layers with respect to the material axes. Input one for each layer.
        """ # nopep8
        return self._cards[4].get_value("angle7")

    @angle7.setter
    def angle7(self, value: float) -> None:
        self._cards[4].set_value("angle7", value)

    @property
    def angle8(self) -> typing.Optional[float]:
        """Get or set the Rotation angle in degrees of layers with respect to the material axes. Input one for each layer.
        """ # nopep8
        return self._cards[4].get_value("angle8")

    @angle8.setter
    def angle8(self, value: float) -> None:
        self._cards[4].set_value("angle8", value)

    @property
    def imatt(self) -> typing.Optional[float]:
        """Get or set the Initiation strain for damage in matrix (transverse) under tensile condition.
        """ # nopep8
        return self._cards[5].get_value("imatt")

    @imatt.setter
    def imatt(self, value: float) -> None:
        self._cards[5].set_value("imatt", value)

    @property
    def ifibt(self) -> typing.Optional[float]:
        """Get or set the Initiation strain for damage in the fiber (longitudinal) under tensile condition.
        """ # nopep8
        return self._cards[5].get_value("ifibt")

    @ifibt.setter
    def ifibt(self, value: float) -> None:
        self._cards[5].set_value("ifibt", value)

    @property
    def iloct(self) -> typing.Optional[float]:
        """Get or set the Initiation strain for the anti-locking mechanism. This parameter should be
        equal to the saturation strain for the fiber damage mechanism under tensile condition.
        """ # nopep8
        return self._cards[5].get_value("iloct")

    @iloct.setter
    def iloct(self, value: float) -> None:
        self._cards[5].set_value("iloct", value)

    @property
    def idelt(self) -> typing.Optional[float]:
        """Get or set the Not working in the current version. Can be used for visualization purpose only.
        """ # nopep8
        return self._cards[5].get_value("idelt")

    @idelt.setter
    def idelt(self, value: float) -> None:
        self._cards[5].set_value("idelt", value)

    @property
    def smatt(self) -> typing.Optional[float]:
        """Get or set the Saturation strain for damage in matrix (transverse) under tensile condition.
        """ # nopep8
        return self._cards[5].get_value("smatt")

    @smatt.setter
    def smatt(self, value: float) -> None:
        self._cards[5].set_value("smatt", value)

    @property
    def sfibt(self) -> typing.Optional[float]:
        """Get or set the Saturation strain for damage in the fiber (longitudinal) under tensile condition..
        """ # nopep8
        return self._cards[5].get_value("sfibt")

    @sfibt.setter
    def sfibt(self, value: float) -> None:
        self._cards[5].set_value("sfibt", value)

    @property
    def sloct(self) -> typing.Optional[float]:
        """Get or set the Saturation strain for the anti-locking mechanism under tensile condition.
        The recommended value for this parameter is (ILOCT+0.02).
        """ # nopep8
        return self._cards[5].get_value("sloct")

    @sloct.setter
    def sloct(self, value: float) -> None:
        self._cards[5].set_value("sloct", value)

    @property
    def sdelt(self) -> typing.Optional[float]:
        """Get or set the Not working in the current version. Can be used for visualization purpose only.
        """ # nopep8
        return self._cards[5].get_value("sdelt")

    @sdelt.setter
    def sdelt(self, value: float) -> None:
        self._cards[5].set_value("sdelt", value)

    @property
    def imatc(self) -> typing.Optional[float]:
        """Get or set the Initiation strain for damage in matrix (transverse) under compressive condition.
        """ # nopep8
        return self._cards[6].get_value("imatc")

    @imatc.setter
    def imatc(self, value: float) -> None:
        self._cards[6].set_value("imatc", value)

    @property
    def ifibc(self) -> typing.Optional[float]:
        """Get or set the Initiation strain for damage in the fiber (longitudinal) under compressive condition.
        """ # nopep8
        return self._cards[6].get_value("ifibc")

    @ifibc.setter
    def ifibc(self, value: float) -> None:
        self._cards[6].set_value("ifibc", value)

    @property
    def ilocc(self) -> typing.Optional[float]:
        """Get or set the Initiation strain for the anti-locking mechanism. This parameter should be
        equal to the saturation strain for the fiber damage mechanism under compressive condition.
        """ # nopep8
        return self._cards[6].get_value("ilocc")

    @ilocc.setter
    def ilocc(self, value: float) -> None:
        self._cards[6].set_value("ilocc", value)

    @property
    def idelc(self) -> typing.Optional[float]:
        """Get or set the Initiation strain for delamination. Not working in the current version. Can be used for visualization purpose only.
        """ # nopep8
        return self._cards[6].get_value("idelc")

    @idelc.setter
    def idelc(self, value: float) -> None:
        self._cards[6].set_value("idelc", value)

    @property
    def smatc(self) -> typing.Optional[float]:
        """Get or set the Saturation strain for damage in matrix (transverse) under compressive condition.
        """ # nopep8
        return self._cards[6].get_value("smatc")

    @smatc.setter
    def smatc(self, value: float) -> None:
        self._cards[6].set_value("smatc", value)

    @property
    def sfibc(self) -> typing.Optional[float]:
        """Get or set the Saturation strain for damage in the fiber (longitudinal) under compressive condition.
        """ # nopep8
        return self._cards[6].get_value("sfibc")

    @sfibc.setter
    def sfibc(self, value: float) -> None:
        self._cards[6].set_value("sfibc", value)

    @property
    def slocc(self) -> typing.Optional[float]:
        """Get or set the Saturation strain for the anti-locking mechanism under compressive condition.
        The recommended value for this parameter is (ILOCC+0.02).
        """ # nopep8
        return self._cards[6].get_value("slocc")

    @slocc.setter
    def slocc(self, value: float) -> None:
        self._cards[6].set_value("slocc", value)

    @property
    def sdelc(self) -> typing.Optional[float]:
        """Get or set the Delamination strain. Not working in the current version. Can be used for visualization purpose only.
        """ # nopep8
        return self._cards[6].get_value("sdelc")

    @sdelc.setter
    def sdelc(self, value: float) -> None:
        self._cards[6].set_value("sdelc", value)

    @property
    def erode(self) -> int:
        """Get or set the Erosion Flag (see remarks)
        EQ.0: Erosion is turned off.
        EQ.1: Non-local strain based erosion criterion.
        EQ.2: Local strain based erosion criterion.
        EQ.3: Use both ERODE = 1 and ERODE = 2 criteria.
        """ # nopep8
        return self._cards[7].get_value("erode")

    @erode.setter
    def erode(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""erode must be one of {0,1,2,3}""")
        self._cards[7].set_value("erode", value)

    @property
    def erpar1(self) -> typing.Optional[float]:
        """Get or set the The erosion parameter #1 used in ERODE types 1 and 3. ERPAR1>=1.0	and the recommended value is ERPAR1 = 1.2.
        """ # nopep8
        return self._cards[7].get_value("erpar1")

    @erpar1.setter
    def erpar1(self, value: float) -> None:
        self._cards[7].set_value("erpar1", value)

    @property
    def erpar2(self) -> typing.Optional[float]:
        """Get or set the The erosion parameter #2 used in ERODE types 2 and 3. The recommended
        value is five times SLOC defined in cards 7 and 8.
        """ # nopep8
        return self._cards[7].get_value("erpar2")

    @erpar2.setter
    def erpar2(self, value: float) -> None:
        self._cards[7].set_value("erpar2", value)

    @property
    def resids(self) -> typing.Optional[float]:
        """Get or set the Residual strength for layer damage.
        """ # nopep8
        return self._cards[7].get_value("resids")

    @resids.setter
    def resids(self, value: float) -> None:
        self._cards[7].set_value("resids", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[8].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[8].cards[0].set_value("title", value)

