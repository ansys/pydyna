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

class MatStoughtonNonAssociatedFlowXue(KeywordBase):
    """DYNA MAT_STOUGHTON_NON_ASSOCIATED_FLOW_XUE keyword"""

    keyword = "MAT"
    subkeyword = "STOUGHTON_NON_ASSOCIATED_FLOW_XUE"
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
                        "r00",
                        float,
                        40,
                        10,
                        kwargs.get("r00", 1.0)
                    ),
                    Field(
                        "r45",
                        float,
                        50,
                        10,
                        kwargs.get("r45", 1.0)
                    ),
                    Field(
                        "r90",
                        float,
                        60,
                        10,
                        kwargs.get("r90", 1.0)
                    ),
                    Field(
                        "sig00",
                        float,
                        70,
                        10,
                        kwargs.get("sig00")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sig45",
                        float,
                        0,
                        10,
                        kwargs.get("sig45")
                    ),
                    Field(
                        "sig90",
                        float,
                        10,
                        10,
                        kwargs.get("sig90")
                    ),
                    Field(
                        "sig_b",
                        float,
                        20,
                        10,
                        kwargs.get("sig_b")
                    ),
                    Field(
                        "lcids",
                        int,
                        30,
                        10,
                        kwargs.get("lcids")
                    ),
                    Field(
                        "lcidv",
                        int,
                        40,
                        10,
                        kwargs.get("lcidv")
                    ),
                    Field(
                        "scale",
                        float,
                        50,
                        10,
                        kwargs.get("scale", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ef0",
                        float,
                        0,
                        10,
                        kwargs.get("ef0")
                    ),
                    Field(
                        "plim",
                        float,
                        10,
                        10,
                        kwargs.get("plim")
                    ),
                    Field(
                        "q",
                        float,
                        20,
                        10,
                        kwargs.get("q")
                    ),
                    Field(
                        "gama",
                        float,
                        30,
                        10,
                        kwargs.get("gama")
                    ),
                    Field(
                        "m",
                        float,
                        40,
                        10,
                        kwargs.get("m")
                    ),
                    Field(
                        "beta",
                        float,
                        50,
                        10,
                        kwargs.get("beta")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "aopt",
                        int,
                        0,
                        10,
                        kwargs.get("aopt")
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
                ],
            ),
            OptionCardSet(
                option_spec = MatStoughtonNonAssociatedFlowXue.option_specs[0],
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
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's Modulus.
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
    def r00(self) -> float:
        """Get or set the Lankford parameters in rolling (0°), diagonal (45°) and transverse (90°) directions, respectively; determined from experiments.  Note if R00, R45, and R90 are not defined or are set to 0.0, then R00 = R45 = R90 = 1.0, which degenerates to the Von-Mises yield.
        """ # nopep8
        return self._cards[0].get_value("r00")

    @r00.setter
    def r00(self, value: float) -> None:
        self._cards[0].set_value("r00", value)

    @property
    def r45(self) -> float:
        """Get or set the Lankford parameters in rolling (0°), diagonal (45°) and transverse (90°) directions, respectively; determined from experiments.  Note if R00, R45, and R90 are not defined or are set to 0.0, then R00 = R45 = R90 = 1.0, which degenerates to the Von-Mises yield..
        """ # nopep8
        return self._cards[0].get_value("r45")

    @r45.setter
    def r45(self, value: float) -> None:
        self._cards[0].set_value("r45", value)

    @property
    def r90(self) -> float:
        """Get or set the Lankford parameters in rolling (0°), diagonal (45°) and transverse (90°) directions, respectively; determined from experiments.  Note if R00, R45, and R90 are not defined or are set to 0.0, then R00 = R45 = R90 = 1.0, which degenerates to the Von-Mises yield..
        """ # nopep8
        return self._cards[0].get_value("r90")

    @r90.setter
    def r90(self, value: float) -> None:
        self._cards[0].set_value("r90", value)

    @property
    def sig00(self) -> typing.Optional[float]:
        """Get or set the Initial yield stress from uniaxial tension tests in rolling (0°) direction
        """ # nopep8
        return self._cards[0].get_value("sig00")

    @sig00.setter
    def sig00(self, value: float) -> None:
        self._cards[0].set_value("sig00", value)

    @property
    def sig45(self) -> typing.Optional[float]:
        """Get or set the Initial yield stress from uniaxial tension tests in diagonal (45°) direction
        """ # nopep8
        return self._cards[1].get_value("sig45")

    @sig45.setter
    def sig45(self, value: float) -> None:
        self._cards[1].set_value("sig45", value)

    @property
    def sig90(self) -> typing.Optional[float]:
        """Get or set the Initial yield stress from uniaxial tension tests in transverse (90°) directions
        """ # nopep8
        return self._cards[1].get_value("sig90")

    @sig90.setter
    def sig90(self, value: float) -> None:
        self._cards[1].set_value("sig90", value)

    @property
    def sig_b(self) -> typing.Optional[float]:
        """Get or set the Initial yield stress from equi-biaxial stretching tests
        """ # nopep8
        return self._cards[1].get_value("sig_b")

    @sig_b.setter
    def sig_b(self, value: float) -> None:
        self._cards[1].set_value("sig_b", value)

    @property
    def lcids(self) -> typing.Optional[int]:
        """Get or set the ID of a load curve defining stress vs. strain hardening behavior from a uniaxial tension test along the rolling direction.
        """ # nopep8
        return self._cards[1].get_value("lcids")

    @lcids.setter
    def lcids(self, value: int) -> None:
        self._cards[1].set_value("lcids", value)

    @property
    def lcidv(self) -> typing.Optional[int]:
        """Get or set the ID of a load curve defining stress scale factors vs. strain rates; determined from experiments.  An example of the curve can be found in Figure 0-2.  Furthermore, strain rates are stored in history variable #5.  Strain rate scale factors are stored in history variable #6.  To turn on the variables for viewing in LS-PrePost, set NEIPS to at least "6" in *DATABASE_EXTENT_BINARY.  It is very useful to know what levels of strain rates, and strain rate scale factors in a particular simulation.  Once d3plot files are opened in LS-PrePost, individual element time history can be plotted via menu option Post → History, or a color contour of the entire part can be viewed with the menu option Post → FriComp → Misc.
        """ # nopep8
        return self._cards[1].get_value("lcidv")

    @lcidv.setter
    def lcidv(self, value: int) -> None:
        self._cards[1].set_value("lcidv", value)

    @property
    def scale(self) -> float:
        """Get or set the This variable can be used to speed up the simulation while equalizing the strain rate effect, useful especially in cases where the pulling speed or punch speed is slow.  For example, if the pulling speed is at 15 mm/s but running the simulation at this speed will take a long time, the pulling speed can be increased to 500 mm/s while SCALE can be set to 0.03, giving the same results as those from 15 mm/s, but with the benefit of greatly reduced computational time, see Figures 0-3 and 0-4.  Note the increased absolute value (within a reasonable range) of mass scaling -1.0*dt2ms frequently used in forming simulation does not affect the strain rates, as shown in the Figure 0-5.
        """ # nopep8
        return self._cards[1].get_value("scale")

    @scale.setter
    def scale(self, value: float) -> None:
        self._cards[1].set_value("scale", value)

    @property
    def ef0(self) -> typing.Optional[float]:
        """Get or set the Material parameters for the option XUE. The parameter k in the
        original paper is assumed to be 1.0. For details, refer to Xue, L.,
        Wierzbicki, T.'s 2009 paper "Numerical simulation of fracture mode
        transition in ductile plates" in the International Journal of Solids and Structures.
        """ # nopep8
        return self._cards[2].get_value("ef0")

    @ef0.setter
    def ef0(self, value: float) -> None:
        self._cards[2].set_value("ef0", value)

    @property
    def plim(self) -> typing.Optional[float]:
        """Get or set the Material parameters for the option XUE. The parameter k in the
        original paper is assumed to be 1.0. For details, refer to Xue, L.,
        Wierzbicki, T.'s 2009 paper "Numerical simulation of fracture mode
        transition in ductile plates" in the International Journal of Solids and Structures.
        """ # nopep8
        return self._cards[2].get_value("plim")

    @plim.setter
    def plim(self, value: float) -> None:
        self._cards[2].set_value("plim", value)

    @property
    def q(self) -> typing.Optional[float]:
        """Get or set the Material parameters for the option XUE. The parameter k in the
        original paper is assumed to be 1.0. For details, refer to Xue, L.,
        Wierzbicki, T.'s 2009 paper "Numerical simulation of fracture mode
        transition in ductile plates" in the International Journal of Solids and Structures..
        """ # nopep8
        return self._cards[2].get_value("q")

    @q.setter
    def q(self, value: float) -> None:
        self._cards[2].set_value("q", value)

    @property
    def gama(self) -> typing.Optional[float]:
        """Get or set the Material parameters for the option XUE. The parameter k in the
        original paper is assumed to be 1.0. For details, refer to Xue, L.,
        Wierzbicki, T.'s 2009 paper "Numerical simulation of fracture mode
        transition in ductile plates" in the International Journal of Solids and Structures.
        """ # nopep8
        return self._cards[2].get_value("gama")

    @gama.setter
    def gama(self, value: float) -> None:
        self._cards[2].set_value("gama", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Material parameters for the option XUE. The parameter k in the
        original paper is assumed to be 1.0. For details, refer to Xue, L.,
        Wierzbicki, T.'s 2009 paper "Numerical simulation of fracture mode
        transition in ductile plates" in the International Journal of Solids and Structures.
        """ # nopep8
        return self._cards[2].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        self._cards[2].set_value("m", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material parameters for the option XUE.  The parameter k in the original paper is assumed to be 1.0.  Note the default BETA value of 0.0 means no progressive weakening damage.  For details, refer to Xue, L., Wierzbicki, T.’s 2009 paper “Numerical simulation of fracture mode transition in ductile plates” in the International Journal of Solids and Structures.
        """ # nopep8
        return self._cards[2].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[2].set_value("beta", value)

    @property
    def aopt(self) -> typing.Optional[int]:
        """Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
        EQ.0.0:	Locally orthotropic with material axes determined by element nodes 1, 2,and 4, as with* DEFINE_COORDINATE_NODES.For shells only, the material axes are then rotated about the normal vector to the surface of the shell by the angle BETA.
        EQ.1.0 : Locally orthotropic with material axes determined by a point, P, in spaceand the global location of the element center; this is the a - direction.This option is for solid elements only.
        EQ.2.0:	Globally orthotropic with material axes determined by vectors defined below, as with* DEFINE_COORDINATE_VECTOR
        EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element.The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.Thus, for solid elements, AOPT = 3 is only available for hexahedrons.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle BETA.BETA may be set in the keyword input for the element.
        EQ.4.0 : Locally orthotropic in a cylindrical coordinate system with the material axes determined by a vector v,and an originating point, P, which define the centerline axis.This option is for solid elements only.
        LT.0.0 : The absolute value of AOPT is a coordinate system ID number(CID on * DEFINE_COORDINATE_OPTION)
        """ # nopep8
        return self._cards[3].get_value("aopt")

    @aopt.setter
    def aopt(self, value: int) -> None:
        self._cards[3].set_value("aopt", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[4].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[4].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1
        """ # nopep8
        return self._cards[4].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[4].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[4].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[4].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2, for shells and solids.
        """ # nopep8
        return self._cards[4].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[4].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2, for shells and solids.
        """ # nopep8
        return self._cards[4].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[4].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2, for shells and solids.
        """ # nopep8
        return self._cards[4].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[4].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[5].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[5].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[5].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[5].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[5].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[5].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2, for solids.
        """ # nopep8
        return self._cards[5].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[5].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2, for solids.
        """ # nopep8
        return self._cards[5].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[5].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2, for solids.
        """ # nopep8
        return self._cards[5].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[5].set_value("d3", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[6].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[6].cards[0].set_value("title", value)

