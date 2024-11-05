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

class Mat040(KeywordBase):
    """DYNA MAT_040 keyword"""

    keyword = "MAT"
    subkeyword = "040"
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
                        "dt",
                        float,
                        30,
                        10,
                        kwargs.get("dt")
                    ),
                    Field(
                        "tramp",
                        float,
                        40,
                        10,
                        kwargs.get("tramp")
                    ),
                    Field(
                        "alpha",
                        float,
                        50,
                        10,
                        kwargs.get("alpha")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcida",
                        float,
                        0,
                        10,
                        kwargs.get("lcida", 0)
                    ),
                    Field(
                        "lcidb",
                        float,
                        10,
                        10,
                        kwargs.get("lcidb", 0)
                    ),
                    Field(
                        "efail",
                        float,
                        20,
                        10,
                        kwargs.get("efail")
                    ),
                    Field(
                        "dtfail",
                        float,
                        30,
                        10,
                        kwargs.get("dtfail")
                    ),
                    Field(
                        "cdamp",
                        float,
                        40,
                        10,
                        kwargs.get("cdamp")
                    ),
                    Field(
                        "aopt",
                        float,
                        50,
                        10,
                        kwargs.get("aopt")
                    ),
                    Field(
                        "macf",
                        int,
                        60,
                        10,
                        kwargs.get("macf", 1)
                    ),
                    Field(
                        "atrack",
                        int,
                        70,
                        10,
                        kwargs.get("atrack", 0)
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
                        "lcidc",
                        int,
                        0,
                        10,
                        kwargs.get("lcidc", 0)
                    ),
                    Field(
                        "lcidab",
                        int,
                        10,
                        10,
                        kwargs.get("lcidab", 0)
                    ),
                    Field(
                        "lcidbc",
                        int,
                        20,
                        10,
                        kwargs.get("lcidbc", 0)
                    ),
                    Field(
                        "lcidca",
                        int,
                        30,
                        10,
                        kwargs.get("lcidca", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat040.option_specs[0],
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
        """Get or set the Young's modulus in a-direction.
        """ # nopep8
        return self._cards[0].get_value("ea")

    @ea.setter
    def ea(self, value: float) -> None:
        self._cards[0].set_value("ea", value)

    @property
    def eb(self) -> typing.Optional[float]:
        """Get or set the Young's modulus in b-direction.
        """ # nopep8
        return self._cards[0].get_value("eb")

    @eb.setter
    def eb(self, value: float) -> None:
        self._cards[0].set_value("eb", value)

    @property
    def ec(self) -> typing.Optional[float]:
        """Get or set the Young's modulus in c-direction.
        """ # nopep8
        return self._cards[0].get_value("ec")

    @ec.setter
    def ec(self, value: float) -> None:
        self._cards[0].set_value("ec", value)

    @property
    def prba(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio ba.
        """ # nopep8
        return self._cards[0].get_value("prba")

    @prba.setter
    def prba(self, value: float) -> None:
        self._cards[0].set_value("prba", value)

    @property
    def prca(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio ca.
        """ # nopep8
        return self._cards[0].get_value("prca")

    @prca.setter
    def prca(self, value: float) -> None:
        self._cards[0].set_value("prca", value)

    @property
    def prcb(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio cb.
        """ # nopep8
        return self._cards[0].get_value("prcb")

    @prcb.setter
    def prcb(self, value: float) -> None:
        self._cards[0].set_value("prcb", value)

    @property
    def gab(self) -> typing.Optional[float]:
        """Get or set the Shear modulus ab.
        """ # nopep8
        return self._cards[1].get_value("gab")

    @gab.setter
    def gab(self, value: float) -> None:
        self._cards[1].set_value("gab", value)

    @property
    def gbc(self) -> typing.Optional[float]:
        """Get or set the Shear modulus bc.
        """ # nopep8
        return self._cards[1].get_value("gbc")

    @gbc.setter
    def gbc(self, value: float) -> None:
        self._cards[1].set_value("gbc", value)

    @property
    def gca(self) -> typing.Optional[float]:
        """Get or set the Shear modulus ca.
        """ # nopep8
        return self._cards[1].get_value("gca")

    @gca.setter
    def gca(self, value: float) -> None:
        self._cards[1].set_value("gca", value)

    @property
    def dt(self) -> typing.Optional[float]:
        """Get or set the Temperature increment for isotropic stress initialization. This option can be used during dynamic relaxation.
        """ # nopep8
        return self._cards[1].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[1].set_value("dt", value)

    @property
    def tramp(self) -> typing.Optional[float]:
        """Get or set the Time to ramp up to the final temperature.
        """ # nopep8
        return self._cards[1].get_value("tramp")

    @tramp.setter
    def tramp(self, value: float) -> None:
        self._cards[1].set_value("tramp", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Thermal expansion coefficient.
        """ # nopep8
        return self._cards[1].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        self._cards[1].set_value("alpha", value)

    @property
    def lcida(self) -> float:
        """Get or set the Optional load curve ID defining the nominal stress versus strain along a-axis.
        """ # nopep8
        return self._cards[2].get_value("lcida")

    @lcida.setter
    def lcida(self, value: float) -> None:
        self._cards[2].set_value("lcida", value)

    @property
    def lcidb(self) -> float:
        """Get or set the Optional load curve ID defining the nominal stress versus strain along b-axis.
        """ # nopep8
        return self._cards[2].get_value("lcidb")

    @lcidb.setter
    def lcidb(self, value: float) -> None:
        self._cards[2].set_value("lcidb", value)

    @property
    def efail(self) -> typing.Optional[float]:
        """Get or set the Failure strain.
        """ # nopep8
        return self._cards[2].get_value("efail")

    @efail.setter
    def efail(self, value: float) -> None:
        self._cards[2].set_value("efail", value)

    @property
    def dtfail(self) -> typing.Optional[float]:
        """Get or set the Time step for automatic element erosion
        """ # nopep8
        return self._cards[2].get_value("dtfail")

    @dtfail.setter
    def dtfail(self, value: float) -> None:
        self._cards[2].set_value("dtfail", value)

    @property
    def cdamp(self) -> typing.Optional[float]:
        """Get or set the Damping coefficient.
        """ # nopep8
        return self._cards[2].get_value("cdamp")

    @cdamp.setter
    def cdamp(self, value: float) -> None:
        self._cards[2].set_value("cdamp", value)

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
    def atrack(self) -> int:
        """Get or set the Material a-axis tracking flag (shell elements only)
        EQ.0:	a - axis rotates with element(default)
        EQ.1 : a - axis also tracks deformation
        """ # nopep8
        return self._cards[2].get_value("atrack")

    @atrack.setter
    def atrack(self, value: int) -> None:
        self._cards[2].set_value("atrack", value)

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
    def v1(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[4].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[4].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[4].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[4].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[4].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[4].set_value("v3", value)

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
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT=3, may be overridden on the element card, see *ELEMENT_SHELL_BETA.
        """ # nopep8
        return self._cards[4].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[4].set_value("beta", value)

    @property
    def lcidc(self) -> int:
        """Get or set the Load curve ID defining the nominal stress versus strain along c-axis. Only solid elements.
        """ # nopep8
        return self._cards[5].get_value("lcidc")

    @lcidc.setter
    def lcidc(self, value: int) -> None:
        self._cards[5].set_value("lcidc", value)

    @property
    def lcidab(self) -> int:
        """Get or set the Load curve ID defining the nominal ab shear stress versus ab-strain in the ab-plane. Only solid elements.
        """ # nopep8
        return self._cards[5].get_value("lcidab")

    @lcidab.setter
    def lcidab(self, value: int) -> None:
        self._cards[5].set_value("lcidab", value)

    @property
    def lcidbc(self) -> int:
        """Get or set the Load curve ID defining the nominal ab shear stress versus ab-strain in the bc-plane. Only solid elements.
        """ # nopep8
        return self._cards[5].get_value("lcidbc")

    @lcidbc.setter
    def lcidbc(self, value: int) -> None:
        self._cards[5].set_value("lcidbc", value)

    @property
    def lcidca(self) -> int:
        """Get or set the Load curve ID defining the nominal ab shear stress versus ab-strain in the ca-plane. Only solid elements.
        """ # nopep8
        return self._cards[5].get_value("lcidca")

    @lcidca.setter
    def lcidca(self, value: int) -> None:
        self._cards[5].set_value("lcidca", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[6].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[6].cards[0].set_value("title", value)

