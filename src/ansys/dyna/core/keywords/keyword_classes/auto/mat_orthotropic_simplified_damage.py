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

class MatOrthotropicSimplifiedDamage(KeywordBase):
    """DYNA MAT_ORTHOTROPIC_SIMPLIFIED_DAMAGE keyword"""

    keyword = "MAT"
    subkeyword = "ORTHOTROPIC_SIMPLIFIED_DAMAGE"
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
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "aopt",
                        float,
                        40,
                        10,
                        kwargs.get("aopt")
                    ),
                    Field(
                        "macf",
                        int,
                        50,
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
                        "nerode",
                        int,
                        0,
                        10,
                        kwargs.get("nerode", 0)
                    ),
                    Field(
                        "ndam",
                        int,
                        10,
                        10,
                        kwargs.get("ndam", 0)
                    ),
                    Field(
                        "eps1tf",
                        float,
                        20,
                        10,
                        kwargs.get("eps1tf", 1.E20)
                    ),
                    Field(
                        "eps2tf",
                        float,
                        30,
                        10,
                        kwargs.get("eps2tf", 1.E20)
                    ),
                    Field(
                        "eps3tf",
                        float,
                        40,
                        10,
                        kwargs.get("eps3tf", 1.E20)
                    ),
                    Field(
                        "eps1cf",
                        float,
                        50,
                        10,
                        kwargs.get("eps1cf", -1.E20)
                    ),
                    Field(
                        "eps2cf",
                        float,
                        60,
                        10,
                        kwargs.get("eps2cf", -1.E20)
                    ),
                    Field(
                        "eps3cf",
                        float,
                        70,
                        10,
                        kwargs.get("eps3cf", -1.E20)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "eps12f",
                        float,
                        0,
                        10,
                        kwargs.get("eps12f", 1.E20)
                    ),
                    Field(
                        "eps23f",
                        float,
                        10,
                        10,
                        kwargs.get("eps23f", 1.E20)
                    ),
                    Field(
                        "eps13f",
                        float,
                        20,
                        10,
                        kwargs.get("eps13f", 1.E20)
                    ),
                    Field(
                        "epsd1t",
                        float,
                        30,
                        10,
                        kwargs.get("epsd1t")
                    ),
                    Field(
                        "epsc1t",
                        float,
                        40,
                        10,
                        kwargs.get("epsc1t")
                    ),
                    Field(
                        "cdam1t",
                        float,
                        50,
                        10,
                        kwargs.get("cdam1t")
                    ),
                    Field(
                        "epsd2t",
                        float,
                        60,
                        10,
                        kwargs.get("epsd2t")
                    ),
                    Field(
                        "epsc2t",
                        float,
                        70,
                        10,
                        kwargs.get("epsc2t")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cdam2t",
                        float,
                        0,
                        10,
                        kwargs.get("cdam2t", 0)
                    ),
                    Field(
                        "epsd3t",
                        float,
                        10,
                        10,
                        kwargs.get("epsd3t", 0)
                    ),
                    Field(
                        "epsc3t",
                        float,
                        20,
                        10,
                        kwargs.get("epsc3t")
                    ),
                    Field(
                        "cdam3t",
                        float,
                        30,
                        10,
                        kwargs.get("cdam3t")
                    ),
                    Field(
                        "epsd1c",
                        float,
                        40,
                        10,
                        kwargs.get("epsd1c")
                    ),
                    Field(
                        "epsc1c",
                        float,
                        50,
                        10,
                        kwargs.get("epsc1c")
                    ),
                    Field(
                        "cdam1c",
                        float,
                        60,
                        10,
                        kwargs.get("cdam1c")
                    ),
                    Field(
                        "epsd2c",
                        float,
                        70,
                        10,
                        kwargs.get("epsd2c")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "epsc2c",
                        float,
                        0,
                        10,
                        kwargs.get("epsc2c")
                    ),
                    Field(
                        "cdam2c",
                        float,
                        10,
                        10,
                        kwargs.get("cdam2c")
                    ),
                    Field(
                        "epsd3c",
                        float,
                        20,
                        10,
                        kwargs.get("epsd3c")
                    ),
                    Field(
                        "epsc3c",
                        float,
                        30,
                        10,
                        kwargs.get("epsc3c")
                    ),
                    Field(
                        "cdam3c",
                        float,
                        40,
                        10,
                        kwargs.get("cdam3c")
                    ),
                    Field(
                        "epsd12",
                        float,
                        50,
                        10,
                        kwargs.get("epsd12")
                    ),
                    Field(
                        "epsc12",
                        float,
                        60,
                        10,
                        kwargs.get("epsc12")
                    ),
                    Field(
                        "cdam12",
                        float,
                        70,
                        10,
                        kwargs.get("cdam12")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "epsd23",
                        float,
                        0,
                        10,
                        kwargs.get("epsd23")
                    ),
                    Field(
                        "epsc23",
                        float,
                        10,
                        10,
                        kwargs.get("epsc23")
                    ),
                    Field(
                        "cdam23",
                        float,
                        20,
                        10,
                        kwargs.get("cdam23")
                    ),
                    Field(
                        "epsd31",
                        float,
                        30,
                        10,
                        kwargs.get("epsd31")
                    ),
                    Field(
                        "epsc31",
                        float,
                        40,
                        10,
                        kwargs.get("epsc31")
                    ),
                    Field(
                        "cdam31",
                        float,
                        50,
                        10,
                        kwargs.get("cdam31")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatOrthotropicSimplifiedDamage.option_specs[0],
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
        """Get or set the Eb, Young's modulus in b-direction.
        """ # nopep8
        return self._cards[0].get_value("eb")

    @eb.setter
    def eb(self, value: float) -> None:
        self._cards[0].set_value("eb", value)

    @property
    def ec(self) -> typing.Optional[float]:
        """Get or set the Eb, Young's modulus in c-direction.
        """ # nopep8
        return self._cards[0].get_value("ec")

    @ec.setter
    def ec(self, value: float) -> None:
        self._cards[0].set_value("ec", value)

    @property
    def prba(self) -> typing.Optional[float]:
        """Get or set the Vba, Poisson's ratio, ba.
        """ # nopep8
        return self._cards[0].get_value("prba")

    @prba.setter
    def prba(self, value: float) -> None:
        self._cards[0].set_value("prba", value)

    @property
    def prca(self) -> typing.Optional[float]:
        """Get or set the Vca, Poisson's ratio, ca.
        """ # nopep8
        return self._cards[0].get_value("prca")

    @prca.setter
    def prca(self, value: float) -> None:
        self._cards[0].set_value("prca", value)

    @property
    def prcb(self) -> typing.Optional[float]:
        """Get or set the Vcb, Poisson's ratio, cb.
        """ # nopep8
        return self._cards[0].get_value("prcb")

    @prcb.setter
    def prcb(self, value: float) -> None:
        self._cards[0].set_value("prcb", value)

    @property
    def gab(self) -> typing.Optional[float]:
        """Get or set the Gab, Shear modulus, ab.
        """ # nopep8
        return self._cards[1].get_value("gab")

    @gab.setter
    def gab(self, value: float) -> None:
        self._cards[1].set_value("gab", value)

    @property
    def gbc(self) -> typing.Optional[float]:
        """Get or set the Gbc, Shear modulus, bc.
        """ # nopep8
        return self._cards[1].get_value("gbc")

    @gbc.setter
    def gbc(self, value: float) -> None:
        self._cards[1].set_value("gbc", value)

    @property
    def gca(self) -> typing.Optional[float]:
        """Get or set the Gca, Shear modulus, ca.
        """ # nopep8
        return self._cards[1].get_value("gca")

    @gca.setter
    def gca(self, value: float) -> None:
        self._cards[1].set_value("gca", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
        EQ.0.0:	Locally orthotropic with material axes determined by element nodes 1, 2,and 4, as with* DEFINE_COORDINATE_NODES.
        EQ.1.0 : Locally orthotropic with material axes determined by a point, P, in spaceand the global location of the element center; this is the a - direction.This option is for solid elements only.
        EQ.2.0:	Globally orthotropic with material axes determined by vectors defined below, as with* DEFINE_COORDINATE_VECTOR
        EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element.The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.Thus, for solid elements, AOPT = 3 is only available for hexahedrons.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle BETA.BETA may be set in the keyword input for the element or in the input for this keyword.Note that for solids, the material axes may be switched depending on the choice of MACF.The switch may occur before or after applying BETA depending on the value of MACF.
        EQ.4.0 : Locally orthotropic in a cylindrical coordinate system with the material axes determined by a vector v,and an originating point, P, which define the centerline axis.This option is for solid elements only.
        LT.0.0 : The absolute value of AOPT is a coordinate system ID number(CID on * DEFINE_COORDINATE_OPTION)
        """ # nopep8
        return self._cards[1].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[1].set_value("aopt", value)

    @property
    def macf(self) -> int:
        """Get or set the Material axes change flag for solid elements:
        EQ. - 4:	Switch material axes b and c before BETA rotation
        EQ. - 3 : Switch material axes a and c before BETA rotation
        EQ. - 2 : Switch material axes a and b before BETA rotation
        EQ.1 : No change, default
        EQ.2 : Switch material axes a and b after BETA rotation
        EQ.3 : Switch material axes a and c after BETA rotation
        EQ.4 : Switch material axes b and c after BETA rotation
        Figure Error!Reference source not found.indicates when LS - DYNA applies MACF during the process to obtain the final material axes.If BETA on * ELEMENT_SOLID_{OPTION} is defined, then that BETA is used for the rotation for all AOPT options.Otherwise, if AOPT = 3, the BETA input on Card 4 rotates the axes.For all other values of AOPT, the material axes will be switched as specified by MACF, but no BETA rotation will be performed.
        """ # nopep8
        return self._cards[1].get_value("macf")

    @macf.setter
    def macf(self, value: int) -> None:
        if value not in [1, 2, 3, 4, -2, -3, -4]:
            raise Exception("""macf must be one of {1,2,3,4,-2,-3,-4}""")
        self._cards[1].set_value("macf", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[2].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[2].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[2].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[2].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1.
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
        """Get or set the Components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[3].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[3].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[3].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[3].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.
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
        """Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SOLID_ORTHO.
        """ # nopep8
        return self._cards[3].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[3].set_value("beta", value)

    @property
    def nerode(self) -> int:
        """Get or set the Failure and erosion flag:
        EQ. 0: No failure (default)
        EQ. 1: Failure as soon as one failure criterion is reached in all
        integration points
        EQ. 2: Failure as soon as one failure criterion is reached in at least
        one integration point
        EQ. 3: Failure as soon as a tension or compression failure criterion
        in the a-direction is reached for one integration point
        EQ. 4: Failure as soon as a tension or compression failure criterion
        in the b-direction is reached for one integration point
        EQ. 5: Failure as soon as a tension or compression failure criterion
        in the c-direction is reached for one integration point
        EQ. 6: Failure as soon as tension or compression failure criteria in
        both the a- and b-directions are reached at a single integration
        point or at 2 different integration points
        EQ. 7: Failure as soon as tension or compression failure criteria in
        both the b- and c-directions are reached at a single integration
        point or at 2 different integration points
        EQ. 8: Failure as soon as tension or compression failure criteria in
        both the a- and c-directions are reached at a single integration
        point or at 2 different integration points
        EQ. 9: Failure as soon as tension or compression failure criteria in
        the 3 directions are reached at a single integration point or at
        different integration points.
        """ # nopep8
        return self._cards[4].get_value("nerode")

    @nerode.setter
    def nerode(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]:
            raise Exception("""nerode must be one of {0,1,2,3,4,5,6,7,8,9}""")
        self._cards[4].set_value("nerode", value)

    @property
    def ndam(self) -> int:
        """Get or set the Damage flag:
        EQ. 0: No damage (default)
        EQ. 1: Damage in tension only (null for compression)
        EQ. 2: Damage in tension and compression.
        """ # nopep8
        return self._cards[4].get_value("ndam")

    @ndam.setter
    def ndam(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""ndam must be one of {0,1,2}""")
        self._cards[4].set_value("ndam", value)

    @property
    def eps1tf(self) -> float:
        """Get or set the Failure strain in tension along the a-direction.
        """ # nopep8
        return self._cards[4].get_value("eps1tf")

    @eps1tf.setter
    def eps1tf(self, value: float) -> None:
        self._cards[4].set_value("eps1tf", value)

    @property
    def eps2tf(self) -> float:
        """Get or set the Failure strain in tension along the b-direction.
        """ # nopep8
        return self._cards[4].get_value("eps2tf")

    @eps2tf.setter
    def eps2tf(self, value: float) -> None:
        self._cards[4].set_value("eps2tf", value)

    @property
    def eps3tf(self) -> float:
        """Get or set the Failure strain in tension along the c-direction.
        """ # nopep8
        return self._cards[4].get_value("eps3tf")

    @eps3tf.setter
    def eps3tf(self, value: float) -> None:
        self._cards[4].set_value("eps3tf", value)

    @property
    def eps1cf(self) -> float:
        """Get or set the Failure strain in compression along the a-direction.
        """ # nopep8
        return self._cards[4].get_value("eps1cf")

    @eps1cf.setter
    def eps1cf(self, value: float) -> None:
        self._cards[4].set_value("eps1cf", value)

    @property
    def eps2cf(self) -> float:
        """Get or set the Failure strain in compression along the b-direction.
        """ # nopep8
        return self._cards[4].get_value("eps2cf")

    @eps2cf.setter
    def eps2cf(self, value: float) -> None:
        self._cards[4].set_value("eps2cf", value)

    @property
    def eps3cf(self) -> float:
        """Get or set the Failure strain in compression along the c-direction.
        """ # nopep8
        return self._cards[4].get_value("eps3cf")

    @eps3cf.setter
    def eps3cf(self, value: float) -> None:
        self._cards[4].set_value("eps3cf", value)

    @property
    def eps12f(self) -> float:
        """Get or set the Failure shear strain in the ab-plane.
        """ # nopep8
        return self._cards[5].get_value("eps12f")

    @eps12f.setter
    def eps12f(self, value: float) -> None:
        self._cards[5].set_value("eps12f", value)

    @property
    def eps23f(self) -> float:
        """Get or set the Failure shear strain in the bc-plane.
        """ # nopep8
        return self._cards[5].get_value("eps23f")

    @eps23f.setter
    def eps23f(self, value: float) -> None:
        self._cards[5].set_value("eps23f", value)

    @property
    def eps13f(self) -> float:
        """Get or set the Failure shear strain in the ac-plane.
        """ # nopep8
        return self._cards[5].get_value("eps13f")

    @eps13f.setter
    def eps13f(self, value: float) -> None:
        self._cards[5].set_value("eps13f", value)

    @property
    def epsd1t(self) -> typing.Optional[float]:
        """Get or set the Damage threshold in tension along the a-direction.
        """ # nopep8
        return self._cards[5].get_value("epsd1t")

    @epsd1t.setter
    def epsd1t(self, value: float) -> None:
        self._cards[5].set_value("epsd1t", value)

    @property
    def epsc1t(self) -> typing.Optional[float]:
        """Get or set the Critical damage threshold in tension along the a-direction.
        """ # nopep8
        return self._cards[5].get_value("epsc1t")

    @epsc1t.setter
    def epsc1t(self, value: float) -> None:
        self._cards[5].set_value("epsc1t", value)

    @property
    def cdam1t(self) -> typing.Optional[float]:
        """Get or set the Critical damage in tension along the a-direction.
        """ # nopep8
        return self._cards[5].get_value("cdam1t")

    @cdam1t.setter
    def cdam1t(self, value: float) -> None:
        self._cards[5].set_value("cdam1t", value)

    @property
    def epsd2t(self) -> typing.Optional[float]:
        """Get or set the Damage threshold in tension along the b-direction.
        """ # nopep8
        return self._cards[5].get_value("epsd2t")

    @epsd2t.setter
    def epsd2t(self, value: float) -> None:
        self._cards[5].set_value("epsd2t", value)

    @property
    def epsc2t(self) -> typing.Optional[float]:
        """Get or set the Critical damage threshold in tension along the b-direction.
        """ # nopep8
        return self._cards[5].get_value("epsc2t")

    @epsc2t.setter
    def epsc2t(self, value: float) -> None:
        self._cards[5].set_value("epsc2t", value)

    @property
    def cdam2t(self) -> float:
        """Get or set the Critical damage in tension along the b-direction.
        """ # nopep8
        return self._cards[6].get_value("cdam2t")

    @cdam2t.setter
    def cdam2t(self, value: float) -> None:
        self._cards[6].set_value("cdam2t", value)

    @property
    def epsd3t(self) -> float:
        """Get or set the Damage threshold in tension along the c-direction.
        """ # nopep8
        return self._cards[6].get_value("epsd3t")

    @epsd3t.setter
    def epsd3t(self, value: float) -> None:
        self._cards[6].set_value("epsd3t", value)

    @property
    def epsc3t(self) -> typing.Optional[float]:
        """Get or set the Critical damage threshold in tension along the c-direction.
        """ # nopep8
        return self._cards[6].get_value("epsc3t")

    @epsc3t.setter
    def epsc3t(self, value: float) -> None:
        self._cards[6].set_value("epsc3t", value)

    @property
    def cdam3t(self) -> typing.Optional[float]:
        """Get or set the Critical damage in tension along the c-direction.
        """ # nopep8
        return self._cards[6].get_value("cdam3t")

    @cdam3t.setter
    def cdam3t(self, value: float) -> None:
        self._cards[6].set_value("cdam3t", value)

    @property
    def epsd1c(self) -> typing.Optional[float]:
        """Get or set the Damage threshold in compression along the a-direction.
        """ # nopep8
        return self._cards[6].get_value("epsd1c")

    @epsd1c.setter
    def epsd1c(self, value: float) -> None:
        self._cards[6].set_value("epsd1c", value)

    @property
    def epsc1c(self) -> typing.Optional[float]:
        """Get or set the Critical damage threshold in compression along the a-direction.
        """ # nopep8
        return self._cards[6].get_value("epsc1c")

    @epsc1c.setter
    def epsc1c(self, value: float) -> None:
        self._cards[6].set_value("epsc1c", value)

    @property
    def cdam1c(self) -> typing.Optional[float]:
        """Get or set the Critical damage in compression along the a-direction.
        """ # nopep8
        return self._cards[6].get_value("cdam1c")

    @cdam1c.setter
    def cdam1c(self, value: float) -> None:
        self._cards[6].set_value("cdam1c", value)

    @property
    def epsd2c(self) -> typing.Optional[float]:
        """Get or set the Damage threshold in compression along the b-direction.
        """ # nopep8
        return self._cards[6].get_value("epsd2c")

    @epsd2c.setter
    def epsd2c(self, value: float) -> None:
        self._cards[6].set_value("epsd2c", value)

    @property
    def epsc2c(self) -> typing.Optional[float]:
        """Get or set the Critical damage threshold in compression along the b-direction.
        """ # nopep8
        return self._cards[7].get_value("epsc2c")

    @epsc2c.setter
    def epsc2c(self, value: float) -> None:
        self._cards[7].set_value("epsc2c", value)

    @property
    def cdam2c(self) -> typing.Optional[float]:
        """Get or set the Critical damage in compression along the b-direction.
        """ # nopep8
        return self._cards[7].get_value("cdam2c")

    @cdam2c.setter
    def cdam2c(self, value: float) -> None:
        self._cards[7].set_value("cdam2c", value)

    @property
    def epsd3c(self) -> typing.Optional[float]:
        """Get or set the Damage threshold in compression along the c-direction.
        """ # nopep8
        return self._cards[7].get_value("epsd3c")

    @epsd3c.setter
    def epsd3c(self, value: float) -> None:
        self._cards[7].set_value("epsd3c", value)

    @property
    def epsc3c(self) -> typing.Optional[float]:
        """Get or set the Critical damage threshold in compression along the c-direction.
        """ # nopep8
        return self._cards[7].get_value("epsc3c")

    @epsc3c.setter
    def epsc3c(self, value: float) -> None:
        self._cards[7].set_value("epsc3c", value)

    @property
    def cdam3c(self) -> typing.Optional[float]:
        """Get or set the Critical damage in compression along the c-direction.
        """ # nopep8
        return self._cards[7].get_value("cdam3c")

    @cdam3c.setter
    def cdam3c(self, value: float) -> None:
        self._cards[7].set_value("cdam3c", value)

    @property
    def epsd12(self) -> typing.Optional[float]:
        """Get or set the Damage threshold for shear in the ab-plane.
        """ # nopep8
        return self._cards[7].get_value("epsd12")

    @epsd12.setter
    def epsd12(self, value: float) -> None:
        self._cards[7].set_value("epsd12", value)

    @property
    def epsc12(self) -> typing.Optional[float]:
        """Get or set the Critical damage threshold for shear in the ab-plane.
        """ # nopep8
        return self._cards[7].get_value("epsc12")

    @epsc12.setter
    def epsc12(self, value: float) -> None:
        self._cards[7].set_value("epsc12", value)

    @property
    def cdam12(self) -> typing.Optional[float]:
        """Get or set the Critical damage for shear in the ab-plane.
        """ # nopep8
        return self._cards[7].get_value("cdam12")

    @cdam12.setter
    def cdam12(self, value: float) -> None:
        self._cards[7].set_value("cdam12", value)

    @property
    def epsd23(self) -> typing.Optional[float]:
        """Get or set the Damage threshold for shear in the bc-plane.
        """ # nopep8
        return self._cards[8].get_value("epsd23")

    @epsd23.setter
    def epsd23(self, value: float) -> None:
        self._cards[8].set_value("epsd23", value)

    @property
    def epsc23(self) -> typing.Optional[float]:
        """Get or set the Critical damage threshold for shear in the bc-plane.
        """ # nopep8
        return self._cards[8].get_value("epsc23")

    @epsc23.setter
    def epsc23(self, value: float) -> None:
        self._cards[8].set_value("epsc23", value)

    @property
    def cdam23(self) -> typing.Optional[float]:
        """Get or set the Critical damage for shear in the bc-plane.
        """ # nopep8
        return self._cards[8].get_value("cdam23")

    @cdam23.setter
    def cdam23(self, value: float) -> None:
        self._cards[8].set_value("cdam23", value)

    @property
    def epsd31(self) -> typing.Optional[float]:
        """Get or set the Damage threshold for shear in the ac-plane.
        """ # nopep8
        return self._cards[8].get_value("epsd31")

    @epsd31.setter
    def epsd31(self, value: float) -> None:
        self._cards[8].set_value("epsd31", value)

    @property
    def epsc31(self) -> typing.Optional[float]:
        """Get or set the Critical damage threshold for shear in the ac-plane.
        """ # nopep8
        return self._cards[8].get_value("epsc31")

    @epsc31.setter
    def epsc31(self, value: float) -> None:
        self._cards[8].set_value("epsc31", value)

    @property
    def cdam31(self) -> typing.Optional[float]:
        """Get or set the Critical damage for shear in the ac-plane.
        """ # nopep8
        return self._cards[8].get_value("cdam31")

    @cdam31.setter
    def cdam31(self, value: float) -> None:
        self._cards[8].set_value("cdam31", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[9].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[9].cards[0].set_value("title", value)

