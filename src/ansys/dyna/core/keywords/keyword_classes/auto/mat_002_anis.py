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

class Mat002Anis(KeywordBase):
    """DYNA MAT_002_ANIS keyword"""

    keyword = "MAT"
    subkeyword = "002_ANIS"
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
                        "c11",
                        float,
                        20,
                        10,
                        kwargs.get("c11")
                    ),
                    Field(
                        "c12",
                        float,
                        30,
                        10,
                        kwargs.get("c12")
                    ),
                    Field(
                        "c22",
                        float,
                        40,
                        10,
                        kwargs.get("c22")
                    ),
                    Field(
                        "c13",
                        float,
                        50,
                        10,
                        kwargs.get("c13")
                    ),
                    Field(
                        "c23",
                        float,
                        60,
                        10,
                        kwargs.get("c23")
                    ),
                    Field(
                        "c33",
                        float,
                        70,
                        10,
                        kwargs.get("c33")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c14",
                        float,
                        0,
                        10,
                        kwargs.get("c14")
                    ),
                    Field(
                        "c24",
                        float,
                        10,
                        10,
                        kwargs.get("c24")
                    ),
                    Field(
                        "c34",
                        float,
                        20,
                        10,
                        kwargs.get("c34")
                    ),
                    Field(
                        "c44",
                        float,
                        30,
                        10,
                        kwargs.get("c44")
                    ),
                    Field(
                        "c15",
                        float,
                        40,
                        10,
                        kwargs.get("c15")
                    ),
                    Field(
                        "c25",
                        float,
                        50,
                        10,
                        kwargs.get("c25")
                    ),
                    Field(
                        "c35",
                        float,
                        60,
                        10,
                        kwargs.get("c35")
                    ),
                    Field(
                        "c45",
                        float,
                        70,
                        10,
                        kwargs.get("c45")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c55",
                        float,
                        0,
                        10,
                        kwargs.get("c55")
                    ),
                    Field(
                        "c16",
                        float,
                        10,
                        10,
                        kwargs.get("c16")
                    ),
                    Field(
                        "c26",
                        float,
                        20,
                        10,
                        kwargs.get("c26")
                    ),
                    Field(
                        "c36",
                        float,
                        30,
                        10,
                        kwargs.get("c36")
                    ),
                    Field(
                        "c46",
                        float,
                        40,
                        10,
                        kwargs.get("c46")
                    ),
                    Field(
                        "c56",
                        float,
                        50,
                        10,
                        kwargs.get("c56")
                    ),
                    Field(
                        "c66",
                        float,
                        60,
                        10,
                        kwargs.get("c66")
                    ),
                    Field(
                        "aopt",
                        float,
                        70,
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
                    Field(
                        "macf",
                        int,
                        60,
                        10,
                        kwargs.get("macf", 1)
                    ),
                    Field(
                        "ihis",
                        int,
                        70,
                        10,
                        kwargs.get("ihis", 0)
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
                        "ref",
                        float,
                        70,
                        10,
                        kwargs.get("ref", 0.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat002Anis.option_specs[0],
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
    def c11(self) -> typing.Optional[float]:
        """Get or set the The 1,1 term in the 6 x 6 anisotropic constitutive matrix. Note that 1 corresponds to the a material direction
        """ # nopep8
        return self._cards[0].get_value("c11")

    @c11.setter
    def c11(self, value: float) -> None:
        self._cards[0].set_value("c11", value)

    @property
    def c12(self) -> typing.Optional[float]:
        """Get or set the The 1,2 term in the 6 x 6 anisotropic constitutive matrix. Note that 2 corresponds to the b material direction
        """ # nopep8
        return self._cards[0].get_value("c12")

    @c12.setter
    def c12(self, value: float) -> None:
        self._cards[0].set_value("c12", value)

    @property
    def c22(self) -> typing.Optional[float]:
        """Get or set the The 2,2 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[0].get_value("c22")

    @c22.setter
    def c22(self, value: float) -> None:
        self._cards[0].set_value("c22", value)

    @property
    def c13(self) -> typing.Optional[float]:
        """Get or set the The 1,3 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[0].get_value("c13")

    @c13.setter
    def c13(self, value: float) -> None:
        self._cards[0].set_value("c13", value)

    @property
    def c23(self) -> typing.Optional[float]:
        """Get or set the The 2,3 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[0].get_value("c23")

    @c23.setter
    def c23(self, value: float) -> None:
        self._cards[0].set_value("c23", value)

    @property
    def c33(self) -> typing.Optional[float]:
        """Get or set the The 3,3 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[0].get_value("c33")

    @c33.setter
    def c33(self, value: float) -> None:
        self._cards[0].set_value("c33", value)

    @property
    def c14(self) -> typing.Optional[float]:
        """Get or set the The 1,4 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[1].get_value("c14")

    @c14.setter
    def c14(self, value: float) -> None:
        self._cards[1].set_value("c14", value)

    @property
    def c24(self) -> typing.Optional[float]:
        """Get or set the The 2,4 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[1].get_value("c24")

    @c24.setter
    def c24(self, value: float) -> None:
        self._cards[1].set_value("c24", value)

    @property
    def c34(self) -> typing.Optional[float]:
        """Get or set the The 3,4 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[1].get_value("c34")

    @c34.setter
    def c34(self, value: float) -> None:
        self._cards[1].set_value("c34", value)

    @property
    def c44(self) -> typing.Optional[float]:
        """Get or set the The 4,4 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[1].get_value("c44")

    @c44.setter
    def c44(self, value: float) -> None:
        self._cards[1].set_value("c44", value)

    @property
    def c15(self) -> typing.Optional[float]:
        """Get or set the The 1,5 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[1].get_value("c15")

    @c15.setter
    def c15(self, value: float) -> None:
        self._cards[1].set_value("c15", value)

    @property
    def c25(self) -> typing.Optional[float]:
        """Get or set the The 2,5 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[1].get_value("c25")

    @c25.setter
    def c25(self, value: float) -> None:
        self._cards[1].set_value("c25", value)

    @property
    def c35(self) -> typing.Optional[float]:
        """Get or set the The 3,5 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[1].get_value("c35")

    @c35.setter
    def c35(self, value: float) -> None:
        self._cards[1].set_value("c35", value)

    @property
    def c45(self) -> typing.Optional[float]:
        """Get or set the The 4,5 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[1].get_value("c45")

    @c45.setter
    def c45(self, value: float) -> None:
        self._cards[1].set_value("c45", value)

    @property
    def c55(self) -> typing.Optional[float]:
        """Get or set the The 1,4 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[2].get_value("c55")

    @c55.setter
    def c55(self, value: float) -> None:
        self._cards[2].set_value("c55", value)

    @property
    def c16(self) -> typing.Optional[float]:
        """Get or set the The 2,4 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[2].get_value("c16")

    @c16.setter
    def c16(self, value: float) -> None:
        self._cards[2].set_value("c16", value)

    @property
    def c26(self) -> typing.Optional[float]:
        """Get or set the The 3,4 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[2].get_value("c26")

    @c26.setter
    def c26(self, value: float) -> None:
        self._cards[2].set_value("c26", value)

    @property
    def c36(self) -> typing.Optional[float]:
        """Get or set the The 4,4 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[2].get_value("c36")

    @c36.setter
    def c36(self, value: float) -> None:
        self._cards[2].set_value("c36", value)

    @property
    def c46(self) -> typing.Optional[float]:
        """Get or set the The 1,5 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[2].get_value("c46")

    @c46.setter
    def c46(self, value: float) -> None:
        self._cards[2].set_value("c46", value)

    @property
    def c56(self) -> typing.Optional[float]:
        """Get or set the The 2,5 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[2].get_value("c56")

    @c56.setter
    def c56(self, value: float) -> None:
        self._cards[2].set_value("c56", value)

    @property
    def c66(self) -> typing.Optional[float]:
        """Get or set the The 3,5 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[2].get_value("c66")

    @c66.setter
    def c66(self, value: float) -> None:
        self._cards[2].set_value("c66", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option (see Figure 0-1 and the Material Directions section):
        EQ.0.0:	Locally orthotropic with material axes determined by element nodes as shown in Figure 0 - 1.  The a - direction is from node 1 to node 2 of the element.The b - direction is orthogonal to the a - direction and is in the plane formed by nodes 1, 2,and 4. When this option is used in two - dimensional planar and axisymmetric analysis, it is critical that the nodes in the element definition be numbered counterclockwise for this option to work correctly.For shells only, the material axes are then rotated about the normal vector to the surface of the shell by the angle BETA.
        EQ.1.0 : Locally orthotropic with material axes determined by a point, P, in spaceand the global location of the element center; this is the a - direction.This option is for solid elements only.
        EQ.2.0:	Globally orthotropic with material axes determined by vectors a and d input below, as with* DEFINE_COORDINATE_VECTOR
        EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element(see Figure 0 - 1).The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.Thus, for solid elements, AOPT = 3 is only available for hexahedrons.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle BETA.BETA may be set in the keyword input for the element or in the input for this keyword.Note that for solids, the material axes may be switched depending on the choice of MACF.The switch may occur before or after applying BETA depending on the value of MACF.
        EQ.4.0 : Locally orthotropic in a cylindrical coordinate system with the material axes determined by a vector v, and an originating point, P, which define the centerline axis.This option is for solid elements only.
        LT.0.0 : |AOPT| is a coordinate system ID(see * DEFINE_COORDINATE_OPTION).
        """ # nopep8
        return self._cards[2].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[2].set_value("aopt", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the x-coordinate of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[3].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[3].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the y-coordinate of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[3].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[3].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the z-coordinate of point p for AOPT = 1 and 4.
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
    def macf(self) -> int:
        """Get or set the Material axes change flag for solid elements:
        EQ.-4:	Switch material axes b and c before BETA rotation
        EQ. - 3: Switch material axes a and c before BETA rotation
        EQ. - 2: Switch material axes a and b before BETA rotation
        EQ.1:  No change, default,
        EQ.2:  switch material axes a and b,
        EQ.3:  switch material axes a and c,
        EQ.4:  switch material axes b and c.
        """ # nopep8
        return self._cards[3].get_value("macf")

    @macf.setter
    def macf(self, value: int) -> None:
        if value not in [1, -4, -3, -2, 2, 3, 4]:
            raise Exception("""macf must be one of {1,-4,-3,-2,2,3,4}""")
        self._cards[3].set_value("macf", value)

    @property
    def ihis(self) -> int:
        """Get or set the Flag for anisotropic stiffness terms initialization (for solid elements only):
        EQ.0:	C11, C12, … from Cards 1b.1, 1b.2,and 1.b3 are used.
        EQ.1 : C11, C12, … are initialized with history data from* INITIAL_‌STRESS_‌SOLID
        """ # nopep8
        return self._cards[3].get_value("ihis")

    @ihis.setter
    def ihis(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ihis must be one of {0,1}""")
        self._cards[3].set_value("ihis", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[4].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[4].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[4].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[4].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3 and 4.
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
        """Get or set the Material angle in degrees for AOPT = 3, which may be overridden on the element card, see *ELEMENT_SHELL.
        """ # nopep8
        return self._cards[4].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[4].set_value("beta", value)

    @property
    def ref(self) -> float:
        """Get or set the Flag to use reference geometry specified with *INITIAL_FOAM_REFERENCE_GEOMETRY to initialize the stress tensor.
        EQ.0.0:	Off
        EQ.1.0 : On
        """ # nopep8
        return self._cards[4].get_value("ref")

    @ref.setter
    def ref(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""ref must be one of {0.0,1.0}""")
        self._cards[4].set_value("ref", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[5].cards[0].set_value("title", value)

