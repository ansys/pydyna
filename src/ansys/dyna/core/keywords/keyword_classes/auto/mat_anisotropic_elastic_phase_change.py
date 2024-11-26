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

class MatAnisotropicElasticPhaseChange(KeywordBase):
    """DYNA MAT_ANISOTROPIC_ELASTIC_PHASE_CHANGE keyword"""

    keyword = "MAT"
    subkeyword = "ANISOTROPIC_ELASTIC_PHASE_CHANGE"
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
                        "c111",
                        float,
                        20,
                        10,
                        kwargs.get("c111")
                    ),
                    Field(
                        "c121",
                        float,
                        30,
                        10,
                        kwargs.get("c121")
                    ),
                    Field(
                        "c221",
                        float,
                        40,
                        10,
                        kwargs.get("c221")
                    ),
                    Field(
                        "c131",
                        float,
                        50,
                        10,
                        kwargs.get("c131")
                    ),
                    Field(
                        "c231",
                        float,
                        60,
                        10,
                        kwargs.get("c231")
                    ),
                    Field(
                        "c331",
                        float,
                        70,
                        10,
                        kwargs.get("c331")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c141",
                        float,
                        0,
                        10,
                        kwargs.get("c141")
                    ),
                    Field(
                        "c241",
                        float,
                        10,
                        10,
                        kwargs.get("c241")
                    ),
                    Field(
                        "c341",
                        float,
                        20,
                        10,
                        kwargs.get("c341")
                    ),
                    Field(
                        "c441",
                        float,
                        30,
                        10,
                        kwargs.get("c441")
                    ),
                    Field(
                        "c151",
                        float,
                        40,
                        10,
                        kwargs.get("c151")
                    ),
                    Field(
                        "c251",
                        float,
                        50,
                        10,
                        kwargs.get("c251")
                    ),
                    Field(
                        "c351",
                        float,
                        60,
                        10,
                        kwargs.get("c351")
                    ),
                    Field(
                        "c451",
                        float,
                        70,
                        10,
                        kwargs.get("c451")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c551",
                        float,
                        0,
                        10,
                        kwargs.get("c551")
                    ),
                    Field(
                        "c161",
                        float,
                        10,
                        10,
                        kwargs.get("c161")
                    ),
                    Field(
                        "c261",
                        float,
                        20,
                        10,
                        kwargs.get("c261")
                    ),
                    Field(
                        "c361",
                        float,
                        30,
                        10,
                        kwargs.get("c361")
                    ),
                    Field(
                        "c461",
                        float,
                        40,
                        10,
                        kwargs.get("c461")
                    ),
                    Field(
                        "c561",
                        float,
                        50,
                        10,
                        kwargs.get("c561")
                    ),
                    Field(
                        "c661",
                        float,
                        60,
                        10,
                        kwargs.get("c661")
                    ),
                    Field(
                        "aopt1",
                        float,
                        70,
                        10,
                        kwargs.get("aopt1")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xp1",
                        float,
                        0,
                        10,
                        kwargs.get("xp1")
                    ),
                    Field(
                        "yp1",
                        float,
                        10,
                        10,
                        kwargs.get("yp1")
                    ),
                    Field(
                        "zp1",
                        float,
                        20,
                        10,
                        kwargs.get("zp1")
                    ),
                    Field(
                        "a11",
                        float,
                        30,
                        10,
                        kwargs.get("a11")
                    ),
                    Field(
                        "a21",
                        float,
                        40,
                        10,
                        kwargs.get("a21")
                    ),
                    Field(
                        "a31",
                        float,
                        50,
                        10,
                        kwargs.get("a31")
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
                        "v11",
                        float,
                        0,
                        10,
                        kwargs.get("v11")
                    ),
                    Field(
                        "v21",
                        float,
                        10,
                        10,
                        kwargs.get("v21")
                    ),
                    Field(
                        "v31",
                        float,
                        20,
                        10,
                        kwargs.get("v31")
                    ),
                    Field(
                        "d11",
                        float,
                        30,
                        10,
                        kwargs.get("d11")
                    ),
                    Field(
                        "d21",
                        float,
                        40,
                        10,
                        kwargs.get("d21")
                    ),
                    Field(
                        "d31",
                        float,
                        50,
                        10,
                        kwargs.get("d31")
                    ),
                    Field(
                        "beta1",
                        float,
                        60,
                        10,
                        kwargs.get("beta1")
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
            Card(
                [
                    Field(
                        "unused",
                        float,
                        0,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        float,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "c112",
                        float,
                        20,
                        10,
                        kwargs.get("c112")
                    ),
                    Field(
                        "c122",
                        float,
                        30,
                        10,
                        kwargs.get("c122")
                    ),
                    Field(
                        "c222",
                        float,
                        40,
                        10,
                        kwargs.get("c222")
                    ),
                    Field(
                        "c132",
                        float,
                        50,
                        10,
                        kwargs.get("c132")
                    ),
                    Field(
                        "c232",
                        float,
                        60,
                        10,
                        kwargs.get("c232")
                    ),
                    Field(
                        "c332",
                        float,
                        70,
                        10,
                        kwargs.get("c332")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c142",
                        float,
                        0,
                        10,
                        kwargs.get("c142")
                    ),
                    Field(
                        "c242",
                        float,
                        10,
                        10,
                        kwargs.get("c242")
                    ),
                    Field(
                        "c342",
                        float,
                        20,
                        10,
                        kwargs.get("c342")
                    ),
                    Field(
                        "c442",
                        float,
                        30,
                        10,
                        kwargs.get("c442")
                    ),
                    Field(
                        "c152",
                        float,
                        40,
                        10,
                        kwargs.get("c152")
                    ),
                    Field(
                        "c252",
                        float,
                        50,
                        10,
                        kwargs.get("c252")
                    ),
                    Field(
                        "c352",
                        float,
                        60,
                        10,
                        kwargs.get("c352")
                    ),
                    Field(
                        "c452",
                        float,
                        70,
                        10,
                        kwargs.get("c452")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c552",
                        float,
                        0,
                        10,
                        kwargs.get("c552")
                    ),
                    Field(
                        "c162",
                        float,
                        10,
                        10,
                        kwargs.get("c162")
                    ),
                    Field(
                        "c262",
                        float,
                        20,
                        10,
                        kwargs.get("c262")
                    ),
                    Field(
                        "c362",
                        float,
                        30,
                        10,
                        kwargs.get("c362")
                    ),
                    Field(
                        "c462",
                        float,
                        40,
                        10,
                        kwargs.get("c462")
                    ),
                    Field(
                        "c562",
                        float,
                        50,
                        10,
                        kwargs.get("c562")
                    ),
                    Field(
                        "c662",
                        float,
                        60,
                        10,
                        kwargs.get("c662")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xp2",
                        float,
                        0,
                        10,
                        kwargs.get("xp2")
                    ),
                    Field(
                        "yp2",
                        float,
                        10,
                        10,
                        kwargs.get("yp2")
                    ),
                    Field(
                        "zp2",
                        float,
                        20,
                        10,
                        kwargs.get("zp2")
                    ),
                    Field(
                        "a12",
                        float,
                        30,
                        10,
                        kwargs.get("a12")
                    ),
                    Field(
                        "a22",
                        float,
                        40,
                        10,
                        kwargs.get("a22")
                    ),
                    Field(
                        "a32",
                        float,
                        50,
                        10,
                        kwargs.get("a32")
                    ),
                    Field(
                        "xp2",
                        float,
                        0,
                        10,
                        kwargs.get("xp2")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "v12",
                        float,
                        0,
                        10,
                        kwargs.get("v12")
                    ),
                    Field(
                        "v22",
                        float,
                        10,
                        10,
                        kwargs.get("v22")
                    ),
                    Field(
                        "v32",
                        float,
                        20,
                        10,
                        kwargs.get("v32")
                    ),
                    Field(
                        "d12",
                        float,
                        30,
                        10,
                        kwargs.get("d12")
                    ),
                    Field(
                        "d22",
                        float,
                        40,
                        10,
                        kwargs.get("d22")
                    ),
                    Field(
                        "d32",
                        float,
                        50,
                        10,
                        kwargs.get("d32")
                    ),
                    Field(
                        "beta2",
                        float,
                        60,
                        10,
                        kwargs.get("beta2")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "x1",
                        float,
                        0,
                        10,
                        kwargs.get("x1")
                    ),
                    Field(
                        "y1",
                        float,
                        10,
                        10,
                        kwargs.get("y1")
                    ),
                    Field(
                        "z1",
                        float,
                        20,
                        10,
                        kwargs.get("z1")
                    ),
                    Field(
                        "x2",
                        float,
                        30,
                        10,
                        kwargs.get("x2")
                    ),
                    Field(
                        "y2",
                        float,
                        40,
                        10,
                        kwargs.get("y2")
                    ),
                    Field(
                        "z2",
                        float,
                        50,
                        10,
                        kwargs.get("z2")
                    ),
                    Field(
                        "thkfac",
                        float,
                        60,
                        10,
                        kwargs.get("thkfac", 1.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatAnisotropicElasticPhaseChange.option_specs[0],
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
        """Get or set the Material identification. A unique number has to be chosen.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def c111(self) -> typing.Optional[float]:
        """Get or set the The 1,1 term in the 6x6 anisotropic constitutive matrix for phase i. Note that 1 corresponds to the a material direction
        """ # nopep8
        return self._cards[0].get_value("c111")

    @c111.setter
    def c111(self, value: float) -> None:
        self._cards[0].set_value("c111", value)

    @property
    def c121(self) -> typing.Optional[float]:
        """Get or set the The 1,2 term in the 6x6 anisotropic constitutive matrix for phase i. Note that 2 corresponds to the b material direction
        """ # nopep8
        return self._cards[0].get_value("c121")

    @c121.setter
    def c121(self, value: float) -> None:
        self._cards[0].set_value("c121", value)

    @property
    def c221(self) -> typing.Optional[float]:
        """Get or set the The 2,2 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[0].get_value("c221")

    @c221.setter
    def c221(self, value: float) -> None:
        self._cards[0].set_value("c221", value)

    @property
    def c131(self) -> typing.Optional[float]:
        """Get or set the The 1,3 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[0].get_value("c131")

    @c131.setter
    def c131(self, value: float) -> None:
        self._cards[0].set_value("c131", value)

    @property
    def c231(self) -> typing.Optional[float]:
        """Get or set the The 2,3 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[0].get_value("c231")

    @c231.setter
    def c231(self, value: float) -> None:
        self._cards[0].set_value("c231", value)

    @property
    def c331(self) -> typing.Optional[float]:
        """Get or set the The 3,3 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[0].get_value("c331")

    @c331.setter
    def c331(self, value: float) -> None:
        self._cards[0].set_value("c331", value)

    @property
    def c141(self) -> typing.Optional[float]:
        """Get or set the The 1,4 term in the 6x6 anisotropic constitutive matrix for phase i.
        """ # nopep8
        return self._cards[1].get_value("c141")

    @c141.setter
    def c141(self, value: float) -> None:
        self._cards[1].set_value("c141", value)

    @property
    def c241(self) -> typing.Optional[float]:
        """Get or set the The 2,4 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[1].get_value("c241")

    @c241.setter
    def c241(self, value: float) -> None:
        self._cards[1].set_value("c241", value)

    @property
    def c341(self) -> typing.Optional[float]:
        """Get or set the The 3,4 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[1].get_value("c341")

    @c341.setter
    def c341(self, value: float) -> None:
        self._cards[1].set_value("c341", value)

    @property
    def c441(self) -> typing.Optional[float]:
        """Get or set the The 4,4 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[1].get_value("c441")

    @c441.setter
    def c441(self, value: float) -> None:
        self._cards[1].set_value("c441", value)

    @property
    def c151(self) -> typing.Optional[float]:
        """Get or set the The 1,5 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[1].get_value("c151")

    @c151.setter
    def c151(self, value: float) -> None:
        self._cards[1].set_value("c151", value)

    @property
    def c251(self) -> typing.Optional[float]:
        """Get or set the The 2, 5 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[1].get_value("c251")

    @c251.setter
    def c251(self, value: float) -> None:
        self._cards[1].set_value("c251", value)

    @property
    def c351(self) -> typing.Optional[float]:
        """Get or set the The 3,5 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[1].get_value("c351")

    @c351.setter
    def c351(self, value: float) -> None:
        self._cards[1].set_value("c351", value)

    @property
    def c451(self) -> typing.Optional[float]:
        """Get or set the The 4,5 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[1].get_value("c451")

    @c451.setter
    def c451(self, value: float) -> None:
        self._cards[1].set_value("c451", value)

    @property
    def c551(self) -> typing.Optional[float]:
        """Get or set the The 5,5 term in the 6x6 anisotropic constitutive matrix for phase i.
        """ # nopep8
        return self._cards[2].get_value("c551")

    @c551.setter
    def c551(self, value: float) -> None:
        self._cards[2].set_value("c551", value)

    @property
    def c161(self) -> typing.Optional[float]:
        """Get or set the The 1,6 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[2].get_value("c161")

    @c161.setter
    def c161(self, value: float) -> None:
        self._cards[2].set_value("c161", value)

    @property
    def c261(self) -> typing.Optional[float]:
        """Get or set the The 2,6 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[2].get_value("c261")

    @c261.setter
    def c261(self, value: float) -> None:
        self._cards[2].set_value("c261", value)

    @property
    def c361(self) -> typing.Optional[float]:
        """Get or set the The 3,6 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[2].get_value("c361")

    @c361.setter
    def c361(self, value: float) -> None:
        self._cards[2].set_value("c361", value)

    @property
    def c461(self) -> typing.Optional[float]:
        """Get or set the The 4,6 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[2].get_value("c461")

    @c461.setter
    def c461(self, value: float) -> None:
        self._cards[2].set_value("c461", value)

    @property
    def c561(self) -> typing.Optional[float]:
        """Get or set the The 5,6 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[2].get_value("c561")

    @c561.setter
    def c561(self, value: float) -> None:
        self._cards[2].set_value("c561", value)

    @property
    def c661(self) -> typing.Optional[float]:
        """Get or set the The 6,6 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[2].get_value("c661")

    @c661.setter
    def c661(self, value: float) -> None:
        self._cards[2].set_value("c661", value)

    @property
    def aopt1(self) -> typing.Optional[float]:
        """Get or set the Material axes option for phase i, see Figure M2-1.
        EQ.0.0: locally orthotropic with material axes determined by element nodes as shown in part (a) of Figure M2-1. The
        a-direction is from node 1 to node 2 of the element. The b-direction is orthogonal to the a-direction and is in the
        plane formed by nodes 1, 2, and 4. When this option is used in two-dimensional planar and axisymmetric analysis,
        it is critical that the nodes in the element definition be numbered counterclockwise for this option to work correctly.
        EQ.1.0: locally orthotropic with material axes determined by a
        point in space and the global location of the element center; this is the a-direction. This option is for solid elements only.
        EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDINATE_VECTOR.
        EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,
        BETA, from a line in the plane of the element defined by the cross product of the vector v with the element normal.
        The plane of a solid element is the midsurface between the inner surface and outer surface defined by the
        first four nodes and the last four nodes of the connectivity of the element, respectively.
        EQ.4.0: locally orthotropic in cylindrical coordinate system with
        the material axes determined by a vector v, and an originating point, P, which define the centerline axis. This option is for solid elements only.
        LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
        *DEFINE_COORDINATE_SYSTEM or *DEFINE_COORDINATE_VECTOR). Available in R3 version of 971 and later.
        """ # nopep8
        return self._cards[2].get_value("aopt1")

    @aopt1.setter
    def aopt1(self, value: float) -> None:
        self._cards[2].set_value("aopt1", value)

    @property
    def xp1(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of the i th phase's point P for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[3].get_value("xp1")

    @xp1.setter
    def xp1(self, value: float) -> None:
        self._cards[3].set_value("xp1", value)

    @property
    def yp1(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of the i th phase's point P for AOPT = 1 and 4
        """ # nopep8
        return self._cards[3].get_value("yp1")

    @yp1.setter
    def yp1(self, value: float) -> None:
        self._cards[3].set_value("yp1", value)

    @property
    def zp1(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of the i th phase's point P for AOPT = 1 and 4
        """ # nopep8
        return self._cards[3].get_value("zp1")

    @zp1.setter
    def zp1(self, value: float) -> None:
        self._cards[3].set_value("zp1", value)

    @property
    def a11(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector a for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("a11")

    @a11.setter
    def a11(self, value: float) -> None:
        self._cards[3].set_value("a11", value)

    @property
    def a21(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector a for AOPT = 2
        """ # nopep8
        return self._cards[3].get_value("a21")

    @a21.setter
    def a21(self, value: float) -> None:
        self._cards[3].set_value("a21", value)

    @property
    def a31(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector a for AOPT = 2
        """ # nopep8
        return self._cards[3].get_value("a31")

    @a31.setter
    def a31(self, value: float) -> None:
        self._cards[3].set_value("a31", value)

    @property
    def macf(self) -> int:
        """Get or set the Material axes change flag for brick elements in phase i:
        EQ.1: No change, default,
        EQ.2: switch material axes a and b,
        EQ.3: switch material axes a and c,
        EQ.4: switch material axes b and c.
        """ # nopep8
        return self._cards[3].get_value("macf")

    @macf.setter
    def macf(self, value: int) -> None:
        if value not in [1, 2, 3, 4]:
            raise Exception("""macf must be one of {1,2,3,4}""")
        self._cards[3].set_value("macf", value)

    @property
    def ihis(self) -> int:
        """Get or set the Flag for anisotropic stiffness terms initialization (for solid elements only).
        EQ.0: C11, C12, … from Cards 1, 2, and 3 are used.
        EQ.1: C11, C12, … are initialized by *INITIAL_STRESS_SOLID's	history data.
        """ # nopep8
        return self._cards[3].get_value("ihis")

    @ihis.setter
    def ihis(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ihis must be one of {0,1}""")
        self._cards[3].set_value("ihis", value)

    @property
    def v11(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[4].get_value("v11")

    @v11.setter
    def v11(self, value: float) -> None:
        self._cards[4].set_value("v11", value)

    @property
    def v21(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector v for AOPT = 3 and 4
        """ # nopep8
        return self._cards[4].get_value("v21")

    @v21.setter
    def v21(self, value: float) -> None:
        self._cards[4].set_value("v21", value)

    @property
    def v31(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector v for AOPT = 3 and 4
        """ # nopep8
        return self._cards[4].get_value("v31")

    @v31.setter
    def v31(self, value: float) -> None:
        self._cards[4].set_value("v31", value)

    @property
    def d11(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector d for AOPT = 2
        """ # nopep8
        return self._cards[4].get_value("d11")

    @d11.setter
    def d11(self, value: float) -> None:
        self._cards[4].set_value("d11", value)

    @property
    def d21(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector d for AOPT = 2
        """ # nopep8
        return self._cards[4].get_value("d21")

    @d21.setter
    def d21(self, value: float) -> None:
        self._cards[4].set_value("d21", value)

    @property
    def d31(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector d for AOPT = 2
        """ # nopep8
        return self._cards[4].get_value("d31")

    @d31.setter
    def d31(self, value: float) -> None:
        self._cards[4].set_value("d31", value)

    @property
    def beta1(self) -> typing.Optional[float]:
        """Get or set the Material angle of i th phase in degrees for AOPT = 3, may be
        overridden on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_SOLID_ORTHO.
        """ # nopep8
        return self._cards[4].get_value("beta1")

    @beta1.setter
    def beta1(self, value: float) -> None:
        self._cards[4].set_value("beta1", value)

    @property
    def ref(self) -> float:
        """Get or set the Use reference geometry to initialize the stress tensor for the i th phase.
        The reference geometry is defined by the keyword: *INITIAL_FOAM_REFERENCE_GEOMETRY (see there for more details).
        EQ.0.0: off,
        EQ.1.0: on.
        """ # nopep8
        return self._cards[4].get_value("ref")

    @ref.setter
    def ref(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""ref must be one of {0.0,1.0}""")
        self._cards[4].set_value("ref", value)

    @property
    def c112(self) -> typing.Optional[float]:
        """Get or set the The 1,1 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[5].get_value("c112")

    @c112.setter
    def c112(self, value: float) -> None:
        self._cards[5].set_value("c112", value)

    @property
    def c122(self) -> typing.Optional[float]:
        """Get or set the The 1,2 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[5].get_value("c122")

    @c122.setter
    def c122(self, value: float) -> None:
        self._cards[5].set_value("c122", value)

    @property
    def c222(self) -> typing.Optional[float]:
        """Get or set the The 2,2 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[5].get_value("c222")

    @c222.setter
    def c222(self, value: float) -> None:
        self._cards[5].set_value("c222", value)

    @property
    def c132(self) -> typing.Optional[float]:
        """Get or set the The 1,3 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[5].get_value("c132")

    @c132.setter
    def c132(self, value: float) -> None:
        self._cards[5].set_value("c132", value)

    @property
    def c232(self) -> typing.Optional[float]:
        """Get or set the The 2, 3 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[5].get_value("c232")

    @c232.setter
    def c232(self, value: float) -> None:
        self._cards[5].set_value("c232", value)

    @property
    def c332(self) -> typing.Optional[float]:
        """Get or set the The 3,3 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[5].get_value("c332")

    @c332.setter
    def c332(self, value: float) -> None:
        self._cards[5].set_value("c332", value)

    @property
    def c142(self) -> typing.Optional[float]:
        """Get or set the The 1,4 term in the 6x6 anisotropic constitutive matrix for phase i.
        """ # nopep8
        return self._cards[6].get_value("c142")

    @c142.setter
    def c142(self, value: float) -> None:
        self._cards[6].set_value("c142", value)

    @property
    def c242(self) -> typing.Optional[float]:
        """Get or set the The 2,4 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[6].get_value("c242")

    @c242.setter
    def c242(self, value: float) -> None:
        self._cards[6].set_value("c242", value)

    @property
    def c342(self) -> typing.Optional[float]:
        """Get or set the The 3,4 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[6].get_value("c342")

    @c342.setter
    def c342(self, value: float) -> None:
        self._cards[6].set_value("c342", value)

    @property
    def c442(self) -> typing.Optional[float]:
        """Get or set the The 4,4 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[6].get_value("c442")

    @c442.setter
    def c442(self, value: float) -> None:
        self._cards[6].set_value("c442", value)

    @property
    def c152(self) -> typing.Optional[float]:
        """Get or set the The 1,5 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[6].get_value("c152")

    @c152.setter
    def c152(self, value: float) -> None:
        self._cards[6].set_value("c152", value)

    @property
    def c252(self) -> typing.Optional[float]:
        """Get or set the The 2,5 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[6].get_value("c252")

    @c252.setter
    def c252(self, value: float) -> None:
        self._cards[6].set_value("c252", value)

    @property
    def c352(self) -> typing.Optional[float]:
        """Get or set the The 3,5 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[6].get_value("c352")

    @c352.setter
    def c352(self, value: float) -> None:
        self._cards[6].set_value("c352", value)

    @property
    def c452(self) -> typing.Optional[float]:
        """Get or set the The 4,5 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[6].get_value("c452")

    @c452.setter
    def c452(self, value: float) -> None:
        self._cards[6].set_value("c452", value)

    @property
    def c552(self) -> typing.Optional[float]:
        """Get or set the The 5,5 term in the 6x6 anisotropic constitutive matrix for phase i.
        """ # nopep8
        return self._cards[7].get_value("c552")

    @c552.setter
    def c552(self, value: float) -> None:
        self._cards[7].set_value("c552", value)

    @property
    def c162(self) -> typing.Optional[float]:
        """Get or set the The 1,6 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[7].get_value("c162")

    @c162.setter
    def c162(self, value: float) -> None:
        self._cards[7].set_value("c162", value)

    @property
    def c262(self) -> typing.Optional[float]:
        """Get or set the The 2,6 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[7].get_value("c262")

    @c262.setter
    def c262(self, value: float) -> None:
        self._cards[7].set_value("c262", value)

    @property
    def c362(self) -> typing.Optional[float]:
        """Get or set the The 3,6 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[7].get_value("c362")

    @c362.setter
    def c362(self, value: float) -> None:
        self._cards[7].set_value("c362", value)

    @property
    def c462(self) -> typing.Optional[float]:
        """Get or set the The 4,6 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[7].get_value("c462")

    @c462.setter
    def c462(self, value: float) -> None:
        self._cards[7].set_value("c462", value)

    @property
    def c562(self) -> typing.Optional[float]:
        """Get or set the The 5,6 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[7].get_value("c562")

    @c562.setter
    def c562(self, value: float) -> None:
        self._cards[7].set_value("c562", value)

    @property
    def c662(self) -> typing.Optional[float]:
        """Get or set the The 6,6 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[7].get_value("c662")

    @c662.setter
    def c662(self, value: float) -> None:
        self._cards[7].set_value("c662", value)

    @property
    def xp2(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of the i th phase's point P for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[8].get_value("xp2")

    @xp2.setter
    def xp2(self, value: float) -> None:
        self._cards[8].set_value("xp2", value)

    @property
    def yp2(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of the i th phase's point P for AOPT = 1 and 4
        """ # nopep8
        return self._cards[8].get_value("yp2")

    @yp2.setter
    def yp2(self, value: float) -> None:
        self._cards[8].set_value("yp2", value)

    @property
    def zp2(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of the i th phase's point P for AOPT = 1 and 4
        """ # nopep8
        return self._cards[8].get_value("zp2")

    @zp2.setter
    def zp2(self, value: float) -> None:
        self._cards[8].set_value("zp2", value)

    @property
    def a12(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector a for AOPT = 2.
        """ # nopep8
        return self._cards[8].get_value("a12")

    @a12.setter
    def a12(self, value: float) -> None:
        self._cards[8].set_value("a12", value)

    @property
    def a22(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector a for AOPT = 2
        """ # nopep8
        return self._cards[8].get_value("a22")

    @a22.setter
    def a22(self, value: float) -> None:
        self._cards[8].set_value("a22", value)

    @property
    def a32(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector a for AOPT = 2
        """ # nopep8
        return self._cards[8].get_value("a32")

    @a32.setter
    def a32(self, value: float) -> None:
        self._cards[8].set_value("a32", value)

    @property
    def xp2(self) -> typing.Optional[float]:
        """Get or set the Material identification. A unique number has to be chosen.
        """ # nopep8
        return self._cards[8].get_value("xp2")

    @xp2.setter
    def xp2(self, value: float) -> None:
        self._cards[8].set_value("xp2", value)

    @property
    def v12(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[9].get_value("v12")

    @v12.setter
    def v12(self, value: float) -> None:
        self._cards[9].set_value("v12", value)

    @property
    def v22(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector v for AOPT = 3 and 4
        """ # nopep8
        return self._cards[9].get_value("v22")

    @v22.setter
    def v22(self, value: float) -> None:
        self._cards[9].set_value("v22", value)

    @property
    def v32(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector v for AOPT = 3 and 4
        """ # nopep8
        return self._cards[9].get_value("v32")

    @v32.setter
    def v32(self, value: float) -> None:
        self._cards[9].set_value("v32", value)

    @property
    def d12(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector d for AOPT = 2
        """ # nopep8
        return self._cards[9].get_value("d12")

    @d12.setter
    def d12(self, value: float) -> None:
        self._cards[9].set_value("d12", value)

    @property
    def d22(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector d for AOPT = 2
        """ # nopep8
        return self._cards[9].get_value("d22")

    @d22.setter
    def d22(self, value: float) -> None:
        self._cards[9].set_value("d22", value)

    @property
    def d32(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector d for AOPT = 2
        """ # nopep8
        return self._cards[9].get_value("d32")

    @d32.setter
    def d32(self, value: float) -> None:
        self._cards[9].set_value("d32", value)

    @property
    def beta2(self) -> typing.Optional[float]:
        """Get or set the Material angle of i th phase in degrees for AOPT = 3, may be
        overridden on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_SOLID_ORTHO.
        """ # nopep8
        return self._cards[9].get_value("beta2")

    @beta2.setter
    def beta2(self, value: float) -> None:
        self._cards[9].set_value("beta2", value)

    @property
    def x1(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point on the phase transition page.
        """ # nopep8
        return self._cards[10].get_value("x1")

    @x1.setter
    def x1(self, value: float) -> None:
        self._cards[10].set_value("x1", value)

    @property
    def y1(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point on the phase transition page.
        """ # nopep8
        return self._cards[10].get_value("y1")

    @y1.setter
    def y1(self, value: float) -> None:
        self._cards[10].set_value("y1", value)

    @property
    def z1(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point on the phase transition page.
        """ # nopep8
        return self._cards[10].get_value("z1")

    @z1.setter
    def z1(self, value: float) -> None:
        self._cards[10].set_value("z1", value)

    @property
    def x2(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point that defines the exterior normal with the first point.
        """ # nopep8
        return self._cards[10].get_value("x2")

    @x2.setter
    def x2(self, value: float) -> None:
        self._cards[10].set_value("x2", value)

    @property
    def y2(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point that defines the exterior normal with the first point
        """ # nopep8
        return self._cards[10].get_value("y2")

    @y2.setter
    def y2(self, value: float) -> None:
        self._cards[10].set_value("y2", value)

    @property
    def z2(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point that defines the exterior normal with the first point
        """ # nopep8
        return self._cards[10].get_value("z2")

    @z2.setter
    def z2(self, value: float) -> None:
        self._cards[10].set_value("z2", value)

    @property
    def thkfac(self) -> float:
        """Get or set the Scale factor applied to the shell thickness after the phase transformation.
        """ # nopep8
        return self._cards[10].get_value("thkfac")

    @thkfac.setter
    def thkfac(self, value: float) -> None:
        self._cards[10].set_value("thkfac", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[11].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[11].cards[0].set_value("title", value)

