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

class MatWoodFir(KeywordBase):
    """DYNA MAT_WOOD_FIR keyword"""

    keyword = "MAT"
    subkeyword = "WOOD_FIR"
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
                        "nplot",
                        int,
                        20,
                        10,
                        kwargs.get("nplot", 1)
                    ),
                    Field(
                        "iters",
                        int,
                        30,
                        10,
                        kwargs.get("iters", 1)
                    ),
                    Field(
                        "irate",
                        int,
                        40,
                        10,
                        kwargs.get("irate", 0)
                    ),
                    Field(
                        "ghard",
                        float,
                        50,
                        10,
                        kwargs.get("ghard", 0)
                    ),
                    Field(
                        "ifail",
                        int,
                        60,
                        10,
                        kwargs.get("ifail", 0)
                    ),
                    Field(
                        "ivol",
                        int,
                        70,
                        10,
                        kwargs.get("ivol", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "mois",
                        float,
                        0,
                        10,
                        kwargs.get("mois")
                    ),
                    Field(
                        "temp",
                        float,
                        10,
                        10,
                        kwargs.get("temp")
                    ),
                    Field(
                        "qual_t",
                        float,
                        20,
                        10,
                        kwargs.get("qual_t")
                    ),
                    Field(
                        "qual_c",
                        float,
                        30,
                        10,
                        kwargs.get("qual_c")
                    ),
                    Field(
                        "units",
                        int,
                        40,
                        10,
                        kwargs.get("units", 0)
                    ),
                    Field(
                        "iqual",
                        int,
                        50,
                        10,
                        kwargs.get("iqual", 0)
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
                        "v1",
                        float,
                        30,
                        10,
                        kwargs.get("v1")
                    ),
                    Field(
                        "v2",
                        float,
                        40,
                        10,
                        kwargs.get("v2")
                    ),
                    Field(
                        "v3",
                        float,
                        50,
                        10,
                        kwargs.get("v3")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatWoodFir.option_specs[0],
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
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def nplot(self) -> int:
        """Get or set the Plotting Options:
        EQ.1. Parallel damage (default).
        EQ.2. Perpendicular damage.
        """ # nopep8
        return self._cards[0].get_value("nplot")

    @nplot.setter
    def nplot(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""nplot must be one of {1,2}""")
        self._cards[0].set_value("nplot", value)

    @property
    def iters(self) -> int:
        """Get or set the Number of plasticity algorithm iterations.  The default is one iteration.
        GE.0:	Original plasticity iteration developed by Murray [2002].
        LT.0:	Plasticity iteration (return mapping) with non-associated flow direction for perpendicular yielding. The absolute value of ITERS is used as number of plasticity algorithm iterations.
        """ # nopep8
        return self._cards[0].get_value("iters")

    @iters.setter
    def iters(self, value: int) -> None:
        self._cards[0].set_value("iters", value)

    @property
    def irate(self) -> int:
        """Get or set the Rate effects option:
        EQ.0. Rate effects model turned off (default).
        EQ.1. Rate effects model turned on.on with the original rate dependence described by Murray [2002].
        EQ.2:	Rate effects model turned on with Johnson-Cook like rate dependence of the strength parameters, as described below in the remarks. Only works in combination with ITERS.LT.0 and OPTION=<BLANK>..
        """ # nopep8
        return self._cards[0].get_value("irate")

    @irate.setter
    def irate(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""irate must be one of {0,1,2}""")
        self._cards[0].set_value("irate", value)

    @property
    def ghard(self) -> float:
        """Get or set the Perfect plasticity override. Values greater than or equal to zero are allowed. Positive values model late time hardening in compression (an increase in strength with increasing strain). A zero value models perfect plasticity (no increase in strength with increasing strain). The default is zero.
        """ # nopep8
        return self._cards[0].get_value("ghard")

    @ghard.setter
    def ghard(self, value: float) -> None:
        self._cards[0].set_value("ghard", value)

    @property
    def ifail(self) -> int:
        """Get or set the Erosion perpendicular to the grain.
        EQ.0. No (default).
        EQ.1. Yes (not recommended except for debugging).
        """ # nopep8
        return self._cards[0].get_value("ifail")

    @ifail.setter
    def ifail(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ifail must be one of {0,1}""")
        self._cards[0].set_value("ifail", value)

    @property
    def ivol(self) -> int:
        """Get or set the Erode on negative volume or strain increments greater than 0.01.
        EQ.0:  No, do not apply erosion criteria.
        EQ.1:  Yes, apply volume and strain erosion criteria
        """ # nopep8
        return self._cards[0].get_value("ivol")

    @ivol.setter
    def ivol(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ivol must be one of {0,1}""")
        self._cards[0].set_value("ivol", value)

    @property
    def mois(self) -> typing.Optional[float]:
        """Get or set the Percent moisture content. If left blank, moisture content defaults to saturated at 30%.
        """ # nopep8
        return self._cards[1].get_value("mois")

    @mois.setter
    def mois(self, value: float) -> None:
        self._cards[1].set_value("mois", value)

    @property
    def temp(self) -> typing.Optional[float]:
        """Get or set the Temperature in C. If left blank, temperature defaults to room temperature at 20 C.
        """ # nopep8
        return self._cards[1].get_value("temp")

    @temp.setter
    def temp(self, value: float) -> None:
        self._cards[1].set_value("temp", value)

    @property
    def qual_t(self) -> typing.Optional[float]:
        """Get or set the Quality factor options. These quality factors reduce the clear wood tension, shear, and compression strengths as a function of grade.
        EQ. 1: Grade 1, 1D, 2, 2D.
        Predefined strength reduction factors are:
        Pine: Qual_T=0.47 in tension/shear.
        Qual_C=0.63 in compression.
        Fir: Qual_T=0.040 in tension/shear.
        Qual_C=0.73 in compression.
        EQ.-1: DS-65 or SEl STR (pine and fir).
        Predefined strength reduction factors are:
        Qual_T=0.80 in tension/shear.
        Qual_C=0.93 in compression.
        EQ.-2: Clear wood.
        No strength reduction factors are applied:
        Qual_T=1.0.
        Qual_C=1.0.
        GT.0: User defined quality factor in tension. Values between 0 and 1 are expected. Values greater than one are allowed, but not be realistic.
        """ # nopep8
        return self._cards[1].get_value("qual_t")

    @qual_t.setter
    def qual_t(self, value: float) -> None:
        self._cards[1].set_value("qual_t", value)

    @property
    def qual_c(self) -> typing.Optional[float]:
        """Get or set the User defined quality factor in compression. This input value is used if Qual_T>0. Values between 0 and 1 are expected. Values greater than one are allowed, but may not be realistic. If left blank, a default value of Qual_C=Qual_T is used.
        """ # nopep8
        return self._cards[1].get_value("qual_c")

    @qual_c.setter
    def qual_c(self, value: float) -> None:
        self._cards[1].set_value("qual_c", value)

    @property
    def units(self) -> int:
        """Get or set the Units options:
        EQ.0: GPa, mm, msec, Kg/mm^3, kN.
        EQ.1: MPa, cm, msec, g/mm^3, Nt.
        EQ.2: MPa, mm, sec, Mg/mm^3, Nt.
        EQ.3: Psi, inch, sec, lb-s^2/inch^4, lb.
        """ # nopep8
        return self._cards[1].get_value("units")

    @units.setter
    def units(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""units must be one of {0,1,2,3}""")
        self._cards[1].set_value("units", value)

    @property
    def iqual(self) -> int:
        """Get or set the Apply quality factors perpendicular to the grain:
        EQ.0: Yes (default).
        EQ.1: No.
        """ # nopep8
        return self._cards[1].get_value("iqual")

    @iqual.setter
    def iqual(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""iqual must be one of {0,1}""")
        self._cards[1].set_value("iqual", value)

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
    def xp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[3].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[3].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[3].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[3].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[3].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[3].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[3].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[3].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[3].set_value("a3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[4].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[4].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[4].set_value("d3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[4].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[4].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[4].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[4].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[4].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[4].set_value("v3", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[5].cards[0].set_value("title", value)

