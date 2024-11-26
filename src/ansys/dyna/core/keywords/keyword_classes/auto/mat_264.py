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

class Mat264(KeywordBase):
    """DYNA MAT_264 keyword"""

    keyword = "MAT"
    subkeyword = "264"
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
                        "cp",
                        float,
                        40,
                        10,
                        kwargs.get("cp")
                    ),
                    Field(
                        "tr",
                        float,
                        50,
                        10,
                        kwargs.get("tr")
                    ),
                    Field(
                        "beta",
                        float,
                        60,
                        10,
                        kwargs.get("beta", 1.0)
                    ),
                    Field(
                        "numint",
                        float,
                        70,
                        10,
                        kwargs.get("numint", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lct00r",
                        int,
                        0,
                        10,
                        kwargs.get("lct00r", 0)
                    ),
                    Field(
                        "lct00t",
                        int,
                        10,
                        10,
                        kwargs.get("lct00t", 0)
                    ),
                    Field(
                        "lcf",
                        int,
                        20,
                        10,
                        kwargs.get("lcf", 0)
                    ),
                    Field(
                        "lcg",
                        int,
                        30,
                        10,
                        kwargs.get("lcg", 0)
                    ),
                    Field(
                        "lch",
                        int,
                        40,
                        10,
                        kwargs.get("lch", 0)
                    ),
                    Field(
                        "lci",
                        int,
                        50,
                        10,
                        kwargs.get("lci", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcc00r",
                        float,
                        0,
                        10,
                        kwargs.get("lcc00r")
                    ),
                    Field(
                        "lcc00t",
                        float,
                        10,
                        10,
                        kwargs.get("lcc00t")
                    ),
                    Field(
                        "lcs45r",
                        float,
                        20,
                        10,
                        kwargs.get("lcs45r")
                    ),
                    Field(
                        "lcs45t",
                        float,
                        30,
                        10,
                        kwargs.get("lcs45t")
                    ),
                    Field(
                        "iflag",
                        int,
                        40,
                        10,
                        kwargs.get("iflag", 0)
                    ),
                    Field(
                        "sfiepm",
                        int,
                        50,
                        10,
                        kwargs.get("sfiepm", 1)
                    ),
                    Field(
                        "niter",
                        int,
                        60,
                        10,
                        kwargs.get("niter", 100)
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
                        "lct90r",
                        int,
                        0,
                        10,
                        kwargs.get("lct90r", 0)
                    ),
                    Field(
                        "lct45r",
                        int,
                        10,
                        10,
                        kwargs.get("lct45r", 0)
                    ),
                    Field(
                        "lctthr",
                        int,
                        20,
                        10,
                        kwargs.get("lctthr", 0)
                    ),
                    Field(
                        "lcc90r",
                        int,
                        30,
                        10,
                        kwargs.get("lcc90r", 0)
                    ),
                    Field(
                        "lcc45r",
                        int,
                        40,
                        10,
                        kwargs.get("lcc45r", 0)
                    ),
                    Field(
                        "lccth",
                        int,
                        50,
                        10,
                        kwargs.get("lccth", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lct90t",
                        int,
                        0,
                        10,
                        kwargs.get("lct90t", 0)
                    ),
                    Field(
                        "lct45t",
                        int,
                        10,
                        10,
                        kwargs.get("lct45t", 0)
                    ),
                    Field(
                        "lcttht",
                        int,
                        20,
                        10,
                        kwargs.get("lcttht", 0)
                    ),
                    Field(
                        "lcc90t",
                        int,
                        30,
                        10,
                        kwargs.get("lcc90t", 0)
                    ),
                    Field(
                        "lcc45t",
                        int,
                        40,
                        10,
                        kwargs.get("lcc45t", 0)
                    ),
                    Field(
                        "lcctht",
                        int,
                        50,
                        10,
                        kwargs.get("lcctht", 0)
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
                        float,
                        60,
                        10,
                        kwargs.get("macf", 1)
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
            OptionCardSet(
                option_spec = Mat264.option_specs[0],
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
        """Get or set the Young's modulus.
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
    def cp(self) -> typing.Optional[float]:
        """Get or set the Specific heat.
        """ # nopep8
        return self._cards[0].get_value("cp")

    @cp.setter
    def cp(self, value: float) -> None:
        self._cards[0].set_value("cp", value)

    @property
    def tr(self) -> typing.Optional[float]:
        """Get or set the Room temperature.
        """ # nopep8
        return self._cards[0].get_value("tr")

    @tr.setter
    def tr(self, value: float) -> None:
        self._cards[0].set_value("tr", value)

    @property
    def beta(self) -> float:
        """Get or set the Amount of plastic work converted into heat
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[0].set_value("beta", value)

    @property
    def numint(self) -> float:
        """Get or set the Number of integration points which must fail before the element is deleted. Available for shells and solids.
        LT.0.0: |NUMINT| is percentage of integration points/layers which must fail before element fails. For fully integrated shells, a methodology is used where a layer fails if one integrationpoint fails and then the given percentage of layers must fail before the element fails.
        """ # nopep8
        return self._cards[0].get_value("numint")

    @numint.setter
    def numint(self, value: float) -> None:
        self._cards[0].set_value("numint", value)

    @property
    def lct00r(self) -> int:
        """Get or set the Load curve ID or Table ID. The load curve ID defines effective stress as a function of effective plastic strain. The table ID defines for each plastic strain rate value a load curve ID giving the (isothermal) effective stress versus effective plastic strain for that rate.
        """ # nopep8
        return self._cards[1].get_value("lct00r")

    @lct00r.setter
    def lct00r(self, value: int) -> None:
        self._cards[1].set_value("lct00r", value)

    @property
    def lct00t(self) -> int:
        """Get or set the Table ID defining for each temperature value a load curve ID giving the (quasi-static) effective stress versus effective plastic strain for that temperature.
        """ # nopep8
        return self._cards[1].get_value("lct00t")

    @lct00t.setter
    def lct00t(self, value: int) -> None:
        self._cards[1].set_value("lct00t", value)

    @property
    def lcf(self) -> int:
        """Get or set the Load curve ID or Table ID. The load curve ID defines plastic failure strain as a function of triaxiality. The table ID defines for each Lode angle a load curve ID giving the plastic failure strain versus triaxiality for that Lode angle. (Table option only for solids and not yet generally supported).
        """ # nopep8
        return self._cards[1].get_value("lcf")

    @lcf.setter
    def lcf(self, value: int) -> None:
        self._cards[1].set_value("lcf", value)

    @property
    def lcg(self) -> int:
        """Get or set the Load curve ID defining plastic failure strain as a function of strain rate.
        """ # nopep8
        return self._cards[1].get_value("lcg")

    @lcg.setter
    def lcg(self, value: int) -> None:
        self._cards[1].set_value("lcg", value)

    @property
    def lch(self) -> int:
        """Get or set the Load curve ID defining plastic failure strain as a function of temperature
        """ # nopep8
        return self._cards[1].get_value("lch")

    @lch.setter
    def lch(self, value: int) -> None:
        self._cards[1].set_value("lch", value)

    @property
    def lci(self) -> int:
        """Get or set the Load curve ID defining plastic failure strain as a function of element size..
        """ # nopep8
        return self._cards[1].get_value("lci")

    @lci.setter
    def lci(self, value: int) -> None:
        self._cards[1].set_value("lci", value)

    @property
    def lcc00r(self) -> typing.Optional[float]:
        """Get or set the Table ID. The curves in this table define compressive yield stress as a function of plastic strain or effective plastic strain (see IFLAG). The table ID defines for each plastic strain rate value or effective plastic strain rate value a load curve ID giving the (isothermal) compressive yield stress versus plastic strain or effective plastic strain for that rate.
        """ # nopep8
        return self._cards[2].get_value("lcc00r")

    @lcc00r.setter
    def lcc00r(self, value: float) -> None:
        self._cards[2].set_value("lcc00r", value)

    @property
    def lcc00t(self) -> typing.Optional[float]:
        """Get or set the Table ID defining for each temperature value a load curve ID giving the (quasi-static) compressive yield stress versus strain for that temperature. The curves in this table define compressive yield stress as a function of plastic strain or effective plastic strain (see IFLAG).
        """ # nopep8
        return self._cards[2].get_value("lcc00t")

    @lcc00t.setter
    def lcc00t(self, value: float) -> None:
        self._cards[2].set_value("lcc00t", value)

    @property
    def lcs45r(self) -> typing.Optional[float]:
        """Get or set the Table ID. The load curves define shear yield stress in function of plastic strain or effective plastic strain (see IFLAG).The table ID defines for each plastic strain rate value or effective plastic strain rate value a load curve ID giving the (isothermal) shear yield stress versus plastic strain or effective plastic strain for that rate.
        """ # nopep8
        return self._cards[2].get_value("lcs45r")

    @lcs45r.setter
    def lcs45r(self, value: float) -> None:
        self._cards[2].set_value("lcs45r", value)

    @property
    def lcs45t(self) -> typing.Optional[float]:
        """Get or set the Table ID defining for each temperature value a load curve ID giving the (quasi-static) shear yield stress versus strain for that temperature. The load curves define shear yield stress as a function of plastic strain or effective plastic strain (see IFLAG).
        """ # nopep8
        return self._cards[2].get_value("lcs45t")

    @lcs45t.setter
    def lcs45t(self, value: float) -> None:
        self._cards[2].set_value("lcs45t", value)

    @property
    def iflag(self) -> int:
        """Get or set the Flag to specify abscissa for LCCR, LCCT, LCSR, LCST:
        EQ.0.0:	Compressive and shear yields are given in a function of plastic strain (default).
        EQ.1.0:	Compressive and shear strain are given in function of effective plastic strain.
        """ # nopep8
        return self._cards[2].get_value("iflag")

    @iflag.setter
    def iflag(self, value: int) -> None:
        self._cards[2].set_value("iflag", value)

    @property
    def sfiepm(self) -> int:
        """Get or set the Scale factor on the initial estimate of the plastic multiplier.
        """ # nopep8
        return self._cards[2].get_value("sfiepm")

    @sfiepm.setter
    def sfiepm(self, value: int) -> None:
        self._cards[2].set_value("sfiepm", value)

    @property
    def niter(self) -> int:
        """Get or set the Maximum number of iterations for the plasticity algorithm.
        """ # nopep8
        return self._cards[2].get_value("niter")

    @niter.setter
    def niter(self, value: int) -> None:
        self._cards[2].set_value("niter", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
        EQ.0.0:	Locally orthotropic with material axes determined by element nodes 1, 2,and 4, as with* DEFINE_COORDINATE_NODES.
        EQ.1.0 : Locally orthotropic with material axes determined by a point, P, in spaceand the global location of the element center; this is the a - direction.
        EQ.2.0:	Globally orthotropic with material axes determined by vectors defined below, as with* DEFINE_COORDINATE_VECTOR
        EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element.The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.Thus, AOPT = 3 is only available for hexahedrons.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle.The angle may be set in the keyword input for the element or in the input for this keyword(see MANGLE).Note that the material axes may be switched depending on the choice of MACF.The switch may occur before or after applying the angle depending on the value of MACF.
        EQ.4.0 : Locally orthotropic in a cylindrical coordinate system with the material axes determined by a vector v,and an originating point, P, which define the centerline axis.
        LT.0.0 : The absolute value of AOPT is a coordinate system ID number(CID on * DEFINE_COORDINATE_OPTION).
        """ # nopep8
        return self._cards[2].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[2].set_value("aopt", value)

    @property
    def lct90r(self) -> int:
        """Get or set the Table ID defining for each plastic strain rate value a load curve ID giving the (isothermal) tensile yield  stress versus  plastic strain for that rate in the 90 degree direction.
        """ # nopep8
        return self._cards[3].get_value("lct90r")

    @lct90r.setter
    def lct90r(self, value: int) -> None:
        self._cards[3].set_value("lct90r", value)

    @property
    def lct45r(self) -> int:
        """Get or set the Table ID defining for each plastic strain rate value a load curve ID giving the (isothermal) tensile yield  stress versus plastic strain for that rate in the 45 degree direction.
        """ # nopep8
        return self._cards[3].get_value("lct45r")

    @lct45r.setter
    def lct45r(self, value: int) -> None:
        self._cards[3].set_value("lct45r", value)

    @property
    def lctthr(self) -> int:
        """Get or set the Table ID defining for each plastic strain rate value a load curve ID giving the (isothermal) tensile yield stress versus plastic strain for that rate in the thickness degree direction.
        """ # nopep8
        return self._cards[3].get_value("lctthr")

    @lctthr.setter
    def lctthr(self, value: int) -> None:
        self._cards[3].set_value("lctthr", value)

    @property
    def lcc90r(self) -> int:
        """Get or set the Table ID defining for each plastic strain rate value a load curve ID giving the (isothermal) compressive yield  stress versus  plastic strain for that rate in the 90 degree direction.
        """ # nopep8
        return self._cards[3].get_value("lcc90r")

    @lcc90r.setter
    def lcc90r(self, value: int) -> None:
        self._cards[3].set_value("lcc90r", value)

    @property
    def lcc45r(self) -> int:
        """Get or set the Table ID defining for each plastic strain rate value a load curve ID giving the (isothermal) compressive yield  stress versus plastic strain for that rate in the 45 degree direction.
        """ # nopep8
        return self._cards[3].get_value("lcc45r")

    @lcc45r.setter
    def lcc45r(self, value: int) -> None:
        self._cards[3].set_value("lcc45r", value)

    @property
    def lccth(self) -> int:
        """Get or set the Table ID defining for each plastic strain rate value a load curve ID giving the (isothermal) compressive yield stress versus plastic strain for that rate in the thickness degree direction.
        """ # nopep8
        return self._cards[3].get_value("lccth")

    @lccth.setter
    def lccth(self, value: int) -> None:
        self._cards[3].set_value("lccth", value)

    @property
    def lct90t(self) -> int:
        """Get or set the Table ID defining for each temperature value a load curve ID giving the (quasistatic) tensile yield  stress versus  plastic strain for that rate in the 90 degree direction.
        """ # nopep8
        return self._cards[4].get_value("lct90t")

    @lct90t.setter
    def lct90t(self, value: int) -> None:
        self._cards[4].set_value("lct90t", value)

    @property
    def lct45t(self) -> int:
        """Get or set the Table ID defining for each temperature value a load curve ID giving the (quasistatic) tensile yield  stress versus plastic strain for that rate in the 45 degree direction.
        """ # nopep8
        return self._cards[4].get_value("lct45t")

    @lct45t.setter
    def lct45t(self, value: int) -> None:
        self._cards[4].set_value("lct45t", value)

    @property
    def lcttht(self) -> int:
        """Get or set the Table ID defining for each temperature value a load curve ID giving the (quasistatic) tensile yield stress versus plastic strain for that rate in the thickness degree direction.
        """ # nopep8
        return self._cards[4].get_value("lcttht")

    @lcttht.setter
    def lcttht(self, value: int) -> None:
        self._cards[4].set_value("lcttht", value)

    @property
    def lcc90t(self) -> int:
        """Get or set the Table ID defining for each temperature value a load curve ID giving the (quasistatic) compressive yield  stress versus  plastic strain for that rate in the 90 degree direction.
        """ # nopep8
        return self._cards[4].get_value("lcc90t")

    @lcc90t.setter
    def lcc90t(self, value: int) -> None:
        self._cards[4].set_value("lcc90t", value)

    @property
    def lcc45t(self) -> int:
        """Get or set the Table ID defining for each temperature value a load curve ID giving the (quasistatic) compressive yield  stress versus plastic strain for that rate in the 45 degree direction.
        """ # nopep8
        return self._cards[4].get_value("lcc45t")

    @lcc45t.setter
    def lcc45t(self, value: int) -> None:
        self._cards[4].set_value("lcc45t", value)

    @property
    def lcctht(self) -> int:
        """Get or set the Table ID defining for each temperature value a load curve ID giving the (quasistatic) compressive yield stress versus plastic strain for that rate in the thickness degree direction.
        """ # nopep8
        return self._cards[4].get_value("lcctht")

    @lcctht.setter
    def lcctht(self, value: int) -> None:
        self._cards[4].set_value("lcctht", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[5].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[5].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[5].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[5].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[5].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[5].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[5].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[5].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[5].set_value("a3", value)

    @property
    def macf(self) -> float:
        """Get or set the Material axes change flag for solid elements:
        EQ. - 4:	Switch material axes b and c before BETA or MANGLE rotation
        EQ. - 3 : Switch material axes a and c before BETA or MANGLE rotation
        EQ. - 2 : Switch material axes a and b before BETA or MANGLE rotation
        EQ.1 : No change, default
        EQ.2 : Switch material axes a and b after BETA or MANGLE rotation
        EQ.3 : Switch material axes a and c after BETA or MANGLE rotation
        EQ.4 : Switch material axes b and c after BETA or MANGLE rotation
        Figure Error!Reference source not found.indicates when LS - DYNA applies MACF during the process to obtain the final material axes.If BETA on * ELEMENT_SOLID_{OPTION} is defined, then BETA is used for the rotation for all AOPT options.Otherwise, for AOPT = 3, MANGLE input on Card 3 rotates the axes.For all other values of AOPT, the material axes will be switched as specified by MACF, but no rotation will be performed.
        """ # nopep8
        return self._cards[5].get_value("macf")

    @macf.setter
    def macf(self, value: float) -> None:
        if value not in [1, 2, 3, 4, -4, -3, -2, -1]:
            raise Exception("""macf must be one of {1,2,3,4,-4,-3,-2,-1}""")
        self._cards[5].set_value("macf", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[6].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[6].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[6].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[6].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[6].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[6].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Components of vector dfor AOPT = 2.
        """ # nopep8
        return self._cards[6].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[6].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Components of vector dfor AOPT = 2.
        """ # nopep8
        return self._cards[6].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[6].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Components of vector dfor AOPT = 2.
        """ # nopep8
        return self._cards[6].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[6].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT=0 and 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA.
        """ # nopep8
        return self._cards[6].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[6].set_value("beta", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[7].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[7].cards[0].set_value("title", value)

