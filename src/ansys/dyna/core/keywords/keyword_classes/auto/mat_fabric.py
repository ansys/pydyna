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

class MatFabric(KeywordBase):
    """DYNA MAT_FABRIC keyword"""

    keyword = "MAT"
    subkeyword = "FABRIC"
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
                        "prab",
                        float,
                        60,
                        10,
                        kwargs.get("prab")
                    ),
                    Field(
                        "unused",
                        float,
                        70,
                        10,
                        kwargs.get("unused")
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
                        "cse",
                        float,
                        30,
                        10,
                        kwargs.get("cse", 0.0)
                    ),
                    Field(
                        "el",
                        float,
                        40,
                        10,
                        kwargs.get("el")
                    ),
                    Field(
                        "prl",
                        float,
                        50,
                        10,
                        kwargs.get("prl")
                    ),
                    Field(
                        "lratio",
                        float,
                        60,
                        10,
                        kwargs.get("lratio")
                    ),
                    Field(
                        "damp",
                        float,
                        70,
                        10,
                        kwargs.get("damp")
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
                    Field(
                        "flc",
                        float,
                        10,
                        10,
                        kwargs.get("flc")
                    ),
                    Field(
                        "fac",
                        float,
                        20,
                        10,
                        kwargs.get("fac")
                    ),
                    Field(
                        "ela",
                        float,
                        30,
                        10,
                        kwargs.get("ela")
                    ),
                    Field(
                        "lnrc",
                        float,
                        40,
                        10,
                        kwargs.get("lnrc", 0.0)
                    ),
                    Field(
                        "form",
                        int,
                        50,
                        10,
                        kwargs.get("form", 0)
                    ),
                    Field(
                        "fvopt",
                        int,
                        60,
                        10,
                        kwargs.get("fvopt", 0)
                    ),
                    Field(
                        "tsrfac",
                        float,
                        70,
                        10,
                        kwargs.get("tsrfac", 0)
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
                        "rgbrth",
                        float,
                        10,
                        10,
                        kwargs.get("rgbrth")
                    ),
                    Field(
                        "a0ref",
                        int,
                        20,
                        10,
                        kwargs.get("a0ref", 0)
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
                        "x0",
                        float,
                        60,
                        10,
                        kwargs.get("x0")
                    ),
                    Field(
                        "x1",
                        float,
                        70,
                        10,
                        kwargs.get("x1")
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
                        "unused",
                        float,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        float,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        float,
                        50,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "beta",
                        float,
                        60,
                        10,
                        kwargs.get("beta")
                    ),
                    Field(
                        "isrefg",
                        int,
                        70,
                        10,
                        kwargs.get("isrefg", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lca",
                        int,
                        0,
                        10,
                        kwargs.get("lca", 0)
                    ),
                    Field(
                        "lcb",
                        int,
                        10,
                        10,
                        kwargs.get("lcb", 0)
                    ),
                    Field(
                        "lcab",
                        int,
                        20,
                        10,
                        kwargs.get("lcab", 0)
                    ),
                    Field(
                        "lcua",
                        int,
                        30,
                        10,
                        kwargs.get("lcua", 0)
                    ),
                    Field(
                        "lcub",
                        int,
                        40,
                        10,
                        kwargs.get("lcub", 0)
                    ),
                    Field(
                        "lcuab",
                        int,
                        50,
                        10,
                        kwargs.get("lcuab", 0)
                    ),
                    Field(
                        "rl",
                        float,
                        60,
                        10,
                        kwargs.get("rl")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcaa",
                        int,
                        0,
                        10,
                        kwargs.get("lcaa")
                    ),
                    Field(
                        "lcbb",
                        int,
                        10,
                        10,
                        kwargs.get("lcbb")
                    ),
                    Field(
                        "h",
                        float,
                        20,
                        10,
                        kwargs.get("h")
                    ),
                    Field(
                        "dt",
                        int,
                        30,
                        10,
                        kwargs.get("dt")
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "ecoat",
                        float,
                        50,
                        10,
                        kwargs.get("ecoat")
                    ),
                    Field(
                        "scoat",
                        float,
                        60,
                        10,
                        kwargs.get("scoat")
                    ),
                    Field(
                        "tcoat",
                        float,
                        70,
                        10,
                        kwargs.get("tcoat")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatFabric.option_specs[0],
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
        """Get or set the Young's modulus - longitudinal direction. For an isotopic elastic fabric material only EA and PRBA are defined and are used as the isotropic Young's modulus and Poisson's ratio, respectively. The input for the fiber directions and liner should be input as zero for the isotropic elastic fabric.
        """ # nopep8
        return self._cards[0].get_value("ea")

    @ea.setter
    def ea(self, value: float) -> None:
        self._cards[0].set_value("ea", value)

    @property
    def eb(self) -> typing.Optional[float]:
        """Get or set the Young's modulus - transverse direction, set to zero for isotropic elastic material.
        """ # nopep8
        return self._cards[0].get_value("eb")

    @eb.setter
    def eb(self, value: float) -> None:
        self._cards[0].set_value("eb", value)

    @property
    def prba(self) -> typing.Optional[float]:
        """Get or set the Minor Poisson's ratio ba direction.
        """ # nopep8
        return self._cards[0].get_value("prba")

    @prba.setter
    def prba(self, value: float) -> None:
        self._cards[0].set_value("prba", value)

    @property
    def prab(self) -> typing.Optional[float]:
        """Get or set the Major Poisson's ratio ca direction, set to zero for isotropic elastic material.
        """ # nopep8
        return self._cards[0].get_value("prab")

    @prab.setter
    def prab(self, value: float) -> None:
        self._cards[0].set_value("prab", value)

    @property
    def gab(self) -> typing.Optional[float]:
        """Get or set the Shear modulus ab direction, set to zero for isotropic elastic material.
        """ # nopep8
        return self._cards[1].get_value("gab")

    @gab.setter
    def gab(self, value: float) -> None:
        self._cards[1].set_value("gab", value)

    @property
    def cse(self) -> float:
        """Get or set the Compressive stress elimination option:
        EQ.0.0: don't eliminate compressive stresses (default),
        EQ.1.0: eliminate compressive stresses (does not apply to linear).
        """ # nopep8
        return self._cards[1].get_value("cse")

    @cse.setter
    def cse(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""cse must be one of {0.0,1.0}""")
        self._cards[1].set_value("cse", value)

    @property
    def el(self) -> typing.Optional[float]:
        """Get or set the Young's modulus for elastic liner (required if LRATIO>0).
        """ # nopep8
        return self._cards[1].get_value("el")

    @el.setter
    def el(self, value: float) -> None:
        self._cards[1].set_value("el", value)

    @property
    def prl(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio for elastic liner (required if LRATIO>0).
        """ # nopep8
        return self._cards[1].get_value("prl")

    @prl.setter
    def prl(self, value: float) -> None:
        self._cards[1].set_value("prl", value)

    @property
    def lratio(self) -> typing.Optional[float]:
        """Get or set the A non-zero value activates the elastic liner and defines the ratio of liner thickness to total fabric thickness (optional).
        """ # nopep8
        return self._cards[1].get_value("lratio")

    @lratio.setter
    def lratio(self, value: float) -> None:
        self._cards[1].set_value("lratio", value)

    @property
    def damp(self) -> typing.Optional[float]:
        """Get or set the Rayleigh damping coefficient.  A 0.05 coefficient is recommended corresponding to 5% of critical damping.  Sometimes larger values are necessary
        """ # nopep8
        return self._cards[1].get_value("damp")

    @damp.setter
    def damp(self, value: float) -> None:
        self._cards[1].set_value("damp", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option:
        EQ.0.0: locally orthotropic with material axes determined by
        element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES, and then rotated about the shell element normal by the angle BETA.
        EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
        EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,
        BETA, from a line in the plane of the element defined by	the cross product of the vector v with the element normal.
        LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
        *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR). Available with the R3 release of Version 971 and later.
        """ # nopep8
        return self._cards[2].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[2].set_value("aopt", value)

    @property
    def flc(self) -> typing.Optional[float]:
        """Get or set the Fabric leakage coefficient (optional), FLC
        LT.0.0: |FLC| is the load curve ID of the curve defining FLC versus time.
        """ # nopep8
        return self._cards[2].get_value("flc")

    @flc.setter
    def flc(self, value: float) -> None:
        self._cards[2].set_value("flc", value)

    @property
    def fac(self) -> typing.Optional[float]:
        """Get or set the Fabric area coefficient (optional), FAC
        LT.0.0: |FAC| is the load curve ID of the curve defining FAC versus ABSOLUTE pressure.
        """ # nopep8
        return self._cards[2].get_value("fac")

    @fac.setter
    def fac(self, value: float) -> None:
        self._cards[2].set_value("fac", value)

    @property
    def ela(self) -> typing.Optional[float]:
        """Get or set the Effective leakage area for blocked fabric, ELA.
        LT.0.0: |ELA| is the load curve ID of the curve defining ELA versus time. The default value of zero assumes that no leakage occurs. A value of .10 would assume that 10% of the blocked fabric is leaking gas.
        """ # nopep8
        return self._cards[2].get_value("ela")

    @ela.setter
    def ela(self, value: float) -> None:
        self._cards[2].set_value("ela", value)

    @property
    def lnrc(self) -> float:
        """Get or set the Flag to turn off compression in liner until the reference geometry is reached.
        EQ.0.0: off (default),
        EQ.1.0: on.
        """ # nopep8
        return self._cards[2].get_value("lnrc")

    @lnrc.setter
    def lnrc(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""lnrc must be one of {0.0,1.0}""")
        self._cards[2].set_value("lnrc", value)

    @property
    def form(self) -> int:
        """Get or set the Flag to modify membrane formulation for fabric material:
        EQ.0: Least costly and very reliable (default).
        EQ.1:invarient local membrane coordinate system
        EQ.2:Green-Lagrange strain formulation
        EQ.3:large strain with nonorthogonal material angles.
        EQ.4:large strain with nonorthogonal material angles and nonlinear stress strain behavior. Define optional load curve IDs on optional card.
        EQ12,13,14 are the updated versions of forms 2,3,4 respectively
        EQ.#14.0: Same as form 14, but invokes reading of card 7
        EQ.24.0: Enhanced version of formulation 14. See Remark 11.
        """ # nopep8
        return self._cards[2].get_value("form")

    @form.setter
    def form(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 12, 13, 14, -14, 24]:
            raise Exception("""form must be one of {0,1,2,3,4,12,13,14,-14,24}""")
        self._cards[2].set_value("form", value)

    @property
    def fvopt(self) -> int:
        """Get or set the Fabric venting option.
        EQ. 1: Wang-Nefske formulas for venting through an orifice are used. Blockage is not considered.
        EQ. 2: Wang-Nefske formulas for venting through an orifice are used. Blockage of venting area due to contact is considered.
        EQ. 3: Leakage formulas of Graefe, Krummheuer, and Siejak [1990] are used. Blockage is not considered.
        EQ. 4: Leakage formulas of Graefe, Krummheuer, and Siejak [1990] are used. Blockage of venting area due to contact is considered.
        EQ. 5: Leakage formulas based on flow through a porous media are used. Blockage is not considered.
        EQ. 6: Leakage formulas based on flow through a porous media are used. Blockage of venting area due to contact is considered.
        EQ. 7: Leakage is based on gas volume outflow versus pressure load curve. Blockage is not considered. Absolute pressure is used in the porous-velocity-versus-pressure load curve, given as FAC in the *MAT_FABRIC card.
        EQ. 8: Leakage is based on gas volume outflow versus pressure load curve. Blockage of venting or porous area due to contact is considered. Absolute pressure is used in the porous-velocity-versus-pressure load curve, given as FAC in the *MAT_FABRIC card.
        LT.0:	|FVOPT| defines the same fabric venting options as above, but a new formula for the leakage area is used to replace the element area. See Remark 16.
        """ # nopep8
        return self._cards[2].get_value("fvopt")

    @fvopt.setter
    def fvopt(self, value: int) -> None:
        self._cards[2].set_value("fvopt", value)

    @property
    def tsrfac(self) -> float:
        """Get or set the Tensile stress cutoff reduction factor:
        LT.0: |TSRFAC| is the load curve ID of the curve defining TSRFAC versus time.
        GT.0 and LT.1:	TSRFAC applied from time 0.
        GE.1:	TSRFAC is the ID of a curve that defines TSRFAC versus time using an alternate method (not available for FORM=0 or 1).
        """ # nopep8
        return self._cards[2].get_value("tsrfac")

    @tsrfac.setter
    def tsrfac(self, value: float) -> None:
        self._cards[2].set_value("tsrfac", value)

    @property
    def rgbrth(self) -> typing.Optional[float]:
        """Get or set the Material dependent birth time of airbag reference geometry. Nonzero
        RGBRTH overwrites the birth time defined in the *AIRBAG_REFERENCE_GEOMETRY_BIRTH section. RGBRTH also applies to
        reference geometry defined by *AIRBAG_SHELL_REFERENCE_GEOMETRY
        """ # nopep8
        return self._cards[3].get_value("rgbrth")

    @rgbrth.setter
    def rgbrth(self, value: float) -> None:
        self._cards[3].set_value("rgbrth", value)

    @property
    def a0ref(self) -> int:
        """Get or set the Calculation option of initial area, A0, used for airbag porosity leakage calculation.
        EQ.0.:	default.  Use the initial geometry defined in *NODE.
        EQ.1.:	Use the reference geometry defined in *AIRBAG_REFERENCE_GEOMETRY or *AIRBAG_SHELL_REFERENCE_GEOMETRY.
        """ # nopep8
        return self._cards[3].get_value("a0ref")

    @a0ref.setter
    def a0ref(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""a0ref must be one of {0,1}""")
        self._cards[3].set_value("a0ref", value)

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
    def x0(self) -> typing.Optional[float]:
        """Get or set the Coefficients of Anagonye and Wang [1999] porosity equation for the leakage area:
        """ # nopep8
        return self._cards[3].get_value("x0")

    @x0.setter
    def x0(self, value: float) -> None:
        self._cards[3].set_value("x0", value)

    @property
    def x1(self) -> typing.Optional[float]:
        """Get or set the Coefficients of Anagonye and Wang [1999] porosity equation for the leakage area:
        """ # nopep8
        return self._cards[3].get_value("x1")

    @x1.setter
    def x1(self, value: float) -> None:
        self._cards[3].set_value("x1", value)

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
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT=3, may be overridden on the element card, see *ELEMENT_SHELL_BETA.
        """ # nopep8
        return self._cards[4].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[4].set_value("beta", value)

    @property
    def isrefg(self) -> int:
        """Get or set the Initial stress by reference geometry for FORM=12
        EQ.0.0:  default.  Not active.
        EQ.1.0:  active
        """ # nopep8
        return self._cards[4].get_value("isrefg")

    @isrefg.setter
    def isrefg(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""isrefg must be one of {0,1}""")
        self._cards[4].set_value("isrefg", value)

    @property
    def lca(self) -> int:
        """Get or set the Load curve ID for stress versus strain along the a-axis fiber; available when FORM=4 only. If zero, EA is used.
        """ # nopep8
        return self._cards[5].get_value("lca")

    @lca.setter
    def lca(self, value: int) -> None:
        self._cards[5].set_value("lca", value)

    @property
    def lcb(self) -> int:
        """Get or set the Load curve ID for stress versus strain along the b-axis fiber; available when FORM=4 only. If zero, EB is used.
        """ # nopep8
        return self._cards[5].get_value("lcb")

    @lcb.setter
    def lcb(self, value: int) -> None:
        self._cards[5].set_value("lcb", value)

    @property
    def lcab(self) -> int:
        """Get or set the Load curve ID for stress versus strain in the ab-plane; available when FORM=4 only. If zero, GAB is used.
        """ # nopep8
        return self._cards[5].get_value("lcab")

    @lcab.setter
    def lcab(self, value: int) -> None:
        self._cards[5].set_value("lcab", value)

    @property
    def lcua(self) -> int:
        """Get or set the Unload/reload curve ID for stress versus strain along the a-axis fiber; available when FORM=4 only. If zero, LCA is used.
        """ # nopep8
        return self._cards[5].get_value("lcua")

    @lcua.setter
    def lcua(self, value: int) -> None:
        self._cards[5].set_value("lcua", value)

    @property
    def lcub(self) -> int:
        """Get or set the Load curve ID for stress versus strain along the b-axis fiber; available when FORM=4 only. If zero, LCB is used.
        """ # nopep8
        return self._cards[5].get_value("lcub")

    @lcub.setter
    def lcub(self, value: int) -> None:
        self._cards[5].set_value("lcub", value)

    @property
    def lcuab(self) -> int:
        """Get or set the Load curve ID for stress versus strain in the ab-plane; available when FORM=4 only. If zero, LCAB is used.
        """ # nopep8
        return self._cards[5].get_value("lcuab")

    @lcuab.setter
    def lcuab(self, value: int) -> None:
        self._cards[5].set_value("lcuab", value)

    @property
    def rl(self) -> typing.Optional[float]:
        """Get or set the Optional reloading parameter for FORM=14.  Values between 0.0 (reloading on unloading curve-default) and 1.0 (reloading on a minimum linear slope between unloading curve and loading curve) are possible.
        """ # nopep8
        return self._cards[5].get_value("rl")

    @rl.setter
    def rl(self, value: float) -> None:
        self._cards[5].set_value("rl", value)

    @property
    def lcaa(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID. Load curve ID defines the stress along the a-axis fiber versus biaxial strain. Table ID defines for each directional strain rate a load curve representing stress along the a-axis fiber versus biaxial strain. Available for FORM=-14 only, if zero, LCA is used.
        """ # nopep8
        return self._cards[6].get_value("lcaa")

    @lcaa.setter
    def lcaa(self, value: int) -> None:
        self._cards[6].set_value("lcaa", value)

    @property
    def lcbb(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID. Load curve ID defines the stress along the b-axis fiber versus biaxial strain. Table ID defines for each directional strain rate a load curve representing stress along the b-axis fiber versus biaxial strain. Available for FORM=-14 only, if zero, LCB is used.
        """ # nopep8
        return self._cards[6].get_value("lcbb")

    @lcbb.setter
    def lcbb(self, value: int) -> None:
        self._cards[6].set_value("lcbb", value)

    @property
    def h(self) -> typing.Optional[float]:
        """Get or set the Normalized hysteresis parameter between 0 and 1.
        """ # nopep8
        return self._cards[6].get_value("h")

    @h.setter
    def h(self, value: float) -> None:
        self._cards[6].set_value("h", value)

    @property
    def dt(self) -> typing.Optional[int]:
        """Get or set the Strain rate averaging option.
        EQ.0.0: Strain rate is evaluated using a running average.
        LT.0.0: Strain rate is evaluated using average of last 11 time steps.
        GT.0.0: Strain rate is averaged over the last DT time units.
        """ # nopep8
        return self._cards[6].get_value("dt")

    @dt.setter
    def dt(self, value: int) -> None:
        self._cards[6].set_value("dt", value)

    @property
    def ecoat(self) -> typing.Optional[float]:
        """Get or set the Young's modulus of coat material, see remark 14.
        """ # nopep8
        return self._cards[6].get_value("ecoat")

    @ecoat.setter
    def ecoat(self, value: float) -> None:
        self._cards[6].set_value("ecoat", value)

    @property
    def scoat(self) -> typing.Optional[float]:
        """Get or set the Yield stress of coat material, see remark 14.
        """ # nopep8
        return self._cards[6].get_value("scoat")

    @scoat.setter
    def scoat(self, value: float) -> None:
        self._cards[6].set_value("scoat", value)

    @property
    def tcoat(self) -> typing.Optional[float]:
        """Get or set the Thickness of coat material, see remark 14.
        """ # nopep8
        return self._cards[6].get_value("tcoat")

    @tcoat.setter
    def tcoat(self, value: float) -> None:
        self._cards[6].set_value("tcoat", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[7].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[7].cards[0].set_value("title", value)

