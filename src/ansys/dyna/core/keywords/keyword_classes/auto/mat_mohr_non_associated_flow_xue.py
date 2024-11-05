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

class MatMohrNonAssociatedFlowXue(KeywordBase):
    """DYNA MAT_MOHR_NON_ASSOCIATED_FLOW_XUE keyword"""

    keyword = "MAT"
    subkeyword = "MOHR_NON_ASSOCIATED_FLOW_XUE"
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
                        "p12",
                        float,
                        40,
                        10,
                        kwargs.get("p12", -0.5)
                    ),
                    Field(
                        "p22",
                        float,
                        50,
                        10,
                        kwargs.get("p22", 1.0)
                    ),
                    Field(
                        "p33",
                        float,
                        60,
                        10,
                        kwargs.get("p33", 3.0)
                    ),
                    Field(
                        "g12",
                        float,
                        70,
                        10,
                        kwargs.get("g12", -0.5)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "g22",
                        float,
                        0,
                        10,
                        kwargs.get("g22", 1.0)
                    ),
                    Field(
                        "g33",
                        float,
                        10,
                        10,
                        kwargs.get("g33", 3.0)
                    ),
                    Field(
                        "lcids",
                        int,
                        20,
                        10,
                        kwargs.get("lcids")
                    ),
                    Field(
                        "lcidv",
                        int,
                        30,
                        10,
                        kwargs.get("lcidv")
                    ),
                    Field(
                        "lcidt",
                        int,
                        40,
                        10,
                        kwargs.get("lcidt")
                    ),
                    Field(
                        "lfld",
                        int,
                        50,
                        10,
                        kwargs.get("lfld", 0)
                    ),
                    Field(
                        "lfrac",
                        int,
                        60,
                        10,
                        kwargs.get("lfrac")
                    ),
                    Field(
                        "w0",
                        float,
                        70,
                        10,
                        kwargs.get("w0")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "a",
                        float,
                        0,
                        10,
                        kwargs.get("a")
                    ),
                    Field(
                        "b0",
                        float,
                        10,
                        10,
                        kwargs.get("b0")
                    ),
                    Field(
                        "gamma",
                        float,
                        20,
                        10,
                        kwargs.get("gamma")
                    ),
                    Field(
                        "c",
                        float,
                        30,
                        10,
                        kwargs.get("c")
                    ),
                    Field(
                        "n",
                        float,
                        40,
                        10,
                        kwargs.get("n")
                    ),
                    Field(
                        "scale",
                        float,
                        50,
                        10,
                        kwargs.get("scale", 1.0)
                    ),
                    Field(
                        "size0",
                        float,
                        60,
                        10,
                        kwargs.get("size0")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "tref",
                        float,
                        0,
                        10,
                        kwargs.get("tref")
                    ),
                    Field(
                        "tmelt",
                        float,
                        10,
                        10,
                        kwargs.get("tmelt")
                    ),
                    Field(
                        "m",
                        float,
                        20,
                        10,
                        kwargs.get("m")
                    ),
                    Field(
                        "eta",
                        float,
                        30,
                        10,
                        kwargs.get("eta")
                    ),
                    Field(
                        "cp",
                        float,
                        40,
                        10,
                        kwargs.get("cp")
                    ),
                    Field(
                        "tini",
                        float,
                        50,
                        10,
                        kwargs.get("tini")
                    ),
                    Field(
                        "depso",
                        float,
                        60,
                        10,
                        kwargs.get("depso")
                    ),
                    Field(
                        "depsad",
                        float,
                        70,
                        10,
                        kwargs.get("depsad")
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
                        "unused",
                        float,
                        20,
                        10,
                        kwargs.get("unused")
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
                ],
            ),
            OptionCardSet(
                option_spec = MatMohrNonAssociatedFlowXue.option_specs[0],
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
    def p12(self) -> float:
        """Get or set the Yield function parameters, defined by Lankford parameters in
        rolling (0 degree), diagonal (45 degree) and transverse (90 degree) directions,
        respectively; see Non-associated flow rule.
        """ # nopep8
        return self._cards[0].get_value("p12")

    @p12.setter
    def p12(self, value: float) -> None:
        self._cards[0].set_value("p12", value)

    @property
    def p22(self) -> float:
        """Get or set the Yield function parameters, defined by Lankford parameters in
        rolling (0 degree), diagonal (45 degree) and transverse (90 degree) directions,
        respectively; see Non-associated flow rule.
        """ # nopep8
        return self._cards[0].get_value("p22")

    @p22.setter
    def p22(self, value: float) -> None:
        self._cards[0].set_value("p22", value)

    @property
    def p33(self) -> float:
        """Get or set the Yield function parameters, defined by Lankford parameters in
        rolling (0 degree), diagonal (45 degree) and transverse (90 degree) directions,
        respectively; see Non-associated flow rule.
        """ # nopep8
        return self._cards[0].get_value("p33")

    @p33.setter
    def p33(self, value: float) -> None:
        self._cards[0].set_value("p33", value)

    @property
    def g12(self) -> float:
        """Get or set the Plastic flow potential parameters, defined by Lankford parameters
        in rolling (0 degree), diagonal (45 degree) and transverse (90 degree) directions,
        respectively; see Non-associated flow rule.
        """ # nopep8
        return self._cards[0].get_value("g12")

    @g12.setter
    def g12(self, value: float) -> None:
        self._cards[0].set_value("g12", value)

    @property
    def g22(self) -> float:
        """Get or set the Plastic flow potential parameters, defined by Lankford parameters
        in rolling (0 degree), diagonal (45 degree) and transverse (90 degree) directions,
        respectively; see Non-associated flow rule.
        """ # nopep8
        return self._cards[1].get_value("g22")

    @g22.setter
    def g22(self, value: float) -> None:
        self._cards[1].set_value("g22", value)

    @property
    def g33(self) -> float:
        """Get or set the Plastic flow potential parameters, defined by Lankford parameters
        in rolling (0 degree), diagonal (45 degree) and transverse (90 degree) directions,
        respectively; see Non-associated flow rule.
        """ # nopep8
        return self._cards[1].get_value("g33")

    @g33.setter
    def g33(self, value: float) -> None:
        self._cards[1].set_value("g33", value)

    @property
    def lcids(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining stress vs. strain hardening behavior from a
        uniaxial tension test; must be along the rolling direction. Also see A
        modified Johnson-Cook.
        """ # nopep8
        return self._cards[1].get_value("lcids")

    @lcids.setter
    def lcids(self, value: int) -> None:
        self._cards[1].set_value("lcids", value)

    @property
    def lcidv(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining stress scale factors vs. strain rates (Figure
        M260B-1 middle); determined from experiments. Strain rates are
        stored in history variable #5. Strain rate scale factors are stored in
        history variable #6. To turn on the variables for viewing in LSPrePost,
        set NEIPS to at least "6" in *DATABASE_EXTENT_BINARY.
        It is very useful to know what levels of strain rates, and strain
        rate scale factors in a particular simulation. Once d3plot files are
        opened in LS-PrePost, individual element time history can be plotted
        via menu option Post → History, or a color contour of the entire part
        can be viewed with the menu option Post → FriComp → Misc. Also
        see A modified Johnson-Cook.
        """ # nopep8
        return self._cards[1].get_value("lcidv")

    @lcidv.setter
    def lcidv(self, value: int) -> None:
        self._cards[1].set_value("lcidv", value)

    @property
    def lcidt(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining stress scale factors vs. temperature in
        Kelvin (Figure M260B-1 bottom), determined from experiments.
        Temperatures are stored in history variable #4. Temperature scale
        factors are stored in history variable #7. To turn on this variable for
        viewing in LS-PrePost, set NEIPS to at least "7" in
        *DATABASE_EXTENT_BINARY. It is very useful to know what
        levels of temperatures and temperature scale factors in a particular
        simulation. Once d3plot files are opened in LS-PrePost, individual
        element time history can be plotted via menu option Post → History,
        or a color contour of the entire part can be viewed with the menu
        option Post → FriComp → Misc. Also see A modified Johnson-Cook..
        """ # nopep8
        return self._cards[1].get_value("lcidt")

    @lcidt.setter
    def lcidt(self, value: int) -> None:
        self._cards[1].set_value("lcidt", value)

    @property
    def lfld(self) -> int:
        """Get or set the Load curve ID defining traditional Forming Limit Diagram for linear strain paths.
        """ # nopep8
        return self._cards[1].get_value("lfld")

    @lfld.setter
    def lfld(self, value: int) -> None:
        self._cards[1].set_value("lfld", value)

    @property
    def lfrac(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining a fracture limit curve. Leave this field
        empty if parameters A, B0, GAMMA, C, N are defined. However, if
        this field is defined, parameters A, B0, GAMMA, C, N will be
        ignored even if they are defined.
        """ # nopep8
        return self._cards[1].get_value("lfrac")

    @lfrac.setter
    def lfrac(self, value: int) -> None:
        self._cards[1].set_value("lfrac", value)

    @property
    def w0(self) -> typing.Optional[float]:
        """Get or set the Neck (FLD failure) width, typically is the blank thickness.
        """ # nopep8
        return self._cards[1].get_value("w0")

    @w0.setter
    def w0(self, value: float) -> None:
        self._cards[1].set_value("w0", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Material parameters for the rate-dependent Hosford-Coulomb
        fracture initiation model, see Rate-dependent Hosford-Coulomb.
        Ignored if LFRAC is defined.
        """ # nopep8
        return self._cards[2].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        self._cards[2].set_value("a", value)

    @property
    def b0(self) -> typing.Optional[float]:
        """Get or set the Material parameters for the rate-dependent Hosford-Coulomb
        fracture initiation model, see Rate-dependent Hosford-Coulomb.
        Ignored if LFRAC is defined.
        """ # nopep8
        return self._cards[2].get_value("b0")

    @b0.setter
    def b0(self, value: float) -> None:
        self._cards[2].set_value("b0", value)

    @property
    def gamma(self) -> typing.Optional[float]:
        """Get or set the Material parameters for the rate-dependent Hosford-Coulomb
        fracture initiation model, see Rate-dependent Hosford-Coulomb.
        Ignored if LFRAC is defined.
        """ # nopep8
        return self._cards[2].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        self._cards[2].set_value("gamma", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Material parameters for the rate-dependent Hosford-Coulomb
        fracture initiation model, see Rate-dependent Hosford-Coulomb.
        Ignored if LFRAC is defined.
        """ # nopep8
        return self._cards[2].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        self._cards[2].set_value("c", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Material parameters for the rate-dependent Hosford-Coulomb
        fracture initiation model, see Rate-dependent Hosford-Coulomb.
        Ignored if LFRAC is defined.
        """ # nopep8
        return self._cards[2].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        self._cards[2].set_value("n", value)

    @property
    def scale(self) -> float:
        """Get or set the This variable can be used to speed up the simulation while
        equalizing the strain rate effect, useful especially in cases where the
        pulling speed or punch speed is slow. For example, if the pulling
        speed is at 15 mm/s but running the simulation at this speed will
        take a long time, the pulling speed can be increased to 500 mm/s
        while "SCALE" can be set to 0.03, giving the same results as those
        from 15 mm/s, but with the benefit of greatly reduced computational
        time, see examples and Figures in *MAT_260A for details.
        Furthermore, the increased absolute value (within a reasonable
        range) of mass scaling -1.0*dt2ms frequently used in forming
        simulation does not affect the strain rates, as shown in the examples
        and Figures in *MAT_260A.
        """ # nopep8
        return self._cards[2].get_value("scale")

    @scale.setter
    def scale(self, value: float) -> None:
        self._cards[2].set_value("scale", value)

    @property
    def size0(self) -> typing.Optional[float]:
        """Get or set the Fracture gage length used in an experimental measurement,	typically between 0.2~0.5mm.
        """ # nopep8
        return self._cards[2].get_value("size0")

    @size0.setter
    def size0(self, value: float) -> None:
        self._cards[2].set_value("size0", value)

    @property
    def tref(self) -> typing.Optional[float]:
        """Get or set the Material parameters for strain softening effect due to temperature.
        TINI is the initial temperature. See A modified Johnson-Cook for
        other parameters' definitions..
        """ # nopep8
        return self._cards[3].get_value("tref")

    @tref.setter
    def tref(self, value: float) -> None:
        self._cards[3].set_value("tref", value)

    @property
    def tmelt(self) -> typing.Optional[float]:
        """Get or set the Material parameters for strain softening effect due to temperature.
        TINI is the initial temperature. See A modified Johnson-Cook for
        other parameters' definitions.
        """ # nopep8
        return self._cards[3].get_value("tmelt")

    @tmelt.setter
    def tmelt(self, value: float) -> None:
        self._cards[3].set_value("tmelt", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Material parameters for strain softening effect due to temperature.
        TINI is the initial temperature. See A modified Johnson-Cook for
        other parameters' definitions.
        """ # nopep8
        return self._cards[3].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        self._cards[3].set_value("m", value)

    @property
    def eta(self) -> typing.Optional[float]:
        """Get or set the Material parameters for strain softening effect due to temperature.
        TINI is the initial temperature. See A modified Johnson-Cook for
        other parameters' definitions.
        """ # nopep8
        return self._cards[3].get_value("eta")

    @eta.setter
    def eta(self, value: float) -> None:
        self._cards[3].set_value("eta", value)

    @property
    def cp(self) -> typing.Optional[float]:
        """Get or set the Material parameters for strain softening effect due to temperature.
        TINI is the initial temperature. See A modified Johnson-Cook for
        other parameters' definitions.
        """ # nopep8
        return self._cards[3].get_value("cp")

    @cp.setter
    def cp(self, value: float) -> None:
        self._cards[3].set_value("cp", value)

    @property
    def tini(self) -> typing.Optional[float]:
        """Get or set the Material parameters for strain softening effect due to temperature.
        TINI is the initial temperature. See A modified Johnson-Cook for
        other parameters' definitions.
        """ # nopep8
        return self._cards[3].get_value("tini")

    @tini.setter
    def tini(self, value: float) -> None:
        self._cards[3].set_value("tini", value)

    @property
    def depso(self) -> typing.Optional[float]:
        """Get or set the Material parameters for strain softening effect due to temperature.
        TINI is the initial temperature. See A modified Johnson-Cook for
        other parameters' definitions.
        """ # nopep8
        return self._cards[3].get_value("depso")

    @depso.setter
    def depso(self, value: float) -> None:
        self._cards[3].set_value("depso", value)

    @property
    def depsad(self) -> typing.Optional[float]:
        """Get or set the Material parameters for strain softening effect due to temperature.
        TINI is the initial temperature. See A modified Johnson-Cook for
        other parameters' definitions.
        """ # nopep8
        return self._cards[3].get_value("depsad")

    @depsad.setter
    def depsad(self, value: float) -> None:
        self._cards[3].set_value("depsad", value)

    @property
    def ef0(self) -> typing.Optional[float]:
        """Get or set the Material parameters for the option XUE. The parameter k in the
        original paper is assumed to be 1.0. For details, refer to Xue, L.,
        Wierzbicki, T.'s 2009 paper "Numerical simulation of fracture mode
        transition in ductile plates" in the International Journal of Solids and Structures
        """ # nopep8
        return self._cards[4].get_value("ef0")

    @ef0.setter
    def ef0(self, value: float) -> None:
        self._cards[4].set_value("ef0", value)

    @property
    def plim(self) -> typing.Optional[float]:
        """Get or set the Material parameters for the option XUE. The parameter k in the
        original paper is assumed to be 1.0. For details, refer to Xue, L.,
        Wierzbicki, T.'s 2009 paper "Numerical simulation of fracture mode
        transition in ductile plates" in the International Journal of Solids and Structures
        """ # nopep8
        return self._cards[4].get_value("plim")

    @plim.setter
    def plim(self, value: float) -> None:
        self._cards[4].set_value("plim", value)

    @property
    def q(self) -> typing.Optional[float]:
        """Get or set the Material parameters for the option XUE. The parameter k in the
        original paper is assumed to be 1.0. For details, refer to Xue, L.,
        Wierzbicki, T.'s 2009 paper "Numerical simulation of fracture mode
        transition in ductile plates" in the International Journal of Solids and Structures
        """ # nopep8
        return self._cards[4].get_value("q")

    @q.setter
    def q(self, value: float) -> None:
        self._cards[4].set_value("q", value)

    @property
    def gama(self) -> typing.Optional[float]:
        """Get or set the Material parameters for the option XUE. The parameter k in the
        original paper is assumed to be 1.0. For details, refer to Xue, L.,
        Wierzbicki, T.'s 2009 paper "Numerical simulation of fracture mode
        transition in ductile plates" in the International Journal of Solids and Structures.
        """ # nopep8
        return self._cards[4].get_value("gama")

    @gama.setter
    def gama(self, value: float) -> None:
        self._cards[4].set_value("gama", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Material parameters for the option XUE. The parameter k in the
        original paper is assumed to be 1.0. For details, refer to Xue, L.,
        Wierzbicki, T.'s 2009 paper "Numerical simulation of fracture mode
        transition in ductile plates" in the International Journal of Solids and Structures.
        """ # nopep8
        return self._cards[4].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        self._cards[4].set_value("m", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option (see MAT_OPTION TROPIC_ELASTIC for a more complete description):
        EQ.0.0:	locally orthotropic with material axes determined by element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES, and then rotated about the shell element normal by theangle BETA.
        EQ.2.0:	globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDINATE_VECTOR:
        EQ.3.0:	locally orthotropic material axes determined by rotating the material axes about the element normal by an angle, BETA, from a line in the plane of the element defined by the cross product of the vector v with the element normal:
        LT.0.0:	the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES, *DEFINE_COORDINATE_SYSTEM or *DEFINE__COORDINATE_VECTOR)..
        """ # nopep8
        return self._cards[5].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[5].set_value("aopt", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[6].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[6].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[6].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[6].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[6].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[6].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[7].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[7].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[7].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[7].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[7].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[7].set_value("v3", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[8].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[8].cards[0].set_value("title", value)

