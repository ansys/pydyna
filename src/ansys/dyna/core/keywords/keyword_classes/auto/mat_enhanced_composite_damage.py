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

class MatEnhancedCompositeDamage(KeywordBase):
    """DYNA MAT_ENHANCED_COMPOSITE_DAMAGE keyword"""

    keyword = "MAT"
    subkeyword = "ENHANCED_COMPOSITE_DAMAGE"
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
                        "(ec)",
                        float,
                        40,
                        10,
                        kwargs.get("(ec)")
                    ),
                    Field(
                        "prba",
                        float,
                        50,
                        10,
                        kwargs.get("prba")
                    ),
                    Field(
                        "(prca)",
                        float,
                        60,
                        10,
                        kwargs.get("(prca)")
                    ),
                    Field(
                        "(prcb)",
                        float,
                        70,
                        10,
                        kwargs.get("(prcb)")
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
                        "(kf)",
                        float,
                        30,
                        10,
                        kwargs.get("(kf)")
                    ),
                    Field(
                        "aopt",
                        float,
                        40,
                        10,
                        kwargs.get("aopt")
                    ),
                    Field(
                        "2way",
                        float,
                        50,
                        10,
                        kwargs.get("2way")
                    ),
                    Field(
                        "ti",
                        float,
                        60,
                        10,
                        kwargs.get("ti")
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
                        "mangle",
                        float,
                        60,
                        10,
                        kwargs.get("mangle")
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
                        "dfailm",
                        float,
                        60,
                        10,
                        kwargs.get("dfailm")
                    ),
                    Field(
                        "dfails",
                        float,
                        70,
                        10,
                        kwargs.get("dfails")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "tfail",
                        float,
                        0,
                        10,
                        kwargs.get("tfail")
                    ),
                    Field(
                        "alph",
                        float,
                        10,
                        10,
                        kwargs.get("alph")
                    ),
                    Field(
                        "soft",
                        float,
                        20,
                        10,
                        kwargs.get("soft", 1.0)
                    ),
                    Field(
                        "fbrt",
                        float,
                        30,
                        10,
                        kwargs.get("fbrt")
                    ),
                    Field(
                        "ycfac",
                        float,
                        40,
                        10,
                        kwargs.get("ycfac", 2.0)
                    ),
                    Field(
                        "dfailt",
                        float,
                        50,
                        10,
                        kwargs.get("dfailt")
                    ),
                    Field(
                        "dfailc",
                        float,
                        60,
                        10,
                        kwargs.get("dfailc")
                    ),
                    Field(
                        "efs",
                        float,
                        70,
                        10,
                        kwargs.get("efs")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xc",
                        float,
                        0,
                        10,
                        kwargs.get("xc")
                    ),
                    Field(
                        "xt",
                        float,
                        10,
                        10,
                        kwargs.get("xt")
                    ),
                    Field(
                        "yc",
                        float,
                        20,
                        10,
                        kwargs.get("yc")
                    ),
                    Field(
                        "yt",
                        float,
                        30,
                        10,
                        kwargs.get("yt")
                    ),
                    Field(
                        "sc",
                        float,
                        40,
                        10,
                        kwargs.get("sc")
                    ),
                    Field(
                        "crit",
                        float,
                        50,
                        10,
                        kwargs.get("crit", 54.0)
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
                        "pel",
                        float,
                        0,
                        10,
                        kwargs.get("pel")
                    ),
                    Field(
                        "epsf",
                        float,
                        10,
                        10,
                        kwargs.get("epsf")
                    ),
                    Field(
                        "epsr",
                        float,
                        20,
                        10,
                        kwargs.get("epsr")
                    ),
                    Field(
                        "tsmd",
                        float,
                        30,
                        10,
                        kwargs.get("tsmd")
                    ),
                    Field(
                        "soft2",
                        float,
                        40,
                        10,
                        kwargs.get("soft2", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "slimt1",
                        float,
                        0,
                        10,
                        kwargs.get("slimt1")
                    ),
                    Field(
                        "slimc1",
                        float,
                        10,
                        10,
                        kwargs.get("slimc1")
                    ),
                    Field(
                        "slimt2",
                        float,
                        20,
                        10,
                        kwargs.get("slimt2")
                    ),
                    Field(
                        "slimc2",
                        float,
                        30,
                        10,
                        kwargs.get("slimc2")
                    ),
                    Field(
                        "slims",
                        float,
                        40,
                        10,
                        kwargs.get("slims")
                    ),
                    Field(
                        "ncyred",
                        float,
                        50,
                        10,
                        kwargs.get("ncyred")
                    ),
                    Field(
                        "softg",
                        float,
                        60,
                        10,
                        kwargs.get("softg", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcxc",
                        int,
                        0,
                        10,
                        kwargs.get("lcxc")
                    ),
                    Field(
                        "lcxt",
                        int,
                        10,
                        10,
                        kwargs.get("lcxt")
                    ),
                    Field(
                        "lcyc",
                        int,
                        20,
                        10,
                        kwargs.get("lcyc")
                    ),
                    Field(
                        "lcyt",
                        int,
                        30,
                        10,
                        kwargs.get("lcyt")
                    ),
                    Field(
                        "lcsc",
                        int,
                        40,
                        10,
                        kwargs.get("lcsc")
                    ),
                    Field(
                        "dt",
                        float,
                        50,
                        10,
                        kwargs.get("dt")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatEnhancedCompositeDamage.option_specs[0],
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
        """Get or set the Ea, Young's modulus - longitudinal direction.
        """ # nopep8
        return self._cards[0].get_value("ea")

    @ea.setter
    def ea(self, value: float) -> None:
        self._cards[0].set_value("ea", value)

    @property
    def eb(self) -> typing.Optional[float]:
        """Get or set the Eb, Young's modulus - transverse direction.
        """ # nopep8
        return self._cards[0].get_value("eb")

    @eb.setter
    def eb(self, value: float) -> None:
        self._cards[0].set_value("eb", value)

    @property
    def _ec_(self) -> typing.Optional[float]:
        """Get or set the Ec, Young's modulus - normal direction (not used).
        """ # nopep8
        return self._cards[0].get_value("(ec)")

    @_ec_.setter
    def _ec_(self, value: float) -> None:
        self._cards[0].set_value("(ec)", value)

    @property
    def prba(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio ba  (not used).
        """ # nopep8
        return self._cards[0].get_value("prba")

    @prba.setter
    def prba(self, value: float) -> None:
        self._cards[0].set_value("prba", value)

    @property
    def _prca_(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio ca (not used).
        """ # nopep8
        return self._cards[0].get_value("(prca)")

    @_prca_.setter
    def _prca_(self, value: float) -> None:
        self._cards[0].set_value("(prca)", value)

    @property
    def _prcb_(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio cb (not used).
        """ # nopep8
        return self._cards[0].get_value("(prcb)")

    @_prcb_.setter
    def _prcb_(self, value: float) -> None:
        self._cards[0].set_value("(prcb)", value)

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
    def _kf_(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus of failed material (not used)
        """ # nopep8
        return self._cards[1].get_value("(kf)")

    @_kf_.setter
    def _kf_(self, value: float) -> None:
        self._cards[1].set_value("(kf)", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option:
        EQ.0.0: locally orthotropic with material axes determined by element nodes N1, N2, and N4, and then,
        for shells only, rotated about the shell element normal by an angle MANGLE
        EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDINATE_VECTOR,
        EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle
        (MANGLE) from a line in the plane of the element defined by the cross product of the vector v with the element normal.
        LT.0.0: the absolute value of AOPT is the coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,_SYSTEM or _VECTOR), Available in R3 version of 971 and later.
        """ # nopep8
        return self._cards[1].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[1].set_value("aopt", value)

    @property
    def _2way(self) -> typing.Optional[float]:
        """Get or set the Flag to turn on 2-way fiber action.
        EQ.0.0:	Standard unidirectional behavior.
        EQ.1.0:	2-way fiber behavior.  The meaning of the fields DFAILT, DFAILC, YC, YT, SLIMT2 and SLIMC are altered if this flag is set.  This option is only available for MAT 54 using thin shells.
        """ # nopep8
        return self._cards[1].get_value("2way")

    @_2way.setter
    def _2way(self, value: float) -> None:
        self._cards[1].set_value("2way", value)

    @property
    def ti(self) -> typing.Optional[float]:
        """Get or set the Flag to turn on transversal isotropic behavior for *MAT_054 solid elements.
        EQ.0.0:	Standard unidirectional behavior
        EQ.1.0 : Transversal isotropic behavior
        """ # nopep8
        return self._cards[1].get_value("ti")

    @ti.setter
    def ti(self, value: float) -> None:
        self._cards[1].set_value("ti", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the x-coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[2].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[2].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the x-coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[2].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[2].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the x-coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[2].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[2].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[2].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[2].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[2].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[2].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[2].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[2].set_value("a3", value)

    @property
    def mangle(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_SOLID_ORTHO.
        """ # nopep8
        return self._cards[2].get_value("mangle")

    @mangle.setter
    def mangle(self, value: float) -> None:
        self._cards[2].set_value("mangle", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[3].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[3].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[3].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[3].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[3].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[3].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[3].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[3].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[3].set_value("d3", value)

    @property
    def dfailm(self) -> typing.Optional[float]:
        """Get or set the Maximum strain for matrix straining in tension or compression. The layer in the element is completely removed after the maximum strain in the matrix direction is reached. The input value is always positive.
        """ # nopep8
        return self._cards[3].get_value("dfailm")

    @dfailm.setter
    def dfailm(self, value: float) -> None:
        self._cards[3].set_value("dfailm", value)

    @property
    def dfails(self) -> typing.Optional[float]:
        """Get or set the Maximum shear strain. The layer in the element is completely removed after the maximum shear strain is reached. The input value is always positive.
        """ # nopep8
        return self._cards[3].get_value("dfails")

    @dfails.setter
    def dfails(self, value: float) -> None:
        self._cards[3].set_value("dfails", value)

    @property
    def tfail(self) -> typing.Optional[float]:
        """Get or set the Time step size criteria for element deletion:
        LT.0:no element deletion by time step size.
        GT.0 and LT.0.1:element is deleted when its time step is smaller than the given value,
        GT.1:element is deleted when the quotient of the actual time step and the original time step drops below the given value.
        """ # nopep8
        return self._cards[4].get_value("tfail")

    @tfail.setter
    def tfail(self, value: float) -> None:
        self._cards[4].set_value("tfail", value)

    @property
    def alph(self) -> typing.Optional[float]:
        """Get or set the Shear stress parameter for the nonlinear term, see Material 22.
        """ # nopep8
        return self._cards[4].get_value("alph")

    @alph.setter
    def alph(self, value: float) -> None:
        self._cards[4].set_value("alph", value)

    @property
    def soft(self) -> float:
        """Get or set the Softening reduction factor for material strength in crashfront elements (default = 1.0). TFAIL must be greater than zero to activate this option.
        """ # nopep8
        return self._cards[4].get_value("soft")

    @soft.setter
    def soft(self, value: float) -> None:
        self._cards[4].set_value("soft", value)

    @property
    def fbrt(self) -> typing.Optional[float]:
        """Get or set the Softening for fiber tensile strength:
        EQ.0.0: tensile strength = Xt
        GT:0.0: tensile strength = Xt , reduced to Xt*FBRT after failure has occurred in compressive matrix mode.
        """ # nopep8
        return self._cards[4].get_value("fbrt")

    @fbrt.setter
    def fbrt(self, value: float) -> None:
        self._cards[4].set_value("fbrt", value)

    @property
    def ycfac(self) -> float:
        """Get or set the Reduction factor for compressive fiber strength after matrix failure. The compressive strength in the fiber direction after compressive matrix failuire is reduced to: Xc=YCFAC* Yc (default YCFAC =2.0).
        """ # nopep8
        return self._cards[4].get_value("ycfac")

    @ycfac.setter
    def ycfac(self, value: float) -> None:
        self._cards[4].set_value("ycfac", value)

    @property
    def dfailt(self) -> typing.Optional[float]:
        """Get or set the Maximum strain for fiber tension. (Maximum 1 = 100% strain). The layer in the element is completely removed after the maximum tensile strain in the fiber direction is reached.
        """ # nopep8
        return self._cards[4].get_value("dfailt")

    @dfailt.setter
    def dfailt(self, value: float) -> None:
        self._cards[4].set_value("dfailt", value)

    @property
    def dfailc(self) -> typing.Optional[float]:
        """Get or set the Maximum strain for fiber compression (Maximum -1 = 100% compression). The layer in the element is completely removed after the maximum tensile strain in the fiber direction is reached. The input value must have a negative sign.
        """ # nopep8
        return self._cards[4].get_value("dfailc")

    @dfailc.setter
    def dfailc(self, value: float) -> None:
        self._cards[4].set_value("dfailc", value)

    @property
    def efs(self) -> typing.Optional[float]:
        """Get or set the Effective failure strain.
        """ # nopep8
        return self._cards[4].get_value("efs")

    @efs.setter
    def efs(self, value: float) -> None:
        self._cards[4].set_value("efs", value)

    @property
    def xc(self) -> typing.Optional[float]:
        """Get or set the Longitudinal compressive strength.
        """ # nopep8
        return self._cards[5].get_value("xc")

    @xc.setter
    def xc(self, value: float) -> None:
        self._cards[5].set_value("xc", value)

    @property
    def xt(self) -> typing.Optional[float]:
        """Get or set the Longitudinal tensile strength.
        """ # nopep8
        return self._cards[5].get_value("xt")

    @xt.setter
    def xt(self, value: float) -> None:
        self._cards[5].set_value("xt", value)

    @property
    def yc(self) -> typing.Optional[float]:
        """Get or set the Transverse compressive strength, b-axis.
        """ # nopep8
        return self._cards[5].get_value("yc")

    @yc.setter
    def yc(self, value: float) -> None:
        self._cards[5].set_value("yc", value)

    @property
    def yt(self) -> typing.Optional[float]:
        """Get or set the Transverse tensile strength, b-axis.
        """ # nopep8
        return self._cards[5].get_value("yt")

    @yt.setter
    def yt(self, value: float) -> None:
        self._cards[5].set_value("yt", value)

    @property
    def sc(self) -> typing.Optional[float]:
        """Get or set the Shear strength, ab plane.
        """ # nopep8
        return self._cards[5].get_value("sc")

    @sc.setter
    def sc(self, value: float) -> None:
        self._cards[5].set_value("sc", value)

    @property
    def crit(self) -> float:
        """Get or set the Failure criterion (material number):
        EQ.54.0: Chang matrix failure criterion (as Material 22) (default),
        EQ.55.0: Tsai-Wu criterion for matrix failure.
        """ # nopep8
        return self._cards[5].get_value("crit")

    @crit.setter
    def crit(self, value: float) -> None:
        if value not in [54.0, 55.0]:
            raise Exception("""crit must be one of {54.0,55.0}""")
        self._cards[5].set_value("crit", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Weighting factor for shear term in tensile fiber mode (0.0 <= BETA <= 1.0).
        """ # nopep8
        return self._cards[5].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[5].set_value("beta", value)

    @property
    def pel(self) -> typing.Optional[float]:
        """Get or set the Percentage of layers which must fail until crashfront is initiated. E.g. |PFL|=80.0, then 80 % of layers must fail until strengths are reduced in neighboring elements. Default: all layers must fail. A single layer fails if 1 in-plane IP fails (PFL>0) or if 4 in-plane IPs fail (PFL<0). (MAT_054 only).
        """ # nopep8
        return self._cards[6].get_value("pel")

    @pel.setter
    def pel(self, value: float) -> None:
        self._cards[6].set_value("pel", value)

    @property
    def epsf(self) -> typing.Optional[float]:
        """Get or set the Damage initiation transverser shear strain. (MAT_054 only).
        """ # nopep8
        return self._cards[6].get_value("epsf")

    @epsf.setter
    def epsf(self, value: float) -> None:
        self._cards[6].set_value("epsf", value)

    @property
    def epsr(self) -> typing.Optional[float]:
        """Get or set the Final rupture transverse shear strain. (MAT_054 only)
        LT.0.0:	|EPSR| is final rupture transverse shear strain. In addition, the element erodes if transverse shear damage reaches TSMD.
        """ # nopep8
        return self._cards[6].get_value("epsr")

    @epsr.setter
    def epsr(self, value: float) -> None:
        self._cards[6].set_value("epsr", value)

    @property
    def tsmd(self) -> typing.Optional[float]:
        """Get or set the Transverse shear maximum damage, default=0.90. (MAT_054 only).
        """ # nopep8
        return self._cards[6].get_value("tsmd")

    @tsmd.setter
    def tsmd(self, value: float) -> None:
        self._cards[6].set_value("tsmd", value)

    @property
    def soft2(self) -> float:
        """Get or set the Ï„ptional â€œorthogonalâ€ softening reduction factor for material strength in crashfront elements (default = 1.0). See remarks.
        """ # nopep8
        return self._cards[6].get_value("soft2")

    @soft2.setter
    def soft2(self, value: float) -> None:
        self._cards[6].set_value("soft2", value)

    @property
    def slimt1(self) -> typing.Optional[float]:
        """Get or set the Factor to determine the minimum stress limit after stress maximum (fiber tension).
        """ # nopep8
        return self._cards[7].get_value("slimt1")

    @slimt1.setter
    def slimt1(self, value: float) -> None:
        self._cards[7].set_value("slimt1", value)

    @property
    def slimc1(self) -> typing.Optional[float]:
        """Get or set the Factor to determine the minimum stress limit after stress maximum (fiber compression).
        """ # nopep8
        return self._cards[7].get_value("slimc1")

    @slimc1.setter
    def slimc1(self, value: float) -> None:
        self._cards[7].set_value("slimc1", value)

    @property
    def slimt2(self) -> typing.Optional[float]:
        """Get or set the Factor to determine the minimum stress limit after stress maximum (matrix tension).
        """ # nopep8
        return self._cards[7].get_value("slimt2")

    @slimt2.setter
    def slimt2(self, value: float) -> None:
        self._cards[7].set_value("slimt2", value)

    @property
    def slimc2(self) -> typing.Optional[float]:
        """Get or set the Factor to determine the minimum stress limit after stress maximum (matrix compression).
        """ # nopep8
        return self._cards[7].get_value("slimc2")

    @slimc2.setter
    def slimc2(self, value: float) -> None:
        self._cards[7].set_value("slimc2", value)

    @property
    def slims(self) -> typing.Optional[float]:
        """Get or set the Factor to determine the minimum stress limit after stress maximum (shear).
        """ # nopep8
        return self._cards[7].get_value("slims")

    @slims.setter
    def slims(self, value: float) -> None:
        self._cards[7].set_value("slims", value)

    @property
    def ncyred(self) -> typing.Optional[float]:
        """Get or set the Number of cycles for stress reduction from maximum to minimum
        """ # nopep8
        return self._cards[7].get_value("ncyred")

    @ncyred.setter
    def ncyred(self, value: float) -> None:
        self._cards[7].set_value("ncyred", value)

    @property
    def softg(self) -> float:
        """Get or set the Softening reduction factor for transverse shear moduli GBC and GCA in crashfront elements (default=1.0)
        """ # nopep8
        return self._cards[7].get_value("softg")

    @softg.setter
    def softg(self, value: float) -> None:
        self._cards[7].set_value("softg", value)

    @property
    def lcxc(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for XC vs. strain rate (XC is ignored with that option).
        """ # nopep8
        return self._cards[8].get_value("lcxc")

    @lcxc.setter
    def lcxc(self, value: int) -> None:
        self._cards[8].set_value("lcxc", value)

    @property
    def lcxt(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for XT vs. strain rate (XT is ignored with that option).
        """ # nopep8
        return self._cards[8].get_value("lcxt")

    @lcxt.setter
    def lcxt(self, value: int) -> None:
        self._cards[8].set_value("lcxt", value)

    @property
    def lcyc(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for YC vs. strain rate (YC is ignored with that option).
        """ # nopep8
        return self._cards[8].get_value("lcyc")

    @lcyc.setter
    def lcyc(self, value: int) -> None:
        self._cards[8].set_value("lcyc", value)

    @property
    def lcyt(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for YT vs. strain rate (YT is ignored with that option).
        """ # nopep8
        return self._cards[8].get_value("lcyt")

    @lcyt.setter
    def lcyt(self, value: int) -> None:
        self._cards[8].set_value("lcyt", value)

    @property
    def lcsc(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for SC vs. strain rate (SC is ignored with that option).
        """ # nopep8
        return self._cards[8].get_value("lcsc")

    @lcsc.setter
    def lcsc(self, value: int) -> None:
        self._cards[8].set_value("lcsc", value)

    @property
    def dt(self) -> typing.Optional[float]:
        """Get or set the Strain rate averaging option.
        EQ.0.0: Strain rate is evaluated using a running average.
        LT.0.0: Strain rate is evaluated using average of last 11 time steps.
        GT.0.0: Strain rate is averaged over the last DT time units
        """ # nopep8
        return self._cards[8].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[8].set_value("dt", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[9].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[9].cards[0].set_value("title", value)

