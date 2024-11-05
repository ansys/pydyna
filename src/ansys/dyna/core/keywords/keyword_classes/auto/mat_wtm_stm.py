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

class MatWtmStm(KeywordBase):
    """DYNA MAT_WTM_STM keyword"""

    keyword = "MAT"
    subkeyword = "WTM_STM"
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
                        "numfi",
                        float,
                        40,
                        10,
                        kwargs.get("numfi")
                    ),
                    Field(
                        "npsc",
                        float,
                        50,
                        10,
                        kwargs.get("npsc")
                    ),
                    Field(
                        "wc",
                        float,
                        60,
                        10,
                        kwargs.get("wc")
                    ),
                    Field(
                        "tauc",
                        float,
                        70,
                        10,
                        kwargs.get("tauc")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sigma0",
                        float,
                        0,
                        10,
                        kwargs.get("sigma0")
                    ),
                    Field(
                        "qr1",
                        float,
                        10,
                        10,
                        kwargs.get("qr1")
                    ),
                    Field(
                        "cr1",
                        float,
                        20,
                        10,
                        kwargs.get("cr1")
                    ),
                    Field(
                        "qr2",
                        float,
                        30,
                        10,
                        kwargs.get("qr2")
                    ),
                    Field(
                        "cr2",
                        float,
                        40,
                        10,
                        kwargs.get("cr2")
                    ),
                    Field(
                        "k",
                        float,
                        50,
                        10,
                        kwargs.get("k")
                    ),
                    Field(
                        "lc",
                        float,
                        60,
                        10,
                        kwargs.get("lc")
                    ),
                    Field(
                        "flg",
                        int,
                        70,
                        10,
                        kwargs.get("flg", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "a1",
                        float,
                        0,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "a2",
                        float,
                        10,
                        10,
                        kwargs.get("a2")
                    ),
                    Field(
                        "a3",
                        float,
                        20,
                        10,
                        kwargs.get("a3")
                    ),
                    Field(
                        "a4",
                        float,
                        30,
                        10,
                        kwargs.get("a4")
                    ),
                    Field(
                        "a5",
                        float,
                        40,
                        10,
                        kwargs.get("a5")
                    ),
                    Field(
                        "a6",
                        float,
                        50,
                        10,
                        kwargs.get("a6")
                    ),
                    Field(
                        "a7",
                        float,
                        60,
                        10,
                        kwargs.get("a7")
                    ),
                    Field(
                        "a8",
                        float,
                        70,
                        10,
                        kwargs.get("a8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "s00",
                        float,
                        0,
                        10,
                        kwargs.get("s00")
                    ),
                    Field(
                        "s45",
                        float,
                        10,
                        10,
                        kwargs.get("s45")
                    ),
                    Field(
                        "s90",
                        float,
                        20,
                        10,
                        kwargs.get("s90")
                    ),
                    Field(
                        "sbb",
                        float,
                        30,
                        10,
                        kwargs.get("sbb")
                    ),
                    Field(
                        "r00",
                        float,
                        40,
                        10,
                        kwargs.get("r00")
                    ),
                    Field(
                        "r45",
                        float,
                        50,
                        10,
                        kwargs.get("r45")
                    ),
                    Field(
                        "r90",
                        float,
                        60,
                        10,
                        kwargs.get("r90")
                    ),
                    Field(
                        "rbb",
                        float,
                        70,
                        10,
                        kwargs.get("rbb")
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
                        "c",
                        float,
                        10,
                        10,
                        kwargs.get("c")
                    ),
                    Field(
                        "h",
                        float,
                        20,
                        10,
                        kwargs.get("h")
                    ),
                    Field(
                        "p",
                        float,
                        30,
                        10,
                        kwargs.get("p")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "qx1",
                        float,
                        0,
                        10,
                        kwargs.get("qx1")
                    ),
                    Field(
                        "cx1",
                        float,
                        10,
                        10,
                        kwargs.get("cx1")
                    ),
                    Field(
                        "qx2",
                        float,
                        20,
                        10,
                        kwargs.get("qx2")
                    ),
                    Field(
                        "cx2",
                        float,
                        30,
                        10,
                        kwargs.get("cx2")
                    ),
                    Field(
                        "edot",
                        float,
                        40,
                        10,
                        kwargs.get("edot")
                    ),
                    Field(
                        "m",
                        float,
                        50,
                        10,
                        kwargs.get("m")
                    ),
                    Field(
                        "emin",
                        float,
                        60,
                        10,
                        kwargs.get("emin")
                    ),
                    Field(
                        "s100",
                        float,
                        70,
                        10,
                        kwargs.get("s100")
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
                        "beta",
                        float,
                        10,
                        10,
                        kwargs.get("beta")
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
                option_spec = MatWtmStm.option_specs[0],
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
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def numfi(self) -> typing.Optional[float]:
        """Get or set the Number of through thickness integration points that must fail before the element is deleted (remember to change this number if switching between full and reduced integration type of elements).
        """ # nopep8
        return self._cards[0].get_value("numfi")

    @numfi.setter
    def numfi(self, value: float) -> None:
        self._cards[0].set_value("numfi", value)

    @property
    def npsc(self) -> typing.Optional[float]:
        """Get or set the Critical value   of the plastic thickness strain (used in the CTS fracture criterion).
        """ # nopep8
        return self._cards[0].get_value("npsc")

    @npsc.setter
    def npsc(self, value: float) -> None:
        self._cards[0].set_value("npsc", value)

    @property
    def wc(self) -> typing.Optional[float]:
        """Get or set the Critical value   for the Cockcroft-Latham fracture criterion
        """ # nopep8
        return self._cards[0].get_value("wc")

    @wc.setter
    def wc(self, value: float) -> None:
        self._cards[0].set_value("wc", value)

    @property
    def tauc(self) -> typing.Optional[float]:
        """Get or set the Critical value   for the Bressan-Williams shear fracture criterion
        """ # nopep8
        return self._cards[0].get_value("tauc")

    @tauc.setter
    def tauc(self, value: float) -> None:
        self._cards[0].set_value("tauc", value)

    @property
    def sigma0(self) -> typing.Optional[float]:
        """Get or set the Initial mean value of yield stress  .
        """ # nopep8
        return self._cards[1].get_value("sigma0")

    @sigma0.setter
    def sigma0(self, value: float) -> None:
        self._cards[1].set_value("sigma0", value)

    @property
    def qr1(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter .
        """ # nopep8
        return self._cards[1].get_value("qr1")

    @qr1.setter
    def qr1(self, value: float) -> None:
        self._cards[1].set_value("qr1", value)

    @property
    def cr1(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter
        """ # nopep8
        return self._cards[1].get_value("cr1")

    @cr1.setter
    def cr1(self, value: float) -> None:
        self._cards[1].set_value("cr1", value)

    @property
    def qr2(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter
        """ # nopep8
        return self._cards[1].get_value("qr2")

    @qr2.setter
    def qr2(self, value: float) -> None:
        self._cards[1].set_value("qr2", value)

    @property
    def cr2(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter
        """ # nopep8
        return self._cards[1].get_value("cr2")

    @cr2.setter
    def cr2(self, value: float) -> None:
        self._cards[1].set_value("cr2", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the equals half YLD2003 exponent  .  Recommended value for FCC materials is  , i.e.  .
        """ # nopep8
        return self._cards[1].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        self._cards[1].set_value("k", value)

    @property
    def lc(self) -> typing.Optional[float]:
        """Get or set the First load curve number for process effects, i.e. the load curve describing the relation between the pre-strain and the yield stress  .  Similar curves for  ,  ,  ,  , and must follow consecutively from this number
        """ # nopep8
        return self._cards[1].get_value("lc")

    @lc.setter
    def lc(self, value: float) -> None:
        self._cards[1].set_value("lc", value)

    @property
    def flg(self) -> int:
        """Get or set the flag
        """ # nopep8
        return self._cards[1].get_value("flg")

    @flg.setter
    def flg(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""flg must be one of {0,1,2}""")
        self._cards[1].set_value("flg", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Yld2003 parameter  .
        """ # nopep8
        return self._cards[2].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[2].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Yld2003 parameter .
        """ # nopep8
        return self._cards[2].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[2].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Yld2003 parameter
        """ # nopep8
        return self._cards[2].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[2].set_value("a3", value)

    @property
    def a4(self) -> typing.Optional[float]:
        """Get or set the Yld2003 parameter
        """ # nopep8
        return self._cards[2].get_value("a4")

    @a4.setter
    def a4(self, value: float) -> None:
        self._cards[2].set_value("a4", value)

    @property
    def a5(self) -> typing.Optional[float]:
        """Get or set the Yld2003 parameter
        """ # nopep8
        return self._cards[2].get_value("a5")

    @a5.setter
    def a5(self, value: float) -> None:
        self._cards[2].set_value("a5", value)

    @property
    def a6(self) -> typing.Optional[float]:
        """Get or set the Yld2003 parameter
        """ # nopep8
        return self._cards[2].get_value("a6")

    @a6.setter
    def a6(self, value: float) -> None:
        self._cards[2].set_value("a6", value)

    @property
    def a7(self) -> typing.Optional[float]:
        """Get or set the Yld2003 parameter
        """ # nopep8
        return self._cards[2].get_value("a7")

    @a7.setter
    def a7(self, value: float) -> None:
        self._cards[2].set_value("a7", value)

    @property
    def a8(self) -> typing.Optional[float]:
        """Get or set the Yld2003 parameter
        """ # nopep8
        return self._cards[2].get_value("a8")

    @a8.setter
    def a8(self, value: float) -> None:
        self._cards[2].set_value("a8", value)

    @property
    def s00(self) -> typing.Optional[float]:
        """Get or set the Yield stress in   direction.
        """ # nopep8
        return self._cards[3].get_value("s00")

    @s00.setter
    def s00(self, value: float) -> None:
        self._cards[3].set_value("s00", value)

    @property
    def s45(self) -> typing.Optional[float]:
        """Get or set the Yield stress in   direction.
        """ # nopep8
        return self._cards[3].get_value("s45")

    @s45.setter
    def s45(self, value: float) -> None:
        self._cards[3].set_value("s45", value)

    @property
    def s90(self) -> typing.Optional[float]:
        """Get or set the Yield stress in   direction
        """ # nopep8
        return self._cards[3].get_value("s90")

    @s90.setter
    def s90(self, value: float) -> None:
        self._cards[3].set_value("s90", value)

    @property
    def sbb(self) -> typing.Optional[float]:
        """Get or set the Balanced biaxial flow stress
        """ # nopep8
        return self._cards[3].get_value("sbb")

    @sbb.setter
    def sbb(self, value: float) -> None:
        self._cards[3].set_value("sbb", value)

    @property
    def r00(self) -> typing.Optional[float]:
        """Get or set the R-ratio in   direction
        """ # nopep8
        return self._cards[3].get_value("r00")

    @r00.setter
    def r00(self, value: float) -> None:
        self._cards[3].set_value("r00", value)

    @property
    def r45(self) -> typing.Optional[float]:
        """Get or set the R-ratio in   direction
        """ # nopep8
        return self._cards[3].get_value("r45")

    @r45.setter
    def r45(self, value: float) -> None:
        self._cards[3].set_value("r45", value)

    @property
    def r90(self) -> typing.Optional[float]:
        """Get or set the R-ratio in   direction
        """ # nopep8
        return self._cards[3].get_value("r90")

    @r90.setter
    def r90(self, value: float) -> None:
        self._cards[3].set_value("r90", value)

    @property
    def rbb(self) -> typing.Optional[float]:
        """Get or set the Balance biaxial flow ratio
        """ # nopep8
        return self._cards[3].get_value("rbb")

    @rbb.setter
    def rbb(self, value: float) -> None:
        self._cards[3].set_value("rbb", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the YLD89 parameter a.
        """ # nopep8
        return self._cards[4].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        self._cards[4].set_value("a", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the YLD89 parameter c.
        """ # nopep8
        return self._cards[4].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        self._cards[4].set_value("c", value)

    @property
    def h(self) -> typing.Optional[float]:
        """Get or set the YLD89 parameter ha
        """ # nopep8
        return self._cards[4].get_value("h")

    @h.setter
    def h(self, value: float) -> None:
        self._cards[4].set_value("h", value)

    @property
    def p(self) -> typing.Optional[float]:
        """Get or set the YLD89 parameter p
        """ # nopep8
        return self._cards[4].get_value("p")

    @p.setter
    def p(self, value: float) -> None:
        self._cards[4].set_value("p", value)

    @property
    def qx1(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening parameter .
        """ # nopep8
        return self._cards[5].get_value("qx1")

    @qx1.setter
    def qx1(self, value: float) -> None:
        self._cards[5].set_value("qx1", value)

    @property
    def cx1(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening parameter .
        """ # nopep8
        return self._cards[5].get_value("cx1")

    @cx1.setter
    def cx1(self, value: float) -> None:
        self._cards[5].set_value("cx1", value)

    @property
    def qx2(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening parameter
        """ # nopep8
        return self._cards[5].get_value("qx2")

    @qx2.setter
    def qx2(self, value: float) -> None:
        self._cards[5].set_value("qx2", value)

    @property
    def cx2(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening parameter
        """ # nopep8
        return self._cards[5].get_value("cx2")

    @cx2.setter
    def cx2(self, value: float) -> None:
        self._cards[5].set_value("cx2", value)

    @property
    def edot(self) -> typing.Optional[float]:
        """Get or set the Strain rate parameter
        """ # nopep8
        return self._cards[5].get_value("edot")

    @edot.setter
    def edot(self, value: float) -> None:
        self._cards[5].set_value("edot", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Strain rate parameter
        """ # nopep8
        return self._cards[5].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        self._cards[5].set_value("m", value)

    @property
    def emin(self) -> typing.Optional[float]:
        """Get or set the Lower limit of the isotropic hardening rate  .  This feature is included to model a non-zero and linear isotropic work hardening rate at large values of effective plastic strain.  If the isotropic work hardening rate predicted by the utilized Voce-type work hardening rule falls below the specified value it is substituted by the prescribed value.  This option should be considered for problems involving extensive plastic deformations.  If process dependent material characteristics are prescribed, i.e. if LC .GT. 0 the same minimum tangent modulus is assumed for all the prescribed work hardening curves
        """ # nopep8
        return self._cards[5].get_value("emin")

    @emin.setter
    def emin(self, value: float) -> None:
        self._cards[5].set_value("emin", value)

    @property
    def s100(self) -> typing.Optional[float]:
        """Get or set the Yield stress at 100% strain for using a power-law approximation beyond the strain defined by EMIN
        """ # nopep8
        return self._cards[5].get_value("s100")

    @s100.setter
    def s100(self, value: float) -> None:
        self._cards[5].set_value("s100", value)

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
        return self._cards[6].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[6].set_value("aopt", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT=3, may be overwritten on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_ SOLID_ORTHO..
        """ # nopep8
        return self._cards[6].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[6].set_value("beta", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1..
        """ # nopep8
        return self._cards[7].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[7].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1..
        """ # nopep8
        return self._cards[7].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[7].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1..
        """ # nopep8
        return self._cards[7].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[7].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[7].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[7].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[7].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[7].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[7].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[7].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[8].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[8].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[8].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[8].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3
        """ # nopep8
        return self._cards[8].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[8].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[8].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[8].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[8].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[8].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[8].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[8].set_value("d3", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[9].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[9].cards[0].set_value("title", value)

