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

class MatWood(KeywordBase):
    """DYNA MAT_WOOD keyword"""

    keyword = "MAT"
    subkeyword = "WOOD"
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
                        "el",
                        float,
                        0,
                        10,
                        kwargs.get("el")
                    ),
                    Field(
                        "et",
                        float,
                        10,
                        10,
                        kwargs.get("et")
                    ),
                    Field(
                        "glt",
                        float,
                        20,
                        10,
                        kwargs.get("glt")
                    ),
                    Field(
                        "gtr",
                        float,
                        30,
                        10,
                        kwargs.get("gtr")
                    ),
                    Field(
                        "pr",
                        float,
                        40,
                        10,
                        kwargs.get("pr")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xt",
                        float,
                        0,
                        10,
                        kwargs.get("xt")
                    ),
                    Field(
                        "xc",
                        float,
                        10,
                        10,
                        kwargs.get("xc")
                    ),
                    Field(
                        "yt",
                        float,
                        20,
                        10,
                        kwargs.get("yt")
                    ),
                    Field(
                        "yc",
                        float,
                        30,
                        10,
                        kwargs.get("yc")
                    ),
                    Field(
                        "sxy",
                        float,
                        40,
                        10,
                        kwargs.get("sxy")
                    ),
                    Field(
                        "syz",
                        float,
                        50,
                        10,
                        kwargs.get("syz")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "gf1||",
                        float,
                        0,
                        10,
                        kwargs.get("gf1||")
                    ),
                    Field(
                        "gf2||",
                        float,
                        10,
                        10,
                        kwargs.get("gf2||")
                    ),
                    Field(
                        "bfit",
                        float,
                        20,
                        10,
                        kwargs.get("bfit")
                    ),
                    Field(
                        "dmax||",
                        float,
                        30,
                        10,
                        kwargs.get("dmax||")
                    ),
                    Field(
                        "gf1p",
                        float,
                        40,
                        10,
                        kwargs.get("gf1p")
                    ),
                    Field(
                        "gf2p",
                        float,
                        50,
                        10,
                        kwargs.get("gf2p")
                    ),
                    Field(
                        "dfit",
                        float,
                        60,
                        10,
                        kwargs.get("dfit")
                    ),
                    Field(
                        "dmaxp",
                        float,
                        70,
                        10,
                        kwargs.get("dmaxp")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "flpar",
                        float,
                        0,
                        10,
                        kwargs.get("flpar")
                    ),
                    Field(
                        "flparc",
                        float,
                        10,
                        10,
                        kwargs.get("flparc")
                    ),
                    Field(
                        "powpar",
                        float,
                        20,
                        10,
                        kwargs.get("powpar")
                    ),
                    Field(
                        "flper",
                        float,
                        30,
                        10,
                        kwargs.get("flper")
                    ),
                    Field(
                        "flperc",
                        float,
                        40,
                        10,
                        kwargs.get("flperc")
                    ),
                    Field(
                        "powper",
                        float,
                        50,
                        10,
                        kwargs.get("powper")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "npar",
                        float,
                        0,
                        10,
                        kwargs.get("npar")
                    ),
                    Field(
                        "cpar",
                        float,
                        10,
                        10,
                        kwargs.get("cpar")
                    ),
                    Field(
                        "nper",
                        float,
                        20,
                        10,
                        kwargs.get("nper")
                    ),
                    Field(
                        "cper",
                        float,
                        30,
                        10,
                        kwargs.get("cper")
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
                        "macf",
                        int,
                        10,
                        10,
                        kwargs.get("macf", 1)
                    ),
                    Field(
                        "beta",
                        float,
                        20,
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
                option_spec = MatWood.option_specs[0],
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
    def el(self) -> typing.Optional[float]:
        """Get or set the Parallel normal modulus.
        """ # nopep8
        return self._cards[1].get_value("el")

    @el.setter
    def el(self, value: float) -> None:
        self._cards[1].set_value("el", value)

    @property
    def et(self) -> typing.Optional[float]:
        """Get or set the Perpendicular normal modulus.
        """ # nopep8
        return self._cards[1].get_value("et")

    @et.setter
    def et(self, value: float) -> None:
        self._cards[1].set_value("et", value)

    @property
    def glt(self) -> typing.Optional[float]:
        """Get or set the Parallel shear modulus (GLT=GLR).
        """ # nopep8
        return self._cards[1].get_value("glt")

    @glt.setter
    def glt(self, value: float) -> None:
        self._cards[1].set_value("glt", value)

    @property
    def gtr(self) -> typing.Optional[float]:
        """Get or set the Perpendicular shear modulus.
        """ # nopep8
        return self._cards[1].get_value("gtr")

    @gtr.setter
    def gtr(self, value: float) -> None:
        self._cards[1].set_value("gtr", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Parallel major Poisson's ratio.
        """ # nopep8
        return self._cards[1].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[1].set_value("pr", value)

    @property
    def xt(self) -> typing.Optional[float]:
        """Get or set the Parallel tensile strength.
        """ # nopep8
        return self._cards[2].get_value("xt")

    @xt.setter
    def xt(self, value: float) -> None:
        self._cards[2].set_value("xt", value)

    @property
    def xc(self) -> typing.Optional[float]:
        """Get or set the Parallel compressive strength.
        """ # nopep8
        return self._cards[2].get_value("xc")

    @xc.setter
    def xc(self, value: float) -> None:
        self._cards[2].set_value("xc", value)

    @property
    def yt(self) -> typing.Optional[float]:
        """Get or set the Perpendicular tensile strength.
        """ # nopep8
        return self._cards[2].get_value("yt")

    @yt.setter
    def yt(self, value: float) -> None:
        self._cards[2].set_value("yt", value)

    @property
    def yc(self) -> typing.Optional[float]:
        """Get or set the Perpendicular compressive strength.
        """ # nopep8
        return self._cards[2].get_value("yc")

    @yc.setter
    def yc(self, value: float) -> None:
        self._cards[2].set_value("yc", value)

    @property
    def sxy(self) -> typing.Optional[float]:
        """Get or set the Parallel shear strength.
        """ # nopep8
        return self._cards[2].get_value("sxy")

    @sxy.setter
    def sxy(self, value: float) -> None:
        self._cards[2].set_value("sxy", value)

    @property
    def syz(self) -> typing.Optional[float]:
        """Get or set the Perpendicular shear strength.
        """ # nopep8
        return self._cards[2].get_value("syz")

    @syz.setter
    def syz(self, value: float) -> None:
        self._cards[2].set_value("syz", value)

    @property
    def gf1__(self) -> typing.Optional[float]:
        """Get or set the Parallel fracture energy in tension.
        """ # nopep8
        return self._cards[3].get_value("gf1||")

    @gf1__.setter
    def gf1__(self, value: float) -> None:
        self._cards[3].set_value("gf1||", value)

    @property
    def gf2__(self) -> typing.Optional[float]:
        """Get or set the Parallel fracture energy in shear.
        """ # nopep8
        return self._cards[3].get_value("gf2||")

    @gf2__.setter
    def gf2__(self, value: float) -> None:
        self._cards[3].set_value("gf2||", value)

    @property
    def bfit(self) -> typing.Optional[float]:
        """Get or set the Parallel softening parameter.
        """ # nopep8
        return self._cards[3].get_value("bfit")

    @bfit.setter
    def bfit(self, value: float) -> None:
        self._cards[3].set_value("bfit", value)

    @property
    def dmax__(self) -> typing.Optional[float]:
        """Get or set the Parallel maximum damage.
        """ # nopep8
        return self._cards[3].get_value("dmax||")

    @dmax__.setter
    def dmax__(self, value: float) -> None:
        self._cards[3].set_value("dmax||", value)

    @property
    def gf1p(self) -> typing.Optional[float]:
        """Get or set the Perpendicular fracture energy in tension.
        """ # nopep8
        return self._cards[3].get_value("gf1p")

    @gf1p.setter
    def gf1p(self, value: float) -> None:
        self._cards[3].set_value("gf1p", value)

    @property
    def gf2p(self) -> typing.Optional[float]:
        """Get or set the Perpendicular fracture energy in shear.
        """ # nopep8
        return self._cards[3].get_value("gf2p")

    @gf2p.setter
    def gf2p(self, value: float) -> None:
        self._cards[3].set_value("gf2p", value)

    @property
    def dfit(self) -> typing.Optional[float]:
        """Get or set the Perpendicular softening parameter.
        """ # nopep8
        return self._cards[3].get_value("dfit")

    @dfit.setter
    def dfit(self, value: float) -> None:
        self._cards[3].set_value("dfit", value)

    @property
    def dmaxp(self) -> typing.Optional[float]:
        """Get or set the Perpendicular maxiumum damage.
        """ # nopep8
        return self._cards[3].get_value("dmaxp")

    @dmaxp.setter
    def dmaxp(self, value: float) -> None:
        self._cards[3].set_value("dmaxp", value)

    @property
    def flpar(self) -> typing.Optional[float]:
        """Get or set the Parallel fluidity parameter for tesion and shear.
        """ # nopep8
        return self._cards[4].get_value("flpar")

    @flpar.setter
    def flpar(self, value: float) -> None:
        self._cards[4].set_value("flpar", value)

    @property
    def flparc(self) -> typing.Optional[float]:
        """Get or set the Parallel fluidity parameter for compresion.
        """ # nopep8
        return self._cards[4].get_value("flparc")

    @flparc.setter
    def flparc(self, value: float) -> None:
        self._cards[4].set_value("flparc", value)

    @property
    def powpar(self) -> typing.Optional[float]:
        """Get or set the Parallel power.
        """ # nopep8
        return self._cards[4].get_value("powpar")

    @powpar.setter
    def powpar(self, value: float) -> None:
        self._cards[4].set_value("powpar", value)

    @property
    def flper(self) -> typing.Optional[float]:
        """Get or set the Perpendicular fluidity parameter for tension and shear.
        """ # nopep8
        return self._cards[4].get_value("flper")

    @flper.setter
    def flper(self, value: float) -> None:
        self._cards[4].set_value("flper", value)

    @property
    def flperc(self) -> typing.Optional[float]:
        """Get or set the Perpendicular fluidity parameter for compression.
        """ # nopep8
        return self._cards[4].get_value("flperc")

    @flperc.setter
    def flperc(self, value: float) -> None:
        self._cards[4].set_value("flperc", value)

    @property
    def powper(self) -> typing.Optional[float]:
        """Get or set the Perpendicular power.
        """ # nopep8
        return self._cards[4].get_value("powper")

    @powper.setter
    def powper(self, value: float) -> None:
        self._cards[4].set_value("powper", value)

    @property
    def npar(self) -> typing.Optional[float]:
        """Get or set the Parallel hardening initiation.
        """ # nopep8
        return self._cards[5].get_value("npar")

    @npar.setter
    def npar(self, value: float) -> None:
        self._cards[5].set_value("npar", value)

    @property
    def cpar(self) -> typing.Optional[float]:
        """Get or set the Parallel hardening rate.
        """ # nopep8
        return self._cards[5].get_value("cpar")

    @cpar.setter
    def cpar(self, value: float) -> None:
        self._cards[5].set_value("cpar", value)

    @property
    def nper(self) -> typing.Optional[float]:
        """Get or set the Perpendicular hardening initiation.
        """ # nopep8
        return self._cards[5].get_value("nper")

    @nper.setter
    def nper(self, value: float) -> None:
        self._cards[5].set_value("nper", value)

    @property
    def cper(self) -> typing.Optional[float]:
        """Get or set the Perpendicular hardening rate.
        """ # nopep8
        return self._cards[5].get_value("cper")

    @cper.setter
    def cper(self, value: float) -> None:
        self._cards[5].set_value("cper", value)

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
        return self._cards[6].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[6].set_value("aopt", value)

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
        return self._cards[6].get_value("macf")

    @macf.setter
    def macf(self, value: int) -> None:
        if value not in [1, 2, 3, 4, -4, -3, -2]:
            raise Exception("""macf must be one of {1,2,3,4,-4,-3,-2}""")
        self._cards[6].set_value("macf", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT=3.
        """ # nopep8
        return self._cards[6].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[6].set_value("beta", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[7].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[7].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[7].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[7].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1.
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
    def v1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[8].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[8].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[8].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[8].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[8].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[8].set_value("v3", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[9].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[9].cards[0].set_value("title", value)

