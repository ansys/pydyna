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

class Mat173(KeywordBase):
    """DYNA MAT_173 keyword"""

    keyword = "MAT"
    subkeyword = "173"
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
                        "gmod",
                        float,
                        20,
                        10,
                        kwargs.get("gmod")
                    ),
                    Field(
                        "rnu",
                        float,
                        30,
                        10,
                        kwargs.get("rnu")
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "phi",
                        float,
                        50,
                        10,
                        kwargs.get("phi")
                    ),
                    Field(
                        "cval",
                        float,
                        60,
                        10,
                        kwargs.get("cval")
                    ),
                    Field(
                        "psi",
                        float,
                        70,
                        10,
                        kwargs.get("psi")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "novoid ",
                        int,
                        0,
                        10,
                        kwargs.get("novoid ")
                    ),
                    Field(
                        "nplanes",
                        int,
                        10,
                        10,
                        kwargs.get("nplanes")
                    ),
                    Field(
                        "extra",
                        int,
                        20,
                        10,
                        kwargs.get("extra")
                    ),
                    Field(
                        "lccpdr",
                        int,
                        30,
                        10,
                        kwargs.get("lccpdr")
                    ),
                    Field(
                        "lccpt",
                        int,
                        40,
                        10,
                        kwargs.get("lccpt")
                    ),
                    Field(
                        "lccjdr",
                        int,
                        50,
                        10,
                        kwargs.get("lccjdr")
                    ),
                    Field(
                        "lccjt",
                        int,
                        60,
                        10,
                        kwargs.get("lccjt")
                    ),
                    Field(
                        "lcsfac",
                        int,
                        70,
                        10,
                        kwargs.get("lcsfac")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "gmoddp",
                        float,
                        0,
                        10,
                        kwargs.get("gmoddp")
                    ),
                    Field(
                        "gmodgr",
                        float,
                        10,
                        10,
                        kwargs.get("gmodgr")
                    ),
                    Field(
                        "lcgmep",
                        int,
                        20,
                        10,
                        kwargs.get("lcgmep")
                    ),
                    Field(
                        "lcphiep",
                        int,
                        30,
                        10,
                        kwargs.get("lcphiep")
                    ),
                    Field(
                        "lcpsiep",
                        int,
                        40,
                        10,
                        kwargs.get("lcpsiep")
                    ),
                    Field(
                        "lcgmst",
                        int,
                        50,
                        10,
                        kwargs.get("lcgmst")
                    ),
                    Field(
                        "cvalgr",
                        float,
                        60,
                        10,
                        kwargs.get("cvalgr")
                    ),
                    Field(
                        "aniso",
                        float,
                        70,
                        10,
                        kwargs.get("aniso", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcgmt",
                        float,
                        0,
                        10,
                        kwargs.get("lcgmt")
                    ),
                    Field(
                        "lccvt",
                        float,
                        10,
                        10,
                        kwargs.get("lccvt")
                    ),
                    Field(
                        "lcpht",
                        float,
                        20,
                        10,
                        kwargs.get("lcpht")
                    ),
                    Field(
                        "epdam1",
                        float,
                        30,
                        10,
                        kwargs.get("epdam1", 1.0E+20)
                    ),
                    Field(
                        "epdam2",
                        float,
                        40,
                        10,
                        kwargs.get("epdam2")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dip",
                        float,
                        0,
                        10,
                        kwargs.get("dip")
                    ),
                    Field(
                        "dipang",
                        float,
                        10,
                        10,
                        kwargs.get("dipang")
                    ),
                    Field(
                        "cplane",
                        float,
                        20,
                        10,
                        kwargs.get("cplane")
                    ),
                    Field(
                        "frplane",
                        float,
                        30,
                        10,
                        kwargs.get("frplane")
                    ),
                    Field(
                        "tplane",
                        float,
                        40,
                        10,
                        kwargs.get("tplane")
                    ),
                    Field(
                        "shrmax",
                        float,
                        50,
                        10,
                        kwargs.get("shrmax", 1.0E+20)
                    ),
                    Field(
                        "local",
                        int,
                        60,
                        10,
                        kwargs.get("local")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat173.option_specs[0],
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
        """Get or set the Material identification.  A unique number.
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
    def gmod(self) -> typing.Optional[float]:
        """Get or set the Elastic shear modulus
        """ # nopep8
        return self._cards[0].get_value("gmod")

    @gmod.setter
    def gmod(self, value: float) -> None:
        self._cards[0].set_value("gmod", value)

    @property
    def rnu(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio
        """ # nopep8
        return self._cards[0].get_value("rnu")

    @rnu.setter
    def rnu(self, value: float) -> None:
        self._cards[0].set_value("rnu", value)

    @property
    def phi(self) -> typing.Optional[float]:
        """Get or set the Angle of friction (radians)
        """ # nopep8
        return self._cards[0].get_value("phi")

    @phi.setter
    def phi(self, value: float) -> None:
        self._cards[0].set_value("phi", value)

    @property
    def cval(self) -> typing.Optional[float]:
        """Get or set the Cohesion value (shear strength at zero normal stress)
        """ # nopep8
        return self._cards[0].get_value("cval")

    @cval.setter
    def cval(self, value: float) -> None:
        self._cards[0].set_value("cval", value)

    @property
    def psi(self) -> typing.Optional[float]:
        """Get or set the Dilation angle (radians)
        """ # nopep8
        return self._cards[0].get_value("psi")

    @psi.setter
    def psi(self, value: float) -> None:
        self._cards[0].set_value("psi", value)

    @property
    def novoid_(self) -> typing.Optional[int]:
        """Get or set the Voiding behavior flag (see Remarks 8 and 9):
        EQ.0:	Voiding behavior on
        EQ.1 : Voiding behavior off.
        """ # nopep8
        return self._cards[1].get_value("novoid ")

    @novoid_.setter
    def novoid_(self, value: int) -> None:
        self._cards[1].set_value("novoid ", value)

    @property
    def nplanes(self) -> typing.Optional[int]:
        """Get or set the Number of joint planes (maximum 6)
        """ # nopep8
        return self._cards[1].get_value("nplanes")

    @nplanes.setter
    def nplanes(self, value: int) -> None:
        self._cards[1].set_value("nplanes", value)

    @property
    def extra(self) -> typing.Optional[int]:
        """Get or set the Flag to input further data. If EXTRA > 0, then Card 4 is read.
        """ # nopep8
        return self._cards[1].get_value("extra")

    @extra.setter
    def extra(self, value: int) -> None:
        self._cards[1].set_value("extra", value)

    @property
    def lccpdr(self) -> typing.Optional[int]:
        """Get or set the Load curve for extra cohesion for parent material (dynamic relaxation)
        """ # nopep8
        return self._cards[1].get_value("lccpdr")

    @lccpdr.setter
    def lccpdr(self, value: int) -> None:
        self._cards[1].set_value("lccpdr", value)

    @property
    def lccpt(self) -> typing.Optional[int]:
        """Get or set the Load curve for extra cohesion for parent material (transient)
        """ # nopep8
        return self._cards[1].get_value("lccpt")

    @lccpt.setter
    def lccpt(self, value: int) -> None:
        self._cards[1].set_value("lccpt", value)

    @property
    def lccjdr(self) -> typing.Optional[int]:
        """Get or set the Load curve for extra cohesion for joints (dynamic relaxation)
        """ # nopep8
        return self._cards[1].get_value("lccjdr")

    @lccjdr.setter
    def lccjdr(self, value: int) -> None:
        self._cards[1].set_value("lccjdr", value)

    @property
    def lccjt(self) -> typing.Optional[int]:
        """Get or set the Load curve for extra cohesion for joints (transient)
        """ # nopep8
        return self._cards[1].get_value("lccjt")

    @lccjt.setter
    def lccjt(self, value: int) -> None:
        self._cards[1].set_value("lccjt", value)

    @property
    def lcsfac(self) -> typing.Optional[int]:
        """Get or set the Load curve giving factor on strength vs. time
        """ # nopep8
        return self._cards[1].get_value("lcsfac")

    @lcsfac.setter
    def lcsfac(self, value: int) -> None:
        self._cards[1].set_value("lcsfac", value)

    @property
    def gmoddp(self) -> typing.Optional[float]:
        """Get or set the Z-coordinate  at which GMOD and CVAL are correct.
        """ # nopep8
        return self._cards[2].get_value("gmoddp")

    @gmoddp.setter
    def gmoddp(self, value: float) -> None:
        self._cards[2].set_value("gmoddp", value)

    @property
    def gmodgr(self) -> typing.Optional[float]:
        """Get or set the Gradient of GMOD versus z-coordinate (usually negative)
        """ # nopep8
        return self._cards[2].get_value("gmodgr")

    @gmodgr.setter
    def gmodgr(self, value: float) -> None:
        self._cards[2].set_value("gmodgr", value)

    @property
    def lcgmep(self) -> typing.Optional[int]:
        """Get or set the Load curve of GMOD versus plastic strain (overrides GMODGR)
        """ # nopep8
        return self._cards[2].get_value("lcgmep")

    @lcgmep.setter
    def lcgmep(self, value: int) -> None:
        self._cards[2].set_value("lcgmep", value)

    @property
    def lcphiep(self) -> typing.Optional[int]:
        """Get or set the Load curve of PHI versus plastic strain
        """ # nopep8
        return self._cards[2].get_value("lcphiep")

    @lcphiep.setter
    def lcphiep(self, value: int) -> None:
        self._cards[2].set_value("lcphiep", value)

    @property
    def lcpsiep(self) -> typing.Optional[int]:
        """Get or set the Load curve of PSI versus plastic strain
        """ # nopep8
        return self._cards[2].get_value("lcpsiep")

    @lcpsiep.setter
    def lcpsiep(self, value: int) -> None:
        self._cards[2].set_value("lcpsiep", value)

    @property
    def lcgmst(self) -> typing.Optional[int]:
        """Get or set the (Leave blank)
        """ # nopep8
        return self._cards[2].get_value("lcgmst")

    @lcgmst.setter
    def lcgmst(self, value: int) -> None:
        self._cards[2].set_value("lcgmst", value)

    @property
    def cvalgr(self) -> typing.Optional[float]:
        """Get or set the Gradient of CVAL versus z-coordinate (usually negative)
        """ # nopep8
        return self._cards[2].get_value("cvalgr")

    @cvalgr.setter
    def cvalgr(self, value: float) -> None:
        self._cards[2].set_value("cvalgr", value)

    @property
    def aniso(self) -> float:
        """Get or set the Factor applied to elastic shear stiffness in global XZ and YZ planes
        """ # nopep8
        return self._cards[2].get_value("aniso")

    @aniso.setter
    def aniso(self, value: float) -> None:
        self._cards[2].set_value("aniso", value)

    @property
    def lcgmt(self) -> typing.Optional[float]:
        """Get or set the Load curve of nondimensional factor on GMOD as a function of time
        """ # nopep8
        return self._cards[3].get_value("lcgmt")

    @lcgmt.setter
    def lcgmt(self, value: float) -> None:
        self._cards[3].set_value("lcgmt", value)

    @property
    def lccvt(self) -> typing.Optional[float]:
        """Get or set the Load curve of nondimensional factor on CVAL as a function of time
        """ # nopep8
        return self._cards[3].get_value("lccvt")

    @lccvt.setter
    def lccvt(self, value: float) -> None:
        self._cards[3].set_value("lccvt", value)

    @property
    def lcpht(self) -> typing.Optional[float]:
        """Get or set the Load curve of nondimensional factor on PHI as a function of time
        """ # nopep8
        return self._cards[3].get_value("lcpht")

    @lcpht.setter
    def lcpht(self, value: float) -> None:
        self._cards[3].set_value("lcpht", value)

    @property
    def epdam1(self) -> float:
        """Get or set the Plastic strain or volumetric void strain at which damage begins
        """ # nopep8
        return self._cards[3].get_value("epdam1")

    @epdam1.setter
    def epdam1(self, value: float) -> None:
        self._cards[3].set_value("epdam1", value)

    @property
    def epdam2(self) -> typing.Optional[float]:
        """Get or set the Plastic strain or volumetric void strain at which element is eroded
        """ # nopep8
        return self._cards[3].get_value("epdam2")

    @epdam2.setter
    def epdam2(self, value: float) -> None:
        self._cards[3].set_value("epdam2", value)

    @property
    def dip(self) -> typing.Optional[float]:
        """Get or set the Angle of the plane in degrees below the horizontal.
        """ # nopep8
        return self._cards[4].get_value("dip")

    @dip.setter
    def dip(self, value: float) -> None:
        self._cards[4].set_value("dip", value)

    @property
    def dipang(self) -> typing.Optional[float]:
        """Get or set the Plan view angle (degrees) of downhill vector drawn on the plane
        """ # nopep8
        return self._cards[4].get_value("dipang")

    @dipang.setter
    def dipang(self, value: float) -> None:
        self._cards[4].set_value("dipang", value)

    @property
    def cplane(self) -> typing.Optional[float]:
        """Get or set the Cohesion for shear behavior on plane
        """ # nopep8
        return self._cards[4].get_value("cplane")

    @cplane.setter
    def cplane(self, value: float) -> None:
        self._cards[4].set_value("cplane", value)

    @property
    def frplane(self) -> typing.Optional[float]:
        """Get or set the Friction angle for shear behavior on plane (degrees)
        """ # nopep8
        return self._cards[4].get_value("frplane")

    @frplane.setter
    def frplane(self, value: float) -> None:
        self._cards[4].set_value("frplane", value)

    @property
    def tplane(self) -> typing.Optional[float]:
        """Get or set the Tensile strength across plane (generally zero or very small)
        """ # nopep8
        return self._cards[4].get_value("tplane")

    @tplane.setter
    def tplane(self, value: float) -> None:
        self._cards[4].set_value("tplane", value)

    @property
    def shrmax(self) -> float:
        """Get or set the Max shear stress on plane (upper limit, independent of compression)
        """ # nopep8
        return self._cards[4].get_value("shrmax")

    @shrmax.setter
    def shrmax(self, value: float) -> None:
        self._cards[4].set_value("shrmax", value)

    @property
    def local(self) -> typing.Optional[int]:
        """Get or set the EQ.0: DIP and DIPANG are with respect to the global axes
        EQ.1: DIP and DIPANG are with respect to the local element axes
        """ # nopep8
        return self._cards[4].get_value("local")

    @local.setter
    def local(self, value: int) -> None:
        self._cards[4].set_value("local", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[5].cards[0].set_value("title", value)

