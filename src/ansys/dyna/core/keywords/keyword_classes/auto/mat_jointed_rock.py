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

class MatJointedRock(KeywordBase):
    """DYNA MAT_JOINTED_ROCK keyword"""

    keyword = "MAT"
    subkeyword = "JOINTED_ROCK"
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
                        "rkf",
                        float,
                        40,
                        10,
                        kwargs.get("rkf", 1.0)
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
                        "str_lim",
                        float,
                        0,
                        10,
                        kwargs.get("str_lim", 0.005)
                    ),
                    Field(
                        "nplanes",
                        int,
                        10,
                        10,
                        kwargs.get("nplanes", 0)
                    ),
                    Field(
                        "elastic",
                        int,
                        20,
                        10,
                        kwargs.get("elastic", 0)
                    ),
                    Field(
                        "lccpdr",
                        int,
                        30,
                        10,
                        kwargs.get("lccpdr", 0)
                    ),
                    Field(
                        "lccpt",
                        int,
                        40,
                        10,
                        kwargs.get("lccpt", 0)
                    ),
                    Field(
                        "lccjdr",
                        int,
                        50,
                        10,
                        kwargs.get("lccjdr", 0)
                    ),
                    Field(
                        "lccjt",
                        int,
                        60,
                        10,
                        kwargs.get("lccjt", 0)
                    ),
                    Field(
                        "lcsfac",
                        int,
                        70,
                        10,
                        kwargs.get("lcsfac", 0)
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
                        "phidp",
                        float,
                        10,
                        10,
                        kwargs.get("phidp")
                    ),
                    Field(
                        "cvaldp",
                        float,
                        20,
                        10,
                        kwargs.get("cvaldp")
                    ),
                    Field(
                        "psidp",
                        float,
                        30,
                        10,
                        kwargs.get("psidp")
                    ),
                    Field(
                        "gmodgr",
                        float,
                        40,
                        10,
                        kwargs.get("gmodgr")
                    ),
                    Field(
                        "phigr",
                        float,
                        50,
                        10,
                        kwargs.get("phigr")
                    ),
                    Field(
                        "cvalgr",
                        float,
                        60,
                        10,
                        kwargs.get("cvalgr")
                    ),
                    Field(
                        "psigr",
                        float,
                        70,
                        10,
                        kwargs.get("psigr")
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
                        "strike",
                        float,
                        10,
                        10,
                        kwargs.get("strike")
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
                        kwargs.get("shrmax", 1.e20)
                    ),
                    Field(
                        "local",
                        float,
                        60,
                        10,
                        kwargs.get("local")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatJointedRock.option_specs[0],
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
        """Get or set the Material identification number, must be unique.
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
    def gmod(self) -> typing.Optional[float]:
        """Get or set the Elastic shear modulus.
        """ # nopep8
        return self._cards[0].get_value("gmod")

    @gmod.setter
    def gmod(self, value: float) -> None:
        self._cards[0].set_value("gmod", value)

    @property
    def rnu(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("rnu")

    @rnu.setter
    def rnu(self, value: float) -> None:
        self._cards[0].set_value("rnu", value)

    @property
    def rkf(self) -> float:
        """Get or set the Failture surface shape parameter.
        """ # nopep8
        return self._cards[0].get_value("rkf")

    @rkf.setter
    def rkf(self, value: float) -> None:
        self._cards[0].set_value("rkf", value)

    @property
    def phi(self) -> typing.Optional[float]:
        """Get or set the Angle of friction (radians).
        """ # nopep8
        return self._cards[0].get_value("phi")

    @phi.setter
    def phi(self, value: float) -> None:
        self._cards[0].set_value("phi", value)

    @property
    def cval(self) -> typing.Optional[float]:
        """Get or set the Cohesion value.
        """ # nopep8
        return self._cards[0].get_value("cval")

    @cval.setter
    def cval(self, value: float) -> None:
        self._cards[0].set_value("cval", value)

    @property
    def psi(self) -> typing.Optional[float]:
        """Get or set the Dilation angle (radians).
        """ # nopep8
        return self._cards[0].get_value("psi")

    @psi.setter
    def psi(self, value: float) -> None:
        self._cards[0].set_value("psi", value)

    @property
    def str_lim(self) -> float:
        """Get or set the Minimum shear strength of material is given by STR_LIM*CVAL.
        """ # nopep8
        return self._cards[1].get_value("str_lim")

    @str_lim.setter
    def str_lim(self, value: float) -> None:
        self._cards[1].set_value("str_lim", value)

    @property
    def nplanes(self) -> int:
        """Get or set the Number of jointed planes (maximum 3).
        """ # nopep8
        return self._cards[1].get_value("nplanes")

    @nplanes.setter
    def nplanes(self, value: int) -> None:
        self._cards[1].set_value("nplanes", value)

    @property
    def elastic(self) -> int:
        """Get or set the Flag = 1 for elastic behaviour only.
        """ # nopep8
        return self._cards[1].get_value("elastic")

    @elastic.setter
    def elastic(self, value: int) -> None:
        self._cards[1].set_value("elastic", value)

    @property
    def lccpdr(self) -> int:
        """Get or set the Loadcurve for extra cohesion for parent material (dynamic relaxation).
        """ # nopep8
        return self._cards[1].get_value("lccpdr")

    @lccpdr.setter
    def lccpdr(self, value: int) -> None:
        self._cards[1].set_value("lccpdr", value)

    @property
    def lccpt(self) -> int:
        """Get or set the Loadcurve for extra cohesion for parent material (transient).
        """ # nopep8
        return self._cards[1].get_value("lccpt")

    @lccpt.setter
    def lccpt(self, value: int) -> None:
        self._cards[1].set_value("lccpt", value)

    @property
    def lccjdr(self) -> int:
        """Get or set the Loadcurve for extra cohesion for joints (dynamic relaxation).
        """ # nopep8
        return self._cards[1].get_value("lccjdr")

    @lccjdr.setter
    def lccjdr(self, value: int) -> None:
        self._cards[1].set_value("lccjdr", value)

    @property
    def lccjt(self) -> int:
        """Get or set the Loadcurve for extra cohesion for joints (transient).
        """ # nopep8
        return self._cards[1].get_value("lccjt")

    @lccjt.setter
    def lccjt(self, value: int) -> None:
        self._cards[1].set_value("lccjt", value)

    @property
    def lcsfac(self) -> int:
        """Get or set the Loadcurve giving factor on strength vs time.
        """ # nopep8
        return self._cards[1].get_value("lcsfac")

    @lcsfac.setter
    def lcsfac(self, value: int) -> None:
        self._cards[1].set_value("lcsfac", value)

    @property
    def gmoddp(self) -> typing.Optional[float]:
        """Get or set the Depth at which shear modulus (GMOD) is correct.
        """ # nopep8
        return self._cards[2].get_value("gmoddp")

    @gmoddp.setter
    def gmoddp(self, value: float) -> None:
        self._cards[2].set_value("gmoddp", value)

    @property
    def phidp(self) -> typing.Optional[float]:
        """Get or set the Depth at which angle of friction (PHI) is correct.
        """ # nopep8
        return self._cards[2].get_value("phidp")

    @phidp.setter
    def phidp(self, value: float) -> None:
        self._cards[2].set_value("phidp", value)

    @property
    def cvaldp(self) -> typing.Optional[float]:
        """Get or set the Depth at which cohesion value (CVAL) is correct.
        """ # nopep8
        return self._cards[2].get_value("cvaldp")

    @cvaldp.setter
    def cvaldp(self, value: float) -> None:
        self._cards[2].set_value("cvaldp", value)

    @property
    def psidp(self) -> typing.Optional[float]:
        """Get or set the Depth at which dilation angle (PSI) is correct.
        """ # nopep8
        return self._cards[2].get_value("psidp")

    @psidp.setter
    def psidp(self, value: float) -> None:
        self._cards[2].set_value("psidp", value)

    @property
    def gmodgr(self) -> typing.Optional[float]:
        """Get or set the Gradient at which shear modulus (GMOD) increases with depth.
        """ # nopep8
        return self._cards[2].get_value("gmodgr")

    @gmodgr.setter
    def gmodgr(self, value: float) -> None:
        self._cards[2].set_value("gmodgr", value)

    @property
    def phigr(self) -> typing.Optional[float]:
        """Get or set the Gradient at which friction angle (PHI) increases with depth.
        """ # nopep8
        return self._cards[2].get_value("phigr")

    @phigr.setter
    def phigr(self, value: float) -> None:
        self._cards[2].set_value("phigr", value)

    @property
    def cvalgr(self) -> typing.Optional[float]:
        """Get or set the Gradient at which cohesion value (CVAL) increases with depth.
        """ # nopep8
        return self._cards[2].get_value("cvalgr")

    @cvalgr.setter
    def cvalgr(self, value: float) -> None:
        self._cards[2].set_value("cvalgr", value)

    @property
    def psigr(self) -> typing.Optional[float]:
        """Get or set the Gradient at which dilation angle (PSI) increases with depth.
        """ # nopep8
        return self._cards[2].get_value("psigr")

    @psigr.setter
    def psigr(self, value: float) -> None:
        self._cards[2].set_value("psigr", value)

    @property
    def dip(self) -> typing.Optional[float]:
        """Get or set the Angle of the plane in degrees below the horizontal.
        """ # nopep8
        return self._cards[3].get_value("dip")

    @dip.setter
    def dip(self, value: float) -> None:
        self._cards[3].set_value("dip", value)

    @property
    def strike(self) -> typing.Optional[float]:
        """Get or set the Plan view angle (degrees) of downhill vector drawn on the plane.
        """ # nopep8
        return self._cards[3].get_value("strike")

    @strike.setter
    def strike(self, value: float) -> None:
        self._cards[3].set_value("strike", value)

    @property
    def cplane(self) -> typing.Optional[float]:
        """Get or set the Cohesion for shear behaviour on plane.
        """ # nopep8
        return self._cards[3].get_value("cplane")

    @cplane.setter
    def cplane(self, value: float) -> None:
        self._cards[3].set_value("cplane", value)

    @property
    def frplane(self) -> typing.Optional[float]:
        """Get or set the Friction angle for shear behaviour on plane (degrees).
        """ # nopep8
        return self._cards[3].get_value("frplane")

    @frplane.setter
    def frplane(self, value: float) -> None:
        self._cards[3].set_value("frplane", value)

    @property
    def tplane(self) -> typing.Optional[float]:
        """Get or set the Tensile strength across plane (generally zero or very small).
        """ # nopep8
        return self._cards[3].get_value("tplane")

    @tplane.setter
    def tplane(self, value: float) -> None:
        self._cards[3].set_value("tplane", value)

    @property
    def shrmax(self) -> float:
        """Get or set the Max shear stress on plane (upper limit, independent of compression).
        """ # nopep8
        return self._cards[3].get_value("shrmax")

    @shrmax.setter
    def shrmax(self, value: float) -> None:
        self._cards[3].set_value("shrmax", value)

    @property
    def local(self) -> typing.Optional[float]:
        """Get or set the EQ=0: DIP and DIPANG are with respect to the global axes.
        EQ=1: DIP and DIPANG are with respect to the local element axes.
        """ # nopep8
        return self._cards[3].get_value("local")

    @local.setter
    def local(self, value: float) -> None:
        self._cards[3].set_value("local", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[4].cards[0].set_value("title", value)

