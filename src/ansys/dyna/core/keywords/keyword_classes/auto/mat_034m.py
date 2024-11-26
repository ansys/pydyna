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

class Mat034M(KeywordBase):
    """DYNA MAT_034M keyword"""

    keyword = "MAT"
    subkeyword = "034M"
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
                        "pxx",
                        float,
                        20,
                        10,
                        kwargs.get("pxx")
                    ),
                    Field(
                        "pyy",
                        float,
                        30,
                        10,
                        kwargs.get("pyy")
                    ),
                    Field(
                        "sxy",
                        float,
                        40,
                        10,
                        kwargs.get("sxy")
                    ),
                    Field(
                        "damp",
                        float,
                        50,
                        10,
                        kwargs.get("damp")
                    ),
                    Field(
                        "th",
                        float,
                        60,
                        10,
                        kwargs.get("th")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "fvopt",
                        float,
                        0,
                        10,
                        kwargs.get("fvopt")
                    ),
                    Field(
                        "x0",
                        float,
                        10,
                        10,
                        kwargs.get("x0")
                    ),
                    Field(
                        "x1",
                        float,
                        20,
                        10,
                        kwargs.get("x1")
                    ),
                    Field(
                        "flc/x2",
                        float,
                        30,
                        10,
                        kwargs.get("flc/x2")
                    ),
                    Field(
                        "fac/x3",
                        float,
                        40,
                        10,
                        kwargs.get("fac/x3")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "isrefg",
                        float,
                        0,
                        10,
                        kwargs.get("isrefg", 0.0)
                    ),
                    Field(
                        "cse",
                        float,
                        10,
                        10,
                        kwargs.get("cse", 0.0)
                    ),
                    Field(
                        "srfac",
                        int,
                        20,
                        10,
                        kwargs.get("srfac")
                    ),
                    Field(
                        "bulkc",
                        float,
                        30,
                        10,
                        kwargs.get("bulkc")
                    ),
                    Field(
                        "jacc",
                        float,
                        40,
                        10,
                        kwargs.get("jacc")
                    ),
                    Field(
                        "fxx",
                        int,
                        50,
                        10,
                        kwargs.get("fxx")
                    ),
                    Field(
                        "fyy",
                        int,
                        60,
                        10,
                        kwargs.get("fyy")
                    ),
                    Field(
                        "dt",
                        float,
                        70,
                        10,
                        kwargs.get("dt")
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
                        "ecoat",
                        float,
                        10,
                        10,
                        kwargs.get("ecoat")
                    ),
                    Field(
                        "scoat",
                        float,
                        20,
                        10,
                        kwargs.get("scoat")
                    ),
                    Field(
                        "tcoat",
                        float,
                        30,
                        10,
                        kwargs.get("tcoat")
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
                option_spec = Mat034M.option_specs[0],
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
    def pxx(self) -> typing.Optional[float]:
        """Get or set the Table giving engineering local XX-stress as function of engineering local XX-strain and YY-strain.
        """ # nopep8
        return self._cards[0].get_value("pxx")

    @pxx.setter
    def pxx(self, value: float) -> None:
        self._cards[0].set_value("pxx", value)

    @property
    def pyy(self) -> typing.Optional[float]:
        """Get or set the Table giving engineering local YY-stress as function of engineering local YY-strain and XX-strain.
        """ # nopep8
        return self._cards[0].get_value("pyy")

    @pyy.setter
    def pyy(self, value: float) -> None:
        self._cards[0].set_value("pyy", value)

    @property
    def sxy(self) -> typing.Optional[float]:
        """Get or set the Curve giving local 2nd Piola-Kirchhoff XY-stress as function of local Green XY-strain.
        """ # nopep8
        return self._cards[0].get_value("sxy")

    @sxy.setter
    def sxy(self, value: float) -> None:
        self._cards[0].set_value("sxy", value)

    @property
    def damp(self) -> typing.Optional[float]:
        """Get or set the Damping coefficient for numerical stability.
        """ # nopep8
        return self._cards[0].get_value("damp")

    @damp.setter
    def damp(self, value: float) -> None:
        self._cards[0].set_value("damp", value)

    @property
    def th(self) -> typing.Optional[float]:
        """Get or set the Table giving hysteresis factor 0 <= H < 1 as function of engineering local XX-strain and YY-strain.
        GT.0.0: TH is table ID
        LE.0.0: -TH is used as constant value for hysteresis factor
        """ # nopep8
        return self._cards[0].get_value("th")

    @th.setter
    def th(self, value: float) -> None:
        self._cards[0].set_value("th", value)

    @property
    def fvopt(self) -> typing.Optional[float]:
        """Get or set the Fabric venting option, see *MAT_FABRIC.
        """ # nopep8
        return self._cards[1].get_value("fvopt")

    @fvopt.setter
    def fvopt(self, value: float) -> None:
        self._cards[1].set_value("fvopt", value)

    @property
    def x0(self) -> typing.Optional[float]:
        """Get or set the Fabric venting option parameters, see *MAT_FABRIC.
        """ # nopep8
        return self._cards[1].get_value("x0")

    @x0.setter
    def x0(self, value: float) -> None:
        self._cards[1].set_value("x0", value)

    @property
    def x1(self) -> typing.Optional[float]:
        """Get or set the Fabric venting option parameters, see *MAT_FABRIC.
        """ # nopep8
        return self._cards[1].get_value("x1")

    @x1.setter
    def x1(self, value: float) -> None:
        self._cards[1].set_value("x1", value)

    @property
    def flc_x2(self) -> typing.Optional[float]:
        """Get or set the Fabric venting option parameters, see *MAT_FABRIC.
        """ # nopep8
        return self._cards[1].get_value("flc/x2")

    @flc_x2.setter
    def flc_x2(self, value: float) -> None:
        self._cards[1].set_value("flc/x2", value)

    @property
    def fac_x3(self) -> typing.Optional[float]:
        """Get or set the Fabric venting option parameters, see *MAT_FABRIC.
        """ # nopep8
        return self._cards[1].get_value("fac/x3")

    @fac_x3.setter
    def fac_x3(self, value: float) -> None:
        self._cards[1].set_value("fac/x3", value)

    @property
    def isrefg(self) -> float:
        """Get or set the Initial stress by reference geometry.
        EQ.0.0: Not active.
        EQ.1.0: Active
        """ # nopep8
        return self._cards[2].get_value("isrefg")

    @isrefg.setter
    def isrefg(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""isrefg must be one of {0.0,1.0}""")
        self._cards[2].set_value("isrefg", value)

    @property
    def cse(self) -> float:
        """Get or set the Compressive stress elimination option.
        EQ.0.0: Don't eliminate compressive stresses,
        EQ.1.0: Eliminate compressive stresses.
        """ # nopep8
        return self._cards[2].get_value("cse")

    @cse.setter
    def cse(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""cse must be one of {0.0,1.0}""")
        self._cards[2].set_value("cse", value)

    @property
    def srfac(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for smooth stress initialization when using a reference geometry.
        """ # nopep8
        return self._cards[2].get_value("srfac")

    @srfac.setter
    def srfac(self, value: int) -> None:
        self._cards[2].set_value("srfac", value)

    @property
    def bulkc(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus for fabric compaction.
        """ # nopep8
        return self._cards[2].get_value("bulkc")

    @bulkc.setter
    def bulkc(self, value: float) -> None:
        self._cards[2].set_value("bulkc", value)

    @property
    def jacc(self) -> typing.Optional[float]:
        """Get or set the Jacobian for the onset of fabric compaction.
        """ # nopep8
        return self._cards[2].get_value("jacc")

    @jacc.setter
    def jacc(self, value: float) -> None:
        self._cards[2].set_value("jacc", value)

    @property
    def fxx(self) -> typing.Optional[int]:
        """Get or set the Load curve giving scale factor of uniaxial stress in first material direction as function of engineering strain rate.
        """ # nopep8
        return self._cards[2].get_value("fxx")

    @fxx.setter
    def fxx(self, value: int) -> None:
        self._cards[2].set_value("fxx", value)

    @property
    def fyy(self) -> typing.Optional[int]:
        """Get or set the Load curve giving scale factor of uniaxial stress in second material direction as function of engineering strain rate.
        """ # nopep8
        return self._cards[2].get_value("fyy")

    @fyy.setter
    def fyy(self, value: int) -> None:
        self._cards[2].set_value("fyy", value)

    @property
    def dt(self) -> typing.Optional[float]:
        """Get or set the Time window for smoothing strain rates used for FXX and FYY.
        """ # nopep8
        return self._cards[2].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[2].set_value("dt", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option, see *MAT_FABRIC.
        """ # nopep8
        return self._cards[3].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[3].set_value("aopt", value)

    @property
    def ecoat(self) -> typing.Optional[float]:
        """Get or set the Young's modulus of coat material to include bending properties. This together with the following two parameters (SCOAT and TCOAT) encompass the same coating/bending feature as in *MAT_FABRIC. Please refer to these manual pages and associated remarks..
        """ # nopep8
        return self._cards[3].get_value("ecoat")

    @ecoat.setter
    def ecoat(self, value: float) -> None:
        self._cards[3].set_value("ecoat", value)

    @property
    def scoat(self) -> typing.Optional[float]:
        """Get or set the Yield stress of coat material, see *MAT_FABRIC.
        """ # nopep8
        return self._cards[3].get_value("scoat")

    @scoat.setter
    def scoat(self, value: float) -> None:
        self._cards[3].set_value("scoat", value)

    @property
    def tcoat(self) -> typing.Optional[float]:
        """Get or set the Thickness of coat material, may be positive or negative, see *MAT_FABRIC.
        """ # nopep8
        return self._cards[3].get_value("tcoat")

    @tcoat.setter
    def tcoat(self, value: float) -> None:
        self._cards[3].set_value("tcoat", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the x-coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[4].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[4].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the y-coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[4].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[4].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the z-coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[4].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[4].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[4].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[4].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[4].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[5].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[5].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[5].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[5].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[5].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[5].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[5].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[5].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[5].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 0 and 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA.
        """ # nopep8
        return self._cards[5].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[5].set_value("beta", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[6].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[6].cards[0].set_value("title", value)

