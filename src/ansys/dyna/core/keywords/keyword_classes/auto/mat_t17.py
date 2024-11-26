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

class MatT17(KeywordBase):
    """DYNA MAT_T17 keyword"""

    keyword = "MAT"
    subkeyword = "T17"
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
                        "tmid",
                        int,
                        0,
                        10,
                        kwargs.get("tmid")
                    ),
                    Field(
                        "nchsp",
                        int,
                        10,
                        10,
                        kwargs.get("nchsp")
                    ),
                    Field(
                        "nchrx",
                        int,
                        20,
                        10,
                        kwargs.get("nchrx")
                    ),
                    Field(
                        "icend",
                        int,
                        30,
                        10,
                        kwargs.get("icend")
                    ),
                    Field(
                        "cend",
                        float,
                        40,
                        10,
                        kwargs.get("cend")
                    ),
                    Field(
                        "gasc",
                        float,
                        50,
                        10,
                        kwargs.get("gasc")
                    ),
                    Field(
                        "fid",
                        int,
                        60,
                        10,
                        kwargs.get("fid")
                    ),
                    Field(
                        "mf",
                        int,
                        70,
                        10,
                        kwargs.get("mf", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "aopt",
                        int,
                        0,
                        10,
                        kwargs.get("aopt", 0)
                    ),
                    Field(
                        "xp",
                        float,
                        10,
                        10,
                        kwargs.get("xp")
                    ),
                    Field(
                        "yp",
                        float,
                        20,
                        10,
                        kwargs.get("yp")
                    ),
                    Field(
                        "zp",
                        float,
                        30,
                        10,
                        kwargs.get("zp")
                    ),
                    Field(
                        "a1",
                        float,
                        40,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "a2",
                        float,
                        50,
                        10,
                        kwargs.get("a2")
                    ),
                    Field(
                        "a3",
                        float,
                        60,
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
                ],
            ),
            Card(
                [
                    Field(
                        "rhof",
                        float,
                        0,
                        10,
                        kwargs.get("rhof")
                    ),
                    Field(
                        "lccf",
                        int,
                        10,
                        10,
                        kwargs.get("lccf")
                    ),
                    Field(
                        "lck1f",
                        int,
                        20,
                        10,
                        kwargs.get("lck1f")
                    ),
                    Field(
                        "lck2f",
                        float,
                        30,
                        10,
                        kwargs.get("lck2f")
                    ),
                    Field(
                        "lck3f",
                        float,
                        40,
                        10,
                        kwargs.get("lck3f")
                    ),
                    Field(
                        "vff",
                        float,
                        50,
                        10,
                        kwargs.get("vff")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "rhoi",
                        float,
                        0,
                        10,
                        kwargs.get("rhoi")
                    ),
                    Field(
                        "lcci",
                        int,
                        10,
                        10,
                        kwargs.get("lcci")
                    ),
                    Field(
                        "lck1i",
                        int,
                        20,
                        10,
                        kwargs.get("lck1i")
                    ),
                    Field(
                        "lck2i",
                        int,
                        30,
                        10,
                        kwargs.get("lck2i")
                    ),
                    Field(
                        "lck3i",
                        int,
                        40,
                        10,
                        kwargs.get("lck3i")
                    ),
                    Field(
                        "vfi",
                        float,
                        50,
                        10,
                        kwargs.get("vfi")
                    ),
                    Field(
                        "mwi",
                        float,
                        60,
                        10,
                        kwargs.get("mwi")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "rci1",
                        float,
                        0,
                        10,
                        kwargs.get("rci1")
                    ),
                    Field(
                        "rci2",
                        float,
                        10,
                        10,
                        kwargs.get("rci2")
                    ),
                    Field(
                        "rci3",
                        float,
                        20,
                        10,
                        kwargs.get("rci3")
                    ),
                    Field(
                        "rci4",
                        float,
                        30,
                        10,
                        kwargs.get("rci4")
                    ),
                    Field(
                        "rci5",
                        float,
                        40,
                        10,
                        kwargs.get("rci5")
                    ),
                    Field(
                        "rci6",
                        float,
                        50,
                        10,
                        kwargs.get("rci6")
                    ),
                    Field(
                        "rci7",
                        float,
                        60,
                        10,
                        kwargs.get("rci7")
                    ),
                    Field(
                        "rci8",
                        float,
                        70,
                        10,
                        kwargs.get("rci8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "rxi1",
                        float,
                        0,
                        10,
                        kwargs.get("rxi1")
                    ),
                    Field(
                        "rxi2",
                        float,
                        10,
                        10,
                        kwargs.get("rxi2")
                    ),
                    Field(
                        "rxi3",
                        float,
                        20,
                        10,
                        kwargs.get("rxi3")
                    ),
                    Field(
                        "rxi4",
                        float,
                        30,
                        10,
                        kwargs.get("rxi4")
                    ),
                    Field(
                        "rxi5",
                        float,
                        40,
                        10,
                        kwargs.get("rxi5")
                    ),
                    Field(
                        "rxi6",
                        float,
                        50,
                        10,
                        kwargs.get("rxi6")
                    ),
                    Field(
                        "rxi7",
                        float,
                        60,
                        10,
                        kwargs.get("rxi7")
                    ),
                    Field(
                        "rxi8",
                        float,
                        70,
                        10,
                        kwargs.get("rxi8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lczi1",
                        float,
                        0,
                        10,
                        kwargs.get("lczi1")
                    ),
                    Field(
                        "lczi2",
                        float,
                        10,
                        10,
                        kwargs.get("lczi2")
                    ),
                    Field(
                        "lczi3",
                        float,
                        20,
                        10,
                        kwargs.get("lczi3")
                    ),
                    Field(
                        "lczi4",
                        float,
                        30,
                        10,
                        kwargs.get("lczi4")
                    ),
                    Field(
                        "lczi5",
                        float,
                        40,
                        10,
                        kwargs.get("lczi5")
                    ),
                    Field(
                        "lczi6",
                        float,
                        50,
                        10,
                        kwargs.get("lczi6")
                    ),
                    Field(
                        "lczi7",
                        float,
                        60,
                        10,
                        kwargs.get("lczi7")
                    ),
                    Field(
                        "lczi8",
                        float,
                        70,
                        10,
                        kwargs.get("lczi8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lczi1",
                        float,
                        0,
                        10,
                        kwargs.get("lczi1")
                    ),
                    Field(
                        "lczi2",
                        float,
                        10,
                        10,
                        kwargs.get("lczi2")
                    ),
                    Field(
                        "lczi3",
                        float,
                        20,
                        10,
                        kwargs.get("lczi3")
                    ),
                    Field(
                        "lczi4",
                        float,
                        30,
                        10,
                        kwargs.get("lczi4")
                    ),
                    Field(
                        "lczi5",
                        float,
                        40,
                        10,
                        kwargs.get("lczi5")
                    ),
                    Field(
                        "lczi6",
                        float,
                        50,
                        10,
                        kwargs.get("lczi6")
                    ),
                    Field(
                        "lczi7",
                        float,
                        60,
                        10,
                        kwargs.get("lczi7")
                    ),
                    Field(
                        "lczi8",
                        float,
                        70,
                        10,
                        kwargs.get("lczi8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lczi1",
                        float,
                        0,
                        10,
                        kwargs.get("lczi1")
                    ),
                    Field(
                        "lczi2",
                        float,
                        10,
                        10,
                        kwargs.get("lczi2")
                    ),
                    Field(
                        "lczi3",
                        float,
                        20,
                        10,
                        kwargs.get("lczi3")
                    ),
                    Field(
                        "lczi4",
                        float,
                        30,
                        10,
                        kwargs.get("lczi4")
                    ),
                    Field(
                        "lczi5",
                        float,
                        40,
                        10,
                        kwargs.get("lczi5")
                    ),
                    Field(
                        "lczi6",
                        float,
                        50,
                        10,
                        kwargs.get("lczi6")
                    ),
                    Field(
                        "lczi7",
                        float,
                        60,
                        10,
                        kwargs.get("lczi7")
                    ),
                    Field(
                        "lczi8",
                        float,
                        70,
                        10,
                        kwargs.get("lczi8")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatT17.option_specs[0],
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
    def tmid(self) -> typing.Optional[int]:
        """Get or set the Thermal material identification. A unique number or label must be specified.
        """ # nopep8
        return self._cards[0].get_value("tmid")

    @tmid.setter
    def tmid(self, value: int) -> None:
        self._cards[0].set_value("tmid", value)

    @property
    def nchsp(self) -> typing.Optional[int]:
        """Get or set the Number of chemical species (maximum 8)
        """ # nopep8
        return self._cards[0].get_value("nchsp")

    @nchsp.setter
    def nchsp(self, value: int) -> None:
        self._cards[0].set_value("nchsp", value)

    @property
    def nchrx(self) -> typing.Optional[int]:
        """Get or set the Number of chemical reactions (maximum 8)
        """ # nopep8
        return self._cards[0].get_value("nchrx")

    @nchrx.setter
    def nchrx(self, value: int) -> None:
        self._cards[0].set_value("nchrx", value)

    @property
    def icend(self) -> typing.Optional[int]:
        """Get or set the Species number controlling reaction termination
        """ # nopep8
        return self._cards[0].get_value("icend")

    @icend.setter
    def icend(self, value: int) -> None:
        self._cards[0].set_value("icend", value)

    @property
    def cend(self) -> typing.Optional[float]:
        """Get or set the Concentration for reaction termination
        """ # nopep8
        return self._cards[0].get_value("cend")

    @cend.setter
    def cend(self, value: float) -> None:
        self._cards[0].set_value("cend", value)

    @property
    def gasc(self) -> typing.Optional[float]:
        """Get or set the Gas constant: 1.987 cal/(g-mole K), 8314. J/(kg-mole K).
        """ # nopep8
        return self._cards[0].get_value("gasc")

    @gasc.setter
    def gasc(self, value: float) -> None:
        self._cards[0].set_value("gasc", value)

    @property
    def fid(self) -> typing.Optional[int]:
        """Get or set the Function ID for user specified chemical reaction rate equation
        """ # nopep8
        return self._cards[0].get_value("fid")

    @fid.setter
    def fid(self, value: int) -> None:
        self._cards[0].set_value("fid", value)

    @property
    def mf(self) -> int:
        """Get or set the ODE solver method:
        EQ.0: default
        EQ.1: an alternative ODE solver
        """ # nopep8
        return self._cards[0].get_value("mf")

    @mf.setter
    def mf(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""mf must be one of {0,1}""")
        self._cards[0].set_value("mf", value)

    @property
    def aopt(self) -> int:
        """Get or set the Material axes definition (see *MAT_OPTIONTROPIC_for a more complete description):
        EQ.0.0:	Locally orthotropic with material axes by element nodes N1, N2 and N4nEQ.1.0:	Locally orthotropic with material axes determined by a point in space and global location of element center
        EQ.2.0:	Globally orthotropic with material axes determined by vectors
        EQ.3.0:	Locally orthotropic with first material axis orthogonal to element normal (defined by element nodes N1, N2 and N4) and to a vector d- Third material direction corresponds to element normal.
        EQ.4.0:	Local orthogonal in cylindrical coordinates with the material axes determined by a vector Image, and an originating point, Image, which define the centerline axis.
        """ # nopep8
        return self._cards[1].get_value("aopt")

    @aopt.setter
    def aopt(self, value: int) -> None:
        if value not in [0.0, 1.0, 2.0, 3.0, 4.0]:
            raise Exception("""aopt must be one of {0.0,1.0,2.0,3.0,4.0}""")
        self._cards[1].set_value("aopt", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1 and 4
        """ # nopep8
        return self._cards[1].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[1].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1 and 4
        """ # nopep8
        return self._cards[1].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[1].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1 and 4
        """ # nopep8
        return self._cards[1].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[1].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2
        """ # nopep8
        return self._cards[1].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[1].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2
        """ # nopep8
        return self._cards[1].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[1].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2
        """ # nopep8
        return self._cards[1].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[1].set_value("a3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2,3 and 4
        """ # nopep8
        return self._cards[2].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[2].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2,3 and 4
        """ # nopep8
        return self._cards[2].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[2].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2,3 and 4
        """ # nopep8
        return self._cards[2].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[2].set_value("d3", value)

    @property
    def rhof(self) -> typing.Optional[float]:
        """Get or set the Density of the filler material
        """ # nopep8
        return self._cards[3].get_value("rhof")

    @rhof.setter
    def rhof(self, value: float) -> None:
        self._cards[3].set_value("rhof", value)

    @property
    def lccf(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying the specific heat as a function of temperature for the filler material.
        """ # nopep8
        return self._cards[3].get_value("lccf")

    @lccf.setter
    def lccf(self, value: int) -> None:
        self._cards[3].set_value("lccf", value)

    @property
    def lck1f(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying the thermal conductivity as a function of temperature for the filler material
        """ # nopep8
        return self._cards[3].get_value("lck1f")

    @lck1f.setter
    def lck1f(self, value: int) -> None:
        self._cards[3].set_value("lck1f", value)

    @property
    def lck2f(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[3].get_value("lck2f")

    @lck2f.setter
    def lck2f(self, value: float) -> None:
        self._cards[3].set_value("lck2f", value)

    @property
    def lck3f(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[3].get_value("lck3f")

    @lck3f.setter
    def lck3f(self, value: float) -> None:
        self._cards[3].set_value("lck3f", value)

    @property
    def vff(self) -> typing.Optional[float]:
        """Get or set the Volume fraction of the filler material. The remaining volume is occupied by the reacting chemicals
        """ # nopep8
        return self._cards[3].get_value("vff")

    @vff.setter
    def vff(self, value: float) -> None:
        self._cards[3].set_value("vff", value)

    @property
    def rhoi(self) -> typing.Optional[float]:
        """Get or set the Density of the ith species
        """ # nopep8
        return self._cards[4].get_value("rhoi")

    @rhoi.setter
    def rhoi(self, value: float) -> None:
        self._cards[4].set_value("rhoi", value)

    @property
    def lcci(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying specific heat vs. temperature for the ith species.
        """ # nopep8
        return self._cards[4].get_value("lcci")

    @lcci.setter
    def lcci(self, value: int) -> None:
        self._cards[4].set_value("lcci", value)

    @property
    def lck1i(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying thermal conductivity vs. temperature for the ith species
        """ # nopep8
        return self._cards[4].get_value("lck1i")

    @lck1i.setter
    def lck1i(self, value: int) -> None:
        self._cards[4].set_value("lck1i", value)

    @property
    def lck2i(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying thermal conductivity vs. temperature for the ith species
        """ # nopep8
        return self._cards[4].get_value("lck2i")

    @lck2i.setter
    def lck2i(self, value: int) -> None:
        self._cards[4].set_value("lck2i", value)

    @property
    def lck3i(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying thermal conductivity vs. temperature for the ith species
        """ # nopep8
        return self._cards[4].get_value("lck3i")

    @lck3i.setter
    def lck3i(self, value: int) -> None:
        self._cards[4].set_value("lck3i", value)

    @property
    def vfi(self) -> typing.Optional[float]:
        """Get or set the Initial fraction of the ith species relative to the other reacting chemicals
        """ # nopep8
        return self._cards[4].get_value("vfi")

    @vfi.setter
    def vfi(self, value: float) -> None:
        self._cards[4].set_value("vfi", value)

    @property
    def mwi(self) -> typing.Optional[float]:
        """Get or set the Molecular weight of the ith species
        """ # nopep8
        return self._cards[4].get_value("mwi")

    @mwi.setter
    def mwi(self, value: float) -> None:
        self._cards[4].set_value("mwi", value)

    @property
    def rci1(self) -> typing.Optional[float]:
        """Get or set the Reaction coefficient for species i in reaction j. Leave blank for undefined reactions
        """ # nopep8
        return self._cards[5].get_value("rci1")

    @rci1.setter
    def rci1(self, value: float) -> None:
        self._cards[5].set_value("rci1", value)

    @property
    def rci2(self) -> typing.Optional[float]:
        """Get or set the Reaction coefficient for species i in reaction j. Leave blank for undefined reactions
        """ # nopep8
        return self._cards[5].get_value("rci2")

    @rci2.setter
    def rci2(self, value: float) -> None:
        self._cards[5].set_value("rci2", value)

    @property
    def rci3(self) -> typing.Optional[float]:
        """Get or set the Reaction coefficient for species i in reaction j. Leave blank for undefined reactions
        """ # nopep8
        return self._cards[5].get_value("rci3")

    @rci3.setter
    def rci3(self, value: float) -> None:
        self._cards[5].set_value("rci3", value)

    @property
    def rci4(self) -> typing.Optional[float]:
        """Get or set the Reaction coefficient for species i in reaction j. Leave blank for undefined reactions
        """ # nopep8
        return self._cards[5].get_value("rci4")

    @rci4.setter
    def rci4(self, value: float) -> None:
        self._cards[5].set_value("rci4", value)

    @property
    def rci5(self) -> typing.Optional[float]:
        """Get or set the Reaction coefficient for species i in reaction j. Leave blank for undefined reactions
        """ # nopep8
        return self._cards[5].get_value("rci5")

    @rci5.setter
    def rci5(self, value: float) -> None:
        self._cards[5].set_value("rci5", value)

    @property
    def rci6(self) -> typing.Optional[float]:
        """Get or set the Reaction coefficient for species i in reaction j. Leave blank for undefined reactions
        """ # nopep8
        return self._cards[5].get_value("rci6")

    @rci6.setter
    def rci6(self, value: float) -> None:
        self._cards[5].set_value("rci6", value)

    @property
    def rci7(self) -> typing.Optional[float]:
        """Get or set the Reaction coefficient for species i in reaction j. Leave blank for undefined reactions
        """ # nopep8
        return self._cards[5].get_value("rci7")

    @rci7.setter
    def rci7(self, value: float) -> None:
        self._cards[5].set_value("rci7", value)

    @property
    def rci8(self) -> typing.Optional[float]:
        """Get or set the Reaction coefficient for species i in reaction j. Leave blank for undefined reactions
        """ # nopep8
        return self._cards[5].get_value("rci8")

    @rci8.setter
    def rci8(self, value: float) -> None:
        self._cards[5].set_value("rci8", value)

    @property
    def rxi1(self) -> typing.Optional[float]:
        """Get or set the Rate exponent for species i in reaction j. Leave blank for undefined reactions.
        """ # nopep8
        return self._cards[6].get_value("rxi1")

    @rxi1.setter
    def rxi1(self, value: float) -> None:
        self._cards[6].set_value("rxi1", value)

    @property
    def rxi2(self) -> typing.Optional[float]:
        """Get or set the Rate exponent for species i in reaction j. Leave blank for undefined reactions.
        """ # nopep8
        return self._cards[6].get_value("rxi2")

    @rxi2.setter
    def rxi2(self, value: float) -> None:
        self._cards[6].set_value("rxi2", value)

    @property
    def rxi3(self) -> typing.Optional[float]:
        """Get or set the Rate exponent for species i in reaction j. Leave blank for undefined reactions.
        """ # nopep8
        return self._cards[6].get_value("rxi3")

    @rxi3.setter
    def rxi3(self, value: float) -> None:
        self._cards[6].set_value("rxi3", value)

    @property
    def rxi4(self) -> typing.Optional[float]:
        """Get or set the Rate exponent for species i in reaction j. Leave blank for undefined reactions.
        """ # nopep8
        return self._cards[6].get_value("rxi4")

    @rxi4.setter
    def rxi4(self, value: float) -> None:
        self._cards[6].set_value("rxi4", value)

    @property
    def rxi5(self) -> typing.Optional[float]:
        """Get or set the Rate exponent for species i in reaction j. Leave blank for undefined reactions.
        """ # nopep8
        return self._cards[6].get_value("rxi5")

    @rxi5.setter
    def rxi5(self, value: float) -> None:
        self._cards[6].set_value("rxi5", value)

    @property
    def rxi6(self) -> typing.Optional[float]:
        """Get or set the Rate exponent for species i in reaction j. Leave blank for undefined reactions.
        """ # nopep8
        return self._cards[6].get_value("rxi6")

    @rxi6.setter
    def rxi6(self, value: float) -> None:
        self._cards[6].set_value("rxi6", value)

    @property
    def rxi7(self) -> typing.Optional[float]:
        """Get or set the Rate exponent for species i in reaction j. Leave blank for undefined reactions.
        """ # nopep8
        return self._cards[6].get_value("rxi7")

    @rxi7.setter
    def rxi7(self, value: float) -> None:
        self._cards[6].set_value("rxi7", value)

    @property
    def rxi8(self) -> typing.Optional[float]:
        """Get or set the Rate exponent for species i in reaction j. Leave blank for undefined reactions.
        """ # nopep8
        return self._cards[6].get_value("rxi8")

    @rxi8.setter
    def rxi8(self, value: float) -> None:
        self._cards[6].set_value("rxi8", value)

    @property
    def lczi1(self) -> typing.Optional[float]:
        """Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
        """ # nopep8
        return self._cards[7].get_value("lczi1")

    @lczi1.setter
    def lczi1(self, value: float) -> None:
        self._cards[7].set_value("lczi1", value)

    @property
    def lczi2(self) -> typing.Optional[float]:
        """Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
        """ # nopep8
        return self._cards[7].get_value("lczi2")

    @lczi2.setter
    def lczi2(self, value: float) -> None:
        self._cards[7].set_value("lczi2", value)

    @property
    def lczi3(self) -> typing.Optional[float]:
        """Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
        """ # nopep8
        return self._cards[7].get_value("lczi3")

    @lczi3.setter
    def lczi3(self, value: float) -> None:
        self._cards[7].set_value("lczi3", value)

    @property
    def lczi4(self) -> typing.Optional[float]:
        """Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
        """ # nopep8
        return self._cards[7].get_value("lczi4")

    @lczi4.setter
    def lczi4(self, value: float) -> None:
        self._cards[7].set_value("lczi4", value)

    @property
    def lczi5(self) -> typing.Optional[float]:
        """Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
        """ # nopep8
        return self._cards[7].get_value("lczi5")

    @lczi5.setter
    def lczi5(self, value: float) -> None:
        self._cards[7].set_value("lczi5", value)

    @property
    def lczi6(self) -> typing.Optional[float]:
        """Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
        """ # nopep8
        return self._cards[7].get_value("lczi6")

    @lczi6.setter
    def lczi6(self, value: float) -> None:
        self._cards[7].set_value("lczi6", value)

    @property
    def lczi7(self) -> typing.Optional[float]:
        """Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
        """ # nopep8
        return self._cards[7].get_value("lczi7")

    @lczi7.setter
    def lczi7(self, value: float) -> None:
        self._cards[7].set_value("lczi7", value)

    @property
    def lczi8(self) -> typing.Optional[float]:
        """Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
        """ # nopep8
        return self._cards[7].get_value("lczi8")

    @lczi8.setter
    def lczi8(self, value: float) -> None:
        self._cards[7].set_value("lczi8", value)

    @property
    def lczi1(self) -> typing.Optional[float]:
        """Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
        """ # nopep8
        return self._cards[8].get_value("lczi1")

    @lczi1.setter
    def lczi1(self, value: float) -> None:
        self._cards[8].set_value("lczi1", value)

    @property
    def lczi2(self) -> typing.Optional[float]:
        """Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
        """ # nopep8
        return self._cards[8].get_value("lczi2")

    @lczi2.setter
    def lczi2(self, value: float) -> None:
        self._cards[8].set_value("lczi2", value)

    @property
    def lczi3(self) -> typing.Optional[float]:
        """Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
        """ # nopep8
        return self._cards[8].get_value("lczi3")

    @lczi3.setter
    def lczi3(self, value: float) -> None:
        self._cards[8].set_value("lczi3", value)

    @property
    def lczi4(self) -> typing.Optional[float]:
        """Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
        """ # nopep8
        return self._cards[8].get_value("lczi4")

    @lczi4.setter
    def lczi4(self, value: float) -> None:
        self._cards[8].set_value("lczi4", value)

    @property
    def lczi5(self) -> typing.Optional[float]:
        """Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
        """ # nopep8
        return self._cards[8].get_value("lczi5")

    @lczi5.setter
    def lczi5(self, value: float) -> None:
        self._cards[8].set_value("lczi5", value)

    @property
    def lczi6(self) -> typing.Optional[float]:
        """Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
        """ # nopep8
        return self._cards[8].get_value("lczi6")

    @lczi6.setter
    def lczi6(self, value: float) -> None:
        self._cards[8].set_value("lczi6", value)

    @property
    def lczi7(self) -> typing.Optional[float]:
        """Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
        """ # nopep8
        return self._cards[8].get_value("lczi7")

    @lczi7.setter
    def lczi7(self, value: float) -> None:
        self._cards[8].set_value("lczi7", value)

    @property
    def lczi8(self) -> typing.Optional[float]:
        """Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
        """ # nopep8
        return self._cards[8].get_value("lczi8")

    @lczi8.setter
    def lczi8(self, value: float) -> None:
        self._cards[8].set_value("lczi8", value)

    @property
    def lczi1(self) -> typing.Optional[float]:
        """Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
        """ # nopep8
        return self._cards[9].get_value("lczi1")

    @lczi1.setter
    def lczi1(self, value: float) -> None:
        self._cards[9].set_value("lczi1", value)

    @property
    def lczi2(self) -> typing.Optional[float]:
        """Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
        """ # nopep8
        return self._cards[9].get_value("lczi2")

    @lczi2.setter
    def lczi2(self, value: float) -> None:
        self._cards[9].set_value("lczi2", value)

    @property
    def lczi3(self) -> typing.Optional[float]:
        """Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
        """ # nopep8
        return self._cards[9].get_value("lczi3")

    @lczi3.setter
    def lczi3(self, value: float) -> None:
        self._cards[9].set_value("lczi3", value)

    @property
    def lczi4(self) -> typing.Optional[float]:
        """Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
        """ # nopep8
        return self._cards[9].get_value("lczi4")

    @lczi4.setter
    def lczi4(self, value: float) -> None:
        self._cards[9].set_value("lczi4", value)

    @property
    def lczi5(self) -> typing.Optional[float]:
        """Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
        """ # nopep8
        return self._cards[9].get_value("lczi5")

    @lczi5.setter
    def lczi5(self, value: float) -> None:
        self._cards[9].set_value("lczi5", value)

    @property
    def lczi6(self) -> typing.Optional[float]:
        """Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
        """ # nopep8
        return self._cards[9].get_value("lczi6")

    @lczi6.setter
    def lczi6(self, value: float) -> None:
        self._cards[9].set_value("lczi6", value)

    @property
    def lczi7(self) -> typing.Optional[float]:
        """Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
        """ # nopep8
        return self._cards[9].get_value("lczi7")

    @lczi7.setter
    def lczi7(self, value: float) -> None:
        self._cards[9].set_value("lczi7", value)

    @property
    def lczi8(self) -> typing.Optional[float]:
        """Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
        """ # nopep8
        return self._cards[9].get_value("lczi8")

    @lczi8.setter
    def lczi8(self, value: float) -> None:
        self._cards[9].set_value("lczi8", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[10].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[10].cards[0].set_value("title", value)

