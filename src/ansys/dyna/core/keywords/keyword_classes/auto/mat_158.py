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

"""Module providing the Mat158 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class Mat158(KeywordBase):
    """DYNA MAT_158 keyword"""

    keyword = "MAT"
    subkeyword = "158"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat158 class."""
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
                        **kwargs,
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ea",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "eb",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ec",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "prba",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "taui",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "gamma1",
                        float,
                        70,
                        10,
                        **kwargs,
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
                        **kwargs,
                    ),
                    Field(
                        "gbc",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "gca",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "slimt1",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "slimc1",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "slimt2",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "slimc2",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "slims",
                        float,
                        70,
                        10,
                        **kwargs,
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
                        **kwargs,
                    ),
                    Field(
                        "tsize",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "erods",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "soft",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "sf",
                        float,
                        40,
                        10,
                        **kwargs,
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
                        **kwargs,
                    ),
                    Field(
                        "yp",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "zp",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "a1",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "a2",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "a3",
                        float,
                        50,
                        10,
                        **kwargs,
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
                        **kwargs,
                    ),
                    Field(
                        "v2",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "v3",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "d1",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "d2",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "d3",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "beta",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "e11c",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "e11t",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "e22c",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "e22t",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "gms",
                        float,
                        40,
                        10,
                        **kwargs,
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
                        **kwargs,
                    ),
                    Field(
                        "xt",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "yc",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "yt",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "sc",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "k",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "gi",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "betai",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat158.option_specs[0],
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
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def ea(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[0].get_value("ea")

    @ea.setter
    def ea(self, value: float) -> None:
        """Set the ea property."""
        self._cards[0].set_value("ea", value)

    @property
    def eb(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[0].get_value("eb")

    @eb.setter
    def eb(self, value: float) -> None:
        """Set the eb property."""
        self._cards[0].set_value("eb", value)

    @property
    def ec(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[0].get_value("ec")

    @ec.setter
    def ec(self, value: float) -> None:
        """Set the ec property."""
        self._cards[0].set_value("ec", value)

    @property
    def prba(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[0].get_value("prba")

    @prba.setter
    def prba(self, value: float) -> None:
        """Set the prba property."""
        self._cards[0].set_value("prba", value)

    @property
    def taui(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[0].get_value("taui")

    @taui.setter
    def taui(self, value: float) -> None:
        """Set the taui property."""
        self._cards[0].set_value("taui", value)

    @property
    def gamma1(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[0].get_value("gamma1")

    @gamma1.setter
    def gamma1(self, value: float) -> None:
        """Set the gamma1 property."""
        self._cards[0].set_value("gamma1", value)

    @property
    def gab(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[1].get_value("gab")

    @gab.setter
    def gab(self, value: float) -> None:
        """Set the gab property."""
        self._cards[1].set_value("gab", value)

    @property
    def gbc(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[1].get_value("gbc")

    @gbc.setter
    def gbc(self, value: float) -> None:
        """Set the gbc property."""
        self._cards[1].set_value("gbc", value)

    @property
    def gca(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[1].get_value("gca")

    @gca.setter
    def gca(self, value: float) -> None:
        """Set the gca property."""
        self._cards[1].set_value("gca", value)

    @property
    def slimt1(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[1].get_value("slimt1")

    @slimt1.setter
    def slimt1(self, value: float) -> None:
        """Set the slimt1 property."""
        self._cards[1].set_value("slimt1", value)

    @property
    def slimc1(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[1].get_value("slimc1")

    @slimc1.setter
    def slimc1(self, value: float) -> None:
        """Set the slimc1 property."""
        self._cards[1].set_value("slimc1", value)

    @property
    def slimt2(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[1].get_value("slimt2")

    @slimt2.setter
    def slimt2(self, value: float) -> None:
        """Set the slimt2 property."""
        self._cards[1].set_value("slimt2", value)

    @property
    def slimc2(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[1].get_value("slimc2")

    @slimc2.setter
    def slimc2(self, value: float) -> None:
        """Set the slimc2 property."""
        self._cards[1].set_value("slimc2", value)

    @property
    def slims(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[1].get_value("slims")

    @slims.setter
    def slims(self, value: float) -> None:
        """Set the slims property."""
        self._cards[1].set_value("slims", value)

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
        """Set the aopt property."""
        self._cards[2].set_value("aopt", value)

    @property
    def tsize(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[2].get_value("tsize")

    @tsize.setter
    def tsize(self, value: float) -> None:
        """Set the tsize property."""
        self._cards[2].set_value("tsize", value)

    @property
    def erods(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[2].get_value("erods")

    @erods.setter
    def erods(self, value: float) -> None:
        """Set the erods property."""
        self._cards[2].set_value("erods", value)

    @property
    def soft(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[2].get_value("soft")

    @soft.setter
    def soft(self, value: float) -> None:
        """Set the soft property."""
        self._cards[2].set_value("soft", value)

    @property
    def sf(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[2].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[2].set_value("sf", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of point p for AOPT = 1
        """ # nopep8
        return self._cards[3].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[3].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[3].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[3].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[3].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[3].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector a for AOPT = 2..
        """ # nopep8
        return self._cards[3].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[3].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector a for AOPT = 2..
        """ # nopep8
        return self._cards[3].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[3].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector a for AOPT = 2..
        """ # nopep8
        return self._cards[3].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[3].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3
        """ # nopep8
        return self._cards[4].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        """Set the v1 property."""
        self._cards[4].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[4].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[4].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[4].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[4].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector d for AOPT = 2..
        """ # nopep8
        return self._cards[4].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[4].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector d for AOPT = 2..
        """ # nopep8
        return self._cards[4].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[4].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector d for AOPT = 2..
        """ # nopep8
        return self._cards[4].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[4].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[4].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[4].set_value("beta", value)

    @property
    def e11c(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[5].get_value("e11c")

    @e11c.setter
    def e11c(self, value: float) -> None:
        """Set the e11c property."""
        self._cards[5].set_value("e11c", value)

    @property
    def e11t(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[5].get_value("e11t")

    @e11t.setter
    def e11t(self, value: float) -> None:
        """Set the e11t property."""
        self._cards[5].set_value("e11t", value)

    @property
    def e22c(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[5].get_value("e22c")

    @e22c.setter
    def e22c(self, value: float) -> None:
        """Set the e22c property."""
        self._cards[5].set_value("e22c", value)

    @property
    def e22t(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[5].get_value("e22t")

    @e22t.setter
    def e22t(self, value: float) -> None:
        """Set the e22t property."""
        self._cards[5].set_value("e22t", value)

    @property
    def gms(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[5].get_value("gms")

    @gms.setter
    def gms(self, value: float) -> None:
        """Set the gms property."""
        self._cards[5].set_value("gms", value)

    @property
    def xc(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[6].get_value("xc")

    @xc.setter
    def xc(self, value: float) -> None:
        """Set the xc property."""
        self._cards[6].set_value("xc", value)

    @property
    def xt(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[6].get_value("xt")

    @xt.setter
    def xt(self, value: float) -> None:
        """Set the xt property."""
        self._cards[6].set_value("xt", value)

    @property
    def yc(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[6].get_value("yc")

    @yc.setter
    def yc(self, value: float) -> None:
        """Set the yc property."""
        self._cards[6].set_value("yc", value)

    @property
    def yt(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[6].get_value("yt")

    @yt.setter
    def yt(self, value: float) -> None:
        """Set the yt property."""
        self._cards[6].set_value("yt", value)

    @property
    def sc(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[6].get_value("sc")

    @sc.setter
    def sc(self, value: float) -> None:
        """Set the sc property."""
        self._cards[6].set_value("sc", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[7].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        """Set the k property."""
        self._cards[7].set_value("k", value)

    @property
    def gi(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[8].get_value("gi")

    @gi.setter
    def gi(self, value: float) -> None:
        """Set the gi property."""
        self._cards[8].set_value("gi", value)

    @property
    def betai(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[8].get_value("betai")

    @betai.setter
    def betai(self, value: float) -> None:
        """Set the betai property."""
        self._cards[8].set_value("betai", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[9].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[9].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

