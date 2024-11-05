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

class DefineDeInjectionEllipse(KeywordBase):
    """DYNA DEFINE_DE_INJECTION_ELLIPSE keyword"""

    keyword = "DEFINE"
    subkeyword = "DE_INJECTION_ELLIPSE"
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
                        "pid",
                        int,
                        0,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "sid",
                        int,
                        10,
                        10,
                        kwargs.get("sid")
                    ),
                    Field(
                        "xc",
                        float,
                        20,
                        10,
                        kwargs.get("xc", 0.0)
                    ),
                    Field(
                        "yc",
                        float,
                        30,
                        10,
                        kwargs.get("yc", 0.0)
                    ),
                    Field(
                        "zc",
                        float,
                        40,
                        10,
                        kwargs.get("zc", 0.0)
                    ),
                    Field(
                        "xl",
                        float,
                        50,
                        10,
                        kwargs.get("xl", 0.0)
                    ),
                    Field(
                        "yl",
                        float,
                        60,
                        10,
                        kwargs.get("yl", 0.0)
                    ),
                    Field(
                        "cid",
                        int,
                        70,
                        10,
                        kwargs.get("cid", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "rmass",
                        float,
                        0,
                        10,
                        kwargs.get("rmass")
                    ),
                    Field(
                        "rmin",
                        float,
                        10,
                        10,
                        kwargs.get("rmin")
                    ),
                    Field(
                        "rmax",
                        float,
                        20,
                        10,
                        kwargs.get("rmax")
                    ),
                    Field(
                        "vx",
                        float,
                        30,
                        10,
                        kwargs.get("vx", 0.0)
                    ),
                    Field(
                        "vy",
                        float,
                        40,
                        10,
                        kwargs.get("vy", 0.0)
                    ),
                    Field(
                        "vz",
                        float,
                        50,
                        10,
                        kwargs.get("vz", 0.0)
                    ),
                    Field(
                        "tbeg",
                        float,
                        60,
                        10,
                        kwargs.get("tbeg", 0.0)
                    ),
                    Field(
                        "tend",
                        float,
                        70,
                        10,
                        kwargs.get("tend", 1.0E20)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ifunc",
                        int,
                        0,
                        10,
                        kwargs.get("ifunc", 0)
                    ),
                    Field(
                        "nid",
                        int,
                        10,
                        10,
                        kwargs.get("nid")
                    ),
                    Field(
                        "imulti",
                        int,
                        20,
                        10,
                        kwargs.get("imulti")
                    ),
                    Field(
                        "lcvx",
                        int,
                        30,
                        10,
                        kwargs.get("lcvx")
                    ),
                    Field(
                        "lcvy",
                        int,
                        40,
                        10,
                        kwargs.get("lcvy")
                    ),
                    Field(
                        "lcvz",
                        int,
                        50,
                        10,
                        kwargs.get("lcvz")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "r1",
                        float,
                        0,
                        10,
                        kwargs.get("r1", 0.0)
                    ),
                    Field(
                        "p1",
                        float,
                        10,
                        10,
                        kwargs.get("p1", 0.0)
                    ),
                    Field(
                        "r2",
                        float,
                        20,
                        10,
                        kwargs.get("r2", 0.0)
                    ),
                    Field(
                        "p2",
                        float,
                        30,
                        10,
                        kwargs.get("p2", 0.0)
                    ),
                    Field(
                        "r3",
                        float,
                        40,
                        10,
                        kwargs.get("r3", 0.0)
                    ),
                    Field(
                        "p3",
                        float,
                        50,
                        10,
                        kwargs.get("p3", 0.0)
                    ),
                    Field(
                        "r4",
                        float,
                        60,
                        10,
                        kwargs.get("r4", 0.0)
                    ),
                    Field(
                        "p4",
                        float,
                        70,
                        10,
                        kwargs.get("p4", 0.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineDeInjectionEllipse.option_specs[0],
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
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of new generated DES nodes
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Node set ID of new generated DES nodes
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[0].set_value("sid", value)

    @property
    def xc(self) -> float:
        """Get or set the X coordinate of the center of injection plane.
        """ # nopep8
        return self._cards[0].get_value("xc")

    @xc.setter
    def xc(self, value: float) -> None:
        self._cards[0].set_value("xc", value)

    @property
    def yc(self) -> float:
        """Get or set the Y coordinate of the center of injection plane.
        """ # nopep8
        return self._cards[0].get_value("yc")

    @yc.setter
    def yc(self, value: float) -> None:
        self._cards[0].set_value("yc", value)

    @property
    def zc(self) -> float:
        """Get or set the Z coordinate of the center of injection plane.
        """ # nopep8
        return self._cards[0].get_value("zc")

    @zc.setter
    def zc(self, value: float) -> None:
        self._cards[0].set_value("zc", value)

    @property
    def xl(self) -> float:
        """Get or set the Length of the rectangular injection plane along X-axis in the coordinate	system(CID) defined.
        """ # nopep8
        return self._cards[0].get_value("xl")

    @xl.setter
    def xl(self, value: float) -> None:
        self._cards[0].set_value("xl", value)

    @property
    def yl(self) -> float:
        """Get or set the Length of the rectangular injection plane along Y-axis in the coordinate	system(CID) defined.
        """ # nopep8
        return self._cards[0].get_value("yl")

    @yl.setter
    def yl(self, value: float) -> None:
        self._cards[0].set_value("yl", value)

    @property
    def cid(self) -> int:
        """Get or set the Optional local coordinate system ID.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[0].set_value("cid", value)

    @property
    def rmass(self) -> typing.Optional[float]:
        """Get or set the Mass flow rate
        """ # nopep8
        return self._cards[1].get_value("rmass")

    @rmass.setter
    def rmass(self, value: float) -> None:
        self._cards[1].set_value("rmass", value)

    @property
    def rmin(self) -> typing.Optional[float]:
        """Get or set the Minimum DES radius (ignored if IMULTI > 1)
        """ # nopep8
        return self._cards[1].get_value("rmin")

    @rmin.setter
    def rmin(self, value: float) -> None:
        self._cards[1].set_value("rmin", value)

    @property
    def rmax(self) -> typing.Optional[float]:
        """Get or set the Maximum DES radius.(ignored if IMULTI > 1)
        """ # nopep8
        return self._cards[1].get_value("rmax")

    @rmax.setter
    def rmax(self, value: float) -> None:
        self._cards[1].set_value("rmax", value)

    @property
    def vx(self) -> float:
        """Get or set the Vector components defining the initial velocity of injected DES in the coordinate system(CID) defined.
        """ # nopep8
        return self._cards[1].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        self._cards[1].set_value("vx", value)

    @property
    def vy(self) -> float:
        """Get or set the Vector components defining the initial velocity of injected DES in the coordinate system(CID) defined.
        """ # nopep8
        return self._cards[1].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        self._cards[1].set_value("vy", value)

    @property
    def vz(self) -> float:
        """Get or set the Vector components defining the initial velocity of injected DES in the coordinate system(CID) defined.
        """ # nopep8
        return self._cards[1].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        self._cards[1].set_value("vz", value)

    @property
    def tbeg(self) -> float:
        """Get or set the Birth time.
        """ # nopep8
        return self._cards[1].get_value("tbeg")

    @tbeg.setter
    def tbeg(self, value: float) -> None:
        self._cards[1].set_value("tbeg", value)

    @property
    def tend(self) -> float:
        """Get or set the Death time.
        """ # nopep8
        return self._cards[1].get_value("tend")

    @tend.setter
    def tend(self, value: float) -> None:
        self._cards[1].set_value("tend", value)

    @property
    def ifunc(self) -> int:
        """Get or set the Distribution of particle radii (ignored if IMULTI > 1):
        EQ.0: Uniform distribution(Default)
        EQ.1: Gaussian distribution (see Remarks)
        """ # nopep8
        return self._cards[2].get_value("ifunc")

    @ifunc.setter
    def ifunc(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ifunc must be one of {0,1}""")
        self._cards[2].set_value("ifunc", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the An optional node ID. If defined, the center of injection plane follows the motion of this node
        """ # nopep8
        return self._cards[2].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[2].set_value("nid", value)

    @property
    def imulti(self) -> typing.Optional[int]:
        """Get or set the Flag for giving a specified mass distribution of injected particles with given radii:
        EQ.1:	Inject the particles with distribution IFUNC using the radii specified with RMINand RMAX(default).
        GT.1 : Inject particles with IMULTI different radii, Ri, with each different size having a specified mass distribution, Pi, given in Card 3.1.IMULTI cannot be greater than 4.
        """ # nopep8
        return self._cards[2].get_value("imulti")

    @imulti.setter
    def imulti(self, value: int) -> None:
        self._cards[2].set_value("imulti", value)

    @property
    def lcvx(self) -> typing.Optional[int]:
        """Get or set the Load curve defines initial injection velocity in x-direction
        """ # nopep8
        return self._cards[2].get_value("lcvx")

    @lcvx.setter
    def lcvx(self, value: int) -> None:
        self._cards[2].set_value("lcvx", value)

    @property
    def lcvy(self) -> typing.Optional[int]:
        """Get or set the Load curve defines initial injection velocity in y-direction
        """ # nopep8
        return self._cards[2].get_value("lcvy")

    @lcvy.setter
    def lcvy(self, value: int) -> None:
        self._cards[2].set_value("lcvy", value)

    @property
    def lcvz(self) -> typing.Optional[int]:
        """Get or set the Load curve defines initial injection velocity in z-direction
        """ # nopep8
        return self._cards[2].get_value("lcvz")

    @lcvz.setter
    def lcvz(self, value: int) -> None:
        self._cards[2].set_value("lcvz", value)

    @property
    def r1(self) -> float:
        """Get or set the Injected particle radius.IMULTI radii may be specified
        """ # nopep8
        return self._cards[3].get_value("r1")

    @r1.setter
    def r1(self, value: float) -> None:
        self._cards[3].set_value("r1", value)

    @property
    def p1(self) -> float:
        """Get or set the The mass percentage of injected particle with radius Ri
        """ # nopep8
        return self._cards[3].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        self._cards[3].set_value("p1", value)

    @property
    def r2(self) -> float:
        """Get or set the Injected particle radius.IMULTI radii may be specified
        """ # nopep8
        return self._cards[3].get_value("r2")

    @r2.setter
    def r2(self, value: float) -> None:
        self._cards[3].set_value("r2", value)

    @property
    def p2(self) -> float:
        """Get or set the The mass percentage of injected particle with radius Ri
        """ # nopep8
        return self._cards[3].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        self._cards[3].set_value("p2", value)

    @property
    def r3(self) -> float:
        """Get or set the Injected particle radius.IMULTI radii may be specified
        """ # nopep8
        return self._cards[3].get_value("r3")

    @r3.setter
    def r3(self, value: float) -> None:
        self._cards[3].set_value("r3", value)

    @property
    def p3(self) -> float:
        """Get or set the The mass percentage of injected particle with radius Ri
        """ # nopep8
        return self._cards[3].get_value("p3")

    @p3.setter
    def p3(self, value: float) -> None:
        self._cards[3].set_value("p3", value)

    @property
    def r4(self) -> float:
        """Get or set the Injected particle radius.IMULTI radii may be specified
        """ # nopep8
        return self._cards[3].get_value("r4")

    @r4.setter
    def r4(self, value: float) -> None:
        self._cards[3].set_value("r4", value)

    @property
    def p4(self) -> float:
        """Get or set the The mass percentage of injected particle with radius Ri
        """ # nopep8
        return self._cards[3].get_value("p4")

    @p4.setter
    def p4(self, value: float) -> None:
        self._cards[3].set_value("p4", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[4].cards[0].set_value("title", value)

