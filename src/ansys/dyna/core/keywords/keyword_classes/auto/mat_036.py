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

class Mat036(KeywordBase):
    """DYNA MAT_036 keyword"""

    keyword = "MAT"
    subkeyword = "036"
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
                        "hr",
                        float,
                        40,
                        10,
                        kwargs.get("hr", 1.0)
                    ),
                    Field(
                        "p1",
                        float,
                        50,
                        10,
                        kwargs.get("p1", 1.0)
                    ),
                    Field(
                        "p2",
                        float,
                        60,
                        10,
                        kwargs.get("p2", 1.0)
                    ),
                    Field(
                        "iter",
                        float,
                        70,
                        10,
                        kwargs.get("iter", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "m",
                        float,
                        0,
                        10,
                        kwargs.get("m")
                    ),
                    Field(
                        "r00",
                        float,
                        10,
                        10,
                        kwargs.get("r00")
                    ),
                    Field(
                        "r45",
                        float,
                        20,
                        10,
                        kwargs.get("r45")
                    ),
                    Field(
                        "r90",
                        float,
                        30,
                        10,
                        kwargs.get("r90")
                    ),
                    Field(
                        "lcid",
                        int,
                        40,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "e0",
                        float,
                        50,
                        10,
                        kwargs.get("e0")
                    ),
                    Field(
                        "spi",
                        float,
                        60,
                        10,
                        kwargs.get("spi")
                    ),
                    Field(
                        "p3",
                        float,
                        70,
                        10,
                        kwargs.get("p3")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "crc1",
                        float,
                        0,
                        10,
                        kwargs.get("crc1")
                    ),
                    Field(
                        "cra1",
                        float,
                        10,
                        10,
                        kwargs.get("cra1")
                    ),
                    Field(
                        "crc2",
                        float,
                        20,
                        10,
                        kwargs.get("crc2")
                    ),
                    Field(
                        "cra2",
                        float,
                        30,
                        10,
                        kwargs.get("cra2")
                    ),
                    Field(
                        "crc3",
                        float,
                        40,
                        10,
                        kwargs.get("crc3")
                    ),
                    Field(
                        "cra3",
                        float,
                        50,
                        10,
                        kwargs.get("cra3")
                    ),
                    Field(
                        "crc4",
                        float,
                        60,
                        10,
                        kwargs.get("crc4")
                    ),
                    Field(
                        "cra4",
                        float,
                        70,
                        10,
                        kwargs.get("cra4")
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
                        "c",
                        float,
                        10,
                        10,
                        kwargs.get("c")
                    ),
                    Field(
                        "p",
                        float,
                        20,
                        10,
                        kwargs.get("p")
                    ),
                    Field(
                        "vlcid",
                        int,
                        30,
                        10,
                        kwargs.get("vlcid")
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "pb",
                        float,
                        50,
                        10,
                        kwargs.get("pb")
                    ),
                    Field(
                        "nlp",
                        int,
                        60,
                        10,
                        kwargs.get("nlp")
                    ),
                    Field(
                        "htb",
                        float,
                        70,
                        10,
                        kwargs.get("htb")
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
                        "htc",
                        int,
                        60,
                        10,
                        kwargs.get("htc")
                    ),
                    Field(
                        "htd",
                        int,
                        70,
                        10,
                        kwargs.get("htd")
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
                    Field(
                        "htflag",
                        float,
                        70,
                        10,
                        kwargs.get("htflag")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "usrfail",
                        float,
                        0,
                        10,
                        kwargs.get("usrfail", 0)
                    ),
                    Field(
                        "lcbi",
                        float,
                        10,
                        10,
                        kwargs.get("lcbi")
                    ),
                    Field(
                        "lcsh",
                        float,
                        20,
                        10,
                        kwargs.get("lcsh")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat036.option_specs[0],
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
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus.
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
    def hr(self) -> float:
        """Get or set the Hardening rule:
        EQ.1.0: linear (default),
        EQ.2.0: exponential.
        EQ.3.0: load curve.
        EQ.4.0: exponential (Voce)
        EQ.5.0: exponential (Gosh)
        EQ.6.0: exponential (Hocket-Sherby)
        EQ.7.0 load curve in three directions
        EQ.8.0: table with temperature dependence
        EQ.9.0: 3d table with temperature and strain rate dependence
        """ # nopep8
        return self._cards[0].get_value("hr")

    @hr.setter
    def hr(self, value: float) -> None:
        if value not in [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0]:
            raise Exception("""hr must be one of {1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0}""")
        self._cards[0].set_value("hr", value)

    @property
    def p1(self) -> float:
        """Get or set the Material parameter: HR.
        EQ.1.0: Tangent modulus, HR.
        EQ.2.0: k, strength coefficient for exponential hardening.
        """ # nopep8
        return self._cards[0].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        self._cards[0].set_value("p1", value)

    @property
    def p2(self) -> float:
        """Get or set the Material parameter: HR.
        EQ.1.0: Yield stress HR.
        EQ.2.0: n, exponent.
        """ # nopep8
        return self._cards[0].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        self._cards[0].set_value("p2", value)

    @property
    def iter(self) -> float:
        """Get or set the Iteration flag for speed:
        ITER.EQ.0.0: fully iterative
        ITER.EQ.1.0: fixed at three iterations
        Generally, ITER=0 is recommended. However, ITER=1 is somewhat faster and may give acceptable results in most problems.
        """ # nopep8
        return self._cards[0].get_value("iter")

    @iter.setter
    def iter(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""iter must be one of {0.0,1.0}""")
        self._cards[0].set_value("iter", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the m, exponent in Barlat's yield surface.
        """ # nopep8
        return self._cards[1].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        self._cards[1].set_value("m", value)

    @property
    def r00(self) -> typing.Optional[float]:
        """Get or set the R00 , Lankford parmeter determined from experiments.
        """ # nopep8
        return self._cards[1].get_value("r00")

    @r00.setter
    def r00(self, value: float) -> None:
        self._cards[1].set_value("r00", value)

    @property
    def r45(self) -> typing.Optional[float]:
        """Get or set the R45 , Lankford parmeter determined from experiments.
        """ # nopep8
        return self._cards[1].get_value("r45")

    @r45.setter
    def r45(self, value: float) -> None:
        self._cards[1].set_value("r45", value)

    @property
    def r90(self) -> typing.Optional[float]:
        """Get or set the R90 , Lankford parmeter determined from experiments.
        """ # nopep8
        return self._cards[1].get_value("r90")

    @r90.setter
    def r90(self, value: float) -> None:
        self._cards[1].set_value("r90", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve/table ID for hardening in the 0 degree direction.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[1].set_value("lcid", value)

    @property
    def e0(self) -> typing.Optional[float]:
        """Get or set the epsilon-0 for determining initial yield stress for exponential hardening (default = 0.0).
        """ # nopep8
        return self._cards[1].get_value("e0")

    @e0.setter
    def e0(self, value: float) -> None:
        self._cards[1].set_value("e0", value)

    @property
    def spi(self) -> typing.Optional[float]:
        """Get or set the spi, if epsilon-0 is zero above (default = 0.0).
        EQ.0.0: e0 = (E/k )**[1/(n -1)]
        LT..02: e0 = spi
        GT..02: e0 = (spi/k)**[1/n].
        """ # nopep8
        return self._cards[1].get_value("spi")

    @spi.setter
    def spi(self, value: float) -> None:
        self._cards[1].set_value("spi", value)

    @property
    def p3(self) -> typing.Optional[float]:
        """Get or set the Material parameter:
        HR EQ.5.0: p,parameter for Gosh exponential hardening
        HR EQ.6.0: n,exponent for Hocket-Sherby exponential hardening
        """ # nopep8
        return self._cards[1].get_value("p3")

    @p3.setter
    def p3(self, value: float) -> None:
        self._cards[1].set_value("p3", value)

    @property
    def crc1(self) -> typing.Optional[float]:
        """Get or set the Chaboche-Roussilier kinematic hardening parameter1
        """ # nopep8
        return self._cards[2].get_value("crc1")

    @crc1.setter
    def crc1(self, value: float) -> None:
        self._cards[2].set_value("crc1", value)

    @property
    def cra1(self) -> typing.Optional[float]:
        """Get or set the Chaboche-Roussilier kinematic hardening parameter
        """ # nopep8
        return self._cards[2].get_value("cra1")

    @cra1.setter
    def cra1(self, value: float) -> None:
        self._cards[2].set_value("cra1", value)

    @property
    def crc2(self) -> typing.Optional[float]:
        """Get or set the Chaboche-Roussilier kinematic hardening parameter
        """ # nopep8
        return self._cards[2].get_value("crc2")

    @crc2.setter
    def crc2(self, value: float) -> None:
        self._cards[2].set_value("crc2", value)

    @property
    def cra2(self) -> typing.Optional[float]:
        """Get or set the Chaboche-Roussilier kinematic hardening parameter
        """ # nopep8
        return self._cards[2].get_value("cra2")

    @cra2.setter
    def cra2(self, value: float) -> None:
        self._cards[2].set_value("cra2", value)

    @property
    def crc3(self) -> typing.Optional[float]:
        """Get or set the Chaboche-Roussilier kinematic hardening parameter
        """ # nopep8
        return self._cards[2].get_value("crc3")

    @crc3.setter
    def crc3(self, value: float) -> None:
        self._cards[2].set_value("crc3", value)

    @property
    def cra3(self) -> typing.Optional[float]:
        """Get or set the Chaboche-Roussilier kinematic hardening parameter
        """ # nopep8
        return self._cards[2].get_value("cra3")

    @cra3.setter
    def cra3(self, value: float) -> None:
        self._cards[2].set_value("cra3", value)

    @property
    def crc4(self) -> typing.Optional[float]:
        """Get or set the Chaboche-Roussilier kinematic hardening parameter
        """ # nopep8
        return self._cards[2].get_value("crc4")

    @crc4.setter
    def crc4(self, value: float) -> None:
        self._cards[2].set_value("crc4", value)

    @property
    def cra4(self) -> typing.Optional[float]:
        """Get or set the Chaboche-Roussilier kinematic hardening parameter
        """ # nopep8
        return self._cards[2].get_value("cra4")

    @cra4.setter
    def cra4(self, value: float) -> None:
        self._cards[2].set_value("cra4", value)

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
        return self._cards[3].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[3].set_value("aopt", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the C in Cowper-Symonds strain rate model
        """ # nopep8
        return self._cards[3].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        self._cards[3].set_value("c", value)

    @property
    def p(self) -> typing.Optional[float]:
        """Get or set the p in Cowper-Symonds strain rate model, p=0.0 for no strain rate effects
        """ # nopep8
        return self._cards[3].get_value("p")

    @p.setter
    def p(self, value: float) -> None:
        self._cards[3].set_value("p", value)

    @property
    def vlcid(self) -> typing.Optional[int]:
        """Get or set the Volume correction curve ID defining the relative volume change (change in volume relative to the initial volume) as a function of the effective plastic strain.  This is only used when nonzero.
        """ # nopep8
        return self._cards[3].get_value("vlcid")

    @vlcid.setter
    def vlcid(self, value: int) -> None:
        self._cards[3].set_value("vlcid", value)

    @property
    def pb(self) -> typing.Optional[float]:
        """Get or set the Barlat89 parameter, p. If PB > 0, parameters AB, CB, and HB are read instead of R00, R45, and R90.
        """ # nopep8
        return self._cards[3].get_value("pb")

    @pb.setter
    def pb(self, value: float) -> None:
        self._cards[3].set_value("pb", value)

    @property
    def nlp(self) -> typing.Optional[int]:
        """Get or set the ID of a load curve of the Forming Limit Diagram (FLD) under linear
        strain paths. In the load curve, abscissas represent minor strains
        while ordinates represent major strains. Define only when option	NLP is used.
        """ # nopep8
        return self._cards[3].get_value("nlp")

    @nlp.setter
    def nlp(self, value: int) -> None:
        self._cards[3].set_value("nlp", value)

    @property
    def htb(self) -> typing.Optional[float]:
        """Get or set the Load curve/Table ID for postforming parameter B in heat treatment
        """ # nopep8
        return self._cards[3].get_value("htb")

    @htb.setter
    def htb(self, value: float) -> None:
        self._cards[3].set_value("htb", value)

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
    def htc(self) -> typing.Optional[int]:
        """Get or set the Load curve/Table ID for postforming parameter C in heat treatment
        """ # nopep8
        return self._cards[4].get_value("htc")

    @htc.setter
    def htc(self, value: int) -> None:
        self._cards[4].set_value("htc", value)

    @property
    def htd(self) -> typing.Optional[int]:
        """Get or set the Load curve/Table ID for postforming parameter D in heat treatment
        """ # nopep8
        return self._cards[4].get_value("htd")

    @htd.setter
    def htd(self, value: int) -> None:
        self._cards[4].set_value("htd", value)

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
        """Get or set the Material angle in degrees for AOPT = 3, which may be overridden on the element card, see *ELEMENT_SHELL.
        """ # nopep8
        return self._cards[5].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[5].set_value("beta", value)

    @property
    def htflag(self) -> typing.Optional[float]:
        """Get or set the Heat treatment flag:
        EQ.0: Preforming stage
        EQ.1: Heat treatment stage
        EQ.2: Postforming stage
        """ # nopep8
        return self._cards[5].get_value("htflag")

    @htflag.setter
    def htflag(self, value: float) -> None:
        self._cards[5].set_value("htflag", value)

    @property
    def usrfail(self) -> float:
        """Get or set the User defined failure flag:
        EQ.0:	no user subroutine is called.
        EQ.1 : user subroutine matusr_‌24 in dyn21.f is called
        """ # nopep8
        return self._cards[6].get_value("usrfail")

    @usrfail.setter
    def usrfail(self, value: float) -> None:
        if value not in [0, 1]:
            raise Exception("""usrfail must be one of {0,1}""")
        self._cards[6].set_value("usrfail", value)

    @property
    def lcbi(self) -> typing.Optional[float]:
        """Get or set the HR.EQ.7:	load curve defining biaxial stress as a function of biaxial strain for hardening rule; see discussion in the formulation section below for a definition.
        HR.NE.7:	ignored
        """ # nopep8
        return self._cards[6].get_value("lcbi")

    @lcbi.setter
    def lcbi(self, value: float) -> None:
        self._cards[6].set_value("lcbi", value)

    @property
    def lcsh(self) -> typing.Optional[float]:
        """Get or set the HR.EQ.7:	load curve defining shear stress as a function of shear strain for hardening; see discussion in the formulation section below for a definition.
        HR.NE.7:	ignored
        """ # nopep8
        return self._cards[6].get_value("lcsh")

    @lcsh.setter
    def lcsh(self, value: float) -> None:
        self._cards[6].set_value("lcsh", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[7].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[7].cards[0].set_value("title", value)

