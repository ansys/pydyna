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

class MatFld3ParameterBarlat(KeywordBase):
    """DYNA MAT_FLD_3-PARAMETER_BARLAT keyword"""

    keyword = "MAT"
    subkeyword = "FLD_3-PARAMETER_BARLAT"
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
                        "fldcid",
                        int,
                        30,
                        10,
                        kwargs.get("fldcid")
                    ),
                    Field(
                        "rn",
                        float,
                        40,
                        10,
                        kwargs.get("rn")
                    ),
                    Field(
                        "rt",
                        float,
                        50,
                        10,
                        kwargs.get("rt")
                    ),
                    Field(
                        "fldsafe",
                        float,
                        60,
                        10,
                        kwargs.get("fldsafe")
                    ),
                    Field(
                        "fldnipf",
                        int,
                        70,
                        10,
                        kwargs.get("fldnipf")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "unused",
                        int,
                        0,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
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
                option_spec = MatFld3ParameterBarlat.option_specs[0],
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
        """ # nopep8
        return self._cards[0].get_value("hr")

    @hr.setter
    def hr(self, value: float) -> None:
        if value not in [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]:
            raise Exception("""hr must be one of {1.0,2.0,3.0,4.0,5.0,6.0}""")
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
        """Get or set the Load curve ID for the hardening rule.
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
        self._cards[2].set_value("aopt", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the C in Cowper-Symonds strain rate model
        """ # nopep8
        return self._cards[2].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        self._cards[2].set_value("c", value)

    @property
    def p(self) -> typing.Optional[float]:
        """Get or set the p in Cowper-Symonds strain rate model, p=0.0 for no strain rate effects
        """ # nopep8
        return self._cards[2].get_value("p")

    @p.setter
    def p(self, value: float) -> None:
        self._cards[2].set_value("p", value)

    @property
    def fldcid(self) -> typing.Optional[int]:
        """Get or set the Volume correction curve ID defining the relative volume change (change in volume relative to the initial volume) as a function of the effective plastic strain.  This is only used when nonzero.
        """ # nopep8
        return self._cards[2].get_value("fldcid")

    @fldcid.setter
    def fldcid(self, value: int) -> None:
        self._cards[2].set_value("fldcid", value)

    @property
    def rn(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[2].get_value("rn")

    @rn.setter
    def rn(self, value: float) -> None:
        self._cards[2].set_value("rn", value)

    @property
    def rt(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[2].get_value("rt")

    @rt.setter
    def rt(self, value: float) -> None:
        self._cards[2].set_value("rt", value)

    @property
    def fldsafe(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[2].get_value("fldsafe")

    @fldsafe.setter
    def fldsafe(self, value: float) -> None:
        self._cards[2].set_value("fldsafe", value)

    @property
    def fldnipf(self) -> typing.Optional[int]:
        """Get or set the 
        """ # nopep8
        return self._cards[2].get_value("fldnipf")

    @fldnipf.setter
    def fldnipf(self, value: int) -> None:
        self._cards[2].set_value("fldnipf", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[3].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[3].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[3].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[4].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[4].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[4].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[4].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[4].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[4].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[4].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[4].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[4].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 3, which may be overridden on the element card, see *ELEMENT_SHELL.
        """ # nopep8
        return self._cards[4].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[4].set_value("beta", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[5].cards[0].set_value("title", value)

