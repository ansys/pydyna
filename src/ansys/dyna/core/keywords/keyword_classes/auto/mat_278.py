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

class Mat278(KeywordBase):
    """DYNA MAT_278 keyword"""

    keyword = "MAT"
    subkeyword = "278"
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
                        "e1",
                        float,
                        20,
                        10,
                        kwargs.get("e1")
                    ),
                    Field(
                        "e2",
                        float,
                        30,
                        10,
                        kwargs.get("e2")
                    ),
                    Field(
                        "g12",
                        float,
                        40,
                        10,
                        kwargs.get("g12")
                    ),
                    Field(
                        "g23",
                        float,
                        50,
                        10,
                        kwargs.get("g23")
                    ),
                    Field(
                        "eu",
                        float,
                        60,
                        10,
                        kwargs.get("eu")
                    ),
                    Field(
                        "c",
                        float,
                        70,
                        10,
                        kwargs.get("c")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "eka",
                        float,
                        0,
                        10,
                        kwargs.get("eka")
                    ),
                    Field(
                        "eua",
                        float,
                        10,
                        10,
                        kwargs.get("eua")
                    ),
                    Field(
                        "vmb",
                        float,
                        20,
                        10,
                        kwargs.get("vmb")
                    ),
                    Field(
                        "ekb",
                        float,
                        30,
                        10,
                        kwargs.get("ekb")
                    ),
                    Field(
                        "thl",
                        float,
                        40,
                        10,
                        kwargs.get("thl")
                    ),
                    Field(
                        "ta",
                        float,
                        50,
                        10,
                        kwargs.get("ta")
                    ),
                    Field(
                        "thi1",
                        float,
                        60,
                        10,
                        kwargs.get("thi1")
                    ),
                    Field(
                        "thi2",
                        float,
                        70,
                        10,
                        kwargs.get("thi2")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "w",
                        float,
                        0,
                        10,
                        kwargs.get("w")
                    ),
                    Field(
                        "span",
                        float,
                        10,
                        10,
                        kwargs.get("span")
                    ),
                    Field(
                        "thick",
                        float,
                        20,
                        10,
                        kwargs.get("thick")
                    ),
                    Field(
                        "h",
                        float,
                        30,
                        10,
                        kwargs.get("h")
                    ),
                    Field(
                        "area",
                        float,
                        40,
                        10,
                        kwargs.get("area")
                    ),
                    Field(
                        "e3",
                        float,
                        50,
                        10,
                        kwargs.get("e3")
                    ),
                    Field(
                        "pr13",
                        float,
                        60,
                        10,
                        kwargs.get("pr13")
                    ),
                    Field(
                        "pr23",
                        float,
                        70,
                        10,
                        kwargs.get("pr23")
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
                        "a1",
                        float,
                        10,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "a2",
                        float,
                        20,
                        10,
                        kwargs.get("a2")
                    ),
                    Field(
                        "a3",
                        float,
                        30,
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
            Card(
                [
                    Field(
                        "vyarn",
                        float,
                        0,
                        10,
                        kwargs.get("vyarn")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "k1",
                        float,
                        0,
                        10,
                        kwargs.get("k1")
                    ),
                    Field(
                        "k2",
                        float,
                        10,
                        10,
                        kwargs.get("k2")
                    ),
                    Field(
                        "c1",
                        float,
                        20,
                        10,
                        kwargs.get("c1")
                    ),
                    Field(
                        "c2",
                        float,
                        30,
                        10,
                        kwargs.get("c2")
                    ),
                    Field(
                        "m",
                        float,
                        40,
                        10,
                        kwargs.get("m")
                    ),
                    Field(
                        "n",
                        float,
                        50,
                        10,
                        kwargs.get("n")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "chexp1",
                        float,
                        0,
                        10,
                        kwargs.get("chexp1")
                    ),
                    Field(
                        "chexp2",
                        float,
                        10,
                        10,
                        kwargs.get("chexp2")
                    ),
                    Field(
                        "chexp3",
                        float,
                        20,
                        10,
                        kwargs.get("chexp3")
                    ),
                    Field(
                        "lcchexp",
                        int,
                        30,
                        10,
                        kwargs.get("lcchexp")
                    ),
                    Field(
                        "lcthexp",
                        int,
                        40,
                        10,
                        kwargs.get("lcthexp")
                    ),
                    Field(
                        "r",
                        float,
                        50,
                        10,
                        kwargs.get("r")
                    ),
                    Field(
                        "tref",
                        float,
                        60,
                        10,
                        kwargs.get("tref")
                    ),
                    Field(
                        "docref",
                        float,
                        70,
                        10,
                        kwargs.get("docref")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "wlftref",
                        float,
                        0,
                        10,
                        kwargs.get("wlftref")
                    ),
                    Field(
                        "wlfa",
                        float,
                        10,
                        10,
                        kwargs.get("wlfa")
                    ),
                    Field(
                        "wlfb",
                        float,
                        20,
                        10,
                        kwargs.get("wlfb")
                    ),
                    Field(
                        "lcg0",
                        int,
                        30,
                        10,
                        kwargs.get("lcg0")
                    ),
                    Field(
                        "lck0",
                        int,
                        40,
                        10,
                        kwargs.get("lck0")
                    ),
                    Field(
                        "idoc",
                        float,
                        50,
                        10,
                        kwargs.get("idoc")
                    ),
                    Field(
                        "incr",
                        int,
                        60,
                        10,
                        kwargs.get("incr", 0)
                    ),
                    Field(
                        "qcure",
                        float,
                        70,
                        10,
                        kwargs.get("qcure")
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
                        kwargs.get("gi")
                    ),
                    Field(
                        "betagi",
                        float,
                        10,
                        10,
                        kwargs.get("betagi")
                    ),
                    Field(
                        "ki",
                        float,
                        20,
                        10,
                        kwargs.get("ki")
                    ),
                    Field(
                        "betaki",
                        float,
                        30,
                        10,
                        kwargs.get("betaki")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat278.option_specs[0],
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
    def e1(self) -> typing.Optional[float]:
        """Get or set the Young's modulus in the yarn's axial direction, E1.
        """ # nopep8
        return self._cards[0].get_value("e1")

    @e1.setter
    def e1(self, value: float) -> None:
        self._cards[0].set_value("e1", value)

    @property
    def e2(self) -> typing.Optional[float]:
        """Get or set the Young's modulus in the yarn's transverse direction, E2.
        """ # nopep8
        return self._cards[0].get_value("e2")

    @e2.setter
    def e2(self, value: float) -> None:
        self._cards[0].set_value("e2", value)

    @property
    def g12(self) -> typing.Optional[float]:
        """Get or set the Shear modulus of the yarns, G12.
        """ # nopep8
        return self._cards[0].get_value("g12")

    @g12.setter
    def g12(self, value: float) -> None:
        self._cards[0].set_value("g12", value)

    @property
    def g23(self) -> typing.Optional[float]:
        """Get or set the Transverse shear modulus.
        """ # nopep8
        return self._cards[0].get_value("g23")

    @g23.setter
    def g23(self, value: float) -> None:
        self._cards[0].set_value("g23", value)

    @property
    def eu(self) -> typing.Optional[float]:
        """Get or set the Ultimate strain at failure.
        """ # nopep8
        return self._cards[0].get_value("eu")

    @eu.setter
    def eu(self, value: float) -> None:
        self._cards[0].set_value("eu", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Coefficient of friction between the fibers.
        """ # nopep8
        return self._cards[0].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        self._cards[0].set_value("c", value)

    @property
    def eka(self) -> typing.Optional[float]:
        """Get or set the Elastic constant of element "a".
        """ # nopep8
        return self._cards[1].get_value("eka")

    @eka.setter
    def eka(self, value: float) -> None:
        self._cards[1].set_value("eka", value)

    @property
    def eua(self) -> typing.Optional[float]:
        """Get or set the Ultimate strain of element "a".
        """ # nopep8
        return self._cards[1].get_value("eua")

    @eua.setter
    def eua(self, value: float) -> None:
        self._cards[1].set_value("eua", value)

    @property
    def vmb(self) -> typing.Optional[float]:
        """Get or set the Damping coefficient of element "b".
        """ # nopep8
        return self._cards[1].get_value("vmb")

    @vmb.setter
    def vmb(self, value: float) -> None:
        self._cards[1].set_value("vmb", value)

    @property
    def ekb(self) -> typing.Optional[float]:
        """Get or set the Elastic constant of element "b"
        """ # nopep8
        return self._cards[1].get_value("ekb")

    @ekb.setter
    def ekb(self, value: float) -> None:
        self._cards[1].set_value("ekb", value)

    @property
    def thl(self) -> typing.Optional[float]:
        """Get or set the Yarn locking angle.
        """ # nopep8
        return self._cards[1].get_value("thl")

    @thl.setter
    def thl(self, value: float) -> None:
        self._cards[1].set_value("thl", value)

    @property
    def ta(self) -> typing.Optional[float]:
        """Get or set the Transition angle of locking.
        """ # nopep8
        return self._cards[1].get_value("ta")

    @ta.setter
    def ta(self, value: float) -> None:
        self._cards[1].set_value("ta", value)

    @property
    def thi1(self) -> typing.Optional[float]:
        """Get or set the Initial braid angle 1.
        """ # nopep8
        return self._cards[1].get_value("thi1")

    @thi1.setter
    def thi1(self, value: float) -> None:
        self._cards[1].set_value("thi1", value)

    @property
    def thi2(self) -> typing.Optional[float]:
        """Get or set the Initial braid angle 2.
        """ # nopep8
        return self._cards[1].get_value("thi2")

    @thi2.setter
    def thi2(self, value: float) -> None:
        self._cards[1].set_value("thi2", value)

    @property
    def w(self) -> typing.Optional[float]:
        """Get or set the Fiber width.
        """ # nopep8
        return self._cards[2].get_value("w")

    @w.setter
    def w(self, value: float) -> None:
        self._cards[2].set_value("w", value)

    @property
    def span(self) -> typing.Optional[float]:
        """Get or set the Span between the fibers.
        """ # nopep8
        return self._cards[2].get_value("span")

    @span.setter
    def span(self, value: float) -> None:
        self._cards[2].set_value("span", value)

    @property
    def thick(self) -> typing.Optional[float]:
        """Get or set the Real fiber thickness.
        """ # nopep8
        return self._cards[2].get_value("thick")

    @thick.setter
    def thick(self, value: float) -> None:
        self._cards[2].set_value("thick", value)

    @property
    def h(self) -> typing.Optional[float]:
        """Get or set the Effective fiber thickness
        """ # nopep8
        return self._cards[2].get_value("h")

    @h.setter
    def h(self, value: float) -> None:
        self._cards[2].set_value("h", value)

    @property
    def area(self) -> typing.Optional[float]:
        """Get or set the Fiber cross-sectional area.
        """ # nopep8
        return self._cards[2].get_value("area")

    @area.setter
    def area(self, value: float) -> None:
        self._cards[2].set_value("area", value)

    @property
    def e3(self) -> typing.Optional[float]:
        """Get or set the Young's modulus in "thickness" direction as defined by the 3rd axis of the material coordinate system (solids only).
        """ # nopep8
        return self._cards[2].get_value("e3")

    @e3.setter
    def e3(self, value: float) -> None:
        self._cards[2].set_value("e3", value)

    @property
    def pr13(self) -> typing.Optional[float]:
        """Get or set the Transverse Poisson's ratio v13(solids only).
        """ # nopep8
        return self._cards[2].get_value("pr13")

    @pr13.setter
    def pr13(self, value: float) -> None:
        self._cards[2].set_value("pr13", value)

    @property
    def pr23(self) -> typing.Optional[float]:
        """Get or set the Transverse Poisson's ratio v23 (solids only).
        """ # nopep8
        return self._cards[2].get_value("pr23")

    @pr23.setter
    def pr23(self, value: float) -> None:
        self._cards[2].set_value("pr23", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option (see MAT_OPTION TROPIC_ELASTIC for a
        more complete description):
        EQ.0.0: locally orthotropic with material axes determined by
        element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES.
        EQ.2.0: globally orthotropic with material axes determined by
        vectors defined below, as with *DEFINE_COORDINATE_VECTOR.
        EQ.3.0: locally orthotropic material axes for each integration
        point determined by rotating the material axes about
        the element normal by an angle, Bi (see
        *PART_COMPOSITE), from a line in the plane of the
        element defined by the cross product of the vector ..
        with the element normal.
        LT.0.0: the absolute value of AOPT is a coordinate system ID
        number (CID on *DEFINE_COORDINATE_NODES,
        *DEFINE_COORDINATE_SYSTEM or *DEFINE_COORDINATE_VECTOR).
        Available in R3 version of 971 and later.
        """ # nopep8
        return self._cards[3].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[3].set_value("aopt", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector a for AOPT = 2.0.
        """ # nopep8
        return self._cards[3].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[3].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector a for AOPT = 2.0.
        """ # nopep8
        return self._cards[3].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[3].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector a for AOPT = 2.0
        """ # nopep8
        return self._cards[3].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[3].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3.0.
        """ # nopep8
        return self._cards[4].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[4].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3.0.
        """ # nopep8
        return self._cards[4].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[4].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3.0.
        """ # nopep8
        return self._cards[4].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[4].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector d for AOPT = 2.0
        """ # nopep8
        return self._cards[4].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[4].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector d for AOPT = 2.0.
        """ # nopep8
        return self._cards[4].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[4].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector d for AOPT = 2.0.
        """ # nopep8
        return self._cards[4].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[4].set_value("d3", value)

    @property
    def vyarn(self) -> typing.Optional[float]:
        """Get or set the Volume fraction of yarn.
        """ # nopep8
        return self._cards[5].get_value("vyarn")

    @vyarn.setter
    def vyarn(self, value: float) -> None:
        self._cards[5].set_value("vyarn", value)

    @property
    def k1(self) -> typing.Optional[float]:
        """Get or set the Parameter k1 for Kamal model.
        """ # nopep8
        return self._cards[6].get_value("k1")

    @k1.setter
    def k1(self, value: float) -> None:
        self._cards[6].set_value("k1", value)

    @property
    def k2(self) -> typing.Optional[float]:
        """Get or set the Parameter k2 for Kamal model.
        """ # nopep8
        return self._cards[6].get_value("k2")

    @k2.setter
    def k2(self, value: float) -> None:
        self._cards[6].set_value("k2", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the Parameter c1 for Kamal model.
        """ # nopep8
        return self._cards[6].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        self._cards[6].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the Parameter c2 for Kamal model
        """ # nopep8
        return self._cards[6].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        self._cards[6].set_value("c2", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Exponent m for Kamal model
        """ # nopep8
        return self._cards[6].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        self._cards[6].set_value("m", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Exponent n for Kamal model.
        """ # nopep8
        return self._cards[6].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        self._cards[6].set_value("n", value)

    @property
    def chexp1(self) -> typing.Optional[float]:
        """Get or set the Quadratic parameter γ2 for chemical shrinkage.
        """ # nopep8
        return self._cards[7].get_value("chexp1")

    @chexp1.setter
    def chexp1(self, value: float) -> None:
        self._cards[7].set_value("chexp1", value)

    @property
    def chexp2(self) -> typing.Optional[float]:
        """Get or set the Quadratic parameter γ1 for chemical shrinkage.
        """ # nopep8
        return self._cards[7].get_value("chexp2")

    @chexp2.setter
    def chexp2(self, value: float) -> None:
        self._cards[7].set_value("chexp2", value)

    @property
    def chexp3(self) -> typing.Optional[float]:
        """Get or set the Quadratic parameter γ0 for chemical shrinkage.
        """ # nopep8
        return self._cards[7].get_value("chexp3")

    @chexp3.setter
    def chexp3(self, value: float) -> None:
        self._cards[7].set_value("chexp3", value)

    @property
    def lcchexp(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to define the coefficient for chemical shrinkage
        γ(α) as a function of the state of cure α. If set, parameters
        CHEXP1, CHEXP2, and CHEXP3 are ignored
        """ # nopep8
        return self._cards[7].get_value("lcchexp")

    @lcchexp.setter
    def lcchexp(self, value: int) -> None:
        self._cards[7].set_value("lcchexp", value)

    @property
    def lcthexp(self) -> typing.Optional[int]:
        """Get or set the Load curve ID or table ID defining the instantaneous coefficient
        of thermal expansion β(α, T) as a function of cure α and
        temperature T. If referring to a load curve, parameter β(T) is a
        function of temperature T.
        """ # nopep8
        return self._cards[7].get_value("lcthexp")

    @lcthexp.setter
    def lcthexp(self, value: int) -> None:
        self._cards[7].set_value("lcthexp", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Gas constant R for Kamal model.
        """ # nopep8
        return self._cards[7].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        self._cards[7].set_value("r", value)

    @property
    def tref(self) -> typing.Optional[float]:
        """Get or set the Reference temperature T0 for secant form of thermal expansion.
        """ # nopep8
        return self._cards[7].get_value("tref")

    @tref.setter
    def tref(self, value: float) -> None:
        self._cards[7].set_value("tref", value)

    @property
    def docref(self) -> typing.Optional[float]:
        """Get or set the Reference degree of cure α0 for sequential form of chemical expansion.
        """ # nopep8
        return self._cards[7].get_value("docref")

    @docref.setter
    def docref(self, value: float) -> None:
        self._cards[7].set_value("docref", value)

    @property
    def wlftref(self) -> typing.Optional[float]:
        """Get or set the Reference temperature for WLF shift function.
        """ # nopep8
        return self._cards[8].get_value("wlftref")

    @wlftref.setter
    def wlftref(self, value: float) -> None:
        self._cards[8].set_value("wlftref", value)

    @property
    def wlfa(self) -> typing.Optional[float]:
        """Get or set the Parameter A for WLF shift function.
        """ # nopep8
        return self._cards[8].get_value("wlfa")

    @wlfa.setter
    def wlfa(self, value: float) -> None:
        self._cards[8].set_value("wlfa", value)

    @property
    def wlfb(self) -> typing.Optional[float]:
        """Get or set the Parameter B for WLF shift function.
        """ # nopep8
        return self._cards[8].get_value("wlfb")

    @wlfb.setter
    def wlfb(self, value: float) -> None:
        self._cards[8].set_value("wlfb", value)

    @property
    def lcg0(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining the instantaneous shear modulus G0 as a function of state of cure
        """ # nopep8
        return self._cards[8].get_value("lcg0")

    @lcg0.setter
    def lcg0(self, value: int) -> None:
        self._cards[8].set_value("lcg0", value)

    @property
    def lck0(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining the instantaneous bulk modulus K0 as a	function of state of cure.
        """ # nopep8
        return self._cards[8].get_value("lck0")

    @lck0.setter
    def lck0(self, value: int) -> None:
        self._cards[8].set_value("lck0", value)

    @property
    def idoc(self) -> typing.Optional[float]:
        """Get or set the Initial degree of cure.
        """ # nopep8
        return self._cards[8].get_value("idoc")

    @idoc.setter
    def idoc(self, value: float) -> None:
        self._cards[8].set_value("idoc", value)

    @property
    def incr(self) -> int:
        """Get or set the Flag for stress formulation:
        EQ.0: total formulation (default)
        EQ.1: incremental formulation (recommended).
        """ # nopep8
        return self._cards[8].get_value("incr")

    @incr.setter
    def incr(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""incr must be one of {0,1}""")
        self._cards[8].set_value("incr", value)

    @property
    def qcure(self) -> typing.Optional[float]:
        """Get or set the Heat generation factor, relating the heat generated in one time step with the increment of the degree of cure in that step.
        """ # nopep8
        return self._cards[8].get_value("qcure")

    @qcure.setter
    def qcure(self, value: float) -> None:
        self._cards[8].set_value("qcure", value)

    @property
    def gi(self) -> typing.Optional[float]:
        """Get or set the Shear relaxation modulus for the ith term for fully cured material.
        """ # nopep8
        return self._cards[9].get_value("gi")

    @gi.setter
    def gi(self, value: float) -> None:
        self._cards[9].set_value("gi", value)

    @property
    def betagi(self) -> typing.Optional[float]:
        """Get or set the Shear decay constant for the ith term for fully cured material.
        """ # nopep8
        return self._cards[9].get_value("betagi")

    @betagi.setter
    def betagi(self, value: float) -> None:
        self._cards[9].set_value("betagi", value)

    @property
    def ki(self) -> typing.Optional[float]:
        """Get or set the Bulk relaxation modulus for the ith term for fully cured material.
        """ # nopep8
        return self._cards[9].get_value("ki")

    @ki.setter
    def ki(self, value: float) -> None:
        self._cards[9].set_value("ki", value)

    @property
    def betaki(self) -> typing.Optional[float]:
        """Get or set the Bulk decay constant for the ith term for fully cured material
        """ # nopep8
        return self._cards[9].get_value("betaki")

    @betaki.setter
    def betaki(self, value: float) -> None:
        self._cards[9].set_value("betaki", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[10].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[10].cards[0].set_value("title", value)

