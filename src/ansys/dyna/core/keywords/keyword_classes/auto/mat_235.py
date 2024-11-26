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

class Mat235(KeywordBase):
    """DYNA MAT_235 keyword"""

    keyword = "MAT"
    subkeyword = "235"
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
                        "v12",
                        float,
                        60,
                        10,
                        kwargs.get("v12")
                    ),
                    Field(
                        "v23",
                        float,
                        70,
                        10,
                        kwargs.get("v23")
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
                        "thi",
                        float,
                        10,
                        10,
                        kwargs.get("thi")
                    ),
                    Field(
                        "thl",
                        float,
                        20,
                        10,
                        kwargs.get("thl")
                    ),
                    Field(
                        "bfi",
                        float,
                        30,
                        10,
                        kwargs.get("bfi")
                    ),
                    Field(
                        "bwi",
                        float,
                        40,
                        10,
                        kwargs.get("bwi")
                    ),
                    Field(
                        "dscf",
                        float,
                        50,
                        10,
                        kwargs.get("dscf")
                    ),
                    Field(
                        "cnst",
                        float,
                        60,
                        10,
                        kwargs.get("cnst")
                    ),
                    Field(
                        "atlr",
                        float,
                        70,
                        10,
                        kwargs.get("atlr")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "vmb",
                        float,
                        0,
                        10,
                        kwargs.get("vmb")
                    ),
                    Field(
                        "vme",
                        float,
                        10,
                        10,
                        kwargs.get("vme")
                    ),
                    Field(
                        "trs",
                        float,
                        20,
                        10,
                        kwargs.get("trs")
                    ),
                    Field(
                        "fflg",
                        float,
                        30,
                        10,
                        kwargs.get("fflg")
                    ),
                    Field(
                        "aopt",
                        float,
                        40,
                        10,
                        kwargs.get("aopt")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "unused",
                        float,
                        0,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        float,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        float,
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
                ],
            ),
            OptionCardSet(
                option_spec = Mat235.option_specs[0],
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
        """Get or set the Material identification.  A unique number or label must be specified.
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
        """Get or set the Young's modulus of the yarn in axial-direction
        """ # nopep8
        return self._cards[0].get_value("e1")

    @e1.setter
    def e1(self, value: float) -> None:
        self._cards[0].set_value("e1", value)

    @property
    def e2(self) -> typing.Optional[float]:
        """Get or set the Young's modulus of the yarn in transverse-direction
        """ # nopep8
        return self._cards[0].get_value("e2")

    @e2.setter
    def e2(self, value: float) -> None:
        self._cards[0].set_value("e2", value)

    @property
    def g12(self) -> typing.Optional[float]:
        """Get or set the G12, shear modulus of the yarns.
        """ # nopep8
        return self._cards[0].get_value("g12")

    @g12.setter
    def g12(self, value: float) -> None:
        self._cards[0].set_value("g12", value)

    @property
    def g23(self) -> typing.Optional[float]:
        """Get or set the G23, transverse shear modulus of the yarns.
        """ # nopep8
        return self._cards[0].get_value("g23")

    @g23.setter
    def g23(self, value: float) -> None:
        self._cards[0].set_value("g23", value)

    @property
    def v12(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("v12")

    @v12.setter
    def v12(self, value: float) -> None:
        self._cards[0].set_value("v12", value)

    @property
    def v23(self) -> typing.Optional[float]:
        """Get or set the Transverse Poisson's ratio
        """ # nopep8
        return self._cards[0].get_value("v23")

    @v23.setter
    def v23(self, value: float) -> None:
        self._cards[0].set_value("v23", value)

    @property
    def xt(self) -> typing.Optional[float]:
        """Get or set the Ultimate strength at failure (not used)..
        """ # nopep8
        return self._cards[1].get_value("xt")

    @xt.setter
    def xt(self, value: float) -> None:
        self._cards[1].set_value("xt", value)

    @property
    def thi(self) -> typing.Optional[float]:
        """Get or set the Initial brade angle.
        """ # nopep8
        return self._cards[1].get_value("thi")

    @thi.setter
    def thi(self, value: float) -> None:
        self._cards[1].set_value("thi", value)

    @property
    def thl(self) -> typing.Optional[float]:
        """Get or set the Yarn locking angle.
        """ # nopep8
        return self._cards[1].get_value("thl")

    @thl.setter
    def thl(self, value: float) -> None:
        self._cards[1].set_value("thl", value)

    @property
    def bfi(self) -> typing.Optional[float]:
        """Get or set the Initial undulation angle in fill direction.
        """ # nopep8
        return self._cards[1].get_value("bfi")

    @bfi.setter
    def bfi(self, value: float) -> None:
        self._cards[1].set_value("bfi", value)

    @property
    def bwi(self) -> typing.Optional[float]:
        """Get or set the Initial undulation angle in warp direction.
        """ # nopep8
        return self._cards[1].get_value("bwi")

    @bwi.setter
    def bwi(self, value: float) -> None:
        self._cards[1].set_value("bwi", value)

    @property
    def dscf(self) -> typing.Optional[float]:
        """Get or set the Discount factor
        """ # nopep8
        return self._cards[1].get_value("dscf")

    @dscf.setter
    def dscf(self, value: float) -> None:
        self._cards[1].set_value("dscf", value)

    @property
    def cnst(self) -> typing.Optional[float]:
        """Get or set the Reorientation damping constant
        """ # nopep8
        return self._cards[1].get_value("cnst")

    @cnst.setter
    def cnst(self, value: float) -> None:
        self._cards[1].set_value("cnst", value)

    @property
    def atlr(self) -> typing.Optional[float]:
        """Get or set the Angle tolerance for locking
        """ # nopep8
        return self._cards[1].get_value("atlr")

    @atlr.setter
    def atlr(self, value: float) -> None:
        self._cards[1].set_value("atlr", value)

    @property
    def vmb(self) -> typing.Optional[float]:
        """Get or set the Viscous modulus for normal strain rate
        """ # nopep8
        return self._cards[2].get_value("vmb")

    @vmb.setter
    def vmb(self, value: float) -> None:
        self._cards[2].set_value("vmb", value)

    @property
    def vme(self) -> typing.Optional[float]:
        """Get or set the Viscous modulus for shear strain rate
        """ # nopep8
        return self._cards[2].get_value("vme")

    @vme.setter
    def vme(self, value: float) -> None:
        self._cards[2].set_value("vme", value)

    @property
    def trs(self) -> typing.Optional[float]:
        """Get or set the Transverse shear modulus of the fabric layer
        """ # nopep8
        return self._cards[2].get_value("trs")

    @trs.setter
    def trs(self, value: float) -> None:
        self._cards[2].set_value("trs", value)

    @property
    def fflg(self) -> typing.Optional[float]:
        """Get or set the F-flag
        """ # nopep8
        return self._cards[2].get_value("fflg")

    @fflg.setter
    def fflg(self, value: float) -> None:
        self._cards[2].set_value("fflg", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option:
        EQ.0.0: locally orthotropic with material axes determined by
        element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES.
        EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
        EQ.3.0: locally orthotropic material axes defined by the cross product of the vector v with the element normal.
        LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
        *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR). Available with the R3 release of Version 971 and later.
        """ # nopep8
        return self._cards[2].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[2].set_value("aopt", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Components of vector 𝐚 for AOPT = 2.0
        """ # nopep8
        return self._cards[3].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[3].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Components of vector 𝐚 for AOPT = 2.0
        """ # nopep8
        return self._cards[3].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[3].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Components of vector 𝐚 for AOPT = 2.0
        """ # nopep8
        return self._cards[3].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[3].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Components of vector 𝐯 for AOPT = 3.0
        """ # nopep8
        return self._cards[4].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[4].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Components of vector 𝐯 for AOPT = 3.0
        """ # nopep8
        return self._cards[4].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[4].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Components of vector 𝐯 for AOPT = 3.0.
        """ # nopep8
        return self._cards[4].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[4].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Components of vector 𝐝 for AOPT = 2.0
        """ # nopep8
        return self._cards[4].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[4].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Components of vector 𝐝 for AOPT = 2.0
        """ # nopep8
        return self._cards[4].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[4].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Components of vector 𝐝 for AOPT = 2.0
        """ # nopep8
        return self._cards[4].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[4].set_value("d3", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[5].cards[0].set_value("title", value)

