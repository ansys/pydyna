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

class Mat211(KeywordBase):
    """DYNA MAT_211 keyword"""

    keyword = "MAT"
    subkeyword = "211"
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
                        "helas",
                        float,
                        40,
                        10,
                        kwargs.get("helas", 0.0)
                    ),
                    Field(
                        "telas",
                        float,
                        50,
                        10,
                        kwargs.get("telas", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcaxh",
                        int,
                        0,
                        10,
                        kwargs.get("lcaxh")
                    ),
                    Field(
                        "lcshh",
                        int,
                        10,
                        10,
                        kwargs.get("lcshh")
                    ),
                    Field(
                        "lcbmh",
                        int,
                        20,
                        10,
                        kwargs.get("lcbmh")
                    ),
                    Field(
                        "sfaxh",
                        float,
                        30,
                        10,
                        kwargs.get("sfaxh", 1.0)
                    ),
                    Field(
                        "sfshh",
                        float,
                        40,
                        10,
                        kwargs.get("sfshh", 1.0)
                    ),
                    Field(
                        "sfbmh",
                        float,
                        50,
                        10,
                        kwargs.get("sfbmh", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dfakh",
                        float,
                        0,
                        10,
                        kwargs.get("dfakh")
                    ),
                    Field(
                        "dfshh",
                        float,
                        10,
                        10,
                        kwargs.get("dfshh")
                    ),
                    Field(
                        "rfbmh",
                        float,
                        20,
                        10,
                        kwargs.get("rfbmh")
                    ),
                    Field(
                        "dmfaxh",
                        float,
                        30,
                        10,
                        kwargs.get("dmfaxh", 0.1)
                    ),
                    Field(
                        "dmfshh",
                        float,
                        40,
                        10,
                        kwargs.get("dmfshh", 0.1)
                    ),
                    Field(
                        "dmfbmh",
                        float,
                        50,
                        10,
                        kwargs.get("dmfbmh", 0.1)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcaxt",
                        int,
                        0,
                        10,
                        kwargs.get("lcaxt")
                    ),
                    Field(
                        "lcsht",
                        int,
                        10,
                        10,
                        kwargs.get("lcsht")
                    ),
                    Field(
                        "lcbmt",
                        int,
                        20,
                        10,
                        kwargs.get("lcbmt")
                    ),
                    Field(
                        "sfaxt",
                        float,
                        30,
                        10,
                        kwargs.get("sfaxt", 1.0)
                    ),
                    Field(
                        "sfsht",
                        float,
                        40,
                        10,
                        kwargs.get("sfsht", 1.0)
                    ),
                    Field(
                        "sbfmt",
                        float,
                        50,
                        10,
                        kwargs.get("sbfmt", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dfaxt",
                        float,
                        0,
                        10,
                        kwargs.get("dfaxt")
                    ),
                    Field(
                        "dfsht",
                        float,
                        10,
                        10,
                        kwargs.get("dfsht")
                    ),
                    Field(
                        "rfbmt",
                        float,
                        20,
                        10,
                        kwargs.get("rfbmt")
                    ),
                    Field(
                        "dfmaxt",
                        float,
                        30,
                        10,
                        kwargs.get("dfmaxt", 0.1)
                    ),
                    Field(
                        "dmfsht",
                        float,
                        40,
                        10,
                        kwargs.get("dmfsht", 0.1)
                    ),
                    Field(
                        "dmfbmt",
                        float,
                        50,
                        10,
                        kwargs.get("dmfbmt", 0.1)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat211.option_specs[0],
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
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus, used only for contact stiffness calculation.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio, used only for contact stiffness calculation
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def helas(self) -> float:
        """Get or set the SPR head end behaviour flag:
        EQ.0.0: Nonlinear.
        EQ.1.0: Elastic (Use first two points on load curves).
        """ # nopep8
        return self._cards[0].get_value("helas")

    @helas.setter
    def helas(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""helas must be one of {0.0,1.0}""")
        self._cards[0].set_value("helas", value)

    @property
    def telas(self) -> float:
        """Get or set the SPR tail end behaviour flag:
        EQ.0.0: Nonlinear.
        EQ.1.0: Elastic (Use first two points on load curves).
        """ # nopep8
        return self._cards[0].get_value("telas")

    @telas.setter
    def telas(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""telas must be one of {0.0,1.0}""")
        self._cards[0].set_value("telas", value)

    @property
    def lcaxh(self) -> typing.Optional[int]:
        """Get or set the Load curve ID, see *DEFINE_CURVE, giving axial force versus
        deformation (head)..
        """ # nopep8
        return self._cards[1].get_value("lcaxh")

    @lcaxh.setter
    def lcaxh(self, value: int) -> None:
        self._cards[1].set_value("lcaxh", value)

    @property
    def lcshh(self) -> typing.Optional[int]:
        """Get or set the Load curve ID, see *DEFINE_CURVE, giving shear force versus
        deformation (head).
        """ # nopep8
        return self._cards[1].get_value("lcshh")

    @lcshh.setter
    def lcshh(self, value: int) -> None:
        self._cards[1].set_value("lcshh", value)

    @property
    def lcbmh(self) -> typing.Optional[int]:
        """Get or set the Load curve ID, see *DEFINE_CURVE, giving moment versus
        rotation (head).
        """ # nopep8
        return self._cards[1].get_value("lcbmh")

    @lcbmh.setter
    def lcbmh(self, value: int) -> None:
        self._cards[1].set_value("lcbmh", value)

    @property
    def sfaxh(self) -> float:
        """Get or set the Scale factor on axial force from curve LCAXH.
        """ # nopep8
        return self._cards[1].get_value("sfaxh")

    @sfaxh.setter
    def sfaxh(self, value: float) -> None:
        self._cards[1].set_value("sfaxh", value)

    @property
    def sfshh(self) -> float:
        """Get or set the Scale factor on shear force from curve LCSHH.
        """ # nopep8
        return self._cards[1].get_value("sfshh")

    @sfshh.setter
    def sfshh(self, value: float) -> None:
        self._cards[1].set_value("sfshh", value)

    @property
    def sfbmh(self) -> float:
        """Get or set the Scale factor on bending moment from curve LCBMH.
        """ # nopep8
        return self._cards[1].get_value("sfbmh")

    @sfbmh.setter
    def sfbmh(self, value: float) -> None:
        self._cards[1].set_value("sfbmh", value)

    @property
    def dfakh(self) -> typing.Optional[float]:
        """Get or set the Optional displacement to start of softening in axial load (head).
        """ # nopep8
        return self._cards[2].get_value("dfakh")

    @dfakh.setter
    def dfakh(self, value: float) -> None:
        self._cards[2].set_value("dfakh", value)

    @property
    def dfshh(self) -> typing.Optional[float]:
        """Get or set the Optional displacement to start of softening in shear load (head).
        """ # nopep8
        return self._cards[2].get_value("dfshh")

    @dfshh.setter
    def dfshh(self, value: float) -> None:
        self._cards[2].set_value("dfshh", value)

    @property
    def rfbmh(self) -> typing.Optional[float]:
        """Get or set the Optional rotation (radians) to start of bending moment softening (head).
        """ # nopep8
        return self._cards[2].get_value("rfbmh")

    @rfbmh.setter
    def rfbmh(self, value: float) -> None:
        self._cards[2].set_value("rfbmh", value)

    @property
    def dmfaxh(self) -> float:
        """Get or set the Scale factor on DFAXH.
        """ # nopep8
        return self._cards[2].get_value("dmfaxh")

    @dmfaxh.setter
    def dmfaxh(self, value: float) -> None:
        self._cards[2].set_value("dmfaxh", value)

    @property
    def dmfshh(self) -> float:
        """Get or set the Scale factor on FFSHH.
        """ # nopep8
        return self._cards[2].get_value("dmfshh")

    @dmfshh.setter
    def dmfshh(self, value: float) -> None:
        self._cards[2].set_value("dmfshh", value)

    @property
    def dmfbmh(self) -> float:
        """Get or set the Scale factor on RFBMH.
        """ # nopep8
        return self._cards[2].get_value("dmfbmh")

    @dmfbmh.setter
    def dmfbmh(self, value: float) -> None:
        self._cards[2].set_value("dmfbmh", value)

    @property
    def lcaxt(self) -> typing.Optional[int]:
        """Get or set the Load curve ID, see *DEFINE_CURVE, giving axial force versus deformation (tail).
        """ # nopep8
        return self._cards[3].get_value("lcaxt")

    @lcaxt.setter
    def lcaxt(self, value: int) -> None:
        self._cards[3].set_value("lcaxt", value)

    @property
    def lcsht(self) -> typing.Optional[int]:
        """Get or set the Load curve ID, see *DEFINE_CURVE, giving shear force versus deformation (tail).
        """ # nopep8
        return self._cards[3].get_value("lcsht")

    @lcsht.setter
    def lcsht(self, value: int) -> None:
        self._cards[3].set_value("lcsht", value)

    @property
    def lcbmt(self) -> typing.Optional[int]:
        """Get or set the Load curve ID, see *DEFINE_CURVE, giving moment versus rotation (tail).
        """ # nopep8
        return self._cards[3].get_value("lcbmt")

    @lcbmt.setter
    def lcbmt(self, value: int) -> None:
        self._cards[3].set_value("lcbmt", value)

    @property
    def sfaxt(self) -> float:
        """Get or set the Scale factor on axial force from curve LCAXT
        """ # nopep8
        return self._cards[3].get_value("sfaxt")

    @sfaxt.setter
    def sfaxt(self, value: float) -> None:
        self._cards[3].set_value("sfaxt", value)

    @property
    def sfsht(self) -> float:
        """Get or set the Scale factor on shear force from curve LCSHT
        """ # nopep8
        return self._cards[3].get_value("sfsht")

    @sfsht.setter
    def sfsht(self, value: float) -> None:
        self._cards[3].set_value("sfsht", value)

    @property
    def sbfmt(self) -> float:
        """Get or set the Scale factor on bending moment from curve LCBMT.
        """ # nopep8
        return self._cards[3].get_value("sbfmt")

    @sbfmt.setter
    def sbfmt(self, value: float) -> None:
        self._cards[3].set_value("sbfmt", value)

    @property
    def dfaxt(self) -> typing.Optional[float]:
        """Get or set the Optional displacement to start of softening in axial load (tail).
        """ # nopep8
        return self._cards[4].get_value("dfaxt")

    @dfaxt.setter
    def dfaxt(self, value: float) -> None:
        self._cards[4].set_value("dfaxt", value)

    @property
    def dfsht(self) -> typing.Optional[float]:
        """Get or set the Optional displacement to start of softening in shear load (tail).
        """ # nopep8
        return self._cards[4].get_value("dfsht")

    @dfsht.setter
    def dfsht(self, value: float) -> None:
        self._cards[4].set_value("dfsht", value)

    @property
    def rfbmt(self) -> typing.Optional[float]:
        """Get or set the Optional rotation (radians) to start of bending moment softening (tail).
        """ # nopep8
        return self._cards[4].get_value("rfbmt")

    @rfbmt.setter
    def rfbmt(self, value: float) -> None:
        self._cards[4].set_value("rfbmt", value)

    @property
    def dfmaxt(self) -> float:
        """Get or set the Scale factor on DFAXT.
        """ # nopep8
        return self._cards[4].get_value("dfmaxt")

    @dfmaxt.setter
    def dfmaxt(self, value: float) -> None:
        self._cards[4].set_value("dfmaxt", value)

    @property
    def dmfsht(self) -> float:
        """Get or set the Scale factor on FFSHT.
        """ # nopep8
        return self._cards[4].get_value("dmfsht")

    @dmfsht.setter
    def dmfsht(self, value: float) -> None:
        self._cards[4].set_value("dmfsht", value)

    @property
    def dmfbmt(self) -> float:
        """Get or set the Scale factor on RFBMT.
        """ # nopep8
        return self._cards[4].get_value("dmfbmt")

    @dmfbmt.setter
    def dmfbmt(self, value: float) -> None:
        self._cards[4].set_value("dmfbmt", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[5].cards[0].set_value("title", value)

