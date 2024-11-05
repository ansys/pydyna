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

class Mat029(KeywordBase):
    """DYNA MAT_029 keyword"""

    keyword = "MAT"
    subkeyword = "029"
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
                        "df",
                        float,
                        40,
                        10,
                        kwargs.get("df")
                    ),
                    Field(
                        "iaflc",
                        int,
                        50,
                        10,
                        kwargs.get("iaflc", 0)
                    ),
                    Field(
                        "ytflag",
                        float,
                        60,
                        10,
                        kwargs.get("ytflag", 0.0)
                    ),
                    Field(
                        "asoft",
                        float,
                        70,
                        10,
                        kwargs.get("asoft")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "m1",
                        float,
                        0,
                        10,
                        kwargs.get("m1")
                    ),
                    Field(
                        "m2",
                        float,
                        10,
                        10,
                        kwargs.get("m2")
                    ),
                    Field(
                        "m3",
                        float,
                        20,
                        10,
                        kwargs.get("m3")
                    ),
                    Field(
                        "m4",
                        float,
                        30,
                        10,
                        kwargs.get("m4")
                    ),
                    Field(
                        "m5",
                        float,
                        40,
                        10,
                        kwargs.get("m5")
                    ),
                    Field(
                        "m6",
                        float,
                        50,
                        10,
                        kwargs.get("m6")
                    ),
                    Field(
                        "m7",
                        float,
                        60,
                        10,
                        kwargs.get("m7")
                    ),
                    Field(
                        "m8",
                        float,
                        70,
                        10,
                        kwargs.get("m8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lc1",
                        int,
                        0,
                        10,
                        kwargs.get("lc1")
                    ),
                    Field(
                        "lc2",
                        int,
                        10,
                        10,
                        kwargs.get("lc2", 0)
                    ),
                    Field(
                        "lc3",
                        int,
                        20,
                        10,
                        kwargs.get("lc3", 0)
                    ),
                    Field(
                        "lc4",
                        int,
                        30,
                        10,
                        kwargs.get("lc4", 0)
                    ),
                    Field(
                        "lc5",
                        int,
                        40,
                        10,
                        kwargs.get("lc5", 0)
                    ),
                    Field(
                        "lc6",
                        int,
                        50,
                        10,
                        kwargs.get("lc6", 0)
                    ),
                    Field(
                        "lc7",
                        int,
                        60,
                        10,
                        kwargs.get("lc7", 0)
                    ),
                    Field(
                        "lc8",
                        int,
                        70,
                        10,
                        kwargs.get("lc8", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lps1",
                        int,
                        0,
                        10,
                        kwargs.get("lps1", 0)
                    ),
                    Field(
                        "sfs1",
                        float,
                        10,
                        10,
                        kwargs.get("sfs1", 1.0)
                    ),
                    Field(
                        "lps2",
                        int,
                        20,
                        10,
                        kwargs.get("lps2", 0)
                    ),
                    Field(
                        "sfs2",
                        float,
                        30,
                        10,
                        kwargs.get("sfs2", 1.0)
                    ),
                    Field(
                        "yms1",
                        float,
                        40,
                        10,
                        kwargs.get("yms1", 1.0E+20)
                    ),
                    Field(
                        "yms2",
                        float,
                        50,
                        10,
                        kwargs.get("yms2")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lpt1",
                        int,
                        0,
                        10,
                        kwargs.get("lpt1", 0)
                    ),
                    Field(
                        "sft1",
                        float,
                        10,
                        10,
                        kwargs.get("sft1", 1.0)
                    ),
                    Field(
                        "lpt2",
                        int,
                        20,
                        10,
                        kwargs.get("lpt2", 0)
                    ),
                    Field(
                        "sft2",
                        float,
                        30,
                        10,
                        kwargs.get("sft2", 1.0)
                    ),
                    Field(
                        "ymt1",
                        float,
                        40,
                        10,
                        kwargs.get("ymt1", 1.0E+20)
                    ),
                    Field(
                        "ymt2",
                        float,
                        50,
                        10,
                        kwargs.get("ymt2")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lpr",
                        int,
                        0,
                        10,
                        kwargs.get("lpr", 0)
                    ),
                    Field(
                        "sfr",
                        float,
                        10,
                        10,
                        kwargs.get("sfr", 1.0)
                    ),
                    Field(
                        "ymr",
                        float,
                        20,
                        10,
                        kwargs.get("ymr", 1.0E+20)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat029.option_specs[0],
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
    def df(self) -> typing.Optional[float]:
        """Get or set the Damping factor. A proper control for the timestep must be maintained by the user.!
        """ # nopep8
        return self._cards[0].get_value("df")

    @df.setter
    def df(self, value: float) -> None:
        self._cards[0].set_value("df", value)

    @property
    def iaflc(self) -> int:
        """Get or set the Axial force load curve option:
        EQ.0: axial load curves are force as a function of strain.
        EQ.1: axial load curves are force as a function of change in length.
        """ # nopep8
        return self._cards[0].get_value("iaflc")

    @iaflc.setter
    def iaflc(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""iaflc must be one of {0,1}""")
        self._cards[0].set_value("iaflc", value)

    @property
    def ytflag(self) -> float:
        """Get or set the Flag to allow beam to yield in tension:
        EQ.0.0: beam does not yield in tension (default),
        EQ.1.0: beam can yield in tension.
        """ # nopep8
        return self._cards[0].get_value("ytflag")

    @ytflag.setter
    def ytflag(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""ytflag must be one of {0.0,1.0}""")
        self._cards[0].set_value("ytflag", value)

    @property
    def asoft(self) -> typing.Optional[float]:
        """Get or set the Axial elastic softening factor applied once hinge has formed. When a hinge has formed the stuffness is reduced by this factor.
        EQ.0: ASOFT is ignored.
        """ # nopep8
        return self._cards[0].get_value("asoft")

    @asoft.setter
    def asoft(self, value: float) -> None:
        self._cards[0].set_value("asoft", value)

    @property
    def m1(self) -> typing.Optional[float]:
        """Get or set the First end moment for force versus (strain/change in length) curve. Define at least one.
        """ # nopep8
        return self._cards[1].get_value("m1")

    @m1.setter
    def m1(self, value: float) -> None:
        self._cards[1].set_value("m1", value)

    @property
    def m2(self) -> typing.Optional[float]:
        """Get or set the Second end moment for force versus (strain/change in length) curve.
        """ # nopep8
        return self._cards[1].get_value("m2")

    @m2.setter
    def m2(self, value: float) -> None:
        self._cards[1].set_value("m2", value)

    @property
    def m3(self) -> typing.Optional[float]:
        """Get or set the Third end moment for force versus (strain/change in length) curve.
        """ # nopep8
        return self._cards[1].get_value("m3")

    @m3.setter
    def m3(self, value: float) -> None:
        self._cards[1].set_value("m3", value)

    @property
    def m4(self) -> typing.Optional[float]:
        """Get or set the Fourth end moment for force versus (strain/change in length) curve.
        """ # nopep8
        return self._cards[1].get_value("m4")

    @m4.setter
    def m4(self, value: float) -> None:
        self._cards[1].set_value("m4", value)

    @property
    def m5(self) -> typing.Optional[float]:
        """Get or set the Fifth end moment for force versus (strain/change in length) curve.
        """ # nopep8
        return self._cards[1].get_value("m5")

    @m5.setter
    def m5(self, value: float) -> None:
        self._cards[1].set_value("m5", value)

    @property
    def m6(self) -> typing.Optional[float]:
        """Get or set the Sixth end moment for force versus (strain/change in length) curve.
        """ # nopep8
        return self._cards[1].get_value("m6")

    @m6.setter
    def m6(self, value: float) -> None:
        self._cards[1].set_value("m6", value)

    @property
    def m7(self) -> typing.Optional[float]:
        """Get or set the Seventh end moment for force versus (strain/change in length) curve.
        """ # nopep8
        return self._cards[1].get_value("m7")

    @m7.setter
    def m7(self, value: float) -> None:
        self._cards[1].set_value("m7", value)

    @property
    def m8(self) -> typing.Optional[float]:
        """Get or set the Eight moment for force versus (strain/change in length) curve.
        """ # nopep8
        return self._cards[1].get_value("m8")

    @m8.setter
    def m8(self, value: float) -> None:
        self._cards[1].set_value("m8", value)

    @property
    def lc1(self) -> typing.Optional[int]:
        """Get or set the Load curve ID (see *DEFINE_CURVE) defining axial force versus strain/change in length for the first end moment.
        """ # nopep8
        return self._cards[2].get_value("lc1")

    @lc1.setter
    def lc1(self, value: int) -> None:
        self._cards[2].set_value("lc1", value)

    @property
    def lc2(self) -> int:
        """Get or set the Load curve ID (see *DEFINE_CURVE) defining axial force versus strain/change in length for the secomd end moment.
        """ # nopep8
        return self._cards[2].get_value("lc2")

    @lc2.setter
    def lc2(self, value: int) -> None:
        self._cards[2].set_value("lc2", value)

    @property
    def lc3(self) -> int:
        """Get or set the Load curve ID (see *DEFINE_CURVE) defining axial force versus strain/change in length for the third end moment.
        """ # nopep8
        return self._cards[2].get_value("lc3")

    @lc3.setter
    def lc3(self, value: int) -> None:
        self._cards[2].set_value("lc3", value)

    @property
    def lc4(self) -> int:
        """Get or set the Load curve ID (see *DEFINE_CURVE) defining axial force versus strain/change in length for the fourth end moment.
        """ # nopep8
        return self._cards[2].get_value("lc4")

    @lc4.setter
    def lc4(self, value: int) -> None:
        self._cards[2].set_value("lc4", value)

    @property
    def lc5(self) -> int:
        """Get or set the Load curve ID (see *DEFINE_CURVE) defining axial force versus strain/change in length for the fifth end moment.
        """ # nopep8
        return self._cards[2].get_value("lc5")

    @lc5.setter
    def lc5(self, value: int) -> None:
        self._cards[2].set_value("lc5", value)

    @property
    def lc6(self) -> int:
        """Get or set the Load curve ID (see *DEFINE_CURVE) defining axial force versus strain/change in length for the sixth end moment.
        """ # nopep8
        return self._cards[2].get_value("lc6")

    @lc6.setter
    def lc6(self, value: int) -> None:
        self._cards[2].set_value("lc6", value)

    @property
    def lc7(self) -> int:
        """Get or set the Load curve ID (see *DEFINE_CURVE) defining axial force versus strain/change in length for the seventh end moment.
        """ # nopep8
        return self._cards[2].get_value("lc7")

    @lc7.setter
    def lc7(self, value: int) -> None:
        self._cards[2].set_value("lc7", value)

    @property
    def lc8(self) -> int:
        """Get or set the Load curve ID (see *DEFINE_CURVE) defining axial force versus strain/change in length for the eight end moment.
        """ # nopep8
        return self._cards[2].get_value("lc8")

    @lc8.setter
    def lc8(self, value: int) -> None:
        self._cards[2].set_value("lc8", value)

    @property
    def lps1(self) -> int:
        """Get or set the Load curve ID for plastic moment versus rotation about s-axis at node 1. If zero, LPS1 is ingored.
        """ # nopep8
        return self._cards[3].get_value("lps1")

    @lps1.setter
    def lps1(self, value: int) -> None:
        self._cards[3].set_value("lps1", value)

    @property
    def sfs1(self) -> float:
        """Get or set the Scale factor for plastic moment versus rotation curve about s-axis at node 1 (default = 1.0).
        """ # nopep8
        return self._cards[3].get_value("sfs1")

    @sfs1.setter
    def sfs1(self, value: float) -> None:
        self._cards[3].set_value("sfs1", value)

    @property
    def lps2(self) -> int:
        """Get or set the Load curve ID for plastic moment versus rotation about s-axis at node 2  (default = LPS1).
        """ # nopep8
        return self._cards[3].get_value("lps2")

    @lps2.setter
    def lps2(self, value: int) -> None:
        self._cards[3].set_value("lps2", value)

    @property
    def sfs2(self) -> float:
        """Get or set the Scale factor for plastic moment versus rotation curve about s-axis at node 2 (default = SFS1).
        """ # nopep8
        return self._cards[3].get_value("sfs2")

    @sfs2.setter
    def sfs2(self, value: float) -> None:
        self._cards[3].set_value("sfs2", value)

    @property
    def yms1(self) -> float:
        """Get or set the Yield moment about s-axis at node 1 for interaction calculations (default = 1.0E+20).
        """ # nopep8
        return self._cards[3].get_value("yms1")

    @yms1.setter
    def yms1(self, value: float) -> None:
        self._cards[3].set_value("yms1", value)

    @property
    def yms2(self) -> typing.Optional[float]:
        """Get or set the Yield moment about s-axis at node 2 for interaction calculations (default = YMS1).
        """ # nopep8
        return self._cards[3].get_value("yms2")

    @yms2.setter
    def yms2(self, value: float) -> None:
        self._cards[3].set_value("yms2", value)

    @property
    def lpt1(self) -> int:
        """Get or set the Load curve ID for plastic moment versus rotation about t-axis at node 1. If zero, LPT1 is ignored.
        """ # nopep8
        return self._cards[4].get_value("lpt1")

    @lpt1.setter
    def lpt1(self, value: int) -> None:
        self._cards[4].set_value("lpt1", value)

    @property
    def sft1(self) -> float:
        """Get or set the Scale factor for plastic moment versus rotation curve about t-axis at node 1 (default = 1.0).
        """ # nopep8
        return self._cards[4].get_value("sft1")

    @sft1.setter
    def sft1(self, value: float) -> None:
        self._cards[4].set_value("sft1", value)

    @property
    def lpt2(self) -> int:
        """Get or set the Load curve ID for plastic moment versus rotation about t-axis at node 2 (default = LPT1).
        """ # nopep8
        return self._cards[4].get_value("lpt2")

    @lpt2.setter
    def lpt2(self, value: int) -> None:
        self._cards[4].set_value("lpt2", value)

    @property
    def sft2(self) -> float:
        """Get or set the Scale factor for plastic moment versus rotation curve about t-axis at node 2 (default = SFS1).
        """ # nopep8
        return self._cards[4].get_value("sft2")

    @sft2.setter
    def sft2(self, value: float) -> None:
        self._cards[4].set_value("sft2", value)

    @property
    def ymt1(self) -> float:
        """Get or set the Yield moment about t-axis at node 1 for interaction calculations (default = 1.0E+20).
        """ # nopep8
        return self._cards[4].get_value("ymt1")

    @ymt1.setter
    def ymt1(self, value: float) -> None:
        self._cards[4].set_value("ymt1", value)

    @property
    def ymt2(self) -> typing.Optional[float]:
        """Get or set the Yield moment about t-axis at node 2 for interaction calculations (default = YMT1).
        """ # nopep8
        return self._cards[4].get_value("ymt2")

    @ymt2.setter
    def ymt2(self, value: float) -> None:
        self._cards[4].set_value("ymt2", value)

    @property
    def lpr(self) -> int:
        """Get or set the Load curve ID for plastic torsional moment versus rotation. If zero, LPR is ignored.
        """ # nopep8
        return self._cards[5].get_value("lpr")

    @lpr.setter
    def lpr(self, value: int) -> None:
        self._cards[5].set_value("lpr", value)

    @property
    def sfr(self) -> float:
        """Get or set the Scale factor for plastic torsional moment versus rotation (default = 1.0).
        """ # nopep8
        return self._cards[5].get_value("sfr")

    @sfr.setter
    def sfr(self, value: float) -> None:
        self._cards[5].set_value("sfr", value)

    @property
    def ymr(self) -> float:
        """Get or set the Torsional yield moment for interaction calculations (default = 1.0E+20).
        """ # nopep8
        return self._cards[5].get_value("ymr")

    @ymr.setter
    def ymr(self, value: float) -> None:
        self._cards[5].set_value("ymr", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[6].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[6].cards[0].set_value("title", value)

