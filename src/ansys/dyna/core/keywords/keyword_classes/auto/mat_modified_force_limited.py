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

class MatModifiedForceLimited(KeywordBase):
    """DYNA MAT_MODIFIED_FORCE_LIMITED keyword"""

    keyword = "MAT"
    subkeyword = "MODIFIED_FORCE_LIMITED"
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
                        kwargs.get("yms2", 1.0E+20)
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
                        kwargs.get("ymt2", 1.0E+20)
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
            Card(
                [
                    Field(
                        "lys1",
                        int,
                        0,
                        10,
                        kwargs.get("lys1", 0)
                    ),
                    Field(
                        "sys1",
                        float,
                        10,
                        10,
                        kwargs.get("sys1", 1.0)
                    ),
                    Field(
                        "lys2",
                        int,
                        20,
                        10,
                        kwargs.get("lys2", 0)
                    ),
                    Field(
                        "sys2",
                        float,
                        30,
                        10,
                        kwargs.get("sys2", 1.0)
                    ),
                    Field(
                        "lyt1",
                        int,
                        40,
                        10,
                        kwargs.get("lyt1", 0)
                    ),
                    Field(
                        "syt1",
                        float,
                        50,
                        10,
                        kwargs.get("syt1", 1.0)
                    ),
                    Field(
                        "lyt2",
                        int,
                        60,
                        10,
                        kwargs.get("lyt2", 0)
                    ),
                    Field(
                        "syt2",
                        float,
                        70,
                        10,
                        kwargs.get("syt2", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lyr",
                        int,
                        0,
                        10,
                        kwargs.get("lyr", 0)
                    ),
                    Field(
                        "syr",
                        float,
                        10,
                        10,
                        kwargs.get("syr", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "hms1_1",
                        float,
                        0,
                        10,
                        kwargs.get("hms1_1")
                    ),
                    Field(
                        "hms1_2",
                        float,
                        10,
                        10,
                        kwargs.get("hms1_2")
                    ),
                    Field(
                        "hms1_3",
                        float,
                        20,
                        10,
                        kwargs.get("hms1_3")
                    ),
                    Field(
                        "hms1_4",
                        float,
                        30,
                        10,
                        kwargs.get("hms1_4")
                    ),
                    Field(
                        "hms1_5",
                        float,
                        40,
                        10,
                        kwargs.get("hms1_5")
                    ),
                    Field(
                        "hms1_6",
                        float,
                        50,
                        10,
                        kwargs.get("hms1_6")
                    ),
                    Field(
                        "hms1_7",
                        float,
                        60,
                        10,
                        kwargs.get("hms1_7")
                    ),
                    Field(
                        "hms1_8",
                        float,
                        70,
                        10,
                        kwargs.get("hms1_8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lpms1_1",
                        int,
                        0,
                        10,
                        kwargs.get("lpms1_1")
                    ),
                    Field(
                        "lpms1_2",
                        int,
                        10,
                        10,
                        kwargs.get("lpms1_2")
                    ),
                    Field(
                        "lpms1_3",
                        int,
                        20,
                        10,
                        kwargs.get("lpms1_3")
                    ),
                    Field(
                        "lpms1_4",
                        int,
                        30,
                        10,
                        kwargs.get("lpms1_4")
                    ),
                    Field(
                        "lpms1_5",
                        int,
                        40,
                        10,
                        kwargs.get("lpms1_5")
                    ),
                    Field(
                        "lpms1_6",
                        int,
                        50,
                        10,
                        kwargs.get("lpms1_6")
                    ),
                    Field(
                        "lpms1_7",
                        int,
                        60,
                        10,
                        kwargs.get("lpms1_7")
                    ),
                    Field(
                        "lpms1_8",
                        int,
                        70,
                        10,
                        kwargs.get("lpms1_8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "hms2_1",
                        float,
                        0,
                        10,
                        kwargs.get("hms2_1")
                    ),
                    Field(
                        "hms2_2",
                        float,
                        10,
                        10,
                        kwargs.get("hms2_2")
                    ),
                    Field(
                        "hms2_3",
                        float,
                        20,
                        10,
                        kwargs.get("hms2_3")
                    ),
                    Field(
                        "hms2_4",
                        float,
                        30,
                        10,
                        kwargs.get("hms2_4")
                    ),
                    Field(
                        "hms2_5",
                        float,
                        40,
                        10,
                        kwargs.get("hms2_5")
                    ),
                    Field(
                        "hms2_6",
                        float,
                        50,
                        10,
                        kwargs.get("hms2_6")
                    ),
                    Field(
                        "hms2_7",
                        float,
                        60,
                        10,
                        kwargs.get("hms2_7")
                    ),
                    Field(
                        "hms2_8",
                        float,
                        70,
                        10,
                        kwargs.get("hms2_8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lpms2_1",
                        int,
                        0,
                        10,
                        kwargs.get("lpms2_1")
                    ),
                    Field(
                        "lpms2_2",
                        int,
                        10,
                        10,
                        kwargs.get("lpms2_2")
                    ),
                    Field(
                        "lpms2_3",
                        int,
                        20,
                        10,
                        kwargs.get("lpms2_3")
                    ),
                    Field(
                        "lpms2_4",
                        int,
                        30,
                        10,
                        kwargs.get("lpms2_4")
                    ),
                    Field(
                        "lpms2_5",
                        int,
                        40,
                        10,
                        kwargs.get("lpms2_5")
                    ),
                    Field(
                        "lpms2_6",
                        int,
                        50,
                        10,
                        kwargs.get("lpms2_6")
                    ),
                    Field(
                        "lpms2_7",
                        int,
                        60,
                        10,
                        kwargs.get("lpms2_7")
                    ),
                    Field(
                        "lpms2_8",
                        int,
                        70,
                        10,
                        kwargs.get("lpms2_8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "hmt1_1",
                        float,
                        0,
                        10,
                        kwargs.get("hmt1_1")
                    ),
                    Field(
                        "hmt1_2",
                        float,
                        10,
                        10,
                        kwargs.get("hmt1_2")
                    ),
                    Field(
                        "hmt1_3",
                        float,
                        20,
                        10,
                        kwargs.get("hmt1_3")
                    ),
                    Field(
                        "hmt1_4",
                        float,
                        30,
                        10,
                        kwargs.get("hmt1_4")
                    ),
                    Field(
                        "hmt1_5",
                        float,
                        40,
                        10,
                        kwargs.get("hmt1_5")
                    ),
                    Field(
                        "hmt1_6",
                        float,
                        50,
                        10,
                        kwargs.get("hmt1_6")
                    ),
                    Field(
                        "hmt1_7",
                        float,
                        60,
                        10,
                        kwargs.get("hmt1_7")
                    ),
                    Field(
                        "hmt1_8",
                        float,
                        70,
                        10,
                        kwargs.get("hmt1_8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lpmt1_1",
                        int,
                        0,
                        10,
                        kwargs.get("lpmt1_1")
                    ),
                    Field(
                        "lpmt1_2",
                        int,
                        10,
                        10,
                        kwargs.get("lpmt1_2")
                    ),
                    Field(
                        "lpmt1_3",
                        int,
                        20,
                        10,
                        kwargs.get("lpmt1_3")
                    ),
                    Field(
                        "lpmt1_4",
                        int,
                        30,
                        10,
                        kwargs.get("lpmt1_4")
                    ),
                    Field(
                        "lpmt1_5",
                        int,
                        40,
                        10,
                        kwargs.get("lpmt1_5")
                    ),
                    Field(
                        "lpmt1_6",
                        int,
                        50,
                        10,
                        kwargs.get("lpmt1_6")
                    ),
                    Field(
                        "lpmt1_7",
                        int,
                        60,
                        10,
                        kwargs.get("lpmt1_7")
                    ),
                    Field(
                        "lpmt1_8",
                        int,
                        70,
                        10,
                        kwargs.get("lpmt1_8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "hmt2_1",
                        float,
                        0,
                        10,
                        kwargs.get("hmt2_1")
                    ),
                    Field(
                        "hmt2_2",
                        float,
                        10,
                        10,
                        kwargs.get("hmt2_2")
                    ),
                    Field(
                        "hmt2_3",
                        float,
                        20,
                        10,
                        kwargs.get("hmt2_3")
                    ),
                    Field(
                        "hmt2_4",
                        float,
                        30,
                        10,
                        kwargs.get("hmt2_4")
                    ),
                    Field(
                        "hmt2_5",
                        float,
                        40,
                        10,
                        kwargs.get("hmt2_5")
                    ),
                    Field(
                        "hmt2_6",
                        float,
                        50,
                        10,
                        kwargs.get("hmt2_6")
                    ),
                    Field(
                        "hmt2_7",
                        float,
                        60,
                        10,
                        kwargs.get("hmt2_7")
                    ),
                    Field(
                        "hmt2_8",
                        float,
                        70,
                        10,
                        kwargs.get("hmt2_8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lpmt2_1",
                        int,
                        0,
                        10,
                        kwargs.get("lpmt2_1")
                    ),
                    Field(
                        "lpmt2_2",
                        int,
                        10,
                        10,
                        kwargs.get("lpmt2_2")
                    ),
                    Field(
                        "lpmt2_3",
                        int,
                        20,
                        10,
                        kwargs.get("lpmt2_3")
                    ),
                    Field(
                        "lpmt2_4",
                        int,
                        30,
                        10,
                        kwargs.get("lpmt2_4")
                    ),
                    Field(
                        "lpmt2_5",
                        int,
                        40,
                        10,
                        kwargs.get("lpmt2_5")
                    ),
                    Field(
                        "lpmt2_6",
                        int,
                        50,
                        10,
                        kwargs.get("lpmt2_6")
                    ),
                    Field(
                        "lpmt2_7",
                        int,
                        60,
                        10,
                        kwargs.get("lpmt2_7")
                    ),
                    Field(
                        "lpmt2_8",
                        int,
                        70,
                        10,
                        kwargs.get("lpmt2_8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "hmr_1",
                        float,
                        0,
                        10,
                        kwargs.get("hmr_1")
                    ),
                    Field(
                        "hmr_2",
                        float,
                        10,
                        10,
                        kwargs.get("hmr_2")
                    ),
                    Field(
                        "hmr_3",
                        float,
                        20,
                        10,
                        kwargs.get("hmr_3")
                    ),
                    Field(
                        "hmr_4",
                        float,
                        30,
                        10,
                        kwargs.get("hmr_4")
                    ),
                    Field(
                        "hmr_5",
                        float,
                        40,
                        10,
                        kwargs.get("hmr_5")
                    ),
                    Field(
                        "hmr_6",
                        float,
                        50,
                        10,
                        kwargs.get("hmr_6")
                    ),
                    Field(
                        "hmr_7",
                        float,
                        60,
                        10,
                        kwargs.get("hmr_7")
                    ),
                    Field(
                        "hmr_8",
                        float,
                        70,
                        10,
                        kwargs.get("hmr_8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lpmr_1",
                        int,
                        0,
                        10,
                        kwargs.get("lpmr_1", 0)
                    ),
                    Field(
                        "lpmr_2",
                        int,
                        10,
                        10,
                        kwargs.get("lpmr_2", 0)
                    ),
                    Field(
                        "lpmr_3",
                        int,
                        20,
                        10,
                        kwargs.get("lpmr_3", 0)
                    ),
                    Field(
                        "lpmr_4",
                        int,
                        30,
                        10,
                        kwargs.get("lpmr_4", 0)
                    ),
                    Field(
                        "lpmr_5",
                        int,
                        40,
                        10,
                        kwargs.get("lpmr_5", 0)
                    ),
                    Field(
                        "lpmr_6",
                        int,
                        50,
                        10,
                        kwargs.get("lpmr_6", 0)
                    ),
                    Field(
                        "lpmr_7",
                        int,
                        60,
                        10,
                        kwargs.get("lpmr_7", 0)
                    ),
                    Field(
                        "lpmr_8",
                        int,
                        70,
                        10,
                        kwargs.get("lpmr_8", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatModifiedForceLimited.option_specs[0],
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
        """Get or set the Damping factor, see definition in notes below. A proper control for the timestep has to be maintained by the user!
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
        EQ.0.0: beam does not yield in tension,
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
        """Get or set the Axial elastic softening factor applied once hinge has formed. When a hinge has formed the stiffness is reduced by this factor. If zero, this factor is ignored.
        """ # nopep8
        return self._cards[0].get_value("asoft")

    @asoft.setter
    def asoft(self, value: float) -> None:
        self._cards[0].set_value("asoft", value)

    @property
    def m1(self) -> typing.Optional[float]:
        """Get or set the Applied end moment for force versus (strain/change in length) curve. At least one must be defined. A maximum of 8 moments can be defined. The values should be in ascending order.
        """ # nopep8
        return self._cards[1].get_value("m1")

    @m1.setter
    def m1(self, value: float) -> None:
        self._cards[1].set_value("m1", value)

    @property
    def m2(self) -> typing.Optional[float]:
        """Get or set the Applied end moment for force versus (strain/change in length) curve. The values should be in ascending order.
        """ # nopep8
        return self._cards[1].get_value("m2")

    @m2.setter
    def m2(self, value: float) -> None:
        self._cards[1].set_value("m2", value)

    @property
    def m3(self) -> typing.Optional[float]:
        """Get or set the Applied end moment for force versus (strain/change in length) curve. The values should be in ascending order.
        """ # nopep8
        return self._cards[1].get_value("m3")

    @m3.setter
    def m3(self, value: float) -> None:
        self._cards[1].set_value("m3", value)

    @property
    def m4(self) -> typing.Optional[float]:
        """Get or set the Applied end moment for force versus (strain/change in length) curve. The values should be in ascending order.
        """ # nopep8
        return self._cards[1].get_value("m4")

    @m4.setter
    def m4(self, value: float) -> None:
        self._cards[1].set_value("m4", value)

    @property
    def m5(self) -> typing.Optional[float]:
        """Get or set the Applied end moment for force versus (strain/change in length) curve. The values should be in ascending order.
        """ # nopep8
        return self._cards[1].get_value("m5")

    @m5.setter
    def m5(self, value: float) -> None:
        self._cards[1].set_value("m5", value)

    @property
    def m6(self) -> typing.Optional[float]:
        """Get or set the Applied end moment for force versus (strain/change in length) curve. The values should be in ascending order.
        """ # nopep8
        return self._cards[1].get_value("m6")

    @m6.setter
    def m6(self, value: float) -> None:
        self._cards[1].set_value("m6", value)

    @property
    def m7(self) -> typing.Optional[float]:
        """Get or set the Applied end moment for force versus (strain/change in length) curve. The values should be in ascending order.
        """ # nopep8
        return self._cards[1].get_value("m7")

    @m7.setter
    def m7(self, value: float) -> None:
        self._cards[1].set_value("m7", value)

    @property
    def m8(self) -> typing.Optional[float]:
        """Get or set the Applied end moment for force versus (strain/change in length) curve. The values should be in ascending order.
        """ # nopep8
        return self._cards[1].get_value("m8")

    @m8.setter
    def m8(self, value: float) -> None:
        self._cards[1].set_value("m8", value)

    @property
    def lc1(self) -> typing.Optional[int]:
        """Get or set the Load curve ID (see *DEFINE_CURVE) defining axial force versus strain/change in length (see AOPT) for the corresponding applied end moment. Define the same number as end moments. Each curve must contain the same number of points.
        """ # nopep8
        return self._cards[2].get_value("lc1")

    @lc1.setter
    def lc1(self, value: int) -> None:
        self._cards[2].set_value("lc1", value)

    @property
    def lc2(self) -> int:
        """Get or set the Load curve ID defining axial force versus strain/change in length (see AOPT) for the corresponding applied end moment.
        """ # nopep8
        return self._cards[2].get_value("lc2")

    @lc2.setter
    def lc2(self, value: int) -> None:
        self._cards[2].set_value("lc2", value)

    @property
    def lc3(self) -> int:
        """Get or set the Load curve ID defining axial force versus strain/change in length (see AOPT) for the corresponding applied end moment.
        """ # nopep8
        return self._cards[2].get_value("lc3")

    @lc3.setter
    def lc3(self, value: int) -> None:
        self._cards[2].set_value("lc3", value)

    @property
    def lc4(self) -> int:
        """Get or set the Load curve ID defining axial force versus strain/change in length (see AOPT) for the corresponding applied end moment.
        """ # nopep8
        return self._cards[2].get_value("lc4")

    @lc4.setter
    def lc4(self, value: int) -> None:
        self._cards[2].set_value("lc4", value)

    @property
    def lc5(self) -> int:
        """Get or set the Load curve ID defining axial force versus strain/change in length (see AOPT) for the corresponding applied end moment.
        """ # nopep8
        return self._cards[2].get_value("lc5")

    @lc5.setter
    def lc5(self, value: int) -> None:
        self._cards[2].set_value("lc5", value)

    @property
    def lc6(self) -> int:
        """Get or set the Load curve ID defining axial force versus strain/change in length (see AOPT) for the corresponding applied end moment.
        """ # nopep8
        return self._cards[2].get_value("lc6")

    @lc6.setter
    def lc6(self, value: int) -> None:
        self._cards[2].set_value("lc6", value)

    @property
    def lc7(self) -> int:
        """Get or set the Load curve ID defining axial force versus strain/change in length (see AOPT) for the corresponding applied end moment.
        """ # nopep8
        return self._cards[2].get_value("lc7")

    @lc7.setter
    def lc7(self, value: int) -> None:
        self._cards[2].set_value("lc7", value)

    @property
    def lc8(self) -> int:
        """Get or set the Load curve ID defining axial force versus strain/change in length (see AOPT) for the corresponding applied end moment.
        """ # nopep8
        return self._cards[2].get_value("lc8")

    @lc8.setter
    def lc8(self, value: int) -> None:
        self._cards[2].set_value("lc8", value)

    @property
    def lps1(self) -> int:
        """Get or set the Load curve ID for plastic moment versus rotation about s-axis at node 1. If zero, this load curve is ignored.
        """ # nopep8
        return self._cards[3].get_value("lps1")

    @lps1.setter
    def lps1(self, value: int) -> None:
        self._cards[3].set_value("lps1", value)

    @property
    def sfs1(self) -> float:
        """Get or set the Scale factor for plastic moment versus rotation curve about s-axis at node 1. Default = 1.0.
        """ # nopep8
        return self._cards[3].get_value("sfs1")

    @sfs1.setter
    def sfs1(self, value: float) -> None:
        self._cards[3].set_value("sfs1", value)

    @property
    def lps2(self) -> int:
        """Get or set the Load curve ID for plastic moment versus rotation about s-axis at node 2. If zero, this load curve is ignored.
        """ # nopep8
        return self._cards[3].get_value("lps2")

    @lps2.setter
    def lps2(self, value: int) -> None:
        self._cards[3].set_value("lps2", value)

    @property
    def sfs2(self) -> float:
        """Get or set the Scale factor for plastic moment versus rotation curve about s-axis at node 2. Default = 1.0.
        """ # nopep8
        return self._cards[3].get_value("sfs2")

    @sfs2.setter
    def sfs2(self, value: float) -> None:
        self._cards[3].set_value("sfs2", value)

    @property
    def yms1(self) -> float:
        """Get or set the Yield moment about s-axis at node 1 for interaction calculations (default set to 1.0E+20 to prevent interaction).
        """ # nopep8
        return self._cards[3].get_value("yms1")

    @yms1.setter
    def yms1(self, value: float) -> None:
        self._cards[3].set_value("yms1", value)

    @property
    def yms2(self) -> float:
        """Get or set the Yield moment about s-axis at node 2 for interaction calculations (default set to YMS1).
        """ # nopep8
        return self._cards[3].get_value("yms2")

    @yms2.setter
    def yms2(self, value: float) -> None:
        self._cards[3].set_value("yms2", value)

    @property
    def lpt1(self) -> int:
        """Get or set the Load curve ID for plastic moment versus rotation about t-axis at node 1. If zero, this load curve is ignored.
        """ # nopep8
        return self._cards[4].get_value("lpt1")

    @lpt1.setter
    def lpt1(self, value: int) -> None:
        self._cards[4].set_value("lpt1", value)

    @property
    def sft1(self) -> float:
        """Get or set the Scale factor for plastic moment versus rotation curve about t-axis at node 1. Default = 1.0.
        """ # nopep8
        return self._cards[4].get_value("sft1")

    @sft1.setter
    def sft1(self, value: float) -> None:
        self._cards[4].set_value("sft1", value)

    @property
    def lpt2(self) -> int:
        """Get or set the Load curve ID for plastic moment versus rotation about t-axis at node 2. If zero, this load curve is ignored.
        """ # nopep8
        return self._cards[4].get_value("lpt2")

    @lpt2.setter
    def lpt2(self, value: int) -> None:
        self._cards[4].set_value("lpt2", value)

    @property
    def sft2(self) -> float:
        """Get or set the Scale factor for plastic moment versus rotation curve about t-axis at node 2. Default = 1.0.
        """ # nopep8
        return self._cards[4].get_value("sft2")

    @sft2.setter
    def sft2(self, value: float) -> None:
        self._cards[4].set_value("sft2", value)

    @property
    def ymt1(self) -> float:
        """Get or set the Yield moment about t-axis at node 1 for interaction calculations (default set to 1.0E+20 to prevent interactions)
        """ # nopep8
        return self._cards[4].get_value("ymt1")

    @ymt1.setter
    def ymt1(self, value: float) -> None:
        self._cards[4].set_value("ymt1", value)

    @property
    def ymt2(self) -> float:
        """Get or set the Yield moment about t-axis at node 2 for interaction calculations (default set to YMT1)
        """ # nopep8
        return self._cards[4].get_value("ymt2")

    @ymt2.setter
    def ymt2(self, value: float) -> None:
        self._cards[4].set_value("ymt2", value)

    @property
    def lpr(self) -> int:
        """Get or set the Load curve ID for plastic torsional moment versus rotation. If zero, this load curve is ignored.
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
        """Get or set the Torsional yield moment for interaction calculations (default set to 1.0E+20 to prevent interaction)
        """ # nopep8
        return self._cards[5].get_value("ymr")

    @ymr.setter
    def ymr(self, value: float) -> None:
        self._cards[5].set_value("ymr", value)

    @property
    def lys1(self) -> int:
        """Get or set the ID of curve defining yield moment as a function of axial force for the s-axis at node 1.
        """ # nopep8
        return self._cards[6].get_value("lys1")

    @lys1.setter
    def lys1(self, value: int) -> None:
        self._cards[6].set_value("lys1", value)

    @property
    def sys1(self) -> float:
        """Get or set the Scale factor applied to load curve LYS1.
        """ # nopep8
        return self._cards[6].get_value("sys1")

    @sys1.setter
    def sys1(self, value: float) -> None:
        self._cards[6].set_value("sys1", value)

    @property
    def lys2(self) -> int:
        """Get or set the ID of curve defining yield moment as a function of axial force for the s-axis at node 2.
        """ # nopep8
        return self._cards[6].get_value("lys2")

    @lys2.setter
    def lys2(self, value: int) -> None:
        self._cards[6].set_value("lys2", value)

    @property
    def sys2(self) -> float:
        """Get or set the Scale factor applied to load curve LYS2.
        """ # nopep8
        return self._cards[6].get_value("sys2")

    @sys2.setter
    def sys2(self, value: float) -> None:
        self._cards[6].set_value("sys2", value)

    @property
    def lyt1(self) -> int:
        """Get or set the ID of curve defining yield moment as a function of axial force for the t-axis at node 1.
        """ # nopep8
        return self._cards[6].get_value("lyt1")

    @lyt1.setter
    def lyt1(self, value: int) -> None:
        self._cards[6].set_value("lyt1", value)

    @property
    def syt1(self) -> float:
        """Get or set the Scale factor applied to load curve LYT1.
        """ # nopep8
        return self._cards[6].get_value("syt1")

    @syt1.setter
    def syt1(self, value: float) -> None:
        self._cards[6].set_value("syt1", value)

    @property
    def lyt2(self) -> int:
        """Get or set the ID of curve defining yield moment as a function of axial force for the t-axis at node 2.
        """ # nopep8
        return self._cards[6].get_value("lyt2")

    @lyt2.setter
    def lyt2(self, value: int) -> None:
        self._cards[6].set_value("lyt2", value)

    @property
    def syt2(self) -> float:
        """Get or set the Scale factor applied to load curve LYT2.
        """ # nopep8
        return self._cards[6].get_value("syt2")

    @syt2.setter
    def syt2(self, value: float) -> None:
        self._cards[6].set_value("syt2", value)

    @property
    def lyr(self) -> int:
        """Get or set the ID of curve defining yield moment as a function of axial force for the torsional axis.
        """ # nopep8
        return self._cards[7].get_value("lyr")

    @lyr.setter
    def lyr(self, value: int) -> None:
        self._cards[7].set_value("lyr", value)

    @property
    def syr(self) -> float:
        """Get or set the Scale factor applied to load curve LYR.
        """ # nopep8
        return self._cards[7].get_value("syr")

    @syr.setter
    def syr(self, value: float) -> None:
        self._cards[7].set_value("syr", value)

    @property
    def hms1_1(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 1.
        """ # nopep8
        return self._cards[8].get_value("hms1_1")

    @hms1_1.setter
    def hms1_1(self, value: float) -> None:
        self._cards[8].set_value("hms1_1", value)

    @property
    def hms1_2(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 1.
        """ # nopep8
        return self._cards[8].get_value("hms1_2")

    @hms1_2.setter
    def hms1_2(self, value: float) -> None:
        self._cards[8].set_value("hms1_2", value)

    @property
    def hms1_3(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 1.
        """ # nopep8
        return self._cards[8].get_value("hms1_3")

    @hms1_3.setter
    def hms1_3(self, value: float) -> None:
        self._cards[8].set_value("hms1_3", value)

    @property
    def hms1_4(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 1.
        """ # nopep8
        return self._cards[8].get_value("hms1_4")

    @hms1_4.setter
    def hms1_4(self, value: float) -> None:
        self._cards[8].set_value("hms1_4", value)

    @property
    def hms1_5(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 1.
        """ # nopep8
        return self._cards[8].get_value("hms1_5")

    @hms1_5.setter
    def hms1_5(self, value: float) -> None:
        self._cards[8].set_value("hms1_5", value)

    @property
    def hms1_6(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 1.
        """ # nopep8
        return self._cards[8].get_value("hms1_6")

    @hms1_6.setter
    def hms1_6(self, value: float) -> None:
        self._cards[8].set_value("hms1_6", value)

    @property
    def hms1_7(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 1.
        """ # nopep8
        return self._cards[8].get_value("hms1_7")

    @hms1_7.setter
    def hms1_7(self, value: float) -> None:
        self._cards[8].set_value("hms1_7", value)

    @property
    def hms1_8(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 1.
        """ # nopep8
        return self._cards[8].get_value("hms1_8")

    @hms1_8.setter
    def hms1_8(self, value: float) -> None:
        self._cards[8].set_value("hms1_8", value)

    @property
    def lpms1_1(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 1 for hinge moment HMS1_1
        """ # nopep8
        return self._cards[9].get_value("lpms1_1")

    @lpms1_1.setter
    def lpms1_1(self, value: int) -> None:
        self._cards[9].set_value("lpms1_1", value)

    @property
    def lpms1_2(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 1 for hinge moment HMS1_2
        """ # nopep8
        return self._cards[9].get_value("lpms1_2")

    @lpms1_2.setter
    def lpms1_2(self, value: int) -> None:
        self._cards[9].set_value("lpms1_2", value)

    @property
    def lpms1_3(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 1 for hinge moment HMS1_3
        """ # nopep8
        return self._cards[9].get_value("lpms1_3")

    @lpms1_3.setter
    def lpms1_3(self, value: int) -> None:
        self._cards[9].set_value("lpms1_3", value)

    @property
    def lpms1_4(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 1 for hinge moment HMS1_4
        """ # nopep8
        return self._cards[9].get_value("lpms1_4")

    @lpms1_4.setter
    def lpms1_4(self, value: int) -> None:
        self._cards[9].set_value("lpms1_4", value)

    @property
    def lpms1_5(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 1 for hinge moment HMS1_5
        """ # nopep8
        return self._cards[9].get_value("lpms1_5")

    @lpms1_5.setter
    def lpms1_5(self, value: int) -> None:
        self._cards[9].set_value("lpms1_5", value)

    @property
    def lpms1_6(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 1 for hinge moment HMS1_6
        """ # nopep8
        return self._cards[9].get_value("lpms1_6")

    @lpms1_6.setter
    def lpms1_6(self, value: int) -> None:
        self._cards[9].set_value("lpms1_6", value)

    @property
    def lpms1_7(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 1 for hinge moment HMS1_7
        """ # nopep8
        return self._cards[9].get_value("lpms1_7")

    @lpms1_7.setter
    def lpms1_7(self, value: int) -> None:
        self._cards[9].set_value("lpms1_7", value)

    @property
    def lpms1_8(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 1 for hinge moment HMS1_8
        """ # nopep8
        return self._cards[9].get_value("lpms1_8")

    @lpms1_8.setter
    def lpms1_8(self, value: int) -> None:
        self._cards[9].set_value("lpms1_8", value)

    @property
    def hms2_1(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 2.
        """ # nopep8
        return self._cards[10].get_value("hms2_1")

    @hms2_1.setter
    def hms2_1(self, value: float) -> None:
        self._cards[10].set_value("hms2_1", value)

    @property
    def hms2_2(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 2.
        """ # nopep8
        return self._cards[10].get_value("hms2_2")

    @hms2_2.setter
    def hms2_2(self, value: float) -> None:
        self._cards[10].set_value("hms2_2", value)

    @property
    def hms2_3(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 2.
        """ # nopep8
        return self._cards[10].get_value("hms2_3")

    @hms2_3.setter
    def hms2_3(self, value: float) -> None:
        self._cards[10].set_value("hms2_3", value)

    @property
    def hms2_4(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 2.
        """ # nopep8
        return self._cards[10].get_value("hms2_4")

    @hms2_4.setter
    def hms2_4(self, value: float) -> None:
        self._cards[10].set_value("hms2_4", value)

    @property
    def hms2_5(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 2.
        """ # nopep8
        return self._cards[10].get_value("hms2_5")

    @hms2_5.setter
    def hms2_5(self, value: float) -> None:
        self._cards[10].set_value("hms2_5", value)

    @property
    def hms2_6(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 2.
        """ # nopep8
        return self._cards[10].get_value("hms2_6")

    @hms2_6.setter
    def hms2_6(self, value: float) -> None:
        self._cards[10].set_value("hms2_6", value)

    @property
    def hms2_7(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 2.
        """ # nopep8
        return self._cards[10].get_value("hms2_7")

    @hms2_7.setter
    def hms2_7(self, value: float) -> None:
        self._cards[10].set_value("hms2_7", value)

    @property
    def hms2_8(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 2.
        """ # nopep8
        return self._cards[10].get_value("hms2_8")

    @hms2_8.setter
    def hms2_8(self, value: float) -> None:
        self._cards[10].set_value("hms2_8", value)

    @property
    def lpms2_1(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 2 for hinge moment HMS2_1
        """ # nopep8
        return self._cards[11].get_value("lpms2_1")

    @lpms2_1.setter
    def lpms2_1(self, value: int) -> None:
        self._cards[11].set_value("lpms2_1", value)

    @property
    def lpms2_2(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 2 for hinge moment HMS2_2
        """ # nopep8
        return self._cards[11].get_value("lpms2_2")

    @lpms2_2.setter
    def lpms2_2(self, value: int) -> None:
        self._cards[11].set_value("lpms2_2", value)

    @property
    def lpms2_3(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 2 for hinge moment HMS2_3
        """ # nopep8
        return self._cards[11].get_value("lpms2_3")

    @lpms2_3.setter
    def lpms2_3(self, value: int) -> None:
        self._cards[11].set_value("lpms2_3", value)

    @property
    def lpms2_4(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 2 for hinge moment HMS2_4
        """ # nopep8
        return self._cards[11].get_value("lpms2_4")

    @lpms2_4.setter
    def lpms2_4(self, value: int) -> None:
        self._cards[11].set_value("lpms2_4", value)

    @property
    def lpms2_5(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 2 for hinge moment HMS2_5
        """ # nopep8
        return self._cards[11].get_value("lpms2_5")

    @lpms2_5.setter
    def lpms2_5(self, value: int) -> None:
        self._cards[11].set_value("lpms2_5", value)

    @property
    def lpms2_6(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 2 for hinge moment HMS2_6
        """ # nopep8
        return self._cards[11].get_value("lpms2_6")

    @lpms2_6.setter
    def lpms2_6(self, value: int) -> None:
        self._cards[11].set_value("lpms2_6", value)

    @property
    def lpms2_7(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 2 for hinge moment HMS2_7
        """ # nopep8
        return self._cards[11].get_value("lpms2_7")

    @lpms2_7.setter
    def lpms2_7(self, value: int) -> None:
        self._cards[11].set_value("lpms2_7", value)

    @property
    def lpms2_8(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 2 for hinge moment HMS2_8
        """ # nopep8
        return self._cards[11].get_value("lpms2_8")

    @lpms2_8.setter
    def lpms2_8(self, value: int) -> None:
        self._cards[11].set_value("lpms2_8", value)

    @property
    def hmt1_1(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 1.
        """ # nopep8
        return self._cards[12].get_value("hmt1_1")

    @hmt1_1.setter
    def hmt1_1(self, value: float) -> None:
        self._cards[12].set_value("hmt1_1", value)

    @property
    def hmt1_2(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 1.
        """ # nopep8
        return self._cards[12].get_value("hmt1_2")

    @hmt1_2.setter
    def hmt1_2(self, value: float) -> None:
        self._cards[12].set_value("hmt1_2", value)

    @property
    def hmt1_3(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 1.
        """ # nopep8
        return self._cards[12].get_value("hmt1_3")

    @hmt1_3.setter
    def hmt1_3(self, value: float) -> None:
        self._cards[12].set_value("hmt1_3", value)

    @property
    def hmt1_4(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 1.
        """ # nopep8
        return self._cards[12].get_value("hmt1_4")

    @hmt1_4.setter
    def hmt1_4(self, value: float) -> None:
        self._cards[12].set_value("hmt1_4", value)

    @property
    def hmt1_5(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 1.
        """ # nopep8
        return self._cards[12].get_value("hmt1_5")

    @hmt1_5.setter
    def hmt1_5(self, value: float) -> None:
        self._cards[12].set_value("hmt1_5", value)

    @property
    def hmt1_6(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 1.
        """ # nopep8
        return self._cards[12].get_value("hmt1_6")

    @hmt1_6.setter
    def hmt1_6(self, value: float) -> None:
        self._cards[12].set_value("hmt1_6", value)

    @property
    def hmt1_7(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 1.
        """ # nopep8
        return self._cards[12].get_value("hmt1_7")

    @hmt1_7.setter
    def hmt1_7(self, value: float) -> None:
        self._cards[12].set_value("hmt1_7", value)

    @property
    def hmt1_8(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 1.
        """ # nopep8
        return self._cards[12].get_value("hmt1_8")

    @hmt1_8.setter
    def hmt1_8(self, value: float) -> None:
        self._cards[12].set_value("hmt1_8", value)

    @property
    def lpmt1_1(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 1 for hinge moment HMT1_1
        """ # nopep8
        return self._cards[13].get_value("lpmt1_1")

    @lpmt1_1.setter
    def lpmt1_1(self, value: int) -> None:
        self._cards[13].set_value("lpmt1_1", value)

    @property
    def lpmt1_2(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 1 for hinge moment HMT1_2
        """ # nopep8
        return self._cards[13].get_value("lpmt1_2")

    @lpmt1_2.setter
    def lpmt1_2(self, value: int) -> None:
        self._cards[13].set_value("lpmt1_2", value)

    @property
    def lpmt1_3(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 1 for hinge moment HMT1_3
        """ # nopep8
        return self._cards[13].get_value("lpmt1_3")

    @lpmt1_3.setter
    def lpmt1_3(self, value: int) -> None:
        self._cards[13].set_value("lpmt1_3", value)

    @property
    def lpmt1_4(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 1 for hinge moment HMT1_4
        """ # nopep8
        return self._cards[13].get_value("lpmt1_4")

    @lpmt1_4.setter
    def lpmt1_4(self, value: int) -> None:
        self._cards[13].set_value("lpmt1_4", value)

    @property
    def lpmt1_5(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 1 for hinge moment HMT1_5
        """ # nopep8
        return self._cards[13].get_value("lpmt1_5")

    @lpmt1_5.setter
    def lpmt1_5(self, value: int) -> None:
        self._cards[13].set_value("lpmt1_5", value)

    @property
    def lpmt1_6(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 1 for hinge moment HMT1_6
        """ # nopep8
        return self._cards[13].get_value("lpmt1_6")

    @lpmt1_6.setter
    def lpmt1_6(self, value: int) -> None:
        self._cards[13].set_value("lpmt1_6", value)

    @property
    def lpmt1_7(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 1 for hinge moment HMT1_7
        """ # nopep8
        return self._cards[13].get_value("lpmt1_7")

    @lpmt1_7.setter
    def lpmt1_7(self, value: int) -> None:
        self._cards[13].set_value("lpmt1_7", value)

    @property
    def lpmt1_8(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 1 for hinge moment HMT1_8
        """ # nopep8
        return self._cards[13].get_value("lpmt1_8")

    @lpmt1_8.setter
    def lpmt1_8(self, value: int) -> None:
        self._cards[13].set_value("lpmt1_8", value)

    @property
    def hmt2_1(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 2.
        """ # nopep8
        return self._cards[14].get_value("hmt2_1")

    @hmt2_1.setter
    def hmt2_1(self, value: float) -> None:
        self._cards[14].set_value("hmt2_1", value)

    @property
    def hmt2_2(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 2.
        """ # nopep8
        return self._cards[14].get_value("hmt2_2")

    @hmt2_2.setter
    def hmt2_2(self, value: float) -> None:
        self._cards[14].set_value("hmt2_2", value)

    @property
    def hmt2_3(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 2.
        """ # nopep8
        return self._cards[14].get_value("hmt2_3")

    @hmt2_3.setter
    def hmt2_3(self, value: float) -> None:
        self._cards[14].set_value("hmt2_3", value)

    @property
    def hmt2_4(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 2.
        """ # nopep8
        return self._cards[14].get_value("hmt2_4")

    @hmt2_4.setter
    def hmt2_4(self, value: float) -> None:
        self._cards[14].set_value("hmt2_4", value)

    @property
    def hmt2_5(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 2.
        """ # nopep8
        return self._cards[14].get_value("hmt2_5")

    @hmt2_5.setter
    def hmt2_5(self, value: float) -> None:
        self._cards[14].set_value("hmt2_5", value)

    @property
    def hmt2_6(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 2.
        """ # nopep8
        return self._cards[14].get_value("hmt2_6")

    @hmt2_6.setter
    def hmt2_6(self, value: float) -> None:
        self._cards[14].set_value("hmt2_6", value)

    @property
    def hmt2_7(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 2.
        """ # nopep8
        return self._cards[14].get_value("hmt2_7")

    @hmt2_7.setter
    def hmt2_7(self, value: float) -> None:
        self._cards[14].set_value("hmt2_7", value)

    @property
    def hmt2_8(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 2.
        """ # nopep8
        return self._cards[14].get_value("hmt2_8")

    @hmt2_8.setter
    def hmt2_8(self, value: float) -> None:
        self._cards[14].set_value("hmt2_8", value)

    @property
    def lpmt2_1(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 2 for hinge moment HMT2_1
        """ # nopep8
        return self._cards[15].get_value("lpmt2_1")

    @lpmt2_1.setter
    def lpmt2_1(self, value: int) -> None:
        self._cards[15].set_value("lpmt2_1", value)

    @property
    def lpmt2_2(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 2 for hinge moment HMT2_2
        """ # nopep8
        return self._cards[15].get_value("lpmt2_2")

    @lpmt2_2.setter
    def lpmt2_2(self, value: int) -> None:
        self._cards[15].set_value("lpmt2_2", value)

    @property
    def lpmt2_3(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 2 for hinge moment HMT2_3
        """ # nopep8
        return self._cards[15].get_value("lpmt2_3")

    @lpmt2_3.setter
    def lpmt2_3(self, value: int) -> None:
        self._cards[15].set_value("lpmt2_3", value)

    @property
    def lpmt2_4(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 2 for hinge moment HMT2_4
        """ # nopep8
        return self._cards[15].get_value("lpmt2_4")

    @lpmt2_4.setter
    def lpmt2_4(self, value: int) -> None:
        self._cards[15].set_value("lpmt2_4", value)

    @property
    def lpmt2_5(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 2 for hinge moment HMT2_5
        """ # nopep8
        return self._cards[15].get_value("lpmt2_5")

    @lpmt2_5.setter
    def lpmt2_5(self, value: int) -> None:
        self._cards[15].set_value("lpmt2_5", value)

    @property
    def lpmt2_6(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 2 for hinge moment HMT2_6
        """ # nopep8
        return self._cards[15].get_value("lpmt2_6")

    @lpmt2_6.setter
    def lpmt2_6(self, value: int) -> None:
        self._cards[15].set_value("lpmt2_6", value)

    @property
    def lpmt2_7(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 2 for hinge moment HMT2_7
        """ # nopep8
        return self._cards[15].get_value("lpmt2_7")

    @lpmt2_7.setter
    def lpmt2_7(self, value: int) -> None:
        self._cards[15].set_value("lpmt2_7", value)

    @property
    def lpmt2_8(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 2 for hinge moment HMT2_8
        """ # nopep8
        return self._cards[15].get_value("lpmt2_8")

    @lpmt2_8.setter
    def lpmt2_8(self, value: int) -> None:
        self._cards[15].set_value("lpmt2_8", value)

    @property
    def hmr_1(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for the torsional axis.
        """ # nopep8
        return self._cards[16].get_value("hmr_1")

    @hmr_1.setter
    def hmr_1(self, value: float) -> None:
        self._cards[16].set_value("hmr_1", value)

    @property
    def hmr_2(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for the torsional axis.
        """ # nopep8
        return self._cards[16].get_value("hmr_2")

    @hmr_2.setter
    def hmr_2(self, value: float) -> None:
        self._cards[16].set_value("hmr_2", value)

    @property
    def hmr_3(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for the torsional axis.
        """ # nopep8
        return self._cards[16].get_value("hmr_3")

    @hmr_3.setter
    def hmr_3(self, value: float) -> None:
        self._cards[16].set_value("hmr_3", value)

    @property
    def hmr_4(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for the torsional axis.
        """ # nopep8
        return self._cards[16].get_value("hmr_4")

    @hmr_4.setter
    def hmr_4(self, value: float) -> None:
        self._cards[16].set_value("hmr_4", value)

    @property
    def hmr_5(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for the torsional axis.
        """ # nopep8
        return self._cards[16].get_value("hmr_5")

    @hmr_5.setter
    def hmr_5(self, value: float) -> None:
        self._cards[16].set_value("hmr_5", value)

    @property
    def hmr_6(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for the torsional axis.
        """ # nopep8
        return self._cards[16].get_value("hmr_6")

    @hmr_6.setter
    def hmr_6(self, value: float) -> None:
        self._cards[16].set_value("hmr_6", value)

    @property
    def hmr_7(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for the torsional axis.
        """ # nopep8
        return self._cards[16].get_value("hmr_7")

    @hmr_7.setter
    def hmr_7(self, value: float) -> None:
        self._cards[16].set_value("hmr_7", value)

    @property
    def hmr_8(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for the torsional axis.
        """ # nopep8
        return self._cards[16].get_value("hmr_8")

    @hmr_8.setter
    def hmr_8(self, value: float) -> None:
        self._cards[16].set_value("hmr_8", value)

    @property
    def lpmr_1(self) -> int:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the torsional axis for hinge moment HMR_1
        """ # nopep8
        return self._cards[17].get_value("lpmr_1")

    @lpmr_1.setter
    def lpmr_1(self, value: int) -> None:
        self._cards[17].set_value("lpmr_1", value)

    @property
    def lpmr_2(self) -> int:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the torsional axis for hinge moment HMR_2
        """ # nopep8
        return self._cards[17].get_value("lpmr_2")

    @lpmr_2.setter
    def lpmr_2(self, value: int) -> None:
        self._cards[17].set_value("lpmr_2", value)

    @property
    def lpmr_3(self) -> int:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the torsional axis for hinge moment HMR_3
        """ # nopep8
        return self._cards[17].get_value("lpmr_3")

    @lpmr_3.setter
    def lpmr_3(self, value: int) -> None:
        self._cards[17].set_value("lpmr_3", value)

    @property
    def lpmr_4(self) -> int:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the torsional axis for hinge moment HMR_4
        """ # nopep8
        return self._cards[17].get_value("lpmr_4")

    @lpmr_4.setter
    def lpmr_4(self, value: int) -> None:
        self._cards[17].set_value("lpmr_4", value)

    @property
    def lpmr_5(self) -> int:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the torsional axis for hinge moment HMR_5
        """ # nopep8
        return self._cards[17].get_value("lpmr_5")

    @lpmr_5.setter
    def lpmr_5(self, value: int) -> None:
        self._cards[17].set_value("lpmr_5", value)

    @property
    def lpmr_6(self) -> int:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the torsional axis for hinge moment HMR_6
        """ # nopep8
        return self._cards[17].get_value("lpmr_6")

    @lpmr_6.setter
    def lpmr_6(self, value: int) -> None:
        self._cards[17].set_value("lpmr_6", value)

    @property
    def lpmr_7(self) -> int:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the torsional axis for hinge moment HMR_7
        """ # nopep8
        return self._cards[17].get_value("lpmr_7")

    @lpmr_7.setter
    def lpmr_7(self, value: int) -> None:
        self._cards[17].set_value("lpmr_7", value)

    @property
    def lpmr_8(self) -> int:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the torsional axis for hinge moment HMR_8
        """ # nopep8
        return self._cards[17].get_value("lpmr_8")

    @lpmr_8.setter
    def lpmr_8(self, value: int) -> None:
        self._cards[17].set_value("lpmr_8", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[18].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[18].cards[0].set_value("title", value)

