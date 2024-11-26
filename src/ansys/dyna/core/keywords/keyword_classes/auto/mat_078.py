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

class Mat078(KeywordBase):
    """DYNA MAT_078 keyword"""

    keyword = "MAT"
    subkeyword = "078"
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
                        "g",
                        float,
                        20,
                        10,
                        kwargs.get("g")
                    ),
                    Field(
                        "k",
                        float,
                        30,
                        10,
                        kwargs.get("k")
                    ),
                    Field(
                        "lcpv",
                        int,
                        40,
                        10,
                        kwargs.get("lcpv")
                    ),
                    Field(
                        "lcyp",
                        int,
                        50,
                        10,
                        kwargs.get("lcyp")
                    ),
                    Field(
                        "lcfp",
                        int,
                        60,
                        10,
                        kwargs.get("lcfp", 0)
                    ),
                    Field(
                        "lcrp",
                        int,
                        70,
                        10,
                        kwargs.get("lcrp", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "pc",
                        float,
                        0,
                        10,
                        kwargs.get("pc")
                    ),
                    Field(
                        "out",
                        float,
                        10,
                        10,
                        kwargs.get("out", 0)
                    ),
                    Field(
                        "b",
                        float,
                        20,
                        10,
                        kwargs.get("b")
                    ),
                    Field(
                        "fail",
                        float,
                        30,
                        10,
                        kwargs.get("fail", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat078.option_specs[0],
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
    def g(self) -> typing.Optional[float]:
        """Get or set the Shear modulus.
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        self._cards[0].set_value("g", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus.
        """ # nopep8
        return self._cards[0].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        self._cards[0].set_value("k", value)

    @property
    def lcpv(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for pressure versus volumetric strain. Defined in compression only. The sign convention requires that both pressure and compressive strain be defined as positive values where the compressive strain is taken as the negative value of the natural logrithm of the relative volume.
        """ # nopep8
        return self._cards[0].get_value("lcpv")

    @lcpv.setter
    def lcpv(self, value: int) -> None:
        self._cards[0].set_value("lcpv", value)

    @property
    def lcyp(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for yield versus pressure:
        GT.0: von Mises stress versus pressure,
        LT.0: Second stress invariant,J2 ,versus pressure. This curve must be defined.
        """ # nopep8
        return self._cards[0].get_value("lcyp")

    @lcyp.setter
    def lcyp(self, value: int) -> None:
        self._cards[0].set_value("lcyp", value)

    @property
    def lcfp(self) -> int:
        """Get or set the Load curve ID for plastic strain at which fracture begins versus pressure.  This load curve ID must be defined if B>0.0.
        """ # nopep8
        return self._cards[0].get_value("lcfp")

    @lcfp.setter
    def lcfp(self, value: int) -> None:
        self._cards[0].set_value("lcfp", value)

    @property
    def lcrp(self) -> int:
        """Get or set the Load curve ID for plastic strain at which residual strength is reached versus pressure. This load curve ID must be defined if B>0.0.
        """ # nopep8
        return self._cards[0].get_value("lcrp")

    @lcrp.setter
    def lcrp(self, value: int) -> None:
        self._cards[0].set_value("lcrp", value)

    @property
    def pc(self) -> typing.Optional[float]:
        """Get or set the Pressure cutoff for tensile fracture
        """ # nopep8
        return self._cards[1].get_value("pc")

    @pc.setter
    def pc(self, value: float) -> None:
        self._cards[1].set_value("pc", value)

    @property
    def out(self) -> float:
        """Get or set the Output option for plastic strain in database:
        EQ.0: volumetric plastic strain (default),
        EQ.1: deviatoric plastic strain.
        """ # nopep8
        return self._cards[1].get_value("out")

    @out.setter
    def out(self, value: float) -> None:
        if value not in [0, 1]:
            raise Exception("""out must be one of {0,1}""")
        self._cards[1].set_value("out", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Residual strength factor after cracking.
        """ # nopep8
        return self._cards[1].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        self._cards[1].set_value("b", value)

    @property
    def fail(self) -> float:
        """Get or set the Flag for failure:
        EQ.0: no failure (default),
        EQ:1: When pressure reaches failure pressure element is eroded,
        EQ.2: When pressure reaches failure pressure element loses it ability to carry tension.
        """ # nopep8
        return self._cards[1].get_value("fail")

    @fail.setter
    def fail(self, value: float) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""fail must be one of {0,1,2}""")
        self._cards[1].set_value("fail", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

