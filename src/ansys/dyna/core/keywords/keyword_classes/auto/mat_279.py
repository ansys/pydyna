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

class Mat279(KeywordBase):
    """DYNA MAT_279 keyword"""

    keyword = "MAT"
    subkeyword = "279"
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
                        "roflg",
                        int,
                        20,
                        10,
                        kwargs.get("roflg", 0)
                    ),
                    Field(
                        "intfail",
                        float,
                        30,
                        10,
                        kwargs.get("intfail")
                    ),
                    Field(
                        "en0",
                        float,
                        40,
                        10,
                        kwargs.get("en0")
                    ),
                    Field(
                        "et0",
                        float,
                        50,
                        10,
                        kwargs.get("et0")
                    ),
                    Field(
                        "en1",
                        float,
                        60,
                        10,
                        kwargs.get("en1")
                    ),
                    Field(
                        "et1",
                        float,
                        70,
                        10,
                        kwargs.get("et1")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "t0n",
                        float,
                        0,
                        10,
                        kwargs.get("t0n")
                    ),
                    Field(
                        "dn",
                        float,
                        10,
                        10,
                        kwargs.get("dn")
                    ),
                    Field(
                        "t1n",
                        float,
                        20,
                        10,
                        kwargs.get("t1n")
                    ),
                    Field(
                        "t0t",
                        float,
                        30,
                        10,
                        kwargs.get("t0t")
                    ),
                    Field(
                        "dt",
                        float,
                        40,
                        10,
                        kwargs.get("dt")
                    ),
                    Field(
                        "t1t",
                        float,
                        50,
                        10,
                        kwargs.get("t1t")
                    ),
                    Field(
                        "e3c",
                        float,
                        60,
                        10,
                        kwargs.get("e3c")
                    ),
                    Field(
                        "cc",
                        float,
                        70,
                        10,
                        kwargs.get("cc")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "asig",
                        float,
                        0,
                        10,
                        kwargs.get("asig")
                    ),
                    Field(
                        "bsig",
                        float,
                        10,
                        10,
                        kwargs.get("bsig")
                    ),
                    Field(
                        "csig",
                        float,
                        20,
                        10,
                        kwargs.get("csig")
                    ),
                    Field(
                        "failn",
                        float,
                        30,
                        10,
                        kwargs.get("failn")
                    ),
                    Field(
                        "failt",
                        float,
                        40,
                        10,
                        kwargs.get("failt")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat279.option_specs[0],
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
    def roflg(self) -> int:
        """Get or set the Flag for whether density is specified per unit area or volume:
        EQ.0:	Specified density is per unit volume(default).
        EQ.1 : Specified density is per unit area for controlling the mass of cohesive elements with an initial volume of zero.
        """ # nopep8
        return self._cards[0].get_value("roflg")

    @roflg.setter
    def roflg(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""roflg must be one of {0,1}""")
        self._cards[0].set_value("roflg", value)

    @property
    def intfail(self) -> typing.Optional[float]:
        """Get or set the The number of integration points required for the cohesive element to be deleted. The value of INTFAIL may range from 1 to 4 with 1 the recommended value.
        LT.0.0:	Employs a Newton - Cotes integration scheme. The element will be deleted when |INTFAIL| integration points have failed.
        EQ.0.0 : Employs a Newton - Cotes integration scheme. The element will not be deleted even if it satisfies the failure criterion.
        GT.0.0 : Employs a Gauss integration scheme. The element will be deleted when INTFAIL integration points have failed.
        """ # nopep8
        return self._cards[0].get_value("intfail")

    @intfail.setter
    def intfail(self, value: float) -> None:
        self._cards[0].set_value("intfail", value)

    @property
    def en0(self) -> typing.Optional[float]:
        """Get or set the The initial tensile stiffness (units of stress / length) normal to the
        plane of the cohesive element.
        """ # nopep8
        return self._cards[0].get_value("en0")

    @en0.setter
    def en0(self, value: float) -> None:
        self._cards[0].set_value("en0", value)

    @property
    def et0(self) -> typing.Optional[float]:
        """Get or set the The initial stiffness (units of stress / length) tangential to the plane
        of the cohesive element.
        """ # nopep8
        return self._cards[0].get_value("et0")

    @et0.setter
    def et0(self, value: float) -> None:
        self._cards[0].set_value("et0", value)

    @property
    def en1(self) -> typing.Optional[float]:
        """Get or set the The final tensile stiffness (units of stress / length) normal to the
        plane of the cohesive element.
        """ # nopep8
        return self._cards[0].get_value("en1")

    @en1.setter
    def en1(self, value: float) -> None:
        self._cards[0].set_value("en1", value)

    @property
    def et1(self) -> typing.Optional[float]:
        """Get or set the The final stiffness (units of stress / length) tangential to the plane of
        the cohesive element.
        """ # nopep8
        return self._cards[0].get_value("et1")

    @et1.setter
    def et1(self, value: float) -> None:
        self._cards[0].set_value("et1", value)

    @property
    def t0n(self) -> typing.Optional[float]:
        """Get or set the Peak tensile traction in normal direction.
        """ # nopep8
        return self._cards[1].get_value("t0n")

    @t0n.setter
    def t0n(self, value: float) -> None:
        self._cards[1].set_value("t0n", value)

    @property
    def dn(self) -> typing.Optional[float]:
        """Get or set the Scale factor (unit of length).
        """ # nopep8
        return self._cards[1].get_value("dn")

    @dn.setter
    def dn(self, value: float) -> None:
        self._cards[1].set_value("dn", value)

    @property
    def t1n(self) -> typing.Optional[float]:
        """Get or set the Final tensile traction in normal direction.
        """ # nopep8
        return self._cards[1].get_value("t1n")

    @t1n.setter
    def t1n(self, value: float) -> None:
        self._cards[1].set_value("t1n", value)

    @property
    def t0t(self) -> typing.Optional[float]:
        """Get or set the Peak tensile traction in tangential direction. If negative, the absolute
        value indicates a curve with respect to the normal traction.
        """ # nopep8
        return self._cards[1].get_value("t0t")

    @t0t.setter
    def t0t(self, value: float) -> None:
        self._cards[1].set_value("t0t", value)

    @property
    def dt(self) -> typing.Optional[float]:
        """Get or set the Scale factor (unit of length). If negative, the absolute value indicates
        a curve with respect to the normal stress.
        """ # nopep8
        return self._cards[1].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[1].set_value("dt", value)

    @property
    def t1t(self) -> typing.Optional[float]:
        """Get or set the Final traction in tangential direction. If negative, the absolute value
        indicates a curve with respect to the normal traction.
        """ # nopep8
        return self._cards[1].get_value("t1t")

    @t1t.setter
    def t1t(self, value: float) -> None:
        self._cards[1].set_value("t1t", value)

    @property
    def e3c(self) -> typing.Optional[float]:
        """Get or set the Elastic parameter in normal compression.
        """ # nopep8
        return self._cards[1].get_value("e3c")

    @e3c.setter
    def e3c(self, value: float) -> None:
        self._cards[1].set_value("e3c", value)

    @property
    def cc(self) -> typing.Optional[float]:
        """Get or set the Elastic parameter in normal compression.
        """ # nopep8
        return self._cards[1].get_value("cc")

    @cc.setter
    def cc(self, value: float) -> None:
        self._cards[1].set_value("cc", value)

    @property
    def asig(self) -> typing.Optional[float]:
        """Get or set the Plasticity hardening parameter in normal compression.
        """ # nopep8
        return self._cards[2].get_value("asig")

    @asig.setter
    def asig(self, value: float) -> None:
        self._cards[2].set_value("asig", value)

    @property
    def bsig(self) -> typing.Optional[float]:
        """Get or set the Plasticity hardening parameter in normal compression.
        """ # nopep8
        return self._cards[2].get_value("bsig")

    @bsig.setter
    def bsig(self, value: float) -> None:
        self._cards[2].set_value("bsig", value)

    @property
    def csig(self) -> typing.Optional[float]:
        """Get or set the Plasticity hardening parameter in normal compression.
        """ # nopep8
        return self._cards[2].get_value("csig")

    @csig.setter
    def csig(self, value: float) -> None:
        self._cards[2].set_value("csig", value)

    @property
    def failn(self) -> typing.Optional[float]:
        """Get or set the Maximum effective separation distance in normal direction. Beyond
        this distance failure occurs.
        """ # nopep8
        return self._cards[2].get_value("failn")

    @failn.setter
    def failn(self, value: float) -> None:
        self._cards[2].set_value("failn", value)

    @property
    def failt(self) -> typing.Optional[float]:
        """Get or set the Maximum effective separation distance in tangential direction.
        Beyond this distance failure occurs.
        """ # nopep8
        return self._cards[2].get_value("failt")

    @failt.setter
    def failt(self, value: float) -> None:
        self._cards[2].set_value("failt", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

