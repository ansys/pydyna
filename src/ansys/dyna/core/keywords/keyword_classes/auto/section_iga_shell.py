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

class SectionIgaShell(KeywordBase):
    """DYNA SECTION_IGA_SHELL keyword"""

    keyword = "SECTION"
    subkeyword = "IGA_SHELL"
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
                        "secid",
                        int,
                        0,
                        10,
                        kwargs.get("secid")
                    ),
                    Field(
                        "elform",
                        int,
                        10,
                        10,
                        kwargs.get("elform", 0)
                    ),
                    Field(
                        "shrf",
                        float,
                        20,
                        10,
                        kwargs.get("shrf", 1.0)
                    ),
                    Field(
                        "nip",
                        int,
                        30,
                        10,
                        kwargs.get("nip", 2)
                    ),
                    Field(
                        "irl",
                        int,
                        40,
                        10,
                        kwargs.get("irl", 0)
                    ),
                    Field(
                        "qr/irid",
                        float,
                        50,
                        10,
                        kwargs.get("qr/irid", 0)
                    ),
                    Field(
                        "icomp",
                        int,
                        60,
                        10,
                        kwargs.get("icomp", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "t",
                        float,
                        0,
                        10,
                        kwargs.get("t", 0.0)
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
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "nloc",
                        float,
                        40,
                        10,
                        kwargs.get("nloc", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "b1",
                        float,
                        0,
                        10,
                        kwargs.get("b1")
                    ),
                    Field(
                        "b2",
                        float,
                        10,
                        10,
                        kwargs.get("b2")
                    ),
                    Field(
                        "b3",
                        float,
                        20,
                        10,
                        kwargs.get("b3")
                    ),
                    Field(
                        "b4",
                        float,
                        30,
                        10,
                        kwargs.get("b4")
                    ),
                    Field(
                        "b5",
                        float,
                        40,
                        10,
                        kwargs.get("b5")
                    ),
                    Field(
                        "b6",
                        float,
                        50,
                        10,
                        kwargs.get("b6")
                    ),
                    Field(
                        "b7",
                        float,
                        60,
                        10,
                        kwargs.get("b7")
                    ),
                    Field(
                        "b8",
                        float,
                        70,
                        10,
                        kwargs.get("b8")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = SectionIgaShell.option_specs[0],
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
    def secid(self) -> typing.Optional[int]:
        """Get or set the Section ID. SECID is referenced on the *PART card and must be unique.
        """ # nopep8
        return self._cards[0].get_value("secid")

    @secid.setter
    def secid(self, value: int) -> None:
        self._cards[0].set_value("secid", value)

    @property
    def elform(self) -> int:
        """Get or set the Element formulation
        EQ.0: Reissner - Mindlin with fibers at the control points
        EQ.1 : Kirchhoff - Love with fibers at the control points
        EQ.2 : Kirchhoff - Love with fibers at the integration points
        EQ.3 : Reissner - Mindlin with fibers at the integration points.
        EQ.5:	Thick shell with thickness stretch based on the ELFORM = 0. See Remark 1.
        EQ.6:	Thick shell with thickness stretch based on ELFORM = 3. See Remark 1
        """ # nopep8
        return self._cards[0].get_value("elform")

    @elform.setter
    def elform(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 5, 6]:
            raise Exception("""elform must be one of {0,1,2,3,5,6}""")
        self._cards[0].set_value("elform", value)

    @property
    def shrf(self) -> float:
        """Get or set the Shear correction factor which scales the transverse shear stress, see Remark 1.
        """ # nopep8
        return self._cards[0].get_value("shrf")

    @shrf.setter
    def shrf(self, value: float) -> None:
        self._cards[0].set_value("shrf", value)

    @property
    def nip(self) -> int:
        """Get or set the Number of through thickness integration points, see Remark 2.
        GT.0.0: Number of quadrature points(up to 10 points).
        """ # nopep8
        return self._cards[0].get_value("nip")

    @nip.setter
    def nip(self, value: int) -> None:
        self._cards[0].set_value("nip", value)

    @property
    def irl(self) -> int:
        """Get or set the Lamina integration rule
        EQ.0: Reduced Gauss - Legendre
        EQ.1 : Gauss - Legendre
        EQ.2 : Patchwise reduced Gauss - Legendre(for biquadratic NURBS only).
        """ # nopep8
        return self._cards[0].get_value("irl")

    @irl.setter
    def irl(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""irl must be one of {0,1,2}""")
        self._cards[0].set_value("irl", value)

    @property
    def qr_irid(self) -> float:
        """Get or set the Fiber quadrature rule or fiber integration rule ID, see *INTEGRATION_SHELL.
        LT.0.0: Absolute value is specified rule number.
        EQ.0.0 : Gauss - Legendre / Gauss - Lobatto(up to 10 points)
        EQ.1.0 : Trapezoidal, not recommended for accuracy reasons.
        """ # nopep8
        return self._cards[0].get_value("qr/irid")

    @qr_irid.setter
    def qr_irid(self, value: float) -> None:
        self._cards[0].set_value("qr/irid", value)

    @property
    def icomp(self) -> int:
        """Get or set the Flag for anisotropic layered composite material model, see Remark 3.
        EQ.1: A material angle in degrees is defined for each through
        thickness integration point.Thus, each layer has one integration point.
        """ # nopep8
        return self._cards[0].get_value("icomp")

    @icomp.setter
    def icomp(self, value: int) -> None:
        self._cards[0].set_value("icomp", value)

    @property
    def t(self) -> float:
        """Get or set the Shell thickness.
        """ # nopep8
        return self._cards[1].get_value("t")

    @t.setter
    def t(self, value: float) -> None:
        self._cards[1].set_value("t", value)

    @property
    def nloc(self) -> float:
        """Get or set the Location of reference surface, see Remark 4.
        """ # nopep8
        return self._cards[1].get_value("nloc")

    @nloc.setter
    def nloc(self, value: float) -> None:
        self._cards[1].set_value("nloc", value)

    @property
    def b1(self) -> typing.Optional[float]:
        """Get or set the Material angle at the ith fiber integration point.
        """ # nopep8
        return self._cards[2].get_value("b1")

    @b1.setter
    def b1(self, value: float) -> None:
        self._cards[2].set_value("b1", value)

    @property
    def b2(self) -> typing.Optional[float]:
        """Get or set the Material angle at the ith fiber integration point.
        """ # nopep8
        return self._cards[2].get_value("b2")

    @b2.setter
    def b2(self, value: float) -> None:
        self._cards[2].set_value("b2", value)

    @property
    def b3(self) -> typing.Optional[float]:
        """Get or set the Material angle at the ith fiber integration point.
        """ # nopep8
        return self._cards[2].get_value("b3")

    @b3.setter
    def b3(self, value: float) -> None:
        self._cards[2].set_value("b3", value)

    @property
    def b4(self) -> typing.Optional[float]:
        """Get or set the Material angle at the ith fiber integration point.
        """ # nopep8
        return self._cards[2].get_value("b4")

    @b4.setter
    def b4(self, value: float) -> None:
        self._cards[2].set_value("b4", value)

    @property
    def b5(self) -> typing.Optional[float]:
        """Get or set the Material angle at the ith fiber integration point.
        """ # nopep8
        return self._cards[2].get_value("b5")

    @b5.setter
    def b5(self, value: float) -> None:
        self._cards[2].set_value("b5", value)

    @property
    def b6(self) -> typing.Optional[float]:
        """Get or set the Material angle at the ith fiber integration point.
        """ # nopep8
        return self._cards[2].get_value("b6")

    @b6.setter
    def b6(self, value: float) -> None:
        self._cards[2].set_value("b6", value)

    @property
    def b7(self) -> typing.Optional[float]:
        """Get or set the Material angle at the ith fiber integration point.
        """ # nopep8
        return self._cards[2].get_value("b7")

    @b7.setter
    def b7(self, value: float) -> None:
        self._cards[2].set_value("b7", value)

    @property
    def b8(self) -> typing.Optional[float]:
        """Get or set the Material angle at the ith fiber integration point.
        """ # nopep8
        return self._cards[2].get_value("b8")

    @b8.setter
    def b8(self, value: float) -> None:
        self._cards[2].set_value("b8", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

