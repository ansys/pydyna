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
from ansys.dyna.core.lib.variable_card import VariableCard
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class SectionTShell(KeywordBase):
    """DYNA SECTION_TSHELL keyword"""

    keyword = "SECTION"
    subkeyword = "TSHELL"
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
                        kwargs.get("elform", 1)
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
                        "propt",
                        float,
                        40,
                        10,
                        kwargs.get("propt", 1.0)
                    ),
                    Field(
                        "qr",
                        int,
                        50,
                        10,
                        kwargs.get("qr", 0)
                    ),
                    Field(
                        "icomp",
                        int,
                        60,
                        10,
                        kwargs.get("icomp", 0)
                    ),
                    Field(
                        "tshear",
                        int,
                        70,
                        10,
                        kwargs.get("tshear", 0)
                    ),
                ],
            ),
            VariableCard(
                "bi",
                8,
                10,
                float,
                lambda: self.nip,
                lambda: self.icomp == 1,
                data = kwargs.get("bi")),
            OptionCardSet(
                option_spec = SectionTShell.option_specs[0],
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
        """Get or set the Element formulation:
        EQ.1: one point reduced integration (default),
        EQ.2: selective reduced 2x2 in plane integration.
        EQ.3: assumed strain 2x2 in plane integration.
        EQ.5:  assumed strain reduced integration.
        EQ.6: assumed strain reduced integration with shell materials
        EQ.7:	assumed strain  2×2 in plane integration
        """ # nopep8
        return self._cards[0].get_value("elform")

    @elform.setter
    def elform(self, value: int) -> None:
        if value not in [1, 2, 3, 5, 6, 7]:
            raise Exception("""elform must be one of {1,2,3,5,6,7}""")
        self._cards[0].set_value("elform", value)

    @property
    def shrf(self) -> float:
        """Get or set the Shear factor (default =1.0). A value of 5/6 is recommended.
        """ # nopep8
        return self._cards[0].get_value("shrf")

    @shrf.setter
    def shrf(self, value: float) -> None:
        self._cards[0].set_value("shrf", value)

    @property
    def nip(self) -> int:
        """Get or set the Number of through shell thickness integration points (default = 2).
        """ # nopep8
        return self._cards[0].get_value("nip")

    @nip.setter
    def nip(self, value: int) -> None:
        self._cards[0].set_value("nip", value)

    @property
    def propt(self) -> float:
        """Get or set the Printout option:
        EQ.1.0: average resultants and fiber lengths (default),
        EQ.2.0: resultants at plan points and fiber lengths,
        EQ.3.0: resultants, stresses at all points, fiber lengths.
        """ # nopep8
        return self._cards[0].get_value("propt")

    @propt.setter
    def propt(self, value: float) -> None:
        if value not in [1.0, 2.0, 3.0]:
            raise Exception("""propt must be one of {1.0,2.0,3.0}""")
        self._cards[0].set_value("propt", value)

    @property
    def qr(self) -> int:
        """Get or set the Quadrature rule:
        LT.0: absolute value is specified rule number,
        EQ.0: Gauss (up to five points are permitted),
        EQ.1: trapezoidal, not recommended for accuracy reasons.
        """ # nopep8
        return self._cards[0].get_value("qr")

    @qr.setter
    def qr(self, value: int) -> None:
        self._cards[0].set_value("qr", value)

    @property
    def icomp(self) -> int:
        """Get or set the Flag for layered composite material mode:
        EQ.0: Flag turned off (default),
        EQ.1: a material angle is defined for each through thickness integration point . For each layer one integration point is used.
        """ # nopep8
        return self._cards[0].get_value("icomp")

    @icomp.setter
    def icomp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""icomp must be one of {0,1}""")
        self._cards[0].set_value("icomp", value)

    @property
    def tshear(self) -> int:
        """Get or set the Flag for transverse shear strain or stress distribution (see Remarks 4 and 5):
        EQ.0.0: Parabolic,
        EQ.1.0: Constant through thickness.
        """ # nopep8
        return self._cards[0].get_value("tshear")

    @tshear.setter
    def tshear(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""tshear must be one of {0,1}""")
        self._cards[0].set_value("tshear", value)

    @property
    def bi(self) -> VariableCard:
        """dynamic array of beta-i: material angle at ith-integration point."""
        return self._cards[1]

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

