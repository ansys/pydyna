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
from ansys.dyna.core.lib.keyword_base import KeywordBase

class InitialStressSection(KeywordBase):
    """DYNA INITIAL_STRESS_SECTION keyword"""

    keyword = "INITIAL"
    subkeyword = "STRESS_SECTION"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "issid",
                        int,
                        0,
                        10,
                        kwargs.get("issid")
                    ),
                    Field(
                        "csid",
                        int,
                        10,
                        10,
                        kwargs.get("csid")
                    ),
                    Field(
                        "lcid",
                        int,
                        20,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "psid",
                        int,
                        30,
                        10,
                        kwargs.get("psid")
                    ),
                    Field(
                        "vid",
                        int,
                        40,
                        10,
                        kwargs.get("vid")
                    ),
                    Field(
                        "izshear",
                        int,
                        50,
                        10,
                        kwargs.get("izshear", 0)
                    ),
                    Field(
                        "istiff",
                        int,
                        60,
                        10,
                        kwargs.get("istiff", 0)
                    ),
                ],
            ),
        ]

    @property
    def issid(self) -> typing.Optional[int]:
        """Get or set the Section stress initialization ID.
        """ # nopep8
        return self._cards[0].get_value("issid")

    @issid.setter
    def issid(self, value: int) -> None:
        self._cards[0].set_value("issid", value)

    @property
    def csid(self) -> typing.Optional[int]:
        """Get or set the Cross Section ID.
        """ # nopep8
        return self._cards[0].get_value("csid")

    @csid.setter
    def csid(self, value: int) -> None:
        self._cards[0].set_value("csid", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining preload stress versus time. When the loas curve ends or goes to zero, the initialization is assumed to be completed.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part set ID.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        self._cards[0].set_value("psid", value)

    @property
    def vid(self) -> typing.Optional[int]:
        """Get or set the Vector ID defining the direction normal to the cross section.  This vector must be defined if *DATABASE_CROSS_SECTION_SET is used to define the cross section.  If the cross section is defined using the PLANE option, the normal used in the definition of the plane is used if VID is left undefined.
        """ # nopep8
        return self._cards[0].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        self._cards[0].set_value("vid", value)

    @property
    def izshear(self) -> int:
        """Get or set the Shear stress flag:
        EQ.0: Shear stresses are prescribed as zero during the time the
        curve is acting to prescribe normal stress.
        EQ.1: Shear stresses are allowed to develop during the time the
        curve is acting to prescribe normal stress.
        For implicit the section can also take bending and is identical to 2;
        EQ 2: Shear and bending stresses are allowed to develop
        during the time the curve is acting to prescribe normal stress
        """ # nopep8
        return self._cards[0].get_value("izshear")

    @izshear.setter
    def izshear(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""izshear must be one of {0,1,2}""")
        self._cards[0].set_value("izshear", value)

    @property
    def istiff(self) -> int:
        """Get or set the Artificial stiffness. Simulates additional linearly elastic "ghost" elements in the cross section.
        These elements prevent mesh distortion by stiffening up the structure.
        GT.0:	load curve ID defining stiffness fraction as a function of time.
        The stiffness of the ghost elements is the load curve value times the stiffness of the material in the part.
        Since the ghost element stress counteracts the preload stress the fraction should be low(1% or less).
        The total section stress is the preload stress minus the ghost element stress.
        LT.0 : |ISTIFF| is the load curve ID for the stiffness fraction as a function of time.
        The preload stress is here automatically adjusted(-/+10 % of original prestress values) such that the total section stress corresponds to the curve in LCID.
        """ # nopep8
        return self._cards[0].get_value("istiff")

    @istiff.setter
    def istiff(self, value: int) -> None:
        self._cards[0].set_value("istiff", value)

