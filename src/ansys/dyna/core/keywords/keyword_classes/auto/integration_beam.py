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

class IntegrationBeam(KeywordBase):
    """DYNA INTEGRATION_BEAM keyword"""

    keyword = "INTEGRATION"
    subkeyword = "BEAM"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "irid",
                        int,
                        0,
                        10,
                        kwargs.get("irid")
                    ),
                    Field(
                        "nip",
                        int,
                        10,
                        10,
                        kwargs.get("nip", 0)
                    ),
                    Field(
                        "ra",
                        float,
                        20,
                        10,
                        kwargs.get("ra", 0.0)
                    ),
                    Field(
                        "icst",
                        int,
                        30,
                        10,
                        kwargs.get("icst", 0)
                    ),
                    Field(
                        "k",
                        int,
                        40,
                        10,
                        kwargs.get("k", 1)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "d1",
                        float,
                        0,
                        10,
                        kwargs.get("d1")
                    ),
                    Field(
                        "d2",
                        float,
                        10,
                        10,
                        kwargs.get("d2")
                    ),
                    Field(
                        "d3",
                        float,
                        20,
                        10,
                        kwargs.get("d3")
                    ),
                    Field(
                        "d4",
                        float,
                        30,
                        10,
                        kwargs.get("d4")
                    ),
                    Field(
                        "sref",
                        float,
                        40,
                        10,
                        kwargs.get("sref", 0.0)
                    ),
                    Field(
                        "tref",
                        float,
                        50,
                        10,
                        kwargs.get("tref", 0.0)
                    ),
                    Field(
                        "d5",
                        float,
                        60,
                        10,
                        kwargs.get("d5")
                    ),
                    Field(
                        "d6",
                        float,
                        70,
                        10,
                        kwargs.get("d6")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "s",
                        float,
                        0,
                        10,
                        kwargs.get("s")
                    ),
                    Field(
                        "t",
                        float,
                        10,
                        10,
                        kwargs.get("t")
                    ),
                    Field(
                        "wf",
                        float,
                        20,
                        10,
                        kwargs.get("wf")
                    ),
                    Field(
                        "pid",
                        int,
                        30,
                        10,
                        kwargs.get("pid")
                    ),
                ],
            ),
        ]

    @property
    def irid(self) -> typing.Optional[int]:
        """Get or set the Integration rule ID. (IRID refers to IRID on *SECTION_BEAM card).
        """ # nopep8
        return self._cards[0].get_value("irid")

    @irid.setter
    def irid(self, value: int) -> None:
        self._cards[0].set_value("irid", value)

    @property
    def nip(self) -> int:
        """Get or set the Number of integration points, see also ICST.
        """ # nopep8
        return self._cards[0].get_value("nip")

    @nip.setter
    def nip(self, value: int) -> None:
        self._cards[0].set_value("nip", value)

    @property
    def ra(self) -> float:
        """Get or set the Relative area of cross section, i.e., the actual cross-sectional area divided by the area defined by the product of the specified thickness in the s direction and the thickness in the t direction. See also ICST below.
        """ # nopep8
        return self._cards[0].get_value("ra")

    @ra.setter
    def ra(self, value: float) -> None:
        self._cards[0].set_value("ra", value)

    @property
    def icst(self) -> int:
        """Get or set the Standard cross section type, ICST. If this type is nonzero then NIP and the relative area above should be input as zero.
        EQ.1: W-section,
        EQ.2: C-section,
        EQ.3: Angle section,
        EQ.4: T-section,
        EQ.5: Rectangular tubing,
        EQ.6: Z-section,
        EQ.7: Trapezoidal section.
        For further information see Users Manual section 17.1.
        """ # nopep8
        return self._cards[0].get_value("icst")

    @icst.setter
    def icst(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22]:
            raise Exception("""icst must be one of {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22}""")
        self._cards[0].set_value("icst", value)

    @property
    def k(self) -> int:
        """Get or set the Integration refinement factor
        """ # nopep8
        return self._cards[0].get_value("k")

    @k.setter
    def k(self, value: int) -> None:
        self._cards[0].set_value("k", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Cross-section dimensions.
        """ # nopep8
        return self._cards[1].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[1].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Cross-section dimensions.
        """ # nopep8
        return self._cards[1].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[1].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Cross-section dimensions.
        """ # nopep8
        return self._cards[1].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[1].set_value("d3", value)

    @property
    def d4(self) -> typing.Optional[float]:
        """Get or set the Cross-section dimensions.
        """ # nopep8
        return self._cards[1].get_value("d4")

    @d4.setter
    def d4(self, value: float) -> None:
        self._cards[1].set_value("d4", value)

    @property
    def sref(self) -> float:
        """Get or set the sref, location of reference surface normal to s, for the Hughes-Liu beam only. This option is only useful if the beam is connected to a shell or another beam on its outer surface, see also *SECTION_BEAM.
        """ # nopep8
        return self._cards[1].get_value("sref")

    @sref.setter
    def sref(self, value: float) -> None:
        self._cards[1].set_value("sref", value)

    @property
    def tref(self) -> float:
        """Get or set the tref, location of reference surface normal to t, for the Hughes-Liu beam only. This option is only useful if the beam is connected to a shell or another beam on its outer surface, see also *SECTION_BEAM.
        """ # nopep8
        return self._cards[1].get_value("tref")

    @tref.setter
    def tref(self, value: float) -> None:
        self._cards[1].set_value("tref", value)

    @property
    def d5(self) -> typing.Optional[float]:
        """Get or set the Cross-section dimensions.
        """ # nopep8
        return self._cards[1].get_value("d5")

    @d5.setter
    def d5(self, value: float) -> None:
        self._cards[1].set_value("d5", value)

    @property
    def d6(self) -> typing.Optional[float]:
        """Get or set the Cross-section dimensions.
        """ # nopep8
        return self._cards[1].get_value("d6")

    @d6.setter
    def d6(self, value: float) -> None:
        self._cards[1].set_value("d6", value)

    @property
    def s(self) -> typing.Optional[float]:
        """Get or set the Normalized s coordinate of integration point, -1.0 <= s <= 1.0.
        """ # nopep8
        return self._cards[2].get_value("s")

    @s.setter
    def s(self, value: float) -> None:
        self._cards[2].set_value("s", value)

    @property
    def t(self) -> typing.Optional[float]:
        """Get or set the Normalized t coordinate of integration point, -1.0 <= t <= 1.0.
        """ # nopep8
        return self._cards[2].get_value("t")

    @t.setter
    def t(self, value: float) -> None:
        self._cards[2].set_value("t", value)

    @property
    def wf(self) -> typing.Optional[float]:
        """Get or set the Weighting factor, Ari, i.e., the area associated with the integration point divided by actual cross sectional area Ari = Ai/A.
        """ # nopep8
        return self._cards[2].get_value("wf")

    @wf.setter
    def wf(self, value: float) -> None:
        self._cards[2].set_value("wf", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Optional PID, used to identify material properties for this integration point.  If zero, the  master  PID (referenced on *ELEMENT) will be used.
        """ # nopep8
        return self._cards[2].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[2].set_value("pid", value)

