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

class PartCompositeTshell(KeywordBase):
    """DYNA PART_COMPOSITE_TSHELL keyword"""

    keyword = "PART"
    subkeyword = "COMPOSITE_TSHELL"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
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
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        10,
                        kwargs.get("pid")
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
                        "unused",
                        float,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        float,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "hgid",
                        int,
                        50,
                        10,
                        kwargs.get("hgid", 0)
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        kwargs.get("unused")
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
            Card(
                [
                    Field(
                        "mid1",
                        int,
                        0,
                        10,
                        kwargs.get("mid1")
                    ),
                    Field(
                        "thick1",
                        float,
                        10,
                        10,
                        kwargs.get("thick1")
                    ),
                    Field(
                        "b1",
                        float,
                        20,
                        10,
                        kwargs.get("b1")
                    ),
                    Field(
                        "tmid1",
                        int,
                        30,
                        10,
                        kwargs.get("tmid1")
                    ),
                    Field(
                        "mid2",
                        int,
                        40,
                        10,
                        kwargs.get("mid2")
                    ),
                    Field(
                        "thick2",
                        float,
                        50,
                        10,
                        kwargs.get("thick2")
                    ),
                    Field(
                        "b2",
                        float,
                        60,
                        10,
                        kwargs.get("b2")
                    ),
                    Field(
                        "tmid2",
                        int,
                        70,
                        10,
                        kwargs.get("tmid2")
                    ),
                ],
            ),
        ]

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Heading for the part.
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[0].set_value("title", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID.
        """ # nopep8
        return self._cards[1].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[1].set_value("pid", value)

    @property
    def elform(self) -> int:
        """Get or set the Element formulation:
        EQ.1: one point reduced integration (default),
        EQ.2: selective reduced 2x2 in plane integration.
        EQ.3: assumed strain 2x2 in plane integration.
        EQ.5:  assumed strain reduced integration.
        """ # nopep8
        return self._cards[1].get_value("elform")

    @elform.setter
    def elform(self, value: int) -> None:
        if value not in [1, 2, 3, 5]:
            raise Exception("""elform must be one of {1,2,3,5}""")
        self._cards[1].set_value("elform", value)

    @property
    def shrf(self) -> float:
        """Get or set the Shear correction factor which scales the transverse shear stress.  The shell formulations in LS-DYNA, with the exception of the BCIZ and DK elements, are based on a first order shear deformation theory that yields constant transverse shear strains which violates the condition of zero traction on the top and bottom surfaces of the shell.  The shear correction factor is attempt to compensate for this error.
        """ # nopep8
        return self._cards[1].get_value("shrf")

    @shrf.setter
    def shrf(self, value: float) -> None:
        self._cards[1].set_value("shrf", value)

    @property
    def hgid(self) -> int:
        """Get or set the Hourglass/bulk viscosity identification defined in the *HOURGLASS Section:
        EQ.0:  default values are used..
        """ # nopep8
        return self._cards[1].get_value("hgid")

    @hgid.setter
    def hgid(self, value: int) -> None:
        self._cards[1].set_value("hgid", value)

    @property
    def tshear(self) -> int:
        """Get or set the Flag for transverse shear strain distribution (see remarks 3 and 4):
        EQ.0: Parabolic,
        EQ.1: Constant through thickness
        """ # nopep8
        return self._cards[1].get_value("tshear")

    @tshear.setter
    def tshear(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""tshear must be one of {0,1}""")
        self._cards[1].set_value("tshear", value)

    @property
    def mid1(self) -> typing.Optional[int]:
        """Get or set the Material ID of integration point i, see *MAT_? Section
        """ # nopep8
        return self._cards[2].get_value("mid1")

    @mid1.setter
    def mid1(self, value: int) -> None:
        self._cards[2].set_value("mid1", value)

    @property
    def thick1(self) -> typing.Optional[float]:
        """Get or set the Thickness of integration point .
        """ # nopep8
        return self._cards[2].get_value("thick1")

    @thick1.setter
    def thick1(self, value: float) -> None:
        self._cards[2].set_value("thick1", value)

    @property
    def b1(self) -> typing.Optional[float]:
        """Get or set the Material angle of integration point i.
        """ # nopep8
        return self._cards[2].get_value("b1")

    @b1.setter
    def b1(self, value: float) -> None:
        self._cards[2].set_value("b1", value)

    @property
    def tmid1(self) -> typing.Optional[int]:
        """Get or set the Thermal ID
        """ # nopep8
        return self._cards[2].get_value("tmid1")

    @tmid1.setter
    def tmid1(self, value: int) -> None:
        self._cards[2].set_value("tmid1", value)

    @property
    def mid2(self) -> typing.Optional[int]:
        """Get or set the Material ID of integration point i, see *MAT_? Section
        """ # nopep8
        return self._cards[2].get_value("mid2")

    @mid2.setter
    def mid2(self, value: int) -> None:
        self._cards[2].set_value("mid2", value)

    @property
    def thick2(self) -> typing.Optional[float]:
        """Get or set the Thickness of integration point
        """ # nopep8
        return self._cards[2].get_value("thick2")

    @thick2.setter
    def thick2(self, value: float) -> None:
        self._cards[2].set_value("thick2", value)

    @property
    def b2(self) -> typing.Optional[float]:
        """Get or set the Material angle of integration point i
        """ # nopep8
        return self._cards[2].get_value("b2")

    @b2.setter
    def b2(self, value: float) -> None:
        self._cards[2].set_value("b2", value)

    @property
    def tmid2(self) -> typing.Optional[int]:
        """Get or set the Thermal ID
        """ # nopep8
        return self._cards[2].get_value("tmid2")

    @tmid2.setter
    def tmid2(self, value: int) -> None:
        self._cards[2].set_value("tmid2", value)

