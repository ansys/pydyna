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

class PartCompositeLong(KeywordBase):
    """DYNA PART_COMPOSITE_LONG keyword"""

    keyword = "PART"
    subkeyword = "COMPOSITE_LONG"

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
                        kwargs.get("elform", 2)
                    ),
                    Field(
                        "shrf",
                        float,
                        20,
                        10,
                        kwargs.get("shrf")
                    ),
                    Field(
                        "nloc",
                        float,
                        30,
                        10,
                        kwargs.get("nloc", 0.0)
                    ),
                    Field(
                        "marea",
                        float,
                        40,
                        10,
                        kwargs.get("marea", 0)
                    ),
                    Field(
                        "hgid",
                        int,
                        50,
                        10,
                        kwargs.get("hgid", 0)
                    ),
                    Field(
                        "adpopt",
                        int,
                        60,
                        10,
                        kwargs.get("adpopt", 0)
                    ),
                    Field(
                        "thshel",
                        int,
                        70,
                        10,
                        kwargs.get("thshel", 0)
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
                        "plyid",
                        int,
                        40,
                        10,
                        kwargs.get("plyid")
                    ),
                    Field(
                        "shrfac",
                        float,
                        50,
                        10,
                        kwargs.get("shrfac")
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        70,
                        10,
                        kwargs.get("unused")
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
        """Get or set the Element formulation options, see Remarks 1 and 2 below:
        EQ.1:  Hughes-Liu,
        EQ.2:  Belytschko-Tsay,
        EQ.3:  BCIZ triangular shell,
        EQ.4:  C0 triangular shell,
        EQ.6:  S/R Hughes-Liu,
        EQ.7:   S/R co-rotational Hughes-Liu,
        EQ.8:   Belytschko-Leviathan shell,
        EQ.9:   Fully integrated Belytschko-Tsay membrane,
        EQ.10: Belytschko-Wong-Chiang,
        EQ.11: Fast (co-rotational) Hughes-Liu,
        EQ.16:  Fully integrated shell element (very fast)
        EQ.-16: Fully integrated shell element modified for higher accuracy
        """ # nopep8
        return self._cards[1].get_value("elform")

    @elform.setter
    def elform(self, value: int) -> None:
        if value not in [2, 1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 16, -16]:
            raise Exception("""elform must be one of {2,1,3,4,5,6,7,8,9,10,11,16,-16}""")
        self._cards[1].set_value("elform", value)

    @property
    def shrf(self) -> typing.Optional[float]:
        """Get or set the Shear correction factor which scales the transverse shear stress.  The shell formulations in LS-DYNA, with the exception of the BCIZ and DK elements, are based on a first order shear deformation theory that yields constant transverse shear strains which violates the condition of zero traction on the top and bottom surfaces of the shell.  The shear correction factor is attempt to compensate for this error.
        """ # nopep8
        return self._cards[1].get_value("shrf")

    @shrf.setter
    def shrf(self, value: float) -> None:
        self._cards[1].set_value("shrf", value)

    @property
    def nloc(self) -> float:
        """Get or set the Location of reference surface for three dimensional shell elements.  If nonzero, the mid-surface of the shell is offset by a value equal to  .  Alternatively, the offset can be specified by using the OFFSET option in the *ELEMENT_SHELL input section.
        EQ. 1.0:  top surface,
        EQ. 0.0:  mid-surface (default),
        EQ.-1.0:  bottom surface..
        """ # nopep8
        return self._cards[1].get_value("nloc")

    @nloc.setter
    def nloc(self, value: float) -> None:
        self._cards[1].set_value("nloc", value)

    @property
    def marea(self) -> float:
        """Get or set the Non-structural mass per unit area.  This is additional mass which comes from materials such as carpeting.  This mass is not directly included in the time step calculation.
        """ # nopep8
        return self._cards[1].get_value("marea")

    @marea.setter
    def marea(self, value: float) -> None:
        self._cards[1].set_value("marea", value)

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
    def adpopt(self) -> int:
        """Get or set the Indicate if this part is adapted or not. See also *CONTROL_ADAPTIVITY.
        EQ.0: no adaptivity (default),
        EQ.1: H-adaptive for 3D shells,
        EQ.2: R-adaptive remeshing for 2D shells.
        """ # nopep8
        return self._cards[1].get_value("adpopt")

    @adpopt.setter
    def adpopt(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""adpopt must be one of {0,1,2}""")
        self._cards[1].set_value("adpopt", value)

    @property
    def thshel(self) -> int:
        """Get or set the Thermal shell formulation:
        EQ.0 Default\nEQ.1 Thick thermal shell
        EQ. 2 Thin thermal shell
        """ # nopep8
        return self._cards[1].get_value("thshel")

    @thshel.setter
    def thshel(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""thshel must be one of {0,1,2}""")
        self._cards[1].set_value("thshel", value)

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
    def plyid(self) -> typing.Optional[int]:
        """Get or set the Ply ID of integration point i (for post-processing purposes)
        """ # nopep8
        return self._cards[2].get_value("plyid")

    @plyid.setter
    def plyid(self, value: int) -> None:
        self._cards[2].set_value("plyid", value)

    @property
    def shrfac(self) -> typing.Optional[float]:
        """Get or set the Transverse shear scale factor
        """ # nopep8
        return self._cards[2].get_value("shrfac")

    @shrfac.setter
    def shrfac(self, value: float) -> None:
        self._cards[2].set_value("shrfac", value)

