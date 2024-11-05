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

class IgaFaceXyz(KeywordBase):
    """DYNA IGA_FACE_XYZ keyword"""

    keyword = "IGA"
    subkeyword = "FACE_XYZ"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "fid",
                        int,
                        0,
                        10,
                        kwargs.get("fid")
                    ),
                    Field(
                        "nid",
                        int,
                        10,
                        10,
                        kwargs.get("nid")
                    ),
                    Field(
                        "ori",
                        int,
                        20,
                        10,
                        kwargs.get("ori", 0)
                    ),
                    Field(
                        "psid",
                        int,
                        30,
                        10,
                        kwargs.get("psid")
                    ),
                    Field(
                        "esid",
                        int,
                        40,
                        10,
                        kwargs.get("esid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "brid1",
                        int,
                        0,
                        10,
                        kwargs.get("brid1")
                    ),
                    Field(
                        "brid2",
                        int,
                        10,
                        10,
                        kwargs.get("brid2")
                    ),
                    Field(
                        "brid3",
                        int,
                        20,
                        10,
                        kwargs.get("brid3")
                    ),
                    Field(
                        "brid4",
                        int,
                        30,
                        10,
                        kwargs.get("brid4")
                    ),
                    Field(
                        "brid5",
                        int,
                        40,
                        10,
                        kwargs.get("brid5")
                    ),
                    Field(
                        "brid6",
                        int,
                        50,
                        10,
                        kwargs.get("brid6")
                    ),
                    Field(
                        "brid7",
                        int,
                        60,
                        10,
                        kwargs.get("brid7")
                    ),
                    Field(
                        "brid8",
                        int,
                        70,
                        10,
                        kwargs.get("brid8")
                    ),
                ],
            ),
        ]

    @property
    def fid(self) -> typing.Optional[int]:
        """Get or set the Physical face ID. A unique number must be chosen.
        """ # nopep8
        return self._cards[0].get_value("fid")

    @fid.setter
    def fid(self, value: int) -> None:
        self._cards[0].set_value("fid", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Physical bivariate NURBS ID, see *IGA_2D_NURBS_XYZ.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[0].set_value("nid", value)

    @property
    def ori(self) -> int:
        """Get or set the Orientation with respect to the physical bivariate NURBS.
        EQ.0: Same
        EQ.1 : Reversed.
        """ # nopep8
        return self._cards[0].get_value("ori")

    @ori.setter
    def ori(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ori must be one of {0,1}""")
        self._cards[0].set_value("ori", value)

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Parametric point set ID, see *IGA_POINT_UVW, *SET_IGA_POINT_UVW.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        self._cards[0].set_value("psid", value)

    @property
    def esid(self) -> typing.Optional[int]:
        """Get or set the Parametric edge set ID, see *IGA_EDGE_UVW, *SET_IGA_EDGE_UVW.
        """ # nopep8
        return self._cards[0].get_value("esid")

    @esid.setter
    def esid(self, value: int) -> None:
        self._cards[0].set_value("esid", value)

    @property
    def brid1(self) -> typing.Optional[int]:
        """Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
        """ # nopep8
        return self._cards[1].get_value("brid1")

    @brid1.setter
    def brid1(self, value: int) -> None:
        self._cards[1].set_value("brid1", value)

    @property
    def brid2(self) -> typing.Optional[int]:
        """Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
        """ # nopep8
        return self._cards[1].get_value("brid2")

    @brid2.setter
    def brid2(self, value: int) -> None:
        self._cards[1].set_value("brid2", value)

    @property
    def brid3(self) -> typing.Optional[int]:
        """Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
        """ # nopep8
        return self._cards[1].get_value("brid3")

    @brid3.setter
    def brid3(self, value: int) -> None:
        self._cards[1].set_value("brid3", value)

    @property
    def brid4(self) -> typing.Optional[int]:
        """Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
        """ # nopep8
        return self._cards[1].get_value("brid4")

    @brid4.setter
    def brid4(self, value: int) -> None:
        self._cards[1].set_value("brid4", value)

    @property
    def brid5(self) -> typing.Optional[int]:
        """Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
        """ # nopep8
        return self._cards[1].get_value("brid5")

    @brid5.setter
    def brid5(self, value: int) -> None:
        self._cards[1].set_value("brid5", value)

    @property
    def brid6(self) -> typing.Optional[int]:
        """Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
        """ # nopep8
        return self._cards[1].get_value("brid6")

    @brid6.setter
    def brid6(self, value: int) -> None:
        self._cards[1].set_value("brid6", value)

    @property
    def brid7(self) -> typing.Optional[int]:
        """Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
        """ # nopep8
        return self._cards[1].get_value("brid7")

    @brid7.setter
    def brid7(self, value: int) -> None:
        self._cards[1].set_value("brid7", value)

    @property
    def brid8(self) -> typing.Optional[int]:
        """Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
        """ # nopep8
        return self._cards[1].get_value("brid8")

    @brid8.setter
    def brid8(self, value: int) -> None:
        self._cards[1].set_value("brid8", value)

