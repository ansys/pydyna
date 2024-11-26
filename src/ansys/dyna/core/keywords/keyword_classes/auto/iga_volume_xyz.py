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

class IgaVolumeXyz(KeywordBase):
    """DYNA IGA_VOLUME_XYZ keyword"""

    keyword = "IGA"
    subkeyword = "VOLUME_XYZ"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "vid",
                        int,
                        0,
                        10,
                        kwargs.get("vid")
                    ),
                    Field(
                        "nid",
                        int,
                        10,
                        10,
                        kwargs.get("nid")
                    ),
                    Field(
                        "psid",
                        int,
                        20,
                        10,
                        kwargs.get("psid")
                    ),
                    Field(
                        "esid",
                        int,
                        30,
                        10,
                        kwargs.get("esid")
                    ),
                    Field(
                        "fsid",
                        int,
                        40,
                        10,
                        kwargs.get("fsid")
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
    def vid(self) -> typing.Optional[int]:
        """Get or set the Parametric volume ID. A unique number must be chosen.
        """ # nopep8
        return self._cards[0].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        self._cards[0].set_value("vid", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Physical trivariate NURBS IDs, see *IGA_3D_NURBS_XYZ.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[0].set_value("nid", value)

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Parametric point set ID, see *IGA_POINT_UVW and *SET_IGA_POINT_UVW, see Remark 2.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        self._cards[0].set_value("psid", value)

    @property
    def esid(self) -> typing.Optional[int]:
        """Get or set the Parametric edge set ID, see *IGA_EDGE_UVW and *SET_IGA_EDGE_UVW, see Remark 2.
        """ # nopep8
        return self._cards[0].get_value("esid")

    @esid.setter
    def esid(self, value: int) -> None:
        self._cards[0].set_value("esid", value)

    @property
    def fsid(self) -> typing.Optional[int]:
        """Get or set the Parametric face set ID, see *IGA_FACE_UVW and *SET_IGA_FACE_UVW, see Remark 2.
        """ # nopep8
        return self._cards[0].get_value("fsid")

    @fsid.setter
    def fsid(self, value: int) -> None:
        self._cards[0].set_value("fsid", value)

    @property
    def brid1(self) -> typing.Optional[int]:
        """Get or set the Two-dimensional boundary representation IDs, see *IGA_2D_BREP, with i = 1, nand n > 0.
        """ # nopep8
        return self._cards[1].get_value("brid1")

    @brid1.setter
    def brid1(self, value: int) -> None:
        self._cards[1].set_value("brid1", value)

    @property
    def brid2(self) -> typing.Optional[int]:
        """Get or set the Two-dimensional boundary representation IDs, see *IGA_2D_BREP, with i = 1, nand n > 0.
        """ # nopep8
        return self._cards[1].get_value("brid2")

    @brid2.setter
    def brid2(self, value: int) -> None:
        self._cards[1].set_value("brid2", value)

    @property
    def brid3(self) -> typing.Optional[int]:
        """Get or set the Two-dimensional boundary representation IDs, see *IGA_2D_BREP, with i = 1, nand n > 0.
        """ # nopep8
        return self._cards[1].get_value("brid3")

    @brid3.setter
    def brid3(self, value: int) -> None:
        self._cards[1].set_value("brid3", value)

    @property
    def brid4(self) -> typing.Optional[int]:
        """Get or set the Two-dimensional boundary representation IDs, see *IGA_2D_BREP, with i = 1, nand n > 0.
        """ # nopep8
        return self._cards[1].get_value("brid4")

    @brid4.setter
    def brid4(self, value: int) -> None:
        self._cards[1].set_value("brid4", value)

    @property
    def brid5(self) -> typing.Optional[int]:
        """Get or set the Two-dimensional boundary representation IDs, see *IGA_2D_BREP, with i = 1, nand n > 0.
        """ # nopep8
        return self._cards[1].get_value("brid5")

    @brid5.setter
    def brid5(self, value: int) -> None:
        self._cards[1].set_value("brid5", value)

    @property
    def brid6(self) -> typing.Optional[int]:
        """Get or set the Two-dimensional boundary representation IDs, see *IGA_2D_BREP, with i = 1, nand n > 0.
        """ # nopep8
        return self._cards[1].get_value("brid6")

    @brid6.setter
    def brid6(self, value: int) -> None:
        self._cards[1].set_value("brid6", value)

    @property
    def brid7(self) -> typing.Optional[int]:
        """Get or set the Two-dimensional boundary representation IDs, see *IGA_2D_BREP, with i = 1, nand n > 0.
        """ # nopep8
        return self._cards[1].get_value("brid7")

    @brid7.setter
    def brid7(self, value: int) -> None:
        self._cards[1].set_value("brid7", value)

    @property
    def brid8(self) -> typing.Optional[int]:
        """Get or set the Two-dimensional boundary representation IDs, see *IGA_2D_BREP, with i = 1, nand n > 0.
        """ # nopep8
        return self._cards[1].get_value("brid8")

    @brid8.setter
    def brid8(self, value: int) -> None:
        self._cards[1].set_value("brid8", value)

