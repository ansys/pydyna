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

class AleMapping(KeywordBase):
    """DYNA ALE_MAPPING keyword"""

    keyword = "ALE"
    subkeyword = "MAPPING"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ammsid",
                        int,
                        0,
                        10,
                        kwargs.get("ammsid")
                    ),
                    Field(
                        "rw",
                        int,
                        10,
                        10,
                        kwargs.get("rw", -1)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ntim",
                        int,
                        0,
                        10,
                        kwargs.get("ntim", 1)
                    ),
                    Field(
                        "tbeg",
                        float,
                        10,
                        10,
                        kwargs.get("tbeg", 0.0)
                    ),
                    Field(
                        "tend",
                        float,
                        20,
                        10,
                        kwargs.get("tend")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "vecid",
                        int,
                        0,
                        10,
                        kwargs.get("vecid")
                    ),
                    Field(
                        "angle",
                        float,
                        10,
                        10,
                        kwargs.get("angle", 0.0)
                    ),
                    Field(
                        "xp",
                        float,
                        20,
                        10,
                        kwargs.get("xp", 0.0)
                    ),
                    Field(
                        "yp",
                        float,
                        30,
                        10,
                        kwargs.get("yp", 0.0)
                    ),
                    Field(
                        "zp",
                        float,
                        40,
                        10,
                        kwargs.get("zp", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "id",
                        int,
                        0,
                        10,
                        kwargs.get("id")
                    ),
                    Field(
                        "type",
                        int,
                        10,
                        10,
                        kwargs.get("type", 0)
                    ),
                    Field(
                        "nvol",
                        int,
                        20,
                        10,
                        kwargs.get("nvol", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "voltyp",
                        int,
                        0,
                        10,
                        kwargs.get("voltyp")
                    ),
                    Field(
                        "vecid1",
                        int,
                        10,
                        10,
                        kwargs.get("vecid1", 0)
                    ),
                    Field(
                        "dw1",
                        float,
                        20,
                        10,
                        kwargs.get("dw1", 0.0)
                    ),
                    Field(
                        "xl",
                        float,
                        30,
                        10,
                        kwargs.get("xl", 0.0)
                    ),
                    Field(
                        "yl",
                        float,
                        40,
                        10,
                        kwargs.get("yl", 0.0)
                    ),
                    Field(
                        "zl",
                        float,
                        50,
                        10,
                        kwargs.get("zl", 0.0)
                    ),
                    Field(
                        "dw2",
                        float,
                        60,
                        10,
                        kwargs.get("dw2", 0.0)
                    ),
                    Field(
                        "dv2",
                        float,
                        70,
                        10,
                        kwargs.get("dv2", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def ammsid(self) -> typing.Optional[int]:
        """Get or set the Set ID of ALE multi-material groups defined in *SET_‌MULTI-MATERIAL_‌GROUP. See Remark 3.
        """ # nopep8
        return self._cards[0].get_value("ammsid")

    @ammsid.setter
    def ammsid(self, value: int) -> None:
        self._cards[0].set_value("ammsid", value)

    @property
    def rw(self) -> int:
        """Get or set the Flag defining if the keyword reads or writes in the mapping:
        EQ.-1: write in the mapping file. See Remark 4.
        GT.0:	read from the mapping file. |RW| defines the rank of *ALE_MAPPING that wrote in the
        mapping file in the previous run if several keywords contributed to the file creation.
        If there was only one keyword (most of the cases), RW=1. See Remark 4..
        """ # nopep8
        return self._cards[0].get_value("rw")

    @rw.setter
    def rw(self, value: int) -> None:
        self._cards[0].set_value("rw", value)

    @property
    def ntim(self) -> int:
        """Get or set the For RW = -1:
        Number of times to write in the mapping file between the times TBEG and TEND. See Remark 5.
        For RW > 0:
        Rank of the data to be read if, during the previous run, a keyword *ALE_MAPPING with RW=-1 wrote several times in the mapping file.
        If there was only one output (most of the cases), NTIM=1. See Remark 5.
        """ # nopep8
        return self._cards[1].get_value("ntim")

    @ntim.setter
    def ntim(self, value: int) -> None:
        self._cards[1].set_value("ntim", value)

    @property
    def tbeg(self) -> float:
        """Get or set the For RW = -1:
        Time to start writing in the mapping file (TBEG=ENDTIM by default). See Remark 5.
        For RW > 0:
        Time to map the data from the mapping file (TBEG=0.0 by default). See Remark 5.
        """ # nopep8
        return self._cards[1].get_value("tbeg")

    @tbeg.setter
    def tbeg(self, value: float) -> None:
        self._cards[1].set_value("tbeg", value)

    @property
    def tend(self) -> typing.Optional[float]:
        """Get or set the For RW = -1:
        Time to stop writing in the mapping file. See Remark 5.
        For RW > 0:
        Ignored.
        """ # nopep8
        return self._cards[1].get_value("tend")

    @tend.setter
    def tend(self, value: float) -> None:
        self._cards[1].set_value("tend", value)

    @property
    def vecid(self) -> typing.Optional[int]:
        """Get or set the For RW = -1:
        Ignored
        For RW > 0:
        ID of the symmetric axis defined by *DEFINE_‌VECTOR.
        The 3 first parameters in *DEFINE_‌VECTOR defines the location of the origin of the previous run. See Remarks 6 and 7.
        """ # nopep8
        return self._cards[2].get_value("vecid")

    @vecid.setter
    def vecid(self, value: int) -> None:
        self._cards[2].set_value("vecid", value)

    @property
    def angle(self) -> float:
        """Get or set the For RW = -1:
        Ignored
        For RW > 0:
        Angle of rotation in degrees around an axis defined by *DEFINE_‌VECTOR for the 3D to 3D mapping. See Remark 7.
        """ # nopep8
        return self._cards[2].get_value("angle")

    @angle.setter
    def angle(self, value: float) -> None:
        self._cards[2].set_value("angle", value)

    @property
    def xp(self) -> float:
        """Get or set the For RW = -1:
        Ignored
        For RW > 0:
        -position of a point on a plane used by specific mappings (only for 2D plain strain to 3D mappings). See Remark 7.
        """ # nopep8
        return self._cards[2].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[2].set_value("xp", value)

    @property
    def yp(self) -> float:
        """Get or set the For RW = -1:
        Ignored
        For RW > 0:
        - position of a point on a plane used by specific mappings (only for 2D plain strain to 3D mappings). See Remark 7.
        """ # nopep8
        return self._cards[2].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[2].set_value("yp", value)

    @property
    def zp(self) -> float:
        """Get or set the For RW = -1:
        Ignored
        For RW > 0:
        - position of a point on a plane used by specific mappings (only for 2D plain strain to 3D mappings). See Remark 7.
        """ # nopep8
        return self._cards[2].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[2].set_value("zp", value)

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Part ID or part set ID or element set ID. See Remark 8.
        """ # nopep8
        return self._cards[3].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[3].set_value("id", value)

    @property
    def type(self) -> int:
        """Get or set the Type of “ID” (see Remark 8):
        EQ.0:	part set ID.
        EQ.1:	part ID.
        EQ.2:	element set ID.
        """ # nopep8
        return self._cards[3].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""type must be one of {0,1,2}""")
        self._cards[3].set_value("type", value)

    @property
    def nvol(self) -> int:
        """Get or set the Number of volumes in which the elements are selected for the mapping. See Remark 8.
        """ # nopep8
        return self._cards[3].get_value("nvol")

    @nvol.setter
    def nvol(self, value: int) -> None:
        self._cards[3].set_value("nvol", value)

    @property
    def voltyp(self) -> typing.Optional[int]:
        """Get or set the Type of volume containing the selected elements for the mapping.
        The absolute value of VOLTYP indicates the type of volume and the sign indicates whether the elements to be selected are in or out of the volume.
        The volume depends on geometrical lengths in a local coordinate system defined by orthonormal axis called ,  and . See Remarks 9,10,11,12 and 13.
        Volume Type
        |VOLTYP|.EQ.1:	Trapezoid 3D (See Figure 0-1).
        |VOLTYP|.EQ.2:	Elliptic truncated cone (See Figure 0-2).
        |VOLTYP|.EQ.3:	Ellipsoid (See Figure 0-3).
        In/Out
        VOLTYP.LT.0:	elements outside the volume are selected.
        VOLTYP.GT.0:	elements inside the volume are selected.
        """ # nopep8
        return self._cards[4].get_value("voltyp")

    @voltyp.setter
    def voltyp(self, value: int) -> None:
        self._cards[4].set_value("voltyp", value)

    @property
    def vecid1(self) -> int:
        """Get or set the ID of the local u-axis defined by *DEFINE_‌VECTOR. See Remark 10.
        """ # nopep8
        return self._cards[4].get_value("vecid1")

    @vecid1.setter
    def vecid1(self, value: int) -> None:
        self._cards[4].set_value("vecid1", value)

    @property
    def dw1(self) -> float:
        """Get or set the Length in the local w-axis direction. See Remark 11..
        """ # nopep8
        return self._cards[4].get_value("dw1")

    @dw1.setter
    def dw1(self, value: float) -> None:
        self._cards[4].set_value("dw1", value)

    @property
    def xl(self) -> float:
        """Get or set the Global -position of a point along a direction parallel to the -axis. See Remarks 10,11,12 and 13.
        """ # nopep8
        return self._cards[4].get_value("xl")

    @xl.setter
    def xl(self, value: float) -> None:
        self._cards[4].set_value("xl", value)

    @property
    def yl(self) -> float:
        """Get or set the Global -position of a point along a direction parallel to the -axis. See Remarks 10,11,12 and 13.
        """ # nopep8
        return self._cards[4].get_value("yl")

    @yl.setter
    def yl(self, value: float) -> None:
        self._cards[4].set_value("yl", value)

    @property
    def zl(self) -> float:
        """Get or set the Global -position of a point along a direction parallel to the -axis. See Remarks 10,11,12 and 13.
        """ # nopep8
        return self._cards[4].get_value("zl")

    @zl.setter
    def zl(self, value: float) -> None:
        self._cards[4].set_value("zl", value)

    @property
    def dw2(self) -> float:
        """Get or set the Length in the local w-axis direction (DW2=DW1 by default). See Remark 11.
        """ # nopep8
        return self._cards[4].get_value("dw2")

    @dw2.setter
    def dw2(self, value: float) -> None:
        self._cards[4].set_value("dw2", value)

    @property
    def dv2(self) -> float:
        """Get or set the Length in the local v-axis direction (DV2=DV1 by default). See Remark 12.
        """ # nopep8
        return self._cards[4].get_value("dv2")

    @dv2.setter
    def dv2(self, value: float) -> None:
        self._cards[4].set_value("dv2", value)

