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

class AlePrescribedMotion(KeywordBase):
    """DYNA ALE_PRESCRIBED_MOTION keyword"""

    keyword = "ALE"
    subkeyword = "PRESCRIBED_MOTION"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "mmsid",
                        int,
                        0,
                        10,
                        kwargs.get("mmsid")
                    ),
                    Field(
                        "inside",
                        int,
                        10,
                        10,
                        kwargs.get("inside", 0)
                    ),
                    Field(
                        "sidr",
                        int,
                        20,
                        10,
                        kwargs.get("sidr", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcvtx",
                        int,
                        0,
                        10,
                        kwargs.get("lcvtx")
                    ),
                    Field(
                        "lcvty",
                        int,
                        10,
                        10,
                        kwargs.get("lcvty")
                    ),
                    Field(
                        "lcvtz",
                        int,
                        20,
                        10,
                        kwargs.get("lcvtz")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcvrx",
                        int,
                        0,
                        10,
                        kwargs.get("lcvrx")
                    ),
                    Field(
                        "lcvry",
                        int,
                        10,
                        10,
                        kwargs.get("lcvry")
                    ),
                    Field(
                        "lcvrz",
                        int,
                        20,
                        10,
                        kwargs.get("lcvrz")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xg",
                        float,
                        0,
                        10,
                        kwargs.get("xg")
                    ),
                    Field(
                        "yg",
                        float,
                        10,
                        10,
                        kwargs.get("yg")
                    ),
                    Field(
                        "zg",
                        float,
                        20,
                        10,
                        kwargs.get("zg")
                    ),
                ],
            ),
        ]

    @property
    def mmsid(self) -> typing.Optional[int]:
        """Get or set the Multi-Material Set ID (see *SET_‌MULTI-MATERIAL_‌GROUP_‌LIST).
        """ # nopep8
        return self._cards[0].get_value("mmsid")

    @mmsid.setter
    def mmsid(self, value: int) -> None:
        self._cards[0].set_value("mmsid", value)

    @property
    def inside(self) -> int:
        """Get or set the Flag to define which nodes the motion is prescribed for (see Remark 2):
        EQ.0:	Nodes connected to at least one ALE element that is at the minimum partially filled by a group of MMSID
        EQ.1:	Nodes connected to at least one ALE element that is fully filled by a group of MMSID
        EQ.2:	Nodes only connected to ALE elements that are fully filled by a group of MMSID.
        """ # nopep8
        return self._cards[0].get_value("inside")

    @inside.setter
    def inside(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""inside must be one of {0,1,2}""")
        self._cards[0].set_value("inside", value)

    @property
    def sidr(self) -> int:
        """Get or set the Flag controlling the use of this keyword during dynamic relaxation.
        EQ.0:	the keyword is applied in normal analysis phase only,
        EQ.1:	the keyword is applied in dynamic relaxation phase but not the normal analysis phase,
        EQ.2:	the keyword is applied in both dynamic relaxation phase and normal analysis phase.
        """ # nopep8
        return self._cards[0].get_value("sidr")

    @sidr.setter
    def sidr(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""sidr must be one of {0,1,2}""")
        self._cards[0].set_value("sidr", value)

    @property
    def lcvtx(self) -> typing.Optional[int]:
        """Get or set the Curve IDs for the translation in each global direction; see *DEFINE_‌CURVE.  See Remark 3.
        """ # nopep8
        return self._cards[1].get_value("lcvtx")

    @lcvtx.setter
    def lcvtx(self, value: int) -> None:
        self._cards[1].set_value("lcvtx", value)

    @property
    def lcvty(self) -> typing.Optional[int]:
        """Get or set the Curve IDs for the translation in each global direction; see *DEFINE_‌CURVE.  See Remark 3..
        """ # nopep8
        return self._cards[1].get_value("lcvty")

    @lcvty.setter
    def lcvty(self, value: int) -> None:
        self._cards[1].set_value("lcvty", value)

    @property
    def lcvtz(self) -> typing.Optional[int]:
        """Get or set the Curve IDs for the translation in each global direction; see *DEFINE_‌CURVE.  See Remark 3..
        """ # nopep8
        return self._cards[1].get_value("lcvtz")

    @lcvtz.setter
    def lcvtz(self, value: int) -> None:
        self._cards[1].set_value("lcvtz", value)

    @property
    def lcvrx(self) -> typing.Optional[int]:
        """Get or set the Curve IDs for the rotation around each global direction; see *DEFINE_‌CURVE.  See Remark 3.
        """ # nopep8
        return self._cards[2].get_value("lcvrx")

    @lcvrx.setter
    def lcvrx(self, value: int) -> None:
        self._cards[2].set_value("lcvrx", value)

    @property
    def lcvry(self) -> typing.Optional[int]:
        """Get or set the Curve IDs for the rotation around each global direction; see *DEFINE_‌CURVE.  See Remark 3.
        """ # nopep8
        return self._cards[2].get_value("lcvry")

    @lcvry.setter
    def lcvry(self, value: int) -> None:
        self._cards[2].set_value("lcvry", value)

    @property
    def lcvrz(self) -> typing.Optional[int]:
        """Get or set the Curve IDs for the rotation around each global direction; see *DEFINE_‌CURVE.  See Remark 3.
        """ # nopep8
        return self._cards[2].get_value("lcvrz")

    @lcvrz.setter
    def lcvrz(self, value: int) -> None:
        self._cards[2].set_value("lcvrz", value)

    @property
    def xg(self) -> typing.Optional[float]:
        """Get or set the Position of the rotation center. See Remark 4.
        """ # nopep8
        return self._cards[3].get_value("xg")

    @xg.setter
    def xg(self, value: float) -> None:
        self._cards[3].set_value("xg", value)

    @property
    def yg(self) -> typing.Optional[float]:
        """Get or set the Position of the rotation center. See Remark 4.
        """ # nopep8
        return self._cards[3].get_value("yg")

    @yg.setter
    def yg(self, value: float) -> None:
        self._cards[3].set_value("yg", value)

    @property
    def zg(self) -> typing.Optional[float]:
        """Get or set the Position of the rotation center. See Remark 4.
        """ # nopep8
        return self._cards[3].get_value("zg")

    @zg.setter
    def zg(self, value: float) -> None:
        self._cards[3].set_value("zg", value)

