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

class LoadBodyGeneralized(KeywordBase):
    """DYNA LOAD_BODY_GENERALIZED keyword"""

    keyword = "LOAD"
    subkeyword = "BODY_GENERALIZED"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "n1",
                        int,
                        0,
                        10,
                        kwargs.get("n1")
                    ),
                    Field(
                        "n2",
                        int,
                        10,
                        10,
                        kwargs.get("n2")
                    ),
                    Field(
                        "lcid",
                        int,
                        20,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "drlcid",
                        int,
                        30,
                        10,
                        kwargs.get("drlcid", 0)
                    ),
                    Field(
                        "xc",
                        float,
                        40,
                        10,
                        kwargs.get("xc", 0.0)
                    ),
                    Field(
                        "yc",
                        float,
                        50,
                        10,
                        kwargs.get("yc", 0.0)
                    ),
                    Field(
                        "zc",
                        float,
                        60,
                        10,
                        kwargs.get("zc", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ax",
                        float,
                        0,
                        10,
                        kwargs.get("ax", 0.0)
                    ),
                    Field(
                        "ay",
                        float,
                        10,
                        10,
                        kwargs.get("ay", 0.0)
                    ),
                    Field(
                        "az",
                        float,
                        20,
                        10,
                        kwargs.get("az", 0.0)
                    ),
                    Field(
                        "omx",
                        float,
                        30,
                        10,
                        kwargs.get("omx", 0.0)
                    ),
                    Field(
                        "omy",
                        float,
                        40,
                        10,
                        kwargs.get("omy", 0.0)
                    ),
                    Field(
                        "omz",
                        float,
                        50,
                        10,
                        kwargs.get("omz", 0.0)
                    ),
                    Field(
                        "cid",
                        int,
                        60,
                        10,
                        kwargs.get("cid", 0)
                    ),
                    Field(
                        "angtyp",
                        str,
                        70,
                        10,
                        kwargs.get("angtyp", "CENT")
                    ),
                ],
            ),
        ]

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Beginning node ID for body force load.
        """ # nopep8
        return self._cards[0].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        self._cards[0].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Ending node ID for body force load.
        """ # nopep8
        return self._cards[0].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        self._cards[0].set_value("n2", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def drlcid(self) -> int:
        """Get or set the Load curve ID for dynamic relaxation phase. Only if dynamic relaxation is defined.
        """ # nopep8
        return self._cards[0].get_value("drlcid")

    @drlcid.setter
    def drlcid(self, value: int) -> None:
        self._cards[0].set_value("drlcid", value)

    @property
    def xc(self) -> float:
        """Get or set the X-center of rotation. Define only for angular velocity.
        """ # nopep8
        return self._cards[0].get_value("xc")

    @xc.setter
    def xc(self, value: float) -> None:
        self._cards[0].set_value("xc", value)

    @property
    def yc(self) -> float:
        """Get or set the Y-center of rotation. Define only for angular velocity.
        """ # nopep8
        return self._cards[0].get_value("yc")

    @yc.setter
    def yc(self, value: float) -> None:
        self._cards[0].set_value("yc", value)

    @property
    def zc(self) -> float:
        """Get or set the Z-center of rotation. Define only for angular velocity.
        """ # nopep8
        return self._cards[0].get_value("zc")

    @zc.setter
    def zc(self, value: float) -> None:
        self._cards[0].set_value("zc", value)

    @property
    def ax(self) -> float:
        """Get or set the Scale factor for acceleration in x-direction.
        """ # nopep8
        return self._cards[1].get_value("ax")

    @ax.setter
    def ax(self, value: float) -> None:
        self._cards[1].set_value("ax", value)

    @property
    def ay(self) -> float:
        """Get or set the Scale factor for acceleration in y-direction.
        """ # nopep8
        return self._cards[1].get_value("ay")

    @ay.setter
    def ay(self, value: float) -> None:
        self._cards[1].set_value("ay", value)

    @property
    def az(self) -> float:
        """Get or set the Scale factor for acceleration in z-direction.
        """ # nopep8
        return self._cards[1].get_value("az")

    @az.setter
    def az(self, value: float) -> None:
        self._cards[1].set_value("az", value)

    @property
    def omx(self) -> float:
        """Get or set the Scale factor for x-angular velocity.
        """ # nopep8
        return self._cards[1].get_value("omx")

    @omx.setter
    def omx(self, value: float) -> None:
        self._cards[1].set_value("omx", value)

    @property
    def omy(self) -> float:
        """Get or set the Scale factor for y-angular velocity.
        """ # nopep8
        return self._cards[1].get_value("omy")

    @omy.setter
    def omy(self, value: float) -> None:
        self._cards[1].set_value("omy", value)

    @property
    def omz(self) -> float:
        """Get or set the Scale factor for z-angular velocity.
        """ # nopep8
        return self._cards[1].get_value("omz")

    @omz.setter
    def omz(self, value: float) -> None:
        self._cards[1].set_value("omz", value)

    @property
    def cid(self) -> int:
        """Get or set the Coordinate system ID to define acceleration in the local coordinate system.  The coordinate (XC, YC, ZC) is defined with respect to the local coordinate system if CID is nonzero.  The accelerations, LCID and their scale factors are with respect to CID.
        """ # nopep8
        return self._cards[1].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[1].set_value("cid", value)

    @property
    def angtyp(self) -> str:
        """Get or set the Type of body loads due to angular motion:
        EQ.CENT:	body load from centrifugal acceleration,ρ[ω×(ω×r)].
        EQ.CORI : body load from Coriolis - type acceleration,2ρ(ω×v).
        EQ.ROTA : body load from rotational acceleration,ρ(α×r),	where ω is the angular velocity, α is the angular acceleration, r is the position vector relative to center of rotationand v is the velocity vector
        """ # nopep8
        return self._cards[1].get_value("angtyp")

    @angtyp.setter
    def angtyp(self, value: str) -> None:
        if value not in ["CENT", "CORI", "ROTA"]:
            raise Exception("""angtyp must be one of {"CENT","CORI","ROTA"}""")
        self._cards[1].set_value("angtyp", value)

