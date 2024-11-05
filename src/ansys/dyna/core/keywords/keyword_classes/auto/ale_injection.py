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

class AleInjection(KeywordBase):
    """DYNA ALE_INJECTION keyword"""

    keyword = "ALE"
    subkeyword = "INJECTION"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "mmgset",
                        int,
                        0,
                        10,
                        kwargs.get("mmgset")
                    ),
                    Field(
                        "segset",
                        int,
                        10,
                        10,
                        kwargs.get("segset")
                    ),
                    Field(
                        "global",
                        int,
                        20,
                        10,
                        kwargs.get("global", 0)
                    ),
                    Field(
                        "lce",
                        int,
                        30,
                        10,
                        kwargs.get("lce", 0)
                    ),
                    Field(
                        "lcrvl",
                        int,
                        40,
                        10,
                        kwargs.get("lcrvl", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcvt",
                        int,
                        0,
                        10,
                        kwargs.get("lcvt", 0)
                    ),
                    Field(
                        "vect",
                        int,
                        10,
                        10,
                        kwargs.get("vect", 0)
                    ),
                    Field(
                        "lcvr",
                        int,
                        20,
                        10,
                        kwargs.get("lcvr", 0)
                    ),
                    Field(
                        "vecr",
                        int,
                        30,
                        10,
                        kwargs.get("vecr", 0)
                    ),
                    Field(
                        "boxv",
                        int,
                        40,
                        10,
                        kwargs.get("boxv", 0)
                    ),
                    Field(
                        "xg",
                        float,
                        50,
                        10,
                        kwargs.get("xg", 0.0)
                    ),
                    Field(
                        "yg",
                        float,
                        60,
                        10,
                        kwargs.get("yg", 0.0)
                    ),
                    Field(
                        "zg",
                        float,
                        70,
                        10,
                        kwargs.get("zg", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "surfct",
                        int,
                        0,
                        10,
                        kwargs.get("surfct", 0)
                    ),
                    Field(
                        "ndiv",
                        int,
                        10,
                        10,
                        kwargs.get("ndiv", 3)
                    ),
                    Field(
                        "xl",
                        float,
                        20,
                        10,
                        kwargs.get("xl", 0.0)
                    ),
                    Field(
                        "yl",
                        float,
                        30,
                        10,
                        kwargs.get("yl", 0.0)
                    ),
                    Field(
                        "zd",
                        float,
                        40,
                        10,
                        kwargs.get("zd", 0.0)
                    ),
                    Field(
                        "zu",
                        float,
                        50,
                        10,
                        kwargs.get("zu", 0.0)
                    ),
                    Field(
                        "xc",
                        float,
                        60,
                        10,
                        kwargs.get("xc", 0.0)
                    ),
                    Field(
                        "yc",
                        float,
                        70,
                        10,
                        kwargs.get("yc", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def mmgset(self) -> typing.Optional[int]:
        """Get or set the Multi-Material Set ID (see *SET_MULTI-MATERIAL_GROUP_LIST).
        """ # nopep8
        return self._cards[0].get_value("mmgset")

    @mmgset.setter
    def mmgset(self, value: int) -> None:
        self._cards[0].set_value("mmgset", value)

    @property
    def segset(self) -> typing.Optional[int]:
        """Get or set the Segment set ID (see *SET_SEGMENT). A local coordinate system
        is created for each segment. See Remark 2.
        """ # nopep8
        return self._cards[0].get_value("segset")

    @segset.setter
    def segset(self, value: int) -> None:
        self._cards[0].set_value("segset", value)

    @property
    def global_(self) -> int:
        """Get or set the Three digit flag to control how to select the elements, how to
        prescribe the velocities and how to define the geometrical
        parameters of Cards 2 and 3 (including BOXV):
        EQ._ _ 0: Geometrical parameters are local to the segments of SEGSET
        EQ._ _ 1: Geometrical parameters are natural to SEGSET
        segments (see Remark 3 and Figure 4-1)
        EQ._ 0 _: Velocities are applied in local coordinate systems
        attached to each segment of SEGSET
        EQ._ 1 _: Velocities are applied in the global coordinate system
        EQ.0 _ _: Select the elements and nodes in the local volume
        around each segment of SEGSET
        EQ.1 _ _: Select the elements in the global volume formed by
        all the segments of SEGSET
        EQ.2 _ _: Select the elements and nodes in the global volume
        formed by all the segments of SEGSET. Velocities are
        applied in the global coordinate system.
        """ # nopep8
        return self._cards[0].get_value("global")

    @global_.setter
    def global_(self, value: int) -> None:
        self._cards[0].set_value("global", value)

    @property
    def lce(self) -> int:
        """Get or set the Curve ID for the internal energy (see Remark 6):
        GT.0: Load curve ID; see *DEFINE_CURVE. See Remark 2.
        LT.0: -LCE is the function ID for the internal energy which
        depends on 26 arguments: time, number of cycles, and
        nodal coordinates of the 8 nodes for the ALE element.
        See *DEFINE_FUNCTION. See Remark 5.
        """ # nopep8
        return self._cards[0].get_value("lce")

    @lce.setter
    def lce(self, value: int) -> None:
        self._cards[0].set_value("lce", value)

    @property
    def lcrvl(self) -> int:
        """Get or set the Curve ID for the relative volume (see Remark 6):
        GT.0: Load curve ID; see *DEFINE_CURVE. See Remark 2.
        LT.0: -LCRVL is the function ID for the relative volume which
        depends on 26 arguments: time, number of cycles, and
        nodal coordinates of the 8 nodes for the ALE element.
        See *DEFINE_FUNCTION. See Remark 5.
        """ # nopep8
        return self._cards[0].get_value("lcrvl")

    @lcrvl.setter
    def lcrvl(self, value: int) -> None:
        self._cards[0].set_value("lcrvl", value)

    @property
    def lcvt(self) -> int:
        """Get or set the Curve ID for the translational velocity:
        GT.0: Load curve ID; see *DEFINE_CURVE.
        LT.0: -LCVT is the function ID for the translational velocity
        which depends on 5 arguments: time, number of cycles,
        and nodal coordinates. See *DEFINE_FUNCTION. See Remark 5..
        """ # nopep8
        return self._cards[1].get_value("lcvt")

    @lcvt.setter
    def lcvt(self, value: int) -> None:
        self._cards[1].set_value("lcvt", value)

    @property
    def vect(self) -> int:
        """Get or set the Vector to orient the translation. See *DEFINE_VECTOR.
        """ # nopep8
        return self._cards[1].get_value("vect")

    @vect.setter
    def vect(self, value: int) -> None:
        self._cards[1].set_value("vect", value)

    @property
    def lcvr(self) -> int:
        """Get or set the Curve ID for the rotational velocity:
        GT.0: Load curve ID; see *DEFINE_CURVE.
        LT.0: -LCVR is the function ID for the rotational velocity which
        depends on 5 arguments: time, number of cycles, and
        nodal coordinates. See *DEFINE_FUNCTION. See Remark	5.
        """ # nopep8
        return self._cards[1].get_value("lcvr")

    @lcvr.setter
    def lcvr(self, value: int) -> None:
        self._cards[1].set_value("lcvr", value)

    @property
    def vecr(self) -> int:
        """Get or set the Vector to orient the rotational axis (see *DEFINE_VECTOR).
        """ # nopep8
        return self._cards[1].get_value("vecr")

    @vecr.setter
    def vecr(self, value: int) -> None:
        self._cards[1].set_value("vecr", value)

    @property
    def boxv(self) -> int:
        """Get or set the Box (see *DEFINE_BOX) defining the region where the velocities are applied (see Remark 7).
        """ # nopep8
        return self._cards[1].get_value("boxv")

    @boxv.setter
    def boxv(self, value: int) -> None:
        self._cards[1].set_value("boxv", value)

    @property
    def xg(self) -> float:
        """Get or set the Position of the rotation center (see Remark 8).
        """ # nopep8
        return self._cards[1].get_value("xg")

    @xg.setter
    def xg(self, value: float) -> None:
        self._cards[1].set_value("xg", value)

    @property
    def yg(self) -> float:
        """Get or set the Position of the rotation center (see Remark 8).
        """ # nopep8
        return self._cards[1].get_value("yg")

    @yg.setter
    def yg(self, value: float) -> None:
        self._cards[1].set_value("yg", value)

    @property
    def zg(self) -> float:
        """Get or set the Position of the rotation center (see Remark 8).
        """ # nopep8
        return self._cards[1].get_value("zg")

    @zg.setter
    def zg(self, value: float) -> None:
        self._cards[1].set_value("zg", value)

    @property
    def surfct(self) -> int:
        """Get or set the Flag to define the surface, inside which the nodes and elements are selected:
        LT.0: -SURFCT is the Function ID (see *DEFINE_FUNCTION)
        for the rotational velocity with 17 arguments: time, number
        of cycles, ALE element center coordinates, segment nodal coordinates.
        EQ.0: Ellipsoid;
        EQ.1: Ellipse-based cylinder;
        EQ.2: Truncated ellipse-based cone;
        EQ.3: Drop geometry meaning a cone for -ZD < z < 0 and
        half an ellipsoid for 0< z < ZU (see Remark 11 and Figure 4-6);
        EQ.4: Box with side lengths -XL < x < XL, -YL < y < YL,
        and -ZD < z < ZU (see Figure 4-7)
        EQ.5: Segment based cylinder (see Remark 12 and Figure 4-8).
        """ # nopep8
        return self._cards[2].get_value("surfct")

    @surfct.setter
    def surfct(self, value: int) -> None:
        self._cards[2].set_value("surfct", value)

    @property
    def ndiv(self) -> int:
        """Get or set the Number of divisions of an element cut by the surface SURFCT to
        compute the volume fractions (see Remark 13 and Figure 4-2).
        """ # nopep8
        return self._cards[2].get_value("ndiv")

    @ndiv.setter
    def ndiv(self, value: int) -> None:
        self._cards[2].set_value("ndiv", value)

    @property
    def xl(self) -> float:
        """Get or set the Length of the geometry SURFCT in the local x-direction.
        """ # nopep8
        return self._cards[2].get_value("xl")

    @xl.setter
    def xl(self, value: float) -> None:
        self._cards[2].set_value("xl", value)

    @property
    def yl(self) -> float:
        """Get or set the Length of the geometry SURFCT in the local y-direction.
        """ # nopep8
        return self._cards[2].get_value("yl")

    @yl.setter
    def yl(self, value: float) -> None:
        self._cards[2].set_value("yl", value)

    @property
    def zd(self) -> float:
        """Get or set the Length for the geometry SURFCT in the local 𝑧-direction for z < 0,
        except for SURFCT = 2 where z > 0. ZD can be input as a negative or positive value.
        """ # nopep8
        return self._cards[2].get_value("zd")

    @zd.setter
    def zd(self, value: float) -> None:
        self._cards[2].set_value("zd", value)

    @property
    def zu(self) -> float:
        """Get or set the Length for the geometry SURFCT in the local 𝑧-direction for z > 0.
        """ # nopep8
        return self._cards[2].get_value("zu")

    @zu.setter
    def zu(self, value: float) -> None:
        self._cards[2].set_value("zu", value)

    @property
    def xc(self) -> float:
        """Get or set the x-coordinate in the segment of the local coordinate center (see Remark 14).
        """ # nopep8
        return self._cards[2].get_value("xc")

    @xc.setter
    def xc(self, value: float) -> None:
        self._cards[2].set_value("xc", value)

    @property
    def yc(self) -> float:
        """Get or set the y-coordinate in the segment of the local coordinate center (see Remark 14).
        """ # nopep8
        return self._cards[2].get_value("yc")

    @yc.setter
    def yc(self, value: float) -> None:
        self._cards[2].set_value("yc", value)

