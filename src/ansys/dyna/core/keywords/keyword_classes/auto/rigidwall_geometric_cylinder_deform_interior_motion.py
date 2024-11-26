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

class RigidwallGeometricCylinderDeformInteriorMotion(KeywordBase):
    """DYNA RIGIDWALL_GEOMETRIC_CYLINDER_DEFORM_INTERIOR_MOTION keyword"""

    keyword = "RIGIDWALL"
    subkeyword = "GEOMETRIC_CYLINDER_DEFORM_INTERIOR_MOTION"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
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
                        "title",
                        str,
                        10,
                        70,
                        kwargs.get("title")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nsid",
                        int,
                        0,
                        10,
                        kwargs.get("nsid")
                    ),
                    Field(
                        "nsidex",
                        int,
                        10,
                        10,
                        kwargs.get("nsidex", 0)
                    ),
                    Field(
                        "boxid",
                        int,
                        20,
                        10,
                        kwargs.get("boxid", 0)
                    ),
                    Field(
                        "birth",
                        float,
                        30,
                        10,
                        kwargs.get("birth", 0.0)
                    ),
                    Field(
                        "death",
                        float,
                        40,
                        10,
                        kwargs.get("death", 1.0E+20)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xt",
                        float,
                        0,
                        10,
                        kwargs.get("xt", 0.0)
                    ),
                    Field(
                        "yt",
                        float,
                        10,
                        10,
                        kwargs.get("yt", 0.0)
                    ),
                    Field(
                        "zt",
                        float,
                        20,
                        10,
                        kwargs.get("zt", 0.0)
                    ),
                    Field(
                        "xh",
                        float,
                        30,
                        10,
                        kwargs.get("xh", 0.0)
                    ),
                    Field(
                        "yh",
                        float,
                        40,
                        10,
                        kwargs.get("yh", 0.0)
                    ),
                    Field(
                        "zh",
                        float,
                        50,
                        10,
                        kwargs.get("zh", 0.0)
                    ),
                    Field(
                        "fric",
                        float,
                        60,
                        10,
                        kwargs.get("fric", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "radcyl",
                        float,
                        0,
                        10,
                        kwargs.get("radcyl")
                    ),
                    Field(
                        "lencyl",
                        float,
                        10,
                        10,
                        kwargs.get("lencyl")
                    ),
                    Field(
                        "nsegs",
                        int,
                        20,
                        10,
                        kwargs.get("nsegs")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "vl",
                        float,
                        0,
                        10,
                        kwargs.get("vl")
                    ),
                    Field(
                        "height",
                        float,
                        10,
                        10,
                        kwargs.get("height")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xp",
                        float,
                        0,
                        10,
                        kwargs.get("xp")
                    ),
                    Field(
                        "yp",
                        float,
                        10,
                        10,
                        kwargs.get("yp")
                    ),
                    Field(
                        "zp",
                        float,
                        20,
                        10,
                        kwargs.get("zp")
                    ),
                    Field(
                        "nl",
                        int,
                        30,
                        10,
                        kwargs.get("nl")
                    ),
                    Field(
                        "narc",
                        int,
                        40,
                        10,
                        kwargs.get("narc")
                    ),
                    Field(
                        "nr",
                        int,
                        50,
                        10,
                        kwargs.get("nr")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcidr",
                        int,
                        0,
                        10,
                        kwargs.get("lcidr")
                    ),
                    Field(
                        "lcida",
                        int,
                        10,
                        10,
                        kwargs.get("lcida")
                    ),
                    Field(
                        "lcidb",
                        int,
                        20,
                        10,
                        kwargs.get("lcidb")
                    ),
                    Field(
                        "lcidg",
                        int,
                        30,
                        10,
                        kwargs.get("lcidg")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcid",
                        int,
                        0,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "opt",
                        int,
                        10,
                        10,
                        kwargs.get("opt", 0)
                    ),
                    Field(
                        "vx",
                        float,
                        20,
                        10,
                        kwargs.get("vx")
                    ),
                    Field(
                        "vy",
                        float,
                        30,
                        10,
                        kwargs.get("vy")
                    ),
                    Field(
                        "vz",
                        float,
                        40,
                        10,
                        kwargs.get("vz")
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Optional Rigidwall ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Ridigwall id descriptor. It is suggested that unique descriptions be used.
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[0].set_value("title", value)

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID containing tracked nodes, see *SET_NODE_OPTION.
        EQ.0: all nodes are tracked with respects to the rigid wall.
        """ # nopep8
        return self._cards[1].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        self._cards[1].set_value("nsid", value)

    @property
    def nsidex(self) -> int:
        """Get or set the Node set ID containing nodes that exempted as tracked nodes, see *SET_NODE_OPTION.
        """ # nopep8
        return self._cards[1].get_value("nsidex")

    @nsidex.setter
    def nsidex(self, value: int) -> None:
        self._cards[1].set_value("nsidex", value)

    @property
    def boxid(self) -> int:
        """Get or set the If defined, only nodes in box are included as tracked nodes for the rigid wall.
        """ # nopep8
        return self._cards[1].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        self._cards[1].set_value("boxid", value)

    @property
    def birth(self) -> float:
        """Get or set the Birth time of rigid wall.  The time values of the load curves that control the motion of the wall are offset by the birth time.
        """ # nopep8
        return self._cards[1].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        self._cards[1].set_value("birth", value)

    @property
    def death(self) -> float:
        """Get or set the Death time of rigid wall.  At this time the wall is deleted from the calculation
        """ # nopep8
        return self._cards[1].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        self._cards[1].set_value("death", value)

    @property
    def xt(self) -> float:
        """Get or set the x-coordinate of tail of any outward drawn normal vector, n, originating on wall (tail) and terminating in space (head).
        """ # nopep8
        return self._cards[2].get_value("xt")

    @xt.setter
    def xt(self, value: float) -> None:
        self._cards[2].set_value("xt", value)

    @property
    def yt(self) -> float:
        """Get or set the y-coordinate of tail of normal vector n.
        """ # nopep8
        return self._cards[2].get_value("yt")

    @yt.setter
    def yt(self, value: float) -> None:
        self._cards[2].set_value("yt", value)

    @property
    def zt(self) -> float:
        """Get or set the z-coordinate of tail of normal vector n.
        """ # nopep8
        return self._cards[2].get_value("zt")

    @zt.setter
    def zt(self, value: float) -> None:
        self._cards[2].set_value("zt", value)

    @property
    def xh(self) -> float:
        """Get or set the x-coordinate of head of normal vector n.
        """ # nopep8
        return self._cards[2].get_value("xh")

    @xh.setter
    def xh(self, value: float) -> None:
        self._cards[2].set_value("xh", value)

    @property
    def yh(self) -> float:
        """Get or set the y-coordinate of head of normal vector n.
        """ # nopep8
        return self._cards[2].get_value("yh")

    @yh.setter
    def yh(self, value: float) -> None:
        self._cards[2].set_value("yh", value)

    @property
    def zh(self) -> float:
        """Get or set the z-coordinate of head of normal vector n.
        """ # nopep8
        return self._cards[2].get_value("zh")

    @zh.setter
    def zh(self, value: float) -> None:
        self._cards[2].set_value("zh", value)

    @property
    def fric(self) -> float:
        """Get or set the Coulomb friction coefficient, except as noted below:
        EQ.0.0: Frictionless sliding when in contact,
        EQ.1.0: No sliding when in contact
        """ # nopep8
        return self._cards[2].get_value("fric")

    @fric.setter
    def fric(self, value: float) -> None:
        self._cards[2].set_value("fric", value)

    @property
    def radcyl(self) -> typing.Optional[float]:
        """Get or set the Radius of cylinder.
        """ # nopep8
        return self._cards[3].get_value("radcyl")

    @radcyl.setter
    def radcyl(self, value: float) -> None:
        self._cards[3].set_value("radcyl", value)

    @property
    def lencyl(self) -> typing.Optional[float]:
        """Get or set the Length of cylinder. Only if a value larger than zero is specified is a finite length is assumed.
        """ # nopep8
        return self._cards[3].get_value("lencyl")

    @lencyl.setter
    def lencyl(self, value: float) -> None:
        self._cards[3].set_value("lencyl", value)

    @property
    def nsegs(self) -> typing.Optional[int]:
        """Get or set the Number of subsections
        """ # nopep8
        return self._cards[3].get_value("nsegs")

    @nsegs.setter
    def nsegs(self, value: int) -> None:
        self._cards[3].set_value("nsegs", value)

    @property
    def vl(self) -> typing.Optional[float]:
        """Get or set the Distance from the Cylinder base
        """ # nopep8
        return self._cards[4].get_value("vl")

    @vl.setter
    def vl(self, value: float) -> None:
        self._cards[4].set_value("vl", value)

    @property
    def height(self) -> typing.Optional[float]:
        """Get or set the Section height
        """ # nopep8
        return self._cards[4].get_value("height")

    @height.setter
    def height(self, value: float) -> None:
        self._cards[4].set_value("height", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point in the local xz-plane, see Remark 1.
        """ # nopep8
        return self._cards[5].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[5].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point in the local xz-plane, see Remark 1.
        """ # nopep8
        return self._cards[5].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[5].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point in the local xz-plane, see Remark 1.
        """ # nopep8
        return self._cards[5].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[5].set_value("zp", value)

    @property
    def nl(self) -> typing.Optional[int]:
        """Get or set the Number of auto-generated elements in the longitudinal direction. If DISPLAY option is not used, NL will be ignored. See Remark 2.
        """ # nopep8
        return self._cards[5].get_value("nl")

    @nl.setter
    def nl(self, value: int) -> None:
        self._cards[5].set_value("nl", value)

    @property
    def narc(self) -> typing.Optional[int]:
        """Get or set the Number of auto-generated elements in the circumferential direction. If DISPLAY option is not used, NARC will be ignored.
        """ # nopep8
        return self._cards[5].get_value("narc")

    @narc.setter
    def narc(self, value: int) -> None:
        self._cards[5].set_value("narc", value)

    @property
    def nr(self) -> typing.Optional[int]:
        """Get or set the Number of auto-generated elements in the radius direction. If DISPLAY option is not used, NR will be ignored.
        """ # nopep8
        return self._cards[5].get_value("nr")

    @nr.setter
    def nr(self, value: int) -> None:
        self._cards[5].set_value("nr", value)

    @property
    def lcidr(self) -> typing.Optional[int]:
        """Get or set the Curve ID to describe the change of the radius over time.
        """ # nopep8
        return self._cards[6].get_value("lcidr")

    @lcidr.setter
    def lcidr(self, value: int) -> None:
        self._cards[6].set_value("lcidr", value)

    @property
    def lcida(self) -> typing.Optional[int]:
        """Get or set the Curve ID to describe the change of the rotation in radians about the local x-axis over time.
        """ # nopep8
        return self._cards[6].get_value("lcida")

    @lcida.setter
    def lcida(self, value: int) -> None:
        self._cards[6].set_value("lcida", value)

    @property
    def lcidb(self) -> typing.Optional[int]:
        """Get or set the Curve ID to describe the change of the bending curvature over time. Bending occurs
        in the local xz-plane with the center of the bending lying on the negative side of the local x-axis.
        """ # nopep8
        return self._cards[6].get_value("lcidb")

    @lcidb.setter
    def lcidb(self, value: int) -> None:
        self._cards[6].set_value("lcidb", value)

    @property
    def lcidg(self) -> typing.Optional[int]:
        """Get or set the Curve ID to describe the change of the rotation in radians about the local z-axis over time.
        """ # nopep8
        return self._cards[6].get_value("lcidg")

    @lcidg.setter
    def lcidg(self, value: int) -> None:
        self._cards[6].set_value("lcidg", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Stonewall motion curve number, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[7].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[7].set_value("lcid", value)

    @property
    def opt(self) -> int:
        """Get or set the Type of motion:
        EQ.0: velocity specified,
        EQ.1: displacement specified.
        """ # nopep8
        return self._cards[7].get_value("opt")

    @opt.setter
    def opt(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""opt must be one of {0,1}""")
        self._cards[7].set_value("opt", value)

    @property
    def vx(self) -> typing.Optional[float]:
        """Get or set the x-direction cosine of velocity/displacement vector.
        """ # nopep8
        return self._cards[7].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        self._cards[7].set_value("vx", value)

    @property
    def vy(self) -> typing.Optional[float]:
        """Get or set the y-direction cosine of velocity/displacement vector.
        """ # nopep8
        return self._cards[7].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        self._cards[7].set_value("vy", value)

    @property
    def vz(self) -> typing.Optional[float]:
        """Get or set the z-direction cosine of velocity/displacement vector.
        """ # nopep8
        return self._cards[7].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        self._cards[7].set_value("vz", value)

