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

"""Module providing the RigidwallGeometricSphereMotionDisplay class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class RigidwallGeometricSphereMotionDisplay(KeywordBase):
    """DYNA RIGIDWALL_GEOMETRIC_SPHERE_MOTION_DISPLAY keyword"""

    keyword = "RIGIDWALL"
    subkeyword = "GEOMETRIC_SPHERE_MOTION_DISPLAY"

    def __init__(self, **kwargs):
        """Initialize the RigidwallGeometricSphereMotionDisplay class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "id",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "title",
                        str,
                        10,
                        70,
                        **kwargs,
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
                        **kwargs,
                    ),
                    Field(
                        "nsidex",
                        int,
                        10,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "boxid",
                        int,
                        20,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "birth",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "death",
                        float,
                        40,
                        10,
                        1.0E+20,
                        **kwargs,
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
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "yt",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "zt",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "xh",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "yh",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "zh",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "fric",
                        float,
                        60,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "radsph",
                        float,
                        0,
                        10,
                        0.0,
                        **kwargs,
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
                        **kwargs,
                    ),
                    Field(
                        "opt",
                        int,
                        10,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "vx",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "vy",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "vz",
                        float,
                        40,
                        10,
                        **kwargs,
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
                        **kwargs,
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        1.0E-09,
                        **kwargs,
                    ),
                    Field(
                        "e",
                        float,
                        20,
                        10,
                        1.0E-04,
                        **kwargs,
                    ),
                    Field(
                        "pr",
                        float,
                        30,
                        10,
                        0.30,
                        **kwargs,
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
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Ridigwall id descriptor. It is suggested that unique descriptions be used.
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[0].set_value("title", value)

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID containing tracked nodes, see *SET_NODE_OPTION.
        EQ.0: all nodes are tracked with respects to the rigid wall.
        """ # nopep8
        return self._cards[1].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        """Set the nsid property."""
        self._cards[1].set_value("nsid", value)

    @property
    def nsidex(self) -> int:
        """Get or set the Node set ID containing nodes that exempted as tracked nodes, see *SET_NODE_OPTION.
        """ # nopep8
        return self._cards[1].get_value("nsidex")

    @nsidex.setter
    def nsidex(self, value: int) -> None:
        """Set the nsidex property."""
        self._cards[1].set_value("nsidex", value)

    @property
    def boxid(self) -> int:
        """Get or set the If defined, only nodes in box are included as tracked nodes for the rigid wall.
        """ # nopep8
        return self._cards[1].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        """Set the boxid property."""
        self._cards[1].set_value("boxid", value)

    @property
    def birth(self) -> float:
        """Get or set the Birth time of rigid wall.  The time values of the load curves that control the motion of the wall are offset by the birth time.
        """ # nopep8
        return self._cards[1].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        """Set the birth property."""
        self._cards[1].set_value("birth", value)

    @property
    def death(self) -> float:
        """Get or set the Death time of rigid wall.  At this time the wall is deleted from the calculation
        """ # nopep8
        return self._cards[1].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        """Set the death property."""
        self._cards[1].set_value("death", value)

    @property
    def xt(self) -> float:
        """Get or set the x-coordinate of tail of any outward drawn normal vector, n, originating on wall (tail) and terminating in space (head).
        """ # nopep8
        return self._cards[2].get_value("xt")

    @xt.setter
    def xt(self, value: float) -> None:
        """Set the xt property."""
        self._cards[2].set_value("xt", value)

    @property
    def yt(self) -> float:
        """Get or set the y-coordinate of tail of normal vector n.
        """ # nopep8
        return self._cards[2].get_value("yt")

    @yt.setter
    def yt(self, value: float) -> None:
        """Set the yt property."""
        self._cards[2].set_value("yt", value)

    @property
    def zt(self) -> float:
        """Get or set the z-coordinate of tail of normal vector n.
        """ # nopep8
        return self._cards[2].get_value("zt")

    @zt.setter
    def zt(self, value: float) -> None:
        """Set the zt property."""
        self._cards[2].set_value("zt", value)

    @property
    def xh(self) -> float:
        """Get or set the x-coordinate of head of normal vector n.
        """ # nopep8
        return self._cards[2].get_value("xh")

    @xh.setter
    def xh(self, value: float) -> None:
        """Set the xh property."""
        self._cards[2].set_value("xh", value)

    @property
    def yh(self) -> float:
        """Get or set the y-coordinate of head of normal vector n.
        """ # nopep8
        return self._cards[2].get_value("yh")

    @yh.setter
    def yh(self, value: float) -> None:
        """Set the yh property."""
        self._cards[2].set_value("yh", value)

    @property
    def zh(self) -> float:
        """Get or set the z-coordinate of head of normal vector n.
        """ # nopep8
        return self._cards[2].get_value("zh")

    @zh.setter
    def zh(self, value: float) -> None:
        """Set the zh property."""
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
        """Set the fric property."""
        self._cards[2].set_value("fric", value)

    @property
    def radsph(self) -> float:
        """Get or set the Radius of sphere.
        """ # nopep8
        return self._cards[3].get_value("radsph")

    @radsph.setter
    def radsph(self, value: float) -> None:
        """Set the radsph property."""
        self._cards[3].set_value("radsph", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Stonewall motion curve number, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[4].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[4].set_value("lcid", value)

    @property
    def opt(self) -> int:
        """Get or set the Type of motion:
        EQ.0: velocity specified,
        EQ.1: displacement specified.
        """ # nopep8
        return self._cards[4].get_value("opt")

    @opt.setter
    def opt(self, value: int) -> None:
        """Set the opt property."""
        if value not in [0, 1, None]:
            raise Exception("""opt must be `None` or one of {0,1}.""")
        self._cards[4].set_value("opt", value)

    @property
    def vx(self) -> typing.Optional[float]:
        """Get or set the x-direction cosine of velocity/displacement vector.
        """ # nopep8
        return self._cards[4].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        """Set the vx property."""
        self._cards[4].set_value("vx", value)

    @property
    def vy(self) -> typing.Optional[float]:
        """Get or set the y-direction cosine of velocity/displacement vector.
        """ # nopep8
        return self._cards[4].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        """Set the vy property."""
        self._cards[4].set_value("vy", value)

    @property
    def vz(self) -> typing.Optional[float]:
        """Get or set the z-direction cosine of velocity/displacement vector.
        """ # nopep8
        return self._cards[4].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        """Set the vz property."""
        self._cards[4].set_value("vz", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Unique part ID for moving geometric rigid wall.  If zero, a part ID will be set that is larger than the maximum of all user defined part IDs.
        """ # nopep8
        return self._cards[5].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[5].set_value("pid", value)

    @property
    def ro(self) -> float:
        """Get or set the Density of rigid wall.
        """ # nopep8
        return self._cards[5].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[5].set_value("ro", value)

    @property
    def e(self) -> float:
        """Get or set the Youngs modulus.
        """ # nopep8
        return self._cards[5].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[5].set_value("e", value)

    @property
    def pr(self) -> float:
        """Get or set the Poissons ratio.
        """ # nopep8
        return self._cards[5].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[5].set_value("pr", value)

