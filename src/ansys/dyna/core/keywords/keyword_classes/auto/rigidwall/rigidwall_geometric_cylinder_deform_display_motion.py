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

"""Module providing the RigidwallGeometricCylinderDeformDisplayMotion class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class RigidwallGeometricCylinderDeformDisplayMotion(KeywordBase):
    """DYNA RIGIDWALL_GEOMETRIC_CYLINDER_DEFORM_DISPLAY_MOTION keyword"""

    keyword = "RIGIDWALL"
    subkeyword = "GEOMETRIC_CYLINDER_DEFORM_DISPLAY_MOTION"
    option_specs = [
        OptionSpec("ID", -2, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the RigidwallGeometricCylinderDeformDisplayMotion class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
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
                        "radcyl",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lencyl",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nsegs",
                        int,
                        20,
                        10,
                        **kwargs,
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
                        **kwargs,
                    ),
                    Field(
                        "height",
                        float,
                        10,
                        10,
                        **kwargs,
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
                        **kwargs,
                    ),
                    Field(
                        "yp",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "zp",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nl",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "narc",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nr",
                        int,
                        50,
                        10,
                        **kwargs,
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
                        **kwargs,
                    ),
                    Field(
                        "lcida",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcidb",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcidg",
                        int,
                        30,
                        10,
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
            OptionCardSet(
                option_spec = RigidwallGeometricCylinderDeformDisplayMotion.option_specs[0],
                cards = [
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
                ],
                **kwargs
            ),
        ]

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID containing tracked nodes, see *SET_NODE_OPTION.
        EQ.0: all nodes are tracked with respects to the rigid wall.
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        """Set the nsid property."""
        self._cards[0].set_value("nsid", value)

    @property
    def nsidex(self) -> int:
        """Get or set the Node set ID containing nodes that exempted as tracked nodes, see *SET_NODE_OPTION.
        """ # nopep8
        return self._cards[0].get_value("nsidex")

    @nsidex.setter
    def nsidex(self, value: int) -> None:
        """Set the nsidex property."""
        self._cards[0].set_value("nsidex", value)

    @property
    def boxid(self) -> int:
        """Get or set the If defined, only nodes in box are included as tracked nodes for the rigid wall.
        """ # nopep8
        return self._cards[0].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        """Set the boxid property."""
        self._cards[0].set_value("boxid", value)

    @property
    def birth(self) -> float:
        """Get or set the Birth time of rigid wall.  The time values of the load curves that control the motion of the wall are offset by the birth time.
        """ # nopep8
        return self._cards[0].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        """Set the birth property."""
        self._cards[0].set_value("birth", value)

    @property
    def death(self) -> float:
        """Get or set the Death time of rigid wall.  At this time the wall is deleted from the calculation
        """ # nopep8
        return self._cards[0].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        """Set the death property."""
        self._cards[0].set_value("death", value)

    @property
    def xt(self) -> float:
        """Get or set the x-coordinate of tail of any outward drawn normal vector, n, originating on wall (tail) and terminating in space (head).
        """ # nopep8
        return self._cards[1].get_value("xt")

    @xt.setter
    def xt(self, value: float) -> None:
        """Set the xt property."""
        self._cards[1].set_value("xt", value)

    @property
    def yt(self) -> float:
        """Get or set the y-coordinate of tail of normal vector n.
        """ # nopep8
        return self._cards[1].get_value("yt")

    @yt.setter
    def yt(self, value: float) -> None:
        """Set the yt property."""
        self._cards[1].set_value("yt", value)

    @property
    def zt(self) -> float:
        """Get or set the z-coordinate of tail of normal vector n.
        """ # nopep8
        return self._cards[1].get_value("zt")

    @zt.setter
    def zt(self, value: float) -> None:
        """Set the zt property."""
        self._cards[1].set_value("zt", value)

    @property
    def xh(self) -> float:
        """Get or set the x-coordinate of head of normal vector n.
        """ # nopep8
        return self._cards[1].get_value("xh")

    @xh.setter
    def xh(self, value: float) -> None:
        """Set the xh property."""
        self._cards[1].set_value("xh", value)

    @property
    def yh(self) -> float:
        """Get or set the y-coordinate of head of normal vector n.
        """ # nopep8
        return self._cards[1].get_value("yh")

    @yh.setter
    def yh(self, value: float) -> None:
        """Set the yh property."""
        self._cards[1].set_value("yh", value)

    @property
    def zh(self) -> float:
        """Get or set the z-coordinate of head of normal vector n.
        """ # nopep8
        return self._cards[1].get_value("zh")

    @zh.setter
    def zh(self, value: float) -> None:
        """Set the zh property."""
        self._cards[1].set_value("zh", value)

    @property
    def fric(self) -> float:
        """Get or set the Coulomb friction coefficient, except as noted below:
        EQ.0.0: Frictionless sliding when in contact,
        EQ.1.0: No sliding when in contact
        """ # nopep8
        return self._cards[1].get_value("fric")

    @fric.setter
    def fric(self, value: float) -> None:
        """Set the fric property."""
        self._cards[1].set_value("fric", value)

    @property
    def radcyl(self) -> typing.Optional[float]:
        """Get or set the Radius of cylinder.
        """ # nopep8
        return self._cards[2].get_value("radcyl")

    @radcyl.setter
    def radcyl(self, value: float) -> None:
        """Set the radcyl property."""
        self._cards[2].set_value("radcyl", value)

    @property
    def lencyl(self) -> typing.Optional[float]:
        """Get or set the Length of cylinder. Only if a value larger than zero is specified is a finite length is assumed.
        """ # nopep8
        return self._cards[2].get_value("lencyl")

    @lencyl.setter
    def lencyl(self, value: float) -> None:
        """Set the lencyl property."""
        self._cards[2].set_value("lencyl", value)

    @property
    def nsegs(self) -> typing.Optional[int]:
        """Get or set the Number of subsections
        """ # nopep8
        return self._cards[2].get_value("nsegs")

    @nsegs.setter
    def nsegs(self, value: int) -> None:
        """Set the nsegs property."""
        self._cards[2].set_value("nsegs", value)

    @property
    def vl(self) -> typing.Optional[float]:
        """Get or set the Distance from the Cylinder base
        """ # nopep8
        return self._cards[3].get_value("vl")

    @vl.setter
    def vl(self, value: float) -> None:
        """Set the vl property."""
        self._cards[3].set_value("vl", value)

    @property
    def height(self) -> typing.Optional[float]:
        """Get or set the Section height
        """ # nopep8
        return self._cards[3].get_value("height")

    @height.setter
    def height(self, value: float) -> None:
        """Set the height property."""
        self._cards[3].set_value("height", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point in the local xz-plane, see Remark 1.
        """ # nopep8
        return self._cards[4].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[4].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point in the local xz-plane, see Remark 1.
        """ # nopep8
        return self._cards[4].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[4].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point in the local xz-plane, see Remark 1.
        """ # nopep8
        return self._cards[4].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[4].set_value("zp", value)

    @property
    def nl(self) -> typing.Optional[int]:
        """Get or set the Number of auto-generated elements in the longitudinal direction. If DISPLAY option is not used, NL will be ignored. See Remark 2.
        """ # nopep8
        return self._cards[4].get_value("nl")

    @nl.setter
    def nl(self, value: int) -> None:
        """Set the nl property."""
        self._cards[4].set_value("nl", value)

    @property
    def narc(self) -> typing.Optional[int]:
        """Get or set the Number of auto-generated elements in the circumferential direction. If DISPLAY option is not used, NARC will be ignored.
        """ # nopep8
        return self._cards[4].get_value("narc")

    @narc.setter
    def narc(self, value: int) -> None:
        """Set the narc property."""
        self._cards[4].set_value("narc", value)

    @property
    def nr(self) -> typing.Optional[int]:
        """Get or set the Number of auto-generated elements in the radius direction. If DISPLAY option is not used, NR will be ignored.
        """ # nopep8
        return self._cards[4].get_value("nr")

    @nr.setter
    def nr(self, value: int) -> None:
        """Set the nr property."""
        self._cards[4].set_value("nr", value)

    @property
    def lcidr(self) -> typing.Optional[int]:
        """Get or set the Curve ID to describe the change of the radius over time.
        """ # nopep8
        return self._cards[5].get_value("lcidr")

    @lcidr.setter
    def lcidr(self, value: int) -> None:
        """Set the lcidr property."""
        self._cards[5].set_value("lcidr", value)

    @property
    def lcida(self) -> typing.Optional[int]:
        """Get or set the Curve ID to describe the change of the rotation in radians about the local x-axis over time.
        """ # nopep8
        return self._cards[5].get_value("lcida")

    @lcida.setter
    def lcida(self, value: int) -> None:
        """Set the lcida property."""
        self._cards[5].set_value("lcida", value)

    @property
    def lcidb(self) -> typing.Optional[int]:
        """Get or set the Curve ID to describe the change of the bending curvature over time. Bending occurs
        in the local xz-plane with the center of the bending lying on the negative side of the local x-axis.
        """ # nopep8
        return self._cards[5].get_value("lcidb")

    @lcidb.setter
    def lcidb(self, value: int) -> None:
        """Set the lcidb property."""
        self._cards[5].set_value("lcidb", value)

    @property
    def lcidg(self) -> typing.Optional[int]:
        """Get or set the Curve ID to describe the change of the rotation in radians about the local z-axis over time.
        """ # nopep8
        return self._cards[5].get_value("lcidg")

    @lcidg.setter
    def lcidg(self, value: int) -> None:
        """Set the lcidg property."""
        self._cards[5].set_value("lcidg", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Stonewall motion curve number, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[6].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[6].set_value("lcid", value)

    @property
    def opt(self) -> int:
        """Get or set the Type of motion:
        EQ.0: velocity specified,
        EQ.1: displacement specified.
        """ # nopep8
        return self._cards[6].get_value("opt")

    @opt.setter
    def opt(self, value: int) -> None:
        """Set the opt property."""
        if value not in [0, 1, None]:
            raise Exception("""opt must be `None` or one of {0,1}.""")
        self._cards[6].set_value("opt", value)

    @property
    def vx(self) -> typing.Optional[float]:
        """Get or set the x-direction cosine of velocity/displacement vector.
        """ # nopep8
        return self._cards[6].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        """Set the vx property."""
        self._cards[6].set_value("vx", value)

    @property
    def vy(self) -> typing.Optional[float]:
        """Get or set the y-direction cosine of velocity/displacement vector.
        """ # nopep8
        return self._cards[6].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        """Set the vy property."""
        self._cards[6].set_value("vy", value)

    @property
    def vz(self) -> typing.Optional[float]:
        """Get or set the z-direction cosine of velocity/displacement vector.
        """ # nopep8
        return self._cards[6].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        """Set the vz property."""
        self._cards[6].set_value("vz", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Unique part ID for moving geometric rigid wall.  If zero, a part ID will be set that is larger than the maximum of all user defined part IDs.
        """ # nopep8
        return self._cards[7].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[7].set_value("pid", value)

    @property
    def ro(self) -> float:
        """Get or set the Density of rigid wall.
        """ # nopep8
        return self._cards[7].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[7].set_value("ro", value)

    @property
    def e(self) -> float:
        """Get or set the Youngs modulus.
        """ # nopep8
        return self._cards[7].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[7].set_value("e", value)

    @property
    def pr(self) -> float:
        """Get or set the Poissons ratio.
        """ # nopep8
        return self._cards[7].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[7].set_value("pr", value)

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Optional Rigidwall ID.
        """ # nopep8
        return self._cards[8].cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[8].cards[0].set_value("id", value)

        if value:
            self.activate_option("ID")

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Rigidwall id descriptor. It is suggested that unique descriptions be used.
        """ # nopep8
        return self._cards[8].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[8].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

