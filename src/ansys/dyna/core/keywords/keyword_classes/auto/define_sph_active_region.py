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
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DefineSphActiveRegion(KeywordBase):
    """DYNA DEFINE_SPH_ACTIVE_REGION keyword"""

    keyword = "DEFINE"
    subkeyword = "SPH_ACTIVE_REGION"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "id",
                        int,
                        0,
                        10,
                        kwargs.get("id", 0)
                    ),
                    Field(
                        "type",
                        int,
                        10,
                        10,
                        kwargs.get("type", 0)
                    ),
                    Field(
                        "stype",
                        int,
                        20,
                        10,
                        kwargs.get("stype", 0)
                    ),
                    Field(
                        "cycle",
                        int,
                        30,
                        10,
                        kwargs.get("cycle", 1)
                    ),
                    Field(
                        "nid",
                        int,
                        40,
                        10,
                        kwargs.get("nid")
                    ),
                    Field(
                        "icid",
                        int,
                        50,
                        10,
                        kwargs.get("icid")
                    ),
                    Field(
                        "ibuff",
                        int,
                        60,
                        10,
                        kwargs.get("ibuff", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ximin",
                        float,
                        0,
                        10,
                        kwargs.get("ximin", 0.0)
                    ),
                    Field(
                        "yimin",
                        float,
                        10,
                        10,
                        kwargs.get("yimin", 0.0)
                    ),
                    Field(
                        "zimin",
                        float,
                        20,
                        10,
                        kwargs.get("zimin", 0.0)
                    ),
                    Field(
                        "ximax",
                        float,
                        30,
                        10,
                        kwargs.get("ximax", 0.0)
                    ),
                    Field(
                        "yimax",
                        float,
                        40,
                        10,
                        kwargs.get("yimax", 0.0)
                    ),
                    Field(
                        "zimax",
                        float,
                        50,
                        10,
                        kwargs.get("zimax", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xomin",
                        float,
                        0,
                        10,
                        kwargs.get("xomin", 0.0)
                    ),
                    Field(
                        "yomin",
                        float,
                        10,
                        10,
                        kwargs.get("yomin", 0.0)
                    ),
                    Field(
                        "zomin",
                        float,
                        20,
                        10,
                        kwargs.get("zomin", 0.0)
                    ),
                    Field(
                        "xomax",
                        float,
                        30,
                        10,
                        kwargs.get("xomax", 0.0)
                    ),
                    Field(
                        "yomax",
                        float,
                        40,
                        10,
                        kwargs.get("yomax", 0.0)
                    ),
                    Field(
                        "zomax",
                        float,
                        50,
                        10,
                        kwargs.get("zomax", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "x0",
                        float,
                        0,
                        10,
                        kwargs.get("x0", 0.0)
                    ),
                    Field(
                        "y0",
                        float,
                        10,
                        10,
                        kwargs.get("y0", 0.0)
                    ),
                    Field(
                        "z0",
                        float,
                        20,
                        10,
                        kwargs.get("z0", 0.0)
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
                ],
            ),
            Card(
                [
                    Field(
                        "rmin",
                        float,
                        0,
                        10,
                        kwargs.get("rmin", 0.0)
                    ),
                    Field(
                        "zmin",
                        float,
                        10,
                        10,
                        kwargs.get("zmin", 0.0)
                    ),
                    Field(
                        "rmax",
                        float,
                        20,
                        10,
                        kwargs.get("rmax", 0.0)
                    ),
                    Field(
                        "zmax",
                        float,
                        30,
                        10,
                        kwargs.get("zmax", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "x0",
                        float,
                        0,
                        10,
                        kwargs.get("x0", 0.0)
                    ),
                    Field(
                        "y0",
                        float,
                        10,
                        10,
                        kwargs.get("y0", 0.0)
                    ),
                    Field(
                        "z0",
                        float,
                        20,
                        10,
                        kwargs.get("z0", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "rmin",
                        float,
                        0,
                        10,
                        kwargs.get("rmin", 0.0)
                    ),
                    Field(
                        "rmax",
                        float,
                        10,
                        10,
                        kwargs.get("rmax", 0.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineSphActiveRegion.option_specs[0],
                cards = [
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
                ],
                **kwargs
            ),
        ]

    @property
    def id(self) -> int:
        """Get or set the Part Set ID/Part ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def type(self) -> int:
        """Get or set the EQ.0: Part set
        EQ.1: Part.
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""type must be one of {0,1}""")
        self._cards[0].set_value("type", value)

    @property
    def stype(self) -> int:
        """Get or set the Type of the region.
        EQ.0: Rectangular box
        EQ.1: Cylinder
        EQ.2: Sphere.
        """ # nopep8
        return self._cards[0].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""stype must be one of {0,1,2}""")
        self._cards[0].set_value("stype", value)

    @property
    def cycle(self) -> int:
        """Get or set the Number of cycles between each check.
        """ # nopep8
        return self._cards[0].get_value("cycle")

    @cycle.setter
    def cycle(self, value: int) -> None:
        self._cards[0].set_value("cycle", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Referential nodal ID, SPH box will move with this node.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[0].set_value("nid", value)

    @property
    def icid(self) -> typing.Optional[int]:
        """Get or set the Local coordinate system ID.
        """ # nopep8
        return self._cards[0].get_value("icid")

    @icid.setter
    def icid(self, value: int) -> None:
        self._cards[0].set_value("icid", value)

    @property
    def ibuff(self) -> int:
        """Get or set the Buffer zone flag, only used when STYPE = 0:
        EQ.0: particles on the edge of the outer box don't get any special treatment.
        EQ.1 : particles on the edge of the outer box are frozen in space and act as neighbors for active particles inside the box.
        This option is mainly used for fluid simulations to prevent the fluid from spilling out of the activation box..
        """ # nopep8
        return self._cards[0].get_value("ibuff")

    @ibuff.setter
    def ibuff(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ibuff must be one of {0,1}""")
        self._cards[0].set_value("ibuff", value)

    @property
    def ximin(self) -> float:
        """Get or set the Minimum x, coordinate of the inner box.
        """ # nopep8
        return self._cards[1].get_value("ximin")

    @ximin.setter
    def ximin(self, value: float) -> None:
        self._cards[1].set_value("ximin", value)

    @property
    def yimin(self) -> float:
        """Get or set the Minimum y coordinate of the inner box.
        """ # nopep8
        return self._cards[1].get_value("yimin")

    @yimin.setter
    def yimin(self, value: float) -> None:
        self._cards[1].set_value("yimin", value)

    @property
    def zimin(self) -> float:
        """Get or set the Minimum z coordinate of the inner box.
        """ # nopep8
        return self._cards[1].get_value("zimin")

    @zimin.setter
    def zimin(self, value: float) -> None:
        self._cards[1].set_value("zimin", value)

    @property
    def ximax(self) -> float:
        """Get or set the Maximum x coordinates of the inner box.
        """ # nopep8
        return self._cards[1].get_value("ximax")

    @ximax.setter
    def ximax(self, value: float) -> None:
        self._cards[1].set_value("ximax", value)

    @property
    def yimax(self) -> float:
        """Get or set the Maximum y coordinates of the inner box.
        """ # nopep8
        return self._cards[1].get_value("yimax")

    @yimax.setter
    def yimax(self, value: float) -> None:
        self._cards[1].set_value("yimax", value)

    @property
    def zimax(self) -> float:
        """Get or set the Maximum z coordinates of the inner box.
        """ # nopep8
        return self._cards[1].get_value("zimax")

    @zimax.setter
    def zimax(self, value: float) -> None:
        self._cards[1].set_value("zimax", value)

    @property
    def xomin(self) -> float:
        """Get or set the Minimum x, coordinate of the outer box.
        """ # nopep8
        return self._cards[2].get_value("xomin")

    @xomin.setter
    def xomin(self, value: float) -> None:
        self._cards[2].set_value("xomin", value)

    @property
    def yomin(self) -> float:
        """Get or set the Minimum y coordinate of the outer box.
        """ # nopep8
        return self._cards[2].get_value("yomin")

    @yomin.setter
    def yomin(self, value: float) -> None:
        self._cards[2].set_value("yomin", value)

    @property
    def zomin(self) -> float:
        """Get or set the Minimum z coordinate of the outer box.
        """ # nopep8
        return self._cards[2].get_value("zomin")

    @zomin.setter
    def zomin(self, value: float) -> None:
        self._cards[2].set_value("zomin", value)

    @property
    def xomax(self) -> float:
        """Get or set the Maximum x coordinates of the outer box.
        """ # nopep8
        return self._cards[2].get_value("xomax")

    @xomax.setter
    def xomax(self, value: float) -> None:
        self._cards[2].set_value("xomax", value)

    @property
    def yomax(self) -> float:
        """Get or set the Maximum y coordinates of the outer box.
        """ # nopep8
        return self._cards[2].get_value("yomax")

    @yomax.setter
    def yomax(self, value: float) -> None:
        self._cards[2].set_value("yomax", value)

    @property
    def zomax(self) -> float:
        """Get or set the Maximum z coordinates of the outer box.
        """ # nopep8
        return self._cards[2].get_value("zomax")

    @zomax.setter
    def zomax(self, value: float) -> None:
        self._cards[2].set_value("zomax", value)

    @property
    def x0(self) -> float:
        """Get or set the Coordinates of the cylinder center. This point also serves as the tail for the vector specifying the direction of the cylinders axis.
        """ # nopep8
        return self._cards[3].get_value("x0")

    @x0.setter
    def x0(self, value: float) -> None:
        self._cards[3].set_value("x0", value)

    @property
    def y0(self) -> float:
        """Get or set the Coordinates of the cylinder center. This point also serves as the tail for the vector specifying the direction of the cylinders axis.
        """ # nopep8
        return self._cards[3].get_value("y0")

    @y0.setter
    def y0(self, value: float) -> None:
        self._cards[3].set_value("y0", value)

    @property
    def z0(self) -> float:
        """Get or set the Coordinates of the cylinder center. This point also serves as the tail for the vector specifying the direction of the cylinders axis.
        """ # nopep8
        return self._cards[3].get_value("z0")

    @z0.setter
    def z0(self, value: float) -> None:
        self._cards[3].set_value("z0", value)

    @property
    def xh(self) -> float:
        """Get or set the Coordinates for the head of the cylinders axial direction vector.
        """ # nopep8
        return self._cards[3].get_value("xh")

    @xh.setter
    def xh(self, value: float) -> None:
        self._cards[3].set_value("xh", value)

    @property
    def yh(self) -> float:
        """Get or set the Coordinates for the head of the cylinders axial direction vector.
        """ # nopep8
        return self._cards[3].get_value("yh")

    @yh.setter
    def yh(self, value: float) -> None:
        self._cards[3].set_value("yh", value)

    @property
    def zh(self) -> float:
        """Get or set the Coordinates for the head of the cylinders axial direction vector.
        """ # nopep8
        return self._cards[3].get_value("zh")

    @zh.setter
    def zh(self, value: float) -> None:
        self._cards[3].set_value("zh", value)

    @property
    def rmin(self) -> float:
        """Get or set the Radius of the interior cylinder.
        """ # nopep8
        return self._cards[4].get_value("rmin")

    @rmin.setter
    def rmin(self, value: float) -> None:
        self._cards[4].set_value("rmin", value)

    @property
    def zmin(self) -> float:
        """Get or set the Length of the interior cylinder.
        """ # nopep8
        return self._cards[4].get_value("zmin")

    @zmin.setter
    def zmin(self, value: float) -> None:
        self._cards[4].set_value("zmin", value)

    @property
    def rmax(self) -> float:
        """Get or set the Radius of the outer cylinder.
        """ # nopep8
        return self._cards[4].get_value("rmax")

    @rmax.setter
    def rmax(self, value: float) -> None:
        self._cards[4].set_value("rmax", value)

    @property
    def zmax(self) -> float:
        """Get or set the Length of the outer cylinder.
        """ # nopep8
        return self._cards[4].get_value("zmax")

    @zmax.setter
    def zmax(self, value: float) -> None:
        self._cards[4].set_value("zmax", value)

    @property
    def x0(self) -> float:
        """Get or set the The spheres center.
        """ # nopep8
        return self._cards[5].get_value("x0")

    @x0.setter
    def x0(self, value: float) -> None:
        self._cards[5].set_value("x0", value)

    @property
    def y0(self) -> float:
        """Get or set the The spheres center.
        """ # nopep8
        return self._cards[5].get_value("y0")

    @y0.setter
    def y0(self, value: float) -> None:
        self._cards[5].set_value("y0", value)

    @property
    def z0(self) -> float:
        """Get or set the The spheres center.
        """ # nopep8
        return self._cards[5].get_value("z0")

    @z0.setter
    def z0(self, value: float) -> None:
        self._cards[5].set_value("z0", value)

    @property
    def rmin(self) -> float:
        """Get or set the Radius of the interior sphere.
        """ # nopep8
        return self._cards[6].get_value("rmin")

    @rmin.setter
    def rmin(self, value: float) -> None:
        self._cards[6].set_value("rmin", value)

    @property
    def rmax(self) -> float:
        """Get or set the Radius of the outer sphere.
        """ # nopep8
        return self._cards[6].get_value("rmax")

    @rmax.setter
    def rmax(self, value: float) -> None:
        self._cards[6].set_value("rmax", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[7].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[7].cards[0].set_value("title", value)

