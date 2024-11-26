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

class DefineBoxSphLocal(KeywordBase):
    """DYNA DEFINE_BOX_SPH_LOCAL keyword"""

    keyword = "DEFINE"
    subkeyword = "BOX_SPH_LOCAL"
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
                        "boxid",
                        int,
                        0,
                        10,
                        kwargs.get("boxid")
                    ),
                    Field(
                        "xmn",
                        float,
                        10,
                        10,
                        kwargs.get("xmn", 0.0)
                    ),
                    Field(
                        "xmx",
                        float,
                        20,
                        10,
                        kwargs.get("xmx", 0.0)
                    ),
                    Field(
                        "ymn",
                        float,
                        30,
                        10,
                        kwargs.get("ymn", 0.0)
                    ),
                    Field(
                        "ymx",
                        float,
                        40,
                        10,
                        kwargs.get("ymx", 0.0)
                    ),
                    Field(
                        "zmn",
                        float,
                        50,
                        10,
                        kwargs.get("zmn", 0.0)
                    ),
                    Field(
                        "zmx",
                        float,
                        60,
                        10,
                        kwargs.get("zmx", 0.0)
                    ),
                    Field(
                        "vid",
                        int,
                        70,
                        10,
                        kwargs.get("vid", 0)
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
                        kwargs.get("lcid", 0)
                    ),
                    Field(
                        "vd",
                        int,
                        10,
                        10,
                        kwargs.get("vd", 0)
                    ),
                    Field(
                        "nid",
                        int,
                        20,
                        10,
                        kwargs.get("nid", 0)
                    ),
                    Field(
                        "ireact",
                        int,
                        30,
                        10,
                        kwargs.get("ireact", 0)
                    ),
                    Field(
                        "ibuff",
                        int,
                        40,
                        10,
                        kwargs.get("ibuff", 0)
                    ),
                    Field(
                        "ishow",
                        int,
                        50,
                        10,
                        kwargs.get("ishow", 0)
                    ),
                    Field(
                        "pid",
                        int,
                        60,
                        10,
                        kwargs.get("pid", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xx",
                        float,
                        0,
                        10,
                        kwargs.get("xx", 0)
                    ),
                    Field(
                        "yx",
                        float,
                        10,
                        10,
                        kwargs.get("yx", 0.0)
                    ),
                    Field(
                        "zx",
                        float,
                        20,
                        10,
                        kwargs.get("zx", 0.0)
                    ),
                    Field(
                        "xv",
                        float,
                        30,
                        10,
                        kwargs.get("xv", 0.0)
                    ),
                    Field(
                        "yv",
                        float,
                        40,
                        10,
                        kwargs.get("yv", 0.0)
                    ),
                    Field(
                        "zv",
                        float,
                        50,
                        10,
                        kwargs.get("zv", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cx",
                        float,
                        0,
                        10,
                        kwargs.get("cx", 0.0)
                    ),
                    Field(
                        "cy",
                        float,
                        10,
                        10,
                        kwargs.get("cy", 0.0)
                    ),
                    Field(
                        "cz",
                        float,
                        20,
                        10,
                        kwargs.get("cz", 0.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineBoxSphLocal.option_specs[0],
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
    def boxid(self) -> typing.Optional[int]:
        """Get or set the Box ID. A unique number must be defined.
        """ # nopep8
        return self._cards[0].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        self._cards[0].set_value("boxid", value)

    @property
    def xmn(self) -> float:
        """Get or set the Minimum x-coordinate.
        """ # nopep8
        return self._cards[0].get_value("xmn")

    @xmn.setter
    def xmn(self, value: float) -> None:
        self._cards[0].set_value("xmn", value)

    @property
    def xmx(self) -> float:
        """Get or set the Maximum x-coordinate.
        """ # nopep8
        return self._cards[0].get_value("xmx")

    @xmx.setter
    def xmx(self, value: float) -> None:
        self._cards[0].set_value("xmx", value)

    @property
    def ymn(self) -> float:
        """Get or set the Minimum y-coordinate.
        """ # nopep8
        return self._cards[0].get_value("ymn")

    @ymn.setter
    def ymn(self, value: float) -> None:
        self._cards[0].set_value("ymn", value)

    @property
    def ymx(self) -> float:
        """Get or set the Maximum y-coordinate.
        """ # nopep8
        return self._cards[0].get_value("ymx")

    @ymx.setter
    def ymx(self, value: float) -> None:
        self._cards[0].set_value("ymx", value)

    @property
    def zmn(self) -> float:
        """Get or set the Minimum z-coordinate.
        """ # nopep8
        return self._cards[0].get_value("zmn")

    @zmn.setter
    def zmn(self, value: float) -> None:
        self._cards[0].set_value("zmn", value)

    @property
    def zmx(self) -> float:
        """Get or set the Maximum z-coordinate.
        """ # nopep8
        return self._cards[0].get_value("zmx")

    @zmx.setter
    def zmx(self, value: float) -> None:
        self._cards[0].set_value("zmx", value)

    @property
    def vid(self) -> int:
        """Get or set the Vector ID of DOF, see *DEFINE_VECTOR.
        """ # nopep8
        return self._cards[0].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        self._cards[0].set_value("vid", value)

    @property
    def lcid(self) -> int:
        """Get or set the Load curve ID to describe motion value versus time, see *DEFINE_CURVE
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[1].set_value("lcid", value)

    @property
    def vd(self) -> int:
        """Get or set the Velocity/Displacement flag:
        EQ.0: velocity,
        EQ.1: displacement
        EQ.2:  referential node
        """ # nopep8
        return self._cards[1].get_value("vd")

    @vd.setter
    def vd(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""vd must be one of {0,1,2}""")
        self._cards[1].set_value("vd", value)

    @property
    def nid(self) -> int:
        """Get or set the Referential nodal ID for VD=2 (SPH box will move with this node)
        """ # nopep8
        return self._cards[1].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[1].set_value("nid", value)

    @property
    def ireact(self) -> int:
        """Get or set the Reactivation flag:
        EQ.0:	particles outside of the box are permanently deactivated,
        EQ.1 : deactivated particles get reactivated when they enter the box
        """ # nopep8
        return self._cards[1].get_value("ireact")

    @ireact.setter
    def ireact(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ireact must be one of {0,1}""")
        self._cards[1].set_value("ireact", value)

    @property
    def ibuff(self) -> int:
        """Get or set the Buffer zone flag:
        EQ.0: particles on the edge of the box don’t get any special treatment.
        EQ.1 : particles on the edge of the box are frozen in space and act as neighbors for active particles inside the box.
        This option is mainly used for fluid simulations to prevent the fluid from spilling out of the activation box.
        """ # nopep8
        return self._cards[1].get_value("ibuff")

    @ibuff.setter
    def ibuff(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ibuff must be one of {0,1}""")
        self._cards[1].set_value("ibuff", value)

    @property
    def ishow(self) -> int:
        """Get or set the Create dummy part to visualize position of activation box in post-processing.
        EQ.0: no part is created.
        EQ.1 : a dummy part is added for visualization
        """ # nopep8
        return self._cards[1].get_value("ishow")

    @ishow.setter
    def ishow(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ishow must be one of {0,1}""")
        self._cards[1].set_value("ishow", value)

    @property
    def pid(self) -> int:
        """Get or set the Part ID used for visualization if ISHOW=1.
        EQ.0:	a unique Part ID is automatically created.
        GT.0 : the part created by ISHOW = 1 is numbered PID.This should be a unique part ID.
        """ # nopep8
        return self._cards[1].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[1].set_value("pid", value)

    @property
    def xx(self) -> float:
        """Get or set the X-coordinate on local x-axis.  Origin lies at (0,0,0).  Define if the LOCAL option is active
        """ # nopep8
        return self._cards[2].get_value("xx")

    @xx.setter
    def xx(self, value: float) -> None:
        self._cards[2].set_value("xx", value)

    @property
    def yx(self) -> float:
        """Get or set the Y-coordinate on local x-axis.  Define if the LOCAL option is active..
        """ # nopep8
        return self._cards[2].get_value("yx")

    @yx.setter
    def yx(self, value: float) -> None:
        self._cards[2].set_value("yx", value)

    @property
    def zx(self) -> float:
        """Get or set the Z-coordinate on local x-axis.  Define if the LOCAL option is active.
        """ # nopep8
        return self._cards[2].get_value("zx")

    @zx.setter
    def zx(self, value: float) -> None:
        self._cards[2].set_value("zx", value)

    @property
    def xv(self) -> float:
        """Get or set the X-coordinate of local x-y vector.  Define if the LOCAL option is active
        """ # nopep8
        return self._cards[2].get_value("xv")

    @xv.setter
    def xv(self, value: float) -> None:
        self._cards[2].set_value("xv", value)

    @property
    def yv(self) -> float:
        """Get or set the Y-coordinate of local x-y vector.  Define if the LOCAL option is active.
        """ # nopep8
        return self._cards[2].get_value("yv")

    @yv.setter
    def yv(self, value: float) -> None:
        self._cards[2].set_value("yv", value)

    @property
    def zv(self) -> float:
        """Get or set the Z-coordinate of local x-y vector.  Define if the LOCAL option is active..
        """ # nopep8
        return self._cards[2].get_value("zv")

    @zv.setter
    def zv(self, value: float) -> None:
        self._cards[2].set_value("zv", value)

    @property
    def cx(self) -> float:
        """Get or set the X-global coordinate of offset vector to origin of local system.  Define if the LOCAL option is active.
        """ # nopep8
        return self._cards[3].get_value("cx")

    @cx.setter
    def cx(self, value: float) -> None:
        self._cards[3].set_value("cx", value)

    @property
    def cy(self) -> float:
        """Get or set the Y-global coordinate of offset vector to origin of local system.  Define if the LOCAL option is active.
        """ # nopep8
        return self._cards[3].get_value("cy")

    @cy.setter
    def cy(self, value: float) -> None:
        self._cards[3].set_value("cy", value)

    @property
    def cz(self) -> float:
        """Get or set the Z-global coordinate of offset vector to origin of local system.  Define if the LOCAL option is active.
        """ # nopep8
        return self._cards[3].get_value("cz")

    @cz.setter
    def cz(self, value: float) -> None:
        self._cards[3].set_value("cz", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[4].cards[0].set_value("title", value)

