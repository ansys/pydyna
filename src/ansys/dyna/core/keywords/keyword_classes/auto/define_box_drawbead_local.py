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

class DefineBoxDrawbeadLocal(KeywordBase):
    """DYNA DEFINE_BOX_DRAWBEAD_LOCAL keyword"""

    keyword = "DEFINE"
    subkeyword = "BOX_DRAWBEAD_LOCAL"
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
                        0,
                        **kwargs,
                    ),
                    Field(
                        "pid",
                        int,
                        10,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "sid",
                        int,
                        20,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "idir",
                        int,
                        30,
                        10,
                        1,
                        **kwargs,
                    ),
                    Field(
                        "stype",
                        int,
                        40,
                        10,
                        4,
                        **kwargs,
                    ),
                    Field(
                        "radius",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "cid",
                        int,
                        60,
                        10,
                        0,
                        **kwargs,
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
                        0,
                        **kwargs,
                    ),
                    Field(
                        "yx",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "zx",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "xv",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "yv",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "zv",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
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
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "cy",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "cz",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineBoxDrawbeadLocal.option_specs[0],
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
    def boxid(self) -> int:
        """Get or set the Box ID. Define unique numbers.
        """ # nopep8
        return self._cards[0].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        self._cards[0].set_value("boxid", value)

    @property
    def pid(self) -> int:
        """Get or set the Part ID of blank.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def sid(self) -> int:
        """Get or set the set ID defining along the drawbead.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[0].set_value("sid", value)

    @property
    def idir(self) -> int:
        """Get or set the Direction of tooling movement:
        EQ.1: tooling moves in x-direction (default),
        EQ.2: tooling moves in y-direction,
        EQ.3: tooling moves in z-direction.
        """ # nopep8
        return self._cards[0].get_value("idir")

    @idir.setter
    def idir(self, value: int) -> None:
        if value not in [1, 2, 3, None]:
            raise Exception("""idir must be `None` or one of {1,2,3}""")
        self._cards[0].set_value("idir", value)

    @property
    def stype(self) -> int:
        """Get or set the Set type:
        EQ.2:  part set ID,
        EQ.3:  part ID,
        EQ.4:  node set ID.
        """ # nopep8
        return self._cards[0].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        if value not in [4, 2, 3, 0, None]:
            raise Exception("""stype must be `None` or one of {4,2,3,0}""")
        self._cards[0].set_value("stype", value)

    @property
    def radius(self) -> float:
        """Get or set the The radius of the tube, which is centered around the draw bead.  Elements of part ID, PID, that lie within the tube will be included in the contact.    If the radius is not defined, a rectangular box is used instead.  This option is recommended for curved draw beads and for draw beads that are not aligned with the global axes.
        """ # nopep8
        return self._cards[0].get_value("radius")

    @radius.setter
    def radius(self, value: float) -> None:
        self._cards[0].set_value("radius", value)

    @property
    def cid(self) -> int:
        """Get or set the Optional coordinate system ID. This optional is only available for the tubular drawbead
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[0].set_value("cid", value)

    @property
    def xx(self) -> float:
        """Get or set the X-coordinate on local x-axis.  Origin lies at (0,0,0).  Define if the LOCAL option is active
        """ # nopep8
        return self._cards[1].get_value("xx")

    @xx.setter
    def xx(self, value: float) -> None:
        self._cards[1].set_value("xx", value)

    @property
    def yx(self) -> float:
        """Get or set the Y-coordinate on local x-axis.  Define if the LOCAL option is active..
        """ # nopep8
        return self._cards[1].get_value("yx")

    @yx.setter
    def yx(self, value: float) -> None:
        self._cards[1].set_value("yx", value)

    @property
    def zx(self) -> float:
        """Get or set the Z-coordinate on local x-axis.  Define if the LOCAL option is active.
        """ # nopep8
        return self._cards[1].get_value("zx")

    @zx.setter
    def zx(self, value: float) -> None:
        self._cards[1].set_value("zx", value)

    @property
    def xv(self) -> float:
        """Get or set the X-coordinate of local x-y vector.  Define if the LOCAL option is active
        """ # nopep8
        return self._cards[1].get_value("xv")

    @xv.setter
    def xv(self, value: float) -> None:
        self._cards[1].set_value("xv", value)

    @property
    def yv(self) -> float:
        """Get or set the Y-coordinate of local x-y vector.  Define if the LOCAL option is active.
        """ # nopep8
        return self._cards[1].get_value("yv")

    @yv.setter
    def yv(self, value: float) -> None:
        self._cards[1].set_value("yv", value)

    @property
    def zv(self) -> float:
        """Get or set the Z-coordinate of local x-y vector.  Define if the LOCAL option is active..
        """ # nopep8
        return self._cards[1].get_value("zv")

    @zv.setter
    def zv(self, value: float) -> None:
        self._cards[1].set_value("zv", value)

    @property
    def cx(self) -> float:
        """Get or set the X-global coordinate of offset vector to origin of local system.  Define if the LOCAL option is active.
        """ # nopep8
        return self._cards[2].get_value("cx")

    @cx.setter
    def cx(self, value: float) -> None:
        self._cards[2].set_value("cx", value)

    @property
    def cy(self) -> float:
        """Get or set the Y-global coordinate of offset vector to origin of local system.  Define if the LOCAL option is active.
        """ # nopep8
        return self._cards[2].get_value("cy")

    @cy.setter
    def cy(self, value: float) -> None:
        self._cards[2].set_value("cy", value)

    @property
    def cz(self) -> float:
        """Get or set the Z-global coordinate of offset vector to origin of local system.  Define if the LOCAL option is active.
        """ # nopep8
        return self._cards[2].get_value("cz")

    @cz.setter
    def cz(self, value: float) -> None:
        self._cards[2].set_value("cz", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

