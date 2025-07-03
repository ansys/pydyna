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

"""Module providing the DefineBoxAdaptiveLocal class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DefineBoxAdaptiveLocal(KeywordBase):
    """DYNA DEFINE_BOX_ADAPTIVE_LOCAL keyword"""

    keyword = "DEFINE"
    subkeyword = "BOX_ADAPTIVE_LOCAL"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineBoxAdaptiveLocal class."""
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
                        **kwargs,
                    ),
                    Field(
                        "xmn",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "xmx",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "ymn",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "ymx",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "zmn",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "zmx",
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
                        "pid",
                        int,
                        0,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "level",
                        int,
                        10,
                        10,
                        1,
                        **kwargs,
                    ),
                    Field(
                        "lidx/ndid",
                        int,
                        20,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "lidy",
                        int,
                        30,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "lidz",
                        int,
                        40,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "brmin",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "brmax",
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
                option_spec = DefineBoxAdaptiveLocal.option_specs[0],
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
        """Get or set the Box ID. Define unique numbers.
        """ # nopep8
        return self._cards[0].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        """Set the boxid property."""
        self._cards[0].set_value("boxid", value)

    @property
    def xmn(self) -> float:
        """Get or set the Minimum x-coordinate.
        """ # nopep8
        return self._cards[0].get_value("xmn")

    @xmn.setter
    def xmn(self, value: float) -> None:
        """Set the xmn property."""
        self._cards[0].set_value("xmn", value)

    @property
    def xmx(self) -> float:
        """Get or set the Maximum x-coordinate.
        """ # nopep8
        return self._cards[0].get_value("xmx")

    @xmx.setter
    def xmx(self, value: float) -> None:
        """Set the xmx property."""
        self._cards[0].set_value("xmx", value)

    @property
    def ymn(self) -> float:
        """Get or set the Minimum y-coordinate.
        """ # nopep8
        return self._cards[0].get_value("ymn")

    @ymn.setter
    def ymn(self, value: float) -> None:
        """Set the ymn property."""
        self._cards[0].set_value("ymn", value)

    @property
    def ymx(self) -> float:
        """Get or set the Maximum y-coordinate.
        """ # nopep8
        return self._cards[0].get_value("ymx")

    @ymx.setter
    def ymx(self, value: float) -> None:
        """Set the ymx property."""
        self._cards[0].set_value("ymx", value)

    @property
    def zmn(self) -> float:
        """Get or set the Minimum z-coordinate.
        """ # nopep8
        return self._cards[0].get_value("zmn")

    @zmn.setter
    def zmn(self, value: float) -> None:
        """Set the zmn property."""
        self._cards[0].set_value("zmn", value)

    @property
    def zmx(self) -> float:
        """Get or set the Maximum z-coordinate.
        """ # nopep8
        return self._cards[0].get_value("zmx")

    @zmx.setter
    def zmx(self, value: float) -> None:
        """Set the zmx property."""
        self._cards[0].set_value("zmx", value)

    @property
    def pid(self) -> int:
        """Get or set the Part ID. If zero, all active elements within box are considered.
        """ # nopep8
        return self._cards[1].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[1].set_value("pid", value)

    @property
    def level(self) -> int:
        """Get or set the Maximum number of refinement levels for elements that are contained in the box. Values of 1, 2, 3, 4,... allow a maximum of 1, 4, 16, 64, ...  elements, respectively, to be created for each original element.
        """ # nopep8
        return self._cards[1].get_value("level")

    @level.setter
    def level(self, value: int) -> None:
        """Set the level property."""
        self._cards[1].set_value("level", value)

    @property
    def lidx_ndid(self) -> int:
        """Get or set the Load curve ID/Node ID.
        GT.0: load curve ID. Define adaptive box movement (displacement
        vs. time) in global X axis.
        LT.0: absolute value is a node ID, whose movement will be
        followed by the moving adaptive box. The node ID can be
        on a moving rigid body.
        EQ.0: no movement.
        """ # nopep8
        return self._cards[1].get_value("lidx/ndid")

    @lidx_ndid.setter
    def lidx_ndid(self, value: int) -> None:
        """Set the lidx_ndid property."""
        self._cards[1].set_value("lidx/ndid", value)

    @property
    def lidy(self) -> int:
        """Get or set the Load curve ID.
        GT.0: load curve ID. Define adaptive box movement (displacement
        vs. time) in global Y axis.
        EQ.0: no movement.
        """ # nopep8
        return self._cards[1].get_value("lidy")

    @lidy.setter
    def lidy(self, value: int) -> None:
        """Set the lidy property."""
        self._cards[1].set_value("lidy", value)

    @property
    def lidz(self) -> int:
        """Get or set the Load curve ID.
        GT.0: load curve ID. Define adaptive box movement (displacement
        vs. time) in global Y axis.
        EQ.0: no movement.
        """ # nopep8
        return self._cards[1].get_value("lidz")

    @lidz.setter
    def lidz(self, value: int) -> None:
        """Set the lidz property."""
        self._cards[1].set_value("lidz", value)

    @property
    def brmin(self) -> float:
        """Get or set the Minimum mesh size in 3D tetrahedron adaptivity.
        """ # nopep8
        return self._cards[1].get_value("brmin")

    @brmin.setter
    def brmin(self, value: float) -> None:
        """Set the brmin property."""
        self._cards[1].set_value("brmin", value)

    @property
    def brmax(self) -> float:
        """Get or set the Maximum mesh size in 3D tetrahedron adaptivity.
        """ # nopep8
        return self._cards[1].get_value("brmax")

    @brmax.setter
    def brmax(self, value: float) -> None:
        """Set the brmax property."""
        self._cards[1].set_value("brmax", value)

    @property
    def xx(self) -> float:
        """Get or set the X-coordinate on local x-axis.  Origin lies at (0,0,0).  Define if the LOCAL option is active
        """ # nopep8
        return self._cards[2].get_value("xx")

    @xx.setter
    def xx(self, value: float) -> None:
        """Set the xx property."""
        self._cards[2].set_value("xx", value)

    @property
    def yx(self) -> float:
        """Get or set the Y-coordinate on local x-axis.  Define if the LOCAL option is active..
        """ # nopep8
        return self._cards[2].get_value("yx")

    @yx.setter
    def yx(self, value: float) -> None:
        """Set the yx property."""
        self._cards[2].set_value("yx", value)

    @property
    def zx(self) -> float:
        """Get or set the Z-coordinate on local x-axis.  Define if the LOCAL option is active.
        """ # nopep8
        return self._cards[2].get_value("zx")

    @zx.setter
    def zx(self, value: float) -> None:
        """Set the zx property."""
        self._cards[2].set_value("zx", value)

    @property
    def xv(self) -> float:
        """Get or set the X-coordinate of local x-y vector.  Define if the LOCAL option is active
        """ # nopep8
        return self._cards[2].get_value("xv")

    @xv.setter
    def xv(self, value: float) -> None:
        """Set the xv property."""
        self._cards[2].set_value("xv", value)

    @property
    def yv(self) -> float:
        """Get or set the Y-coordinate of local x-y vector.  Define if the LOCAL option is active.
        """ # nopep8
        return self._cards[2].get_value("yv")

    @yv.setter
    def yv(self, value: float) -> None:
        """Set the yv property."""
        self._cards[2].set_value("yv", value)

    @property
    def zv(self) -> float:
        """Get or set the Z-coordinate of local x-y vector.  Define if the LOCAL option is active..
        """ # nopep8
        return self._cards[2].get_value("zv")

    @zv.setter
    def zv(self, value: float) -> None:
        """Set the zv property."""
        self._cards[2].set_value("zv", value)

    @property
    def cx(self) -> float:
        """Get or set the X-global coordinate of offset vector to origin of local system.  Define if the LOCAL option is active.
        """ # nopep8
        return self._cards[3].get_value("cx")

    @cx.setter
    def cx(self, value: float) -> None:
        """Set the cx property."""
        self._cards[3].set_value("cx", value)

    @property
    def cy(self) -> float:
        """Get or set the Y-global coordinate of offset vector to origin of local system.  Define if the LOCAL option is active.
        """ # nopep8
        return self._cards[3].get_value("cy")

    @cy.setter
    def cy(self, value: float) -> None:
        """Set the cy property."""
        self._cards[3].set_value("cy", value)

    @property
    def cz(self) -> float:
        """Get or set the Z-global coordinate of offset vector to origin of local system.  Define if the LOCAL option is active.
        """ # nopep8
        return self._cards[3].get_value("cz")

    @cz.setter
    def cz(self, value: float) -> None:
        """Set the cz property."""
        self._cards[3].set_value("cz", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[4].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

