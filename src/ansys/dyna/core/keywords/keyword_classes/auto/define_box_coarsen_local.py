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

class DefineBoxCoarsenLocal(KeywordBase):
    """DYNA DEFINE_BOX_COARSEN_LOCAL keyword"""

    keyword = "DEFINE"
    subkeyword = "BOX_COARSEN_LOCAL"
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
                        "iflag",
                        int,
                        70,
                        10,
                        kwargs.get("iflag", 0)
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
                option_spec = DefineBoxCoarsenLocal.option_specs[0],
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
    def iflag(self) -> int:
        """Get or set the Flag for protecting elements inside or outside of box:
        EQ.0: Elements outside box can not be coarsened,
        EQ.1: Elements inside box can not be coarsened.
        """ # nopep8
        return self._cards[0].get_value("iflag")

    @iflag.setter
    def iflag(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""iflag must be one of {0,1}""")
        self._cards[0].set_value("iflag", value)

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

