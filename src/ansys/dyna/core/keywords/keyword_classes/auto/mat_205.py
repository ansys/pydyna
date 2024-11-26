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

class Mat205(KeywordBase):
    """DYNA MAT_205 keyword"""

    keyword = "MAT"
    subkeyword = "205"
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
                        "mid",
                        int,
                        0,
                        10,
                        kwargs.get("mid")
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        kwargs.get("ro")
                    ),
                    Field(
                        "stiff",
                        float,
                        20,
                        10,
                        kwargs.get("stiff")
                    ),
                    Field(
                        "fric",
                        float,
                        30,
                        10,
                        kwargs.get("fric")
                    ),
                    Field(
                        "damp",
                        float,
                        40,
                        10,
                        kwargs.get("damp")
                    ),
                    Field(
                        "dmxpz",
                        float,
                        50,
                        10,
                        kwargs.get("dmxpz", 1.e20)
                    ),
                    Field(
                        "limpz",
                        float,
                        60,
                        10,
                        kwargs.get("limpz")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dmxpx",
                        float,
                        0,
                        10,
                        kwargs.get("dmxpx", 1.e20)
                    ),
                    Field(
                        "dmxnx",
                        float,
                        10,
                        10,
                        kwargs.get("dmxnx", 1.e20)
                    ),
                    Field(
                        "dmxpy",
                        float,
                        20,
                        10,
                        kwargs.get("dmxpy", 1.e20)
                    ),
                    Field(
                        "dmxny",
                        float,
                        30,
                        10,
                        kwargs.get("dmxny", 1.e20)
                    ),
                    Field(
                        "limpx",
                        float,
                        40,
                        10,
                        kwargs.get("limpx")
                    ),
                    Field(
                        "limnx",
                        float,
                        50,
                        10,
                        kwargs.get("limnx")
                    ),
                    Field(
                        "limpy",
                        float,
                        60,
                        10,
                        kwargs.get("limpy")
                    ),
                    Field(
                        "limny",
                        float,
                        70,
                        10,
                        kwargs.get("limny")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "krotx",
                        float,
                        0,
                        10,
                        kwargs.get("krotx")
                    ),
                    Field(
                        "kroty",
                        float,
                        10,
                        10,
                        kwargs.get("kroty")
                    ),
                    Field(
                        "krotz",
                        float,
                        20,
                        10,
                        kwargs.get("krotz")
                    ),
                    Field(
                        "tkrot",
                        float,
                        30,
                        10,
                        kwargs.get("tkrot")
                    ),
                    Field(
                        "fbondh",
                        float,
                        40,
                        10,
                        kwargs.get("fbondh")
                    ),
                    Field(
                        "fbondt",
                        float,
                        50,
                        10,
                        kwargs.get("fbondt")
                    ),
                    Field(
                        "dbondh",
                        float,
                        60,
                        10,
                        kwargs.get("dbondh", 1.e20)
                    ),
                    Field(
                        "dbondt",
                        float,
                        70,
                        10,
                        kwargs.get("dbondt", 1.e20)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcz",
                        int,
                        0,
                        10,
                        kwargs.get("lcz", 0)
                    ),
                    Field(
                        "dampz",
                        float,
                        10,
                        10,
                        kwargs.get("dampz")
                    ),
                    Field(
                        "stiffh",
                        float,
                        20,
                        10,
                        kwargs.get("stiffh")
                    ),
                    Field(
                        "frmax",
                        float,
                        30,
                        10,
                        kwargs.get("frmax")
                    ),
                    Field(
                        "damph",
                        float,
                        40,
                        10,
                        kwargs.get("damph")
                    ),
                    Field(
                        "gap0",
                        float,
                        50,
                        10,
                        kwargs.get("gap0")
                    ),
                    Field(
                        "afac",
                        float,
                        60,
                        10,
                        kwargs.get("afac", 1.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat205.option_specs[0],
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
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number or label must be specified.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def stiff(self) -> typing.Optional[float]:
        """Get or set the Stiffness (Force/length units).
        """ # nopep8
        return self._cards[0].get_value("stiff")

    @stiff.setter
    def stiff(self, value: float) -> None:
        self._cards[0].set_value("stiff", value)

    @property
    def fric(self) -> typing.Optional[float]:
        """Get or set the Friction coefficient (dimensionless).
        """ # nopep8
        return self._cards[0].get_value("fric")

    @fric.setter
    def fric(self, value: float) -> None:
        self._cards[0].set_value("fric", value)

    @property
    def damp(self) -> typing.Optional[float]:
        """Get or set the Damping factor (dimensionless), in the range 0 to 1. Suggested value 0.5.
        """ # nopep8
        return self._cards[0].get_value("damp")

    @damp.setter
    def damp(self, value: float) -> None:
        self._cards[0].set_value("damp", value)

    @property
    def dmxpz(self) -> float:
        """Get or set the Displacement limit in positive local Z direction (uplift).
        """ # nopep8
        return self._cards[0].get_value("dmxpz")

    @dmxpz.setter
    def dmxpz(self, value: float) -> None:
        self._cards[0].set_value("dmxpz", value)

    @property
    def limpz(self) -> typing.Optional[float]:
        """Get or set the Action when Node 2 passes DMXPZ:
        EQ.0:	element is deleted
        EQ.1:	further displacement is resisted by stiffness STIFF.
        """ # nopep8
        return self._cards[0].get_value("limpz")

    @limpz.setter
    def limpz(self, value: float) -> None:
        self._cards[0].set_value("limpz", value)

    @property
    def dmxpx(self) -> float:
        """Get or set the Displacement limit in positive local X direction.
        """ # nopep8
        return self._cards[1].get_value("dmxpx")

    @dmxpx.setter
    def dmxpx(self, value: float) -> None:
        self._cards[1].set_value("dmxpx", value)

    @property
    def dmxnx(self) -> float:
        """Get or set the Displacement limit in negative local X direction.
        """ # nopep8
        return self._cards[1].get_value("dmxnx")

    @dmxnx.setter
    def dmxnx(self, value: float) -> None:
        self._cards[1].set_value("dmxnx", value)

    @property
    def dmxpy(self) -> float:
        """Get or set the Displacement limit in positive local Y direction.
        """ # nopep8
        return self._cards[1].get_value("dmxpy")

    @dmxpy.setter
    def dmxpy(self, value: float) -> None:
        self._cards[1].set_value("dmxpy", value)

    @property
    def dmxny(self) -> float:
        """Get or set the Displacement limit in negative local Y direction.
        """ # nopep8
        return self._cards[1].get_value("dmxny")

    @dmxny.setter
    def dmxny(self, value: float) -> None:
        self._cards[1].set_value("dmxny", value)

    @property
    def limpx(self) -> typing.Optional[float]:
        """Get or set the Action when Node 2 passes DMXPX:
        EQ.0:	element is deleted
        EQ.1:	further displacement is resisted by stiffness STIFF.
        """ # nopep8
        return self._cards[1].get_value("limpx")

    @limpx.setter
    def limpx(self, value: float) -> None:
        self._cards[1].set_value("limpx", value)

    @property
    def limnx(self) -> typing.Optional[float]:
        """Get or set the Action when Node 2 passes DMXNX:
        EQ.0:	element is deleted
        EQ.1:	further displacement is resisted by stiffness STIFF.
        """ # nopep8
        return self._cards[1].get_value("limnx")

    @limnx.setter
    def limnx(self, value: float) -> None:
        self._cards[1].set_value("limnx", value)

    @property
    def limpy(self) -> typing.Optional[float]:
        """Get or set the Action when Node 2 passes DMXPY:
        EQ.0:	element is deleted
        EQ.1:	further displacement is resisted by stiffness STIFF.
        """ # nopep8
        return self._cards[1].get_value("limpy")

    @limpy.setter
    def limpy(self, value: float) -> None:
        self._cards[1].set_value("limpy", value)

    @property
    def limny(self) -> typing.Optional[float]:
        """Get or set the Action when Node 2 passes DMXNY:
        EQ.0:	element is deleted
        EQ.1:	further displacement is resisted by stiffness STIFF.
        """ # nopep8
        return self._cards[1].get_value("limny")

    @limny.setter
    def limny(self, value: float) -> None:
        self._cards[1].set_value("limny", value)

    @property
    def krotx(self) -> typing.Optional[float]:
        """Get or set the Rotational stiffness about local X.
        """ # nopep8
        return self._cards[2].get_value("krotx")

    @krotx.setter
    def krotx(self, value: float) -> None:
        self._cards[2].set_value("krotx", value)

    @property
    def kroty(self) -> typing.Optional[float]:
        """Get or set the Rotational stiffness about local Y.
        """ # nopep8
        return self._cards[2].get_value("kroty")

    @kroty.setter
    def kroty(self, value: float) -> None:
        self._cards[2].set_value("kroty", value)

    @property
    def krotz(self) -> typing.Optional[float]:
        """Get or set the Rotational stiffness about local Z.
        """ # nopep8
        return self._cards[2].get_value("krotz")

    @krotz.setter
    def krotz(self, value: float) -> None:
        self._cards[2].set_value("krotz", value)

    @property
    def tkrot(self) -> typing.Optional[float]:
        """Get or set the Time at which rotational stiffness becomes active.
        """ # nopep8
        return self._cards[2].get_value("tkrot")

    @tkrot.setter
    def tkrot(self, value: float) -> None:
        self._cards[2].set_value("tkrot", value)

    @property
    def fbondh(self) -> typing.Optional[float]:
        """Get or set the Force to break initial bond in plane of contact surface.
        """ # nopep8
        return self._cards[2].get_value("fbondh")

    @fbondh.setter
    def fbondh(self, value: float) -> None:
        self._cards[2].set_value("fbondh", value)

    @property
    def fbondt(self) -> typing.Optional[float]:
        """Get or set the Force to break initial bond in tension, normal to contact surface.
        """ # nopep8
        return self._cards[2].get_value("fbondt")

    @fbondt.setter
    def fbondt(self, value: float) -> None:
        self._cards[2].set_value("fbondt", value)

    @property
    def dbondh(self) -> float:
        """Get or set the Displacement over which bond force in the plane of the contact surface reduces from FBONDH to zero.
        """ # nopep8
        return self._cards[2].get_value("dbondh")

    @dbondh.setter
    def dbondh(self, value: float) -> None:
        self._cards[2].set_value("dbondh", value)

    @property
    def dbondt(self) -> float:
        """Get or set the Displacement over which bond force normal to the contact surface reduces from FBONDT to zero.
        """ # nopep8
        return self._cards[2].get_value("dbondt")

    @dbondt.setter
    def dbondt(self, value: float) -> None:
        self._cards[2].set_value("dbondt", value)

    @property
    def lcz(self) -> int:
        """Get or set the Optional loadcurve ID giving force-displacement for compression in local Z (x-axis: displacement; y-axis: force).
        """ # nopep8
        return self._cards[3].get_value("lcz")

    @lcz.setter
    def lcz(self, value: int) -> None:
        self._cards[3].set_value("lcz", value)

    @property
    def dampz(self) -> typing.Optional[float]:
        """Get or set the Viscous damping coefficient in local Z (additional to effect of DAMP) (force/velocity units).
        """ # nopep8
        return self._cards[3].get_value("dampz")

    @dampz.setter
    def dampz(self, value: float) -> None:
        self._cards[3].set_value("dampz", value)

    @property
    def stiffh(self) -> typing.Optional[float]:
        """Get or set the Elastic stiffness in local X and Y.
        """ # nopep8
        return self._cards[3].get_value("stiffh")

    @stiffh.setter
    def stiffh(self, value: float) -> None:
        self._cards[3].set_value("stiffh", value)

    @property
    def frmax(self) -> typing.Optional[float]:
        """Get or set the Upper limit on friction force.
        """ # nopep8
        return self._cards[3].get_value("frmax")

    @frmax.setter
    def frmax(self, value: float) -> None:
        self._cards[3].set_value("frmax", value)

    @property
    def damph(self) -> typing.Optional[float]:
        """Get or set the Viscous damping coefficient in local X and Y (additional to effect of DAMP) (force/velocity units).
        """ # nopep8
        return self._cards[3].get_value("damph")

    @damph.setter
    def damph(self, value: float) -> None:
        self._cards[3].set_value("damph", value)

    @property
    def gap0(self) -> typing.Optional[float]:
        """Get or set the Initial gap in local Z direction (length units).
        """ # nopep8
        return self._cards[3].get_value("gap0")

    @gap0.setter
    def gap0(self, value: float) -> None:
        self._cards[3].set_value("gap0", value)

    @property
    def afac(self) -> float:
        """Get or set the Scale factor applied to all stiffnesses and forces.
        """ # nopep8
        return self._cards[3].get_value("afac")

    @afac.setter
    def afac(self, value: float) -> None:
        self._cards[3].set_value("afac", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[4].cards[0].set_value("title", value)

