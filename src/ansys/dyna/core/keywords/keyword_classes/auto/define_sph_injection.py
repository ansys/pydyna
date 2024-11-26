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

class DefineSphInjection(KeywordBase):
    """DYNA DEFINE_SPH_INJECTION keyword"""

    keyword = "DEFINE"
    subkeyword = "SPH_INJECTION"
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
                        "pid",
                        int,
                        0,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "nsid",
                        int,
                        10,
                        10,
                        kwargs.get("nsid")
                    ),
                    Field(
                        "cid",
                        int,
                        20,
                        10,
                        kwargs.get("cid")
                    ),
                    Field(
                        "vx",
                        float,
                        30,
                        10,
                        kwargs.get("vx", 0.0)
                    ),
                    Field(
                        "vy",
                        float,
                        40,
                        10,
                        kwargs.get("vy", 0.0)
                    ),
                    Field(
                        "vz",
                        float,
                        50,
                        10,
                        kwargs.get("vz", 0.0)
                    ),
                    Field(
                        "area",
                        float,
                        60,
                        10,
                        kwargs.get("area", 0.0)
                    ),
                    Field(
                        "vmag",
                        int,
                        70,
                        10,
                        kwargs.get("vmag", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "tbeg",
                        float,
                        0,
                        10,
                        kwargs.get("tbeg", 0.0)
                    ),
                    Field(
                        "tend",
                        float,
                        10,
                        10,
                        kwargs.get("tend", 1.e+20)
                    ),
                    Field(
                        "nid",
                        int,
                        20,
                        10,
                        kwargs.get("nid", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineSphInjection.option_specs[0],
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
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of newly generated SPH elements.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID. Nodes are used for initial injection position for the SPH elements.
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        self._cards[0].set_value("nsid", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Local coordinate system ID, see *DEFINE_COORDINATE_SYSTEM. X and Y coordinates define the injection plane, Z coordinate defines the normal to the injection plane.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[0].set_value("cid", value)

    @property
    def vx(self) -> float:
        """Get or set the X-velocity of the inject elements
        """ # nopep8
        return self._cards[0].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        self._cards[0].set_value("vx", value)

    @property
    def vy(self) -> float:
        """Get or set the Y-velocity of the inject elements
        """ # nopep8
        return self._cards[0].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        self._cards[0].set_value("vy", value)

    @property
    def vz(self) -> float:
        """Get or set the Z-velocity of the inject elements
        """ # nopep8
        return self._cards[0].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        self._cards[0].set_value("vz", value)

    @property
    def area(self) -> float:
        """Get or set the The area of initial injection surface. The density of injection flow comes from the material models see *MAT definition.
        """ # nopep8
        return self._cards[0].get_value("area")

    @area.setter
    def area(self, value: float) -> None:
        self._cards[0].set_value("area", value)

    @property
    def vmag(self) -> int:
        """Get or set the Injected particle velocity multiplier:
        GT.0:	The velocity of the injected particles is multiplied by VMAG.
        LT.0 : |VMAG| is a curve ID defining the magnitude of the velocity vector with respect to time, for variable injection speed.
        """ # nopep8
        return self._cards[0].get_value("vmag")

    @vmag.setter
    def vmag(self, value: int) -> None:
        self._cards[0].set_value("vmag", value)

    @property
    def tbeg(self) -> float:
        """Get or set the Birth time.
        """ # nopep8
        return self._cards[1].get_value("tbeg")

    @tbeg.setter
    def tbeg(self, value: float) -> None:
        self._cards[1].set_value("tbeg", value)

    @property
    def tend(self) -> float:
        """Get or set the End time.
        """ # nopep8
        return self._cards[1].get_value("tend")

    @tend.setter
    def tend(self, value: float) -> None:
        self._cards[1].set_value("tend", value)

    @property
    def nid(self) -> int:
        """Get or set the An optional node ID. If defined, the center of the injection plane follows the motion of this node.
        """ # nopep8
        return self._cards[1].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[1].set_value("nid", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

