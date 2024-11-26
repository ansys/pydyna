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

class DefineSphAmbientDrag(KeywordBase):
    """DYNA DEFINE_SPH_AMBIENT_DRAG keyword"""

    keyword = "DEFINE"
    subkeyword = "SPH_AMBIENT_DRAG"
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
                        "icid",
                        int,
                        0,
                        10,
                        kwargs.get("icid", 0)
                    ),
                    Field(
                        "vx",
                        float,
                        10,
                        10,
                        kwargs.get("vx", 0.0)
                    ),
                    Field(
                        "vy",
                        float,
                        20,
                        10,
                        kwargs.get("vy", 0.0)
                    ),
                    Field(
                        "vz",
                        float,
                        30,
                        10,
                        kwargs.get("vz", 0.0)
                    ),
                    Field(
                        "rhoa",
                        float,
                        40,
                        10,
                        kwargs.get("rhoa")
                    ),
                    Field(
                        "mua",
                        float,
                        50,
                        10,
                        kwargs.get("mua")
                    ),
                    Field(
                        "sftens",
                        float,
                        60,
                        10,
                        kwargs.get("sftens")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineSphAmbientDrag.option_specs[0],
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
    def icid(self) -> int:
        """Get or set the Coupling with ICFD:
        EQ.0: No coupling
        """ # nopep8
        return self._cards[0].get_value("icid")

    @icid.setter
    def icid(self, value: int) -> None:
        self._cards[0].set_value("icid", value)

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
    def rhoa(self) -> typing.Optional[float]:
        """Get or set the Density of the ambient material
        """ # nopep8
        return self._cards[0].get_value("rhoa")

    @rhoa.setter
    def rhoa(self, value: float) -> None:
        self._cards[0].set_value("rhoa", value)

    @property
    def mua(self) -> typing.Optional[float]:
        """Get or set the Viscosity of the ambient material
        """ # nopep8
        return self._cards[0].get_value("mua")

    @mua.setter
    def mua(self, value: float) -> None:
        self._cards[0].set_value("mua", value)

    @property
    def sftens(self) -> typing.Optional[float]:
        """Get or set the Surface tension coefficient for the interface between the SPH fluid and ambient materials
        """ # nopep8
        return self._cards[0].get_value("sftens")

    @sftens.setter
    def sftens(self, value: float) -> None:
        self._cards[0].set_value("sftens", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

