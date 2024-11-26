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

class MatAddThermalExpansion(KeywordBase):
    """DYNA MAT_ADD_THERMAL_EXPANSION keyword"""

    keyword = "MAT"
    subkeyword = "ADD_THERMAL_EXPANSION"
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
                        "lcid",
                        int,
                        10,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "mult",
                        float,
                        20,
                        10,
                        kwargs.get("mult", 1.0)
                    ),
                    Field(
                        "lcid",
                        int,
                        30,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "multy",
                        float,
                        40,
                        10,
                        kwargs.get("multy", 1.0)
                    ),
                    Field(
                        "lcid",
                        int,
                        50,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "multz",
                        float,
                        60,
                        10,
                        kwargs.get("multz", 1.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatAddThermalExpansion.option_specs[0],
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
        """Get or set the Part ID for which the thermal expansion property applies
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the LCID.GT.0: Load curve ID defining thermal expansion coefficient as a function of temperature LCID.
        EQ.0: Thermal expansion coefficient given by constant MULT
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def mult(self) -> float:
        """Get or set the Scale factor scaling load curve given by LCID
        """ # nopep8
        return self._cards[0].get_value("mult")

    @mult.setter
    def mult(self, value: float) -> None:
        self._cards[0].set_value("mult", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining thermal expansion coefficient in local y-direction as a function of temperature.  If zero, the thermal  expansion coefficient in local y-direction given by constant MULTY, if MULTY=0 as well, the properties in x-direction are used.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def multy(self) -> float:
        """Get or set the Scale factor scaling load curve given by LCIDY
        """ # nopep8
        return self._cards[0].get_value("multy")

    @multy.setter
    def multy(self, value: float) -> None:
        self._cards[0].set_value("multy", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining thermal expansion coefficient in local z-direction as a function of temperature.  If zero, the thermal  expansion coefficient in local z-direction given by constant MULTZ, if MULTZ=0 as well, the properties in x-direction are used.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def multz(self) -> float:
        """Get or set the Scale factor scaling load curve given by LCIDZ
        """ # nopep8
        return self._cards[0].get_value("multz")

    @multz.setter
    def multz(self, value: float) -> None:
        self._cards[0].set_value("multz", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

