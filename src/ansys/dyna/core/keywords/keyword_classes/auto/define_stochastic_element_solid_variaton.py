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

class DefineStochasticElementSolidVariaton(KeywordBase):
    """DYNA DEFINE_STOCHASTIC_ELEMENT_SOLID_VARIATON keyword"""

    keyword = "DEFINE"
    subkeyword = "STOCHASTIC_ELEMENT_SOLID_VARIATON"
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
                        "ide",
                        int,
                        0,
                        10,
                        kwargs.get("ide", 0)
                    ),
                    Field(
                        "varsy",
                        float,
                        10,
                        10,
                        kwargs.get("varsy", 0.0)
                    ),
                    Field(
                        "varf",
                        float,
                        20,
                        10,
                        kwargs.get("varf", 0.0)
                    ),
                    Field(
                        "varro",
                        float,
                        30,
                        10,
                        kwargs.get("varro", 0.0)
                    ),
                    Field(
                        "vare",
                        float,
                        40,
                        10,
                        kwargs.get("vare", 0.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineStochasticElementSolidVariaton.option_specs[0],
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
    def ide(self) -> int:
        """Get or set the Element ID.
        """ # nopep8
        return self._cards[0].get_value("ide")

    @ide.setter
    def ide(self, value: int) -> None:
        self._cards[0].set_value("ide", value)

    @property
    def varsy(self) -> float:
        """Get or set the The yield stress and its hardening function are scaled by 1.+VARSY.
        """ # nopep8
        return self._cards[0].get_value("varsy")

    @varsy.setter
    def varsy(self, value: float) -> None:
        self._cards[0].set_value("varsy", value)

    @property
    def varf(self) -> float:
        """Get or set the The failure criterion is scaled by 1+VARF.
        """ # nopep8
        return self._cards[0].get_value("varf")

    @varf.setter
    def varf(self, value: float) -> None:
        self._cards[0].set_value("varf", value)

    @property
    def varro(self) -> float:
        """Get or set the The density is scaled by 1+VARRO. This is intended to be used with topology optimization. This option is not available for shell elements.
        """ # nopep8
        return self._cards[0].get_value("varro")

    @varro.setter
    def varro(self, value: float) -> None:
        self._cards[0].set_value("varro", value)

    @property
    def vare(self) -> float:
        """Get or set the The elastic moduli are scaled by 1+VARE. This is intended to be used with topology optimization.
        """ # nopep8
        return self._cards[0].get_value("vare")

    @vare.setter
    def vare(self, value: float) -> None:
        self._cards[0].set_value("vare", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

