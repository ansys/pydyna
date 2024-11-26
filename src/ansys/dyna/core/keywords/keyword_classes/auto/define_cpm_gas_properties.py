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

class DefineCpmGasProperties(KeywordBase):
    """DYNA DEFINE_CPM_GAS_PROPERTIES keyword"""

    keyword = "DEFINE"
    subkeyword = "CPM_GAS_PROPERTIES"
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
                        kwargs.get("id")
                    ),
                    Field(
                        "xmm",
                        float,
                        10,
                        10,
                        kwargs.get("xmm")
                    ),
                    Field(
                        "cp0",
                        float,
                        20,
                        10,
                        kwargs.get("cp0")
                    ),
                    Field(
                        "cp1",
                        float,
                        30,
                        10,
                        kwargs.get("cp1")
                    ),
                    Field(
                        "cp2",
                        float,
                        40,
                        10,
                        kwargs.get("cp2")
                    ),
                    Field(
                        "cp3",
                        float,
                        50,
                        10,
                        kwargs.get("cp3")
                    ),
                    Field(
                        "cp4",
                        float,
                        60,
                        10,
                        kwargs.get("cp4")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "mut0",
                        float,
                        0,
                        10,
                        kwargs.get("mut0")
                    ),
                    Field(
                        "mut1",
                        float,
                        10,
                        10,
                        kwargs.get("mut1")
                    ),
                    Field(
                        "mut2",
                        float,
                        20,
                        10,
                        kwargs.get("mut2")
                    ),
                    Field(
                        "mut3",
                        float,
                        30,
                        10,
                        kwargs.get("mut3")
                    ),
                    Field(
                        "mut4",
                        float,
                        40,
                        10,
                        kwargs.get("mut4")
                    ),
                    Field(
                        "chm_id",
                        int,
                        50,
                        10,
                        kwargs.get("chm_id")
                    ),
                    Field(
                        "vini",
                        float,
                        60,
                        10,
                        kwargs.get("vini", 0.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineCpmGasProperties.option_specs[0],
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
    def id(self) -> typing.Optional[int]:
        """Get or set the Unique ID for this card
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def xmm(self) -> typing.Optional[float]:
        """Get or set the Molar mass
        """ # nopep8
        return self._cards[0].get_value("xmm")

    @xmm.setter
    def xmm(self, value: float) -> None:
        self._cards[0].set_value("xmm", value)

    @property
    def cp0(self) -> typing.Optional[float]:
        """Get or set the Coefficients of temperature dependent specific heat with constant pressure
        """ # nopep8
        return self._cards[0].get_value("cp0")

    @cp0.setter
    def cp0(self, value: float) -> None:
        self._cards[0].set_value("cp0", value)

    @property
    def cp1(self) -> typing.Optional[float]:
        """Get or set the Coefficients of temperature dependent specific heat with constant pressure
        """ # nopep8
        return self._cards[0].get_value("cp1")

    @cp1.setter
    def cp1(self, value: float) -> None:
        self._cards[0].set_value("cp1", value)

    @property
    def cp2(self) -> typing.Optional[float]:
        """Get or set the Coefficients of temperature dependent specific heat with constant pressure
        """ # nopep8
        return self._cards[0].get_value("cp2")

    @cp2.setter
    def cp2(self, value: float) -> None:
        self._cards[0].set_value("cp2", value)

    @property
    def cp3(self) -> typing.Optional[float]:
        """Get or set the Coefficients of temperature dependent specific heat with constant pressure
        """ # nopep8
        return self._cards[0].get_value("cp3")

    @cp3.setter
    def cp3(self, value: float) -> None:
        self._cards[0].set_value("cp3", value)

    @property
    def cp4(self) -> typing.Optional[float]:
        """Get or set the Coefficients of temperature dependent specific heat with constant pressure
        """ # nopep8
        return self._cards[0].get_value("cp4")

    @cp4.setter
    def cp4(self, value: float) -> None:
        self._cards[0].set_value("cp4", value)

    @property
    def mut0(self) -> typing.Optional[float]:
        """Get or set the Coefficients of temperature dependent Joule-Thomson effect
        """ # nopep8
        return self._cards[1].get_value("mut0")

    @mut0.setter
    def mut0(self, value: float) -> None:
        self._cards[1].set_value("mut0", value)

    @property
    def mut1(self) -> typing.Optional[float]:
        """Get or set the Coefficients of temperature dependent Joule-Thomson effect
        """ # nopep8
        return self._cards[1].get_value("mut1")

    @mut1.setter
    def mut1(self, value: float) -> None:
        self._cards[1].set_value("mut1", value)

    @property
    def mut2(self) -> typing.Optional[float]:
        """Get or set the Coefficients of temperature dependent Joule-Thomson effect
        """ # nopep8
        return self._cards[1].get_value("mut2")

    @mut2.setter
    def mut2(self, value: float) -> None:
        self._cards[1].set_value("mut2", value)

    @property
    def mut3(self) -> typing.Optional[float]:
        """Get or set the Coefficients of temperature dependent Joule-Thomson effect
        """ # nopep8
        return self._cards[1].get_value("mut3")

    @mut3.setter
    def mut3(self, value: float) -> None:
        self._cards[1].set_value("mut3", value)

    @property
    def mut4(self) -> typing.Optional[float]:
        """Get or set the Coefficients of temperature dependent Joule-Thomson effect
        """ # nopep8
        return self._cards[1].get_value("mut4")

    @mut4.setter
    def mut4(self, value: float) -> None:
        self._cards[1].set_value("mut4", value)

    @property
    def chm_id(self) -> typing.Optional[int]:
        """Get or set the Chamber ID (see Remark 1)
        """ # nopep8
        return self._cards[1].get_value("chm_id")

    @chm_id.setter
    def chm_id(self, value: int) -> None:
        self._cards[1].set_value("chm_id", value)

    @property
    def vini(self) -> float:
        """Get or set the Initial volume for user defined inflator (see Remark 1):
        EQ.0.0:	user defined inflator disabled
        GT.0.0:	initial volume
        LT.0.0:	calculate volume based on chamber geometry.
        """ # nopep8
        return self._cards[1].get_value("vini")

    @vini.setter
    def vini(self, value: float) -> None:
        self._cards[1].set_value("vini", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

