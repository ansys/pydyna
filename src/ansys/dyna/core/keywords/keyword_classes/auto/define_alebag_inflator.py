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

class DefineAlebagInflator(KeywordBase):
    """DYNA DEFINE_ALEBAG_INFLATOR keyword"""

    keyword = "DEFINE"
    subkeyword = "ALEBAG_INFLATOR"
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
                        "infid",
                        int,
                        0,
                        10,
                        kwargs.get("infid")
                    ),
                    Field(
                        "unused",
                        int,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "ngas",
                        int,
                        40,
                        10,
                        kwargs.get("ngas", 0)
                    ),
                    Field(
                        "norif",
                        int,
                        50,
                        10,
                        kwargs.get("norif", 0)
                    ),
                    Field(
                        "lcvel",
                        int,
                        60,
                        10,
                        kwargs.get("lcvel")
                    ),
                    Field(
                        "lct",
                        int,
                        70,
                        10,
                        kwargs.get("lct")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcidm",
                        int,
                        0,
                        10,
                        kwargs.get("lcidm")
                    ),
                    Field(
                        "unused",
                        int,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "mwgas",
                        float,
                        30,
                        10,
                        kwargs.get("mwgas", 0)
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "gasa",
                        float,
                        50,
                        10,
                        kwargs.get("gasa", 0)
                    ),
                    Field(
                        "gasb",
                        float,
                        60,
                        10,
                        kwargs.get("gasb", 0)
                    ),
                    Field(
                        "gasc",
                        float,
                        70,
                        10,
                        kwargs.get("gasc", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nodeid",
                        int,
                        0,
                        10,
                        kwargs.get("nodeid", 0)
                    ),
                    Field(
                        "vecid",
                        int,
                        10,
                        10,
                        kwargs.get("vecid", 0)
                    ),
                    Field(
                        "orifare",
                        float,
                        20,
                        10,
                        kwargs.get("orifare", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineAlebagInflator.option_specs[0],
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
    def infid(self) -> typing.Optional[int]:
        """Get or set the Inflator ID.
        """ # nopep8
        return self._cards[0].get_value("infid")

    @infid.setter
    def infid(self, value: int) -> None:
        self._cards[0].set_value("infid", value)

    @property
    def ngas(self) -> int:
        """Get or set the Number of Gas components
        """ # nopep8
        return self._cards[0].get_value("ngas")

    @ngas.setter
    def ngas(self, value: int) -> None:
        self._cards[0].set_value("ngas", value)

    @property
    def norif(self) -> int:
        """Get or set the Number of point source (define below)
        """ # nopep8
        return self._cards[0].get_value("norif")

    @norif.setter
    def norif(self, value: int) -> None:
        self._cards[0].set_value("norif", value)

    @property
    def lcvel(self) -> typing.Optional[int]:
        """Get or set the Load curve Id for inlet velocity. Used only for ALE phase.
        """ # nopep8
        return self._cards[0].get_value("lcvel")

    @lcvel.setter
    def lcvel(self, value: int) -> None:
        self._cards[0].set_value("lcvel", value)

    @property
    def lct(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for temperature
        """ # nopep8
        return self._cards[0].get_value("lct")

    @lct.setter
    def lct(self, value: int) -> None:
        self._cards[0].set_value("lct", value)

    @property
    def lcidm(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for mass flow rate
        """ # nopep8
        return self._cards[1].get_value("lcidm")

    @lcidm.setter
    def lcidm(self, value: int) -> None:
        self._cards[1].set_value("lcidm", value)

    @property
    def mwgas(self) -> float:
        """Get or set the Molecular weight of gas components
        """ # nopep8
        return self._cards[1].get_value("mwgas")

    @mwgas.setter
    def mwgas(self, value: float) -> None:
        self._cards[1].set_value("mwgas", value)

    @property
    def gasa(self) -> float:
        """Get or set the First Coefficient of molar heat capacity at constant pressure
        """ # nopep8
        return self._cards[1].get_value("gasa")

    @gasa.setter
    def gasa(self, value: float) -> None:
        self._cards[1].set_value("gasa", value)

    @property
    def gasb(self) -> float:
        """Get or set the Second Coefficient of molar heat capacity at constant pressure
        """ # nopep8
        return self._cards[1].get_value("gasb")

    @gasb.setter
    def gasb(self, value: float) -> None:
        self._cards[1].set_value("gasb", value)

    @property
    def gasc(self) -> float:
        """Get or set the Third Coefficient of molar heat capacity at constant pressure
        """ # nopep8
        return self._cards[1].get_value("gasc")

    @gasc.setter
    def gasc(self, value: float) -> None:
        self._cards[1].set_value("gasc", value)

    @property
    def nodeid(self) -> int:
        """Get or set the Node ID defining the point source
        """ # nopep8
        return self._cards[2].get_value("nodeid")

    @nodeid.setter
    def nodeid(self, value: int) -> None:
        self._cards[2].set_value("nodeid", value)

    @property
    def vecid(self) -> int:
        """Get or set the Vector Id defining the direction of flow at the point source
        """ # nopep8
        return self._cards[2].get_value("vecid")

    @vecid.setter
    def vecid(self, value: int) -> None:
        self._cards[2].set_value("vecid", value)

    @property
    def orifare(self) -> float:
        """Get or set the Orifice area at the point source
        """ # nopep8
        return self._cards[2].get_value("orifare")

    @orifare.setter
    def orifare(self, value: float) -> None:
        self._cards[2].set_value("orifare", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

