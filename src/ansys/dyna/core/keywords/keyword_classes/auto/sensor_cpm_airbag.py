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

class SensorCpmAirbag(KeywordBase):
    """DYNA SENSOR_CPM_AIRBAG keyword"""

    keyword = "SENSOR"
    subkeyword = "CPM_AIRBAG"
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
                        "cpmid",
                        int,
                        0,
                        10,
                        kwargs.get("cpmid")
                    ),
                    Field(
                        "switid",
                        int,
                        10,
                        10,
                        kwargs.get("switid")
                    ),
                    Field(
                        "tbirth",
                        float,
                        20,
                        10,
                        kwargs.get("tbirth")
                    ),
                    Field(
                        "tdeath",
                        float,
                        30,
                        10,
                        kwargs.get("tdeath")
                    ),
                    Field(
                        "tdr",
                        float,
                        40,
                        10,
                        kwargs.get("tdr")
                    ),
                    Field(
                        "defps",
                        int,
                        50,
                        10,
                        kwargs.get("defps")
                    ),
                    Field(
                        "rbpid",
                        int,
                        60,
                        10,
                        kwargs.get("rbpid")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = SensorCpmAirbag.option_specs[0],
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
    def cpmid(self) -> typing.Optional[int]:
        """Get or set the Bag ID of *AIRBAG_PARTICLE_ID.
        """ # nopep8
        return self._cards[0].get_value("cpmid")

    @cpmid.setter
    def cpmid(self, value: int) -> None:
        self._cards[0].set_value("cpmid", value)

    @property
    def switid(self) -> typing.Optional[int]:
        """Get or set the Switch ID of *SENSOR_SWITCH.
        """ # nopep8
        return self._cards[0].get_value("switid")

    @switid.setter
    def switid(self, value: int) -> None:
        self._cards[0].set_value("switid", value)

    @property
    def tbirth(self) -> typing.Optional[float]:
        """Get or set the If SWITID is set, TBIRTH is not active. If SWITID is 0, TBIRTH is
        the activation time for the bag with ID = CPMID. All of the time
        dependent curves that are used in this bag will be offset by the value of TBIRTH.
        """ # nopep8
        return self._cards[0].get_value("tbirth")

    @tbirth.setter
    def tbirth(self, value: float) -> None:
        self._cards[0].set_value("tbirth", value)

    @property
    def tdeath(self) -> typing.Optional[float]:
        """Get or set the Disable the CPMID bag when the simulation time exceeds this value.
        """ # nopep8
        return self._cards[0].get_value("tdeath")

    @tdeath.setter
    def tdeath(self, value: float) -> None:
        self._cards[0].set_value("tdeath", value)

    @property
    def tdr(self) -> typing.Optional[float]:
        """Get or set the If TDR is greater than 0 the bag with ID = CPMID will be rigid
        starting at first cycle and switch to deformable at time TDR.
        """ # nopep8
        return self._cards[0].get_value("tdr")

    @tdr.setter
    def tdr(self, value: float) -> None:
        self._cards[0].set_value("tdr", value)

    @property
    def defps(self) -> typing.Optional[int]:
        """Get or set the Part set ID specifiying which parts of the bag with ID = CPMID are deformable.
        """ # nopep8
        return self._cards[0].get_value("defps")

    @defps.setter
    def defps(self, value: int) -> None:
        self._cards[0].set_value("defps", value)

    @property
    def rbpid(self) -> typing.Optional[int]:
        """Get or set the Part ID of the rigid body to which the part is merged.
        """ # nopep8
        return self._cards[0].get_value("rbpid")

    @rbpid.setter
    def rbpid(self, value: int) -> None:
        self._cards[0].set_value("rbpid", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

