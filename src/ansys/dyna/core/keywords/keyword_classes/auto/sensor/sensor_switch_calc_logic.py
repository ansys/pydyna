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

"""Module providing the SensorSwitchCalcLogic class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_SENSORSWITCHCALCLOGIC_CARD0 = (
    FieldSchema("switid", int, 0, 10, None),
    FieldSchema("swit1", int, 10, 10, None),
    FieldSchema("swit2", int, 20, 10, None),
    FieldSchema("swit3", int, 30, 10, None),
    FieldSchema("swit4", int, 40, 10, None),
    FieldSchema("swit5", int, 50, 10, None),
    FieldSchema("swit6", int, 60, 10, None),
    FieldSchema("swit7", int, 70, 10, None),
)

_SENSORSWITCHCALCLOGIC_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SensorSwitchCalcLogic(KeywordBase):
    """DYNA SENSOR_SWITCH_CALC_LOGIC keyword"""

    keyword = "SENSOR"
    subkeyword = "SWITCH_CALC_LOGIC"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the SensorSwitchCalcLogic class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SENSORSWITCHCALCLOGIC_CARD0,
                **kwargs,
            ),            OptionCardSet(
                option_spec = SensorSwitchCalcLogic.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SENSORSWITCHCALCLOGIC_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def switid(self) -> typing.Optional[int]:
        """Get or set the Switch ID can be referred directly by *SENSOR_CONTROL to control the status of entities like CONTACT and AIRBAG, or can be referred to by *SENSOR_SWITCH_CALC-LOGIC for logic computation.
        """ # nopep8
        return self._cards[0].get_value("switid")

    @switid.setter
    def switid(self, value: int) -> None:
        """Set the switid property."""
        self._cards[0].set_value("switid", value)

    @property
    def swit1(self) -> typing.Optional[int]:
        """Get or set the Input a positive sensor ID for AND and negative ID for OR
        """ # nopep8
        return self._cards[0].get_value("swit1")

    @swit1.setter
    def swit1(self, value: int) -> None:
        """Set the swit1 property."""
        self._cards[0].set_value("swit1", value)

    @property
    def swit2(self) -> typing.Optional[int]:
        """Get or set the Input a positive sensor ID for AND and negative ID for OR
        """ # nopep8
        return self._cards[0].get_value("swit2")

    @swit2.setter
    def swit2(self, value: int) -> None:
        """Set the swit2 property."""
        self._cards[0].set_value("swit2", value)

    @property
    def swit3(self) -> typing.Optional[int]:
        """Get or set the Input a positive sensor ID for AND and negative ID for OR
        """ # nopep8
        return self._cards[0].get_value("swit3")

    @swit3.setter
    def swit3(self, value: int) -> None:
        """Set the swit3 property."""
        self._cards[0].set_value("swit3", value)

    @property
    def swit4(self) -> typing.Optional[int]:
        """Get or set the Input a positive sensor ID for AND and negative ID for OR
        """ # nopep8
        return self._cards[0].get_value("swit4")

    @swit4.setter
    def swit4(self, value: int) -> None:
        """Set the swit4 property."""
        self._cards[0].set_value("swit4", value)

    @property
    def swit5(self) -> typing.Optional[int]:
        """Get or set the Input a positive sensor ID for AND and negative ID for OR
        """ # nopep8
        return self._cards[0].get_value("swit5")

    @swit5.setter
    def swit5(self, value: int) -> None:
        """Set the swit5 property."""
        self._cards[0].set_value("swit5", value)

    @property
    def swit6(self) -> typing.Optional[int]:
        """Get or set the Input a positive sensor ID for AND and negative ID for OR
        """ # nopep8
        return self._cards[0].get_value("swit6")

    @swit6.setter
    def swit6(self, value: int) -> None:
        """Set the swit6 property."""
        self._cards[0].set_value("swit6", value)

    @property
    def swit7(self) -> typing.Optional[int]:
        """Get or set the Input a positive sensor ID for AND and negative ID for OR
        """ # nopep8
        return self._cards[0].get_value("swit7")

    @swit7.setter
    def swit7(self, value: int) -> None:
        """Set the swit7 property."""
        self._cards[0].set_value("swit7", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[1].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

