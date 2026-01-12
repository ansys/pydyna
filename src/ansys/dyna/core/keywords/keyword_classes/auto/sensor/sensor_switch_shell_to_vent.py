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

"""Module providing the SensorSwitchShellToVent class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_SENSORSWITCHSHELLTOVENT_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("type", int, 10, 10, 0),
    FieldSchema("c23", float, 20, 10, 0.7),
    FieldSchema("amax", float, 30, 10, None),
)

_SENSORSWITCHSHELLTOVENT_CARD1 = (
    FieldSchema("ssid", int, 0, 10, None),
    FieldSchema("ftime", float, 10, 10, 0.0),
    FieldSchema("c23v", float, 20, 10, 0.7),
)

_SENSORSWITCHSHELLTOVENT_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SensorSwitchShellToVent(KeywordBase):
    """DYNA SENSOR_SWITCH_SHELL_TO_VENT keyword"""

    keyword = "SENSOR"
    subkeyword = "SWITCH_SHELL_TO_VENT"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the SensorSwitchShellToVent class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SENSORSWITCHSHELLTOVENT_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SENSORSWITCHSHELLTOVENT_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = SensorSwitchShellToVent.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SENSORSWITCHSHELLTOVENT_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Part set ID/Part ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def type(self) -> int:
        """Get or set the EQ.0: Part
        EQ.1: Part set
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        """Set the type property."""
        if value not in [0, 1, None]:
            raise Exception("""type must be `None` or one of {0,1}.""")
        self._cards[0].set_value("type", value)

    @property
    def c23(self) -> float:
        """Get or set the Vent Coefficient (Default = 0.7)
        LT.0: User defined load curve ID. The vent coefficient will be
        determined by this pressure-vent_coeff curve.
        """ # nopep8
        return self._cards[0].get_value("c23")

    @c23.setter
    def c23(self, value: float) -> None:
        """Set the c23 property."""
        self._cards[0].set_value("c23", value)

    @property
    def amax(self) -> typing.Optional[float]:
        """Get or set the Maximum allowable area for failed vent surface area(VA).  If the area is bigger than AMAX, C23 will be scaled down by a factor of AMAX/VA.Otherwise C23 will be used
        """ # nopep8
        return self._cards[0].get_value("amax")

    @amax.setter
    def amax(self, value: float) -> None:
        """Set the amax property."""
        self._cards[0].set_value("amax", value)

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the ID of *SET_SHELL_LIST.
        """ # nopep8
        return self._cards[1].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[1].set_value("ssid", value)

    @property
    def ftime(self) -> float:
        """Get or set the Time to convert shell list to vent. (Default is from t = 0.)
        """ # nopep8
        return self._cards[1].get_value("ftime")

    @ftime.setter
    def ftime(self, value: float) -> None:
        """Set the ftime property."""
        self._cards[1].set_value("ftime", value)

    @property
    def c23v(self) -> float:
        """Get or set the Vent Coefficient (Default = C23)
        LT.0: User defined load curve ID. The vent coefficient will be
        determined by this pressure-vent_coeff curve.
        """ # nopep8
        return self._cards[1].get_value("c23v")

    @c23v.setter
    def c23v(self, value: float) -> None:
        """Set the c23v property."""
        self._cards[1].set_value("c23v", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[2].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

