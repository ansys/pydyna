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

"""Module providing the SensorDefineFunctionUpdate class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_SENSORDEFINEFUNCTIONUPDATE_CARD0 = (
    FieldSchema("sensid", int, 0, 10, None),
    FieldSchema("func", int, 10, 10, None),
    FieldSchema("sens1", int, 20, 10, None),
    FieldSchema("sens2", int, 30, 10, None),
    FieldSchema("sens3", int, 40, 10, None),
    FieldSchema("sens4", int, 50, 10, None),
    FieldSchema("sens5", int, 60, 10, None),
    FieldSchema("sens6", int, 70, 10, None),
)

_SENSORDEFINEFUNCTIONUPDATE_CARD1 = (
    FieldSchema("sensi", int, 0, 10, None),
    FieldSchema("sensi_1", int, 10, 10, None, "sensi+1"),
    FieldSchema("sensi_2", int, 20, 10, None, "sensi+2"),
    FieldSchema("sensi_3", int, 30, 10, None, "sensi+3"),
    FieldSchema("sensi_4", int, 40, 10, None, "sensi+4"),
    FieldSchema("sensi_5", int, 50, 10, None, "sensi+5"),
    FieldSchema("sensi_6", int, 60, 10, None, "sensi+6"),
    FieldSchema("sensi_7", int, 70, 10, None, "sensi+7"),
)

_SENSORDEFINEFUNCTIONUPDATE_CARD2 = (
    FieldSchema("birth", float, 0, 10, None),
    FieldSchema("death", float, 10, 10, None),
    FieldSchema("dtupd", float, 20, 10, None),
)

_SENSORDEFINEFUNCTIONUPDATE_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SensorDefineFunctionUpdate(KeywordBase):
    """DYNA SENSOR_DEFINE_FUNCTION_UPDATE keyword"""

    keyword = "SENSOR"
    subkeyword = "DEFINE_FUNCTION_UPDATE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the SensorDefineFunctionUpdate class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SENSORDEFINEFUNCTIONUPDATE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SENSORDEFINEFUNCTIONUPDATE_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SENSORDEFINEFUNCTIONUPDATE_CARD2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = SensorDefineFunctionUpdate.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SENSORDEFINEFUNCTIONUPDATE_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def sensid(self) -> typing.Optional[int]:
        """Get or set the Sensor ID.
        """ # nopep8
        return self._cards[0].get_value("sensid")

    @sensid.setter
    def sensid(self, value: int) -> None:
        """Set the sensid property."""
        self._cards[0].set_value("sensid", value)

    @property
    def func(self) -> typing.Optional[int]:
        """Get or set the Function ID.
        """ # nopep8
        return self._cards[0].get_value("func")

    @func.setter
    def func(self, value: int) -> None:
        """Set the func property."""
        self._cards[0].set_value("func", value)

    @property
    def sens1(self) -> typing.Optional[int]:
        """Get or set the 1st Sensor ID, the value of which will be used as the 1st argument of
        function FUNC. If defined as negative, the absolute value of SENS1,
        |SENS1|, is the number of sensors to be input. If |SENS1| > 5,
        additional cards will be needed to input the ID of all sensors. The number of sensor is limited to 15
        """ # nopep8
        return self._cards[0].get_value("sens1")

    @sens1.setter
    def sens1(self, value: int) -> None:
        """Set the sens1 property."""
        self._cards[0].set_value("sens1", value)

    @property
    def sens2(self) -> typing.Optional[int]:
        """Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
        """ # nopep8
        return self._cards[0].get_value("sens2")

    @sens2.setter
    def sens2(self, value: int) -> None:
        """Set the sens2 property."""
        self._cards[0].set_value("sens2", value)

    @property
    def sens3(self) -> typing.Optional[int]:
        """Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
        """ # nopep8
        return self._cards[0].get_value("sens3")

    @sens3.setter
    def sens3(self, value: int) -> None:
        """Set the sens3 property."""
        self._cards[0].set_value("sens3", value)

    @property
    def sens4(self) -> typing.Optional[int]:
        """Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC
        """ # nopep8
        return self._cards[0].get_value("sens4")

    @sens4.setter
    def sens4(self, value: int) -> None:
        """Set the sens4 property."""
        self._cards[0].set_value("sens4", value)

    @property
    def sens5(self) -> typing.Optional[int]:
        """Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
        """ # nopep8
        return self._cards[0].get_value("sens5")

    @sens5.setter
    def sens5(self, value: int) -> None:
        """Set the sens5 property."""
        self._cards[0].set_value("sens5", value)

    @property
    def sens6(self) -> typing.Optional[int]:
        """Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
        """ # nopep8
        return self._cards[0].get_value("sens6")

    @sens6.setter
    def sens6(self, value: int) -> None:
        """Set the sens6 property."""
        self._cards[0].set_value("sens6", value)

    @property
    def sensi(self) -> typing.Optional[int]:
        """Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
        """ # nopep8
        return self._cards[1].get_value("sensi")

    @sensi.setter
    def sensi(self, value: int) -> None:
        """Set the sensi property."""
        self._cards[1].set_value("sensi", value)

    @property
    def sensi_1(self) -> typing.Optional[int]:
        """Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
        """ # nopep8
        return self._cards[1].get_value("sensi_1")

    @sensi_1.setter
    def sensi_1(self, value: int) -> None:
        """Set the sensi_1 property."""
        self._cards[1].set_value("sensi_1", value)

    @property
    def sensi_2(self) -> typing.Optional[int]:
        """Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC
        """ # nopep8
        return self._cards[1].get_value("sensi_2")

    @sensi_2.setter
    def sensi_2(self, value: int) -> None:
        """Set the sensi_2 property."""
        self._cards[1].set_value("sensi_2", value)

    @property
    def sensi_3(self) -> typing.Optional[int]:
        """Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
        """ # nopep8
        return self._cards[1].get_value("sensi_3")

    @sensi_3.setter
    def sensi_3(self, value: int) -> None:
        """Set the sensi_3 property."""
        self._cards[1].set_value("sensi_3", value)

    @property
    def sensi_4(self) -> typing.Optional[int]:
        """Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
        """ # nopep8
        return self._cards[1].get_value("sensi_4")

    @sensi_4.setter
    def sensi_4(self, value: int) -> None:
        """Set the sensi_4 property."""
        self._cards[1].set_value("sensi_4", value)

    @property
    def sensi_5(self) -> typing.Optional[int]:
        """Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC
        """ # nopep8
        return self._cards[1].get_value("sensi_5")

    @sensi_5.setter
    def sensi_5(self, value: int) -> None:
        """Set the sensi_5 property."""
        self._cards[1].set_value("sensi_5", value)

    @property
    def sensi_6(self) -> typing.Optional[int]:
        """Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
        """ # nopep8
        return self._cards[1].get_value("sensi_6")

    @sensi_6.setter
    def sensi_6(self, value: int) -> None:
        """Set the sensi_6 property."""
        self._cards[1].set_value("sensi_6", value)

    @property
    def sensi_7(self) -> typing.Optional[int]:
        """Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
        """ # nopep8
        return self._cards[1].get_value("sensi_7")

    @sensi_7.setter
    def sensi_7(self, value: int) -> None:
        """Set the sensi_7 property."""
        self._cards[1].set_value("sensi_7", value)

    @property
    def birth(self) -> typing.Optional[float]:
        """Get or set the Sensor IBirth time of this sensor.
        """ # nopep8
        return self._cards[2].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        """Set the birth property."""
        self._cards[2].set_value("birth", value)

    @property
    def death(self) -> typing.Optional[float]:
        """Get or set the Death time of this sensor.
        """ # nopep8
        return self._cards[2].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        """Set the death property."""
        self._cards[2].set_value("death", value)

    @property
    def dtupd(self) -> typing.Optional[float]:
        """Get or set the Time interval between updates. If negative, -DTUPD is the curve defining update interval as a function of time.
        """ # nopep8
        return self._cards[2].get_value("dtupd")

    @dtupd.setter
    def dtupd(self, value: float) -> None:
        """Set the dtupd property."""
        self._cards[2].set_value("dtupd", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[3].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

