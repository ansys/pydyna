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

"""Module providing the SensorDefineCalc_MathUpdate class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_SENSORDEFINECALC_MATHUPDATE_CARD0 = (
    FieldSchema("sensid", int, 0, 10, None),
    FieldSchema("calc", str, 10, 10, None),
    FieldSchema("sens1", int, 20, 10, None),
    FieldSchema("sens2", int, 30, 10, None),
    FieldSchema("sens3", int, 40, 10, None),
    FieldSchema("sens4", int, 50, 10, None),
    FieldSchema("sens5", int, 60, 10, None),
    FieldSchema("sens6", int, 70, 10, None),
)

_SENSORDEFINECALC_MATHUPDATE_CARD1 = (
    FieldSchema("birth", float, 0, 10, None),
    FieldSchema("death", float, 10, 10, None),
    FieldSchema("dtupd", float, 20, 10, None),
)

_SENSORDEFINECALC_MATHUPDATE_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SensorDefineCalc_MathUpdate(KeywordBase):
    """DYNA SENSOR_DEFINE_CALC-MATH_UPDATE keyword"""

    keyword = "SENSOR"
    subkeyword = "DEFINE_CALC-MATH_UPDATE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the SensorDefineCalc_MathUpdate class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SENSORDEFINECALC_MATHUPDATE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SENSORDEFINECALC_MATHUPDATE_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = SensorDefineCalc_MathUpdate.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SENSORDEFINECALC_MATHUPDATE_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def sensid(self) -> typing.Optional[int]:
        """Get or set the sensor ID
        """ # nopep8
        return self._cards[0].get_value("sensid")

    @sensid.setter
    def sensid(self, value: int) -> None:
        """Set the sensid property."""
        self._cards[0].set_value("sensid", value)

    @property
    def calc(self) -> typing.Optional[str]:
        """Get or set the Mathematic calculation.
        """ # nopep8
        return self._cards[0].get_value("calc")

    @calc.setter
    def calc(self, value: str) -> None:
        """Set the calc property."""
        self._cards[0].set_value("calc", value)

    @property
    def sens1(self) -> typing.Optional[int]:
        """Get or set the ith Sensor ID
        """ # nopep8
        return self._cards[0].get_value("sens1")

    @sens1.setter
    def sens1(self, value: int) -> None:
        """Set the sens1 property."""
        self._cards[0].set_value("sens1", value)

    @property
    def sens2(self) -> typing.Optional[int]:
        """Get or set the ith Sensor ID
        """ # nopep8
        return self._cards[0].get_value("sens2")

    @sens2.setter
    def sens2(self, value: int) -> None:
        """Set the sens2 property."""
        self._cards[0].set_value("sens2", value)

    @property
    def sens3(self) -> typing.Optional[int]:
        """Get or set the ith Sensor ID
        """ # nopep8
        return self._cards[0].get_value("sens3")

    @sens3.setter
    def sens3(self, value: int) -> None:
        """Set the sens3 property."""
        self._cards[0].set_value("sens3", value)

    @property
    def sens4(self) -> typing.Optional[int]:
        """Get or set the ith Sensor ID
        """ # nopep8
        return self._cards[0].get_value("sens4")

    @sens4.setter
    def sens4(self, value: int) -> None:
        """Set the sens4 property."""
        self._cards[0].set_value("sens4", value)

    @property
    def sens5(self) -> typing.Optional[int]:
        """Get or set the ith Sensor ID
        """ # nopep8
        return self._cards[0].get_value("sens5")

    @sens5.setter
    def sens5(self, value: int) -> None:
        """Set the sens5 property."""
        self._cards[0].set_value("sens5", value)

    @property
    def sens6(self) -> typing.Optional[int]:
        """Get or set the ith Sensor ID
        """ # nopep8
        return self._cards[0].get_value("sens6")

    @sens6.setter
    def sens6(self, value: int) -> None:
        """Set the sens6 property."""
        self._cards[0].set_value("sens6", value)

    @property
    def birth(self) -> typing.Optional[float]:
        """Get or set the Sensor IBirth time of this sensor.
        """ # nopep8
        return self._cards[1].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        """Set the birth property."""
        self._cards[1].set_value("birth", value)

    @property
    def death(self) -> typing.Optional[float]:
        """Get or set the Death time of this sensor.
        """ # nopep8
        return self._cards[1].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        """Set the death property."""
        self._cards[1].set_value("death", value)

    @property
    def dtupd(self) -> typing.Optional[float]:
        """Get or set the Time interval between updates. If negative, -DTUPD is the curve defining update interval as a function of time.
        """ # nopep8
        return self._cards[1].get_value("dtupd")

    @dtupd.setter
    def dtupd(self, value: float) -> None:
        """Set the dtupd property."""
        self._cards[1].set_value("dtupd", value)

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


class SensorDefineCalcMathUpdate(SensorDefineCalc_MathUpdate):
    """Alias for SENSOR keyword."""
    subkeyword = "DEFINE_CALC_MATH_UPDATE"
