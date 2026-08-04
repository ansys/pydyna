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

"""Module providing the EfvOutputGauge class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVOUTPUTGAUGE_CARD0 = (
    FieldSchema("dt", float, 0, 10, None),
    FieldSchema("beg", float, 10, 10, None),
    FieldSchema("end", float, 20, 10, 1e+20),
)

_EFVOUTPUTGAUGE_CARD1 = (
    FieldSchema("x", float, 0, 10, None),
    FieldSchema("y", float, 10, 10, None),
    FieldSchema("z", float, 20, 10, None),
    FieldSchema("setid", int, 30, 10, None),
    FieldSchema("mov", int, 40, 10, 0),
)

class EfvOutputGauge(KeywordBase):
    """DYNA EFV_OUTPUT_GAUGE keyword"""

    keyword = "EFV"
    subkeyword = "OUTPUT_GAUGE"

    def __init__(self, **kwargs):
        """Initialize the EfvOutputGauge class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVOUTPUTGAUGE_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVOUTPUTGAUGE_CARD1,
                **kwargs,
            ),
        ]
    @property
    def dt(self) -> typing.Optional[float]:
        """Get or set the Time interval between outputs:
        GT.0.0:	Provide DT, BEG,and END in units of time.DT is the time interval between outputs.
        LT.0.0 : Provide DT, BEG,and END in number of cycles. | DT | is the number of cycles between outputs.
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        """Set the dt property."""
        self._cards[0].set_value("dt", value)

    @property
    def beg(self) -> typing.Optional[float]:
        """Get or set the Time when the outputs starts.
        """ # nopep8
        return self._cards[0].get_value("beg")

    @beg.setter
    def beg(self, value: float) -> None:
        """Set the beg property."""
        self._cards[0].set_value("beg", value)

    @property
    def end(self) -> float:
        """Get or set the Time when the outputs ends.
        """ # nopep8
        return self._cards[0].get_value("end")

    @end.setter
    def end(self, value: float) -> None:
        """Set the end property."""
        self._cards[0].set_value("end", value)

    @property
    def x(self) -> typing.Optional[float]:
        """Get or set the Coordinates locating the gauge
        """ # nopep8
        return self._cards[1].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        """Set the x property."""
        self._cards[1].set_value("x", value)

    @property
    def y(self) -> typing.Optional[float]:
        """Get or set the Coordinates locating the gauge
        """ # nopep8
        return self._cards[1].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        """Set the y property."""
        self._cards[1].set_value("y", value)

    @property
    def z(self) -> typing.Optional[float]:
        """Get or set the Coordinates locating the gauge
        """ # nopep8
        return self._cards[1].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        """Set the z property."""
        self._cards[1].set_value("z", value)

    @property
    def setid(self) -> typing.Optional[int]:
        """Get or set the Part set ID defined in *EFV_STRUCTURED_MESH or *EFV_CYLINDRICAL_MESH. It identifies the Finite Volume Euler mesh. A unique number must be specified.
        """ # nopep8
        return self._cards[1].get_value("setid")

    @setid.setter
    def setid(self, value: int) -> None:
        """Set the setid property."""
        self._cards[1].set_value("setid", value)

    @property
    def mov(self) -> int:
        """Get or set the Gauge motion flag:
        EQ.0: Fixed
        EQ.1: Follow the material
        """ # nopep8
        return self._cards[1].get_value("mov")

    @mov.setter
    def mov(self, value: int) -> None:
        """Set the mov property."""
        if value not in [0, 1, None]:
            raise Exception("""mov must be `None` or one of {0,1}.""")
        self._cards[1].set_value("mov", value)

