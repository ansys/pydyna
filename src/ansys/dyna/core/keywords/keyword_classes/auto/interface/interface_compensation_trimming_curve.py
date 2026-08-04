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

"""Module providing the InterfaceCompensationTrimmingCurve class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_INTERFACECOMPENSATIONTRIMMINGCURVE_CARD0 = (
    FieldSchema("otrcname", str, 0, 80, None),
)

_INTERFACECOMPENSATIONTRIMMINGCURVE_CARD1 = (
    FieldSchema("utrcname", str, 0, 80, None),
)

_INTERFACECOMPENSATIONTRIMMINGCURVE_CARD2 = (
    FieldSchema("vx", float, 0, 10, 0.0),
    FieldSchema("vy", float, 10, 10, 0.0),
    FieldSchema("vz", float, 20, 10, 0.0),
    FieldSchema("depth", float, 30, 10, None),
    FieldSchema("if3d", int, 40, 10, 0),
)

class InterfaceCompensationTrimmingCurve(KeywordBase):
    """DYNA INTERFACE_COMPENSATION_TRIMMING_CURVE keyword"""

    keyword = "INTERFACE"
    subkeyword = "COMPENSATION_TRIMMING_CURVE"

    def __init__(self, **kwargs):
        """Initialize the InterfaceCompensationTrimmingCurve class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INTERFACECOMPENSATIONTRIMMINGCURVE_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _INTERFACECOMPENSATIONTRIMMINGCURVE_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _INTERFACECOMPENSATIONTRIMMINGCURVE_CARD2,
                **kwargs,
            ),
        ]
    @property
    def otrcname(self) -> typing.Optional[str]:
        """Get or set the File name for the original trimming curve
        """ # nopep8
        return self._cards[0].get_value("otrcname")

    @otrcname.setter
    def otrcname(self, value: str) -> None:
        """Set the otrcname property."""
        self._cards[0].set_value("otrcname", value)

    @property
    def utrcname(self) -> typing.Optional[str]:
        """Get or set the File name for the updated trimming curve due to compensation
        """ # nopep8
        return self._cards[1].get_value("utrcname")

    @utrcname.setter
    def utrcname(self, value: str) -> None:
        """Set the utrcname property."""
        self._cards[1].set_value("utrcname", value)

    @property
    def vx(self) -> float:
        """Get or set the Components of the trimming vector for 2D trimming
        """ # nopep8
        return self._cards[2].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        """Set the vx property."""
        self._cards[2].set_value("vx", value)

    @property
    def vy(self) -> float:
        """Get or set the Components of the trimming vector for 2D trimming
        """ # nopep8
        return self._cards[2].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        """Set the vy property."""
        self._cards[2].set_value("vy", value)

    @property
    def vz(self) -> float:
        """Get or set the Components of the trimming vector for 2D trimming
        """ # nopep8
        return self._cards[2].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        """Set the vz property."""
        self._cards[2].set_value("vz", value)

    @property
    def depth(self) -> typing.Optional[float]:
        """Get or set the For 2D trimming, depth of the trimming. For 3D, it is the minimum normal distance from the trimming curve to the blank element
        """ # nopep8
        return self._cards[2].get_value("depth")

    @depth.setter
    def depth(self, value: float) -> None:
        """Set the depth property."""
        self._cards[2].set_value("depth", value)

    @property
    def if3d(self) -> int:
        """Get or set the Flag indicating whether the trimming is 2D or 3D:
        EQ.0 : 2D trimming
        EQ.1 : 3D trimming
        """ # nopep8
        return self._cards[2].get_value("if3d")

    @if3d.setter
    def if3d(self, value: int) -> None:
        """Set the if3d property."""
        if value not in [0, 1, None]:
            raise Exception("""if3d must be `None` or one of {0,1}.""")
        self._cards[2].set_value("if3d", value)

