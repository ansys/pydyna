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

"""Module providing the IcfdControlTurbSynthesis class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDCONTROLTURBSYNTHESIS_CARD0 = (
    FieldSchema("pid", int, 0, 10, 0),
    FieldSchema("iu", float, 10, 10, 0.001),
    FieldSchema("iv", float, 20, 10, 0.001),
    FieldSchema("iw", float, 30, 10, 0.001),
    FieldSchema("ls", float, 40, 10, None),
)

class IcfdControlTurbSynthesis(KeywordBase):
    """DYNA ICFD_CONTROL_TURB_SYNTHESIS keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_TURB_SYNTHESIS"

    def __init__(self, **kwargs):
        """Initialize the IcfdControlTurbSynthesis class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLTURBSYNTHESIS_CARD0,
                **kwargs,
            ),        ]
    @property
    def pid(self) -> int:
        """Get or set the Part ID of the surface with the turbulent velocity inlet condition.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def iu(self) -> float:
        """Get or set the Intensity of field fluctuations (in %) over x,y,z directions.
        """ # nopep8
        return self._cards[0].get_value("iu")

    @iu.setter
    def iu(self, value: float) -> None:
        """Set the iu property."""
        self._cards[0].set_value("iu", value)

    @property
    def iv(self) -> float:
        """Get or set the Intensity of field fluctuations (in %) over x,y,z directions.
        """ # nopep8
        return self._cards[0].get_value("iv")

    @iv.setter
    def iv(self, value: float) -> None:
        """Set the iv property."""
        self._cards[0].set_value("iv", value)

    @property
    def iw(self) -> float:
        """Get or set the Intensity of field fluctuations (in %) over x,y,z directions.
        """ # nopep8
        return self._cards[0].get_value("iw")

    @iw.setter
    def iw(self, value: float) -> None:
        """Set the iw property."""
        self._cards[0].set_value("iw", value)

    @property
    def ls(self) -> typing.Optional[float]:
        """Get or set the Integral length scale of turbulence
        """ # nopep8
        return self._cards[0].get_value("ls")

    @ls.setter
    def ls(self, value: float) -> None:
        """Set the ls property."""
        self._cards[0].set_value("ls", value)

