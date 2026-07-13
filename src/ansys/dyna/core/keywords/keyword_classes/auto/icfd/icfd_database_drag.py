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

"""Module providing the IcfdDatabaseDrag class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDDATABASEDRAG_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("cpid", int, 10, 10, None),
    FieldSchema("dtout", float, 20, 10, 0.0),
    FieldSchema("perout", int, 30, 10, 0),
    FieldSchema("divi", int, 40, 10, 10),
    FieldSchema("elout", int, 50, 10, 0),
)

class IcfdDatabaseDrag(KeywordBase):
    """DYNA ICFD_DATABASE_DRAG keyword"""

    keyword = "ICFD"
    subkeyword = "DATABASE_DRAG"

    def __init__(self, **kwargs):
        """Initialize the IcfdDatabaseDrag class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDDATABASEDRAG_CARD0,
                **kwargs,
            ),
        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of the surface (see *ICFD_PART) or fluid volume (see *ICFD_PART_VOL) in the case of the VOL keyword option where the drag force will be computed
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def cpid(self) -> typing.Optional[int]:
        """Get or set the Center point ID (see *ICFD_DEFINE_POINT) used for the calculation of the force�s moment. By default, the reference frame center is used, that is, (0,0,0).
        """ # nopep8
        return self._cards[0].get_value("cpid")

    @cpid.setter
    def cpid(self, value: int) -> None:
        """Set the cpid property."""
        self._cards[0].set_value("cpid", value)

    @property
    def dtout(self) -> float:
        """Get or set the Time interval between outputs. If DTOUT is equal to 0.0, then the ICFD timestep is used.
        """ # nopep8
        return self._cards[0].get_value("dtout")

    @dtout.setter
    def dtout(self, value: float) -> None:
        """Set the dtout property."""
        self._cards[0].set_value("dtout", value)

    @property
    def perout(self) -> int:
        """Get or set the Flag for outputting the contribution of the different elements to the total drag in fractions of the total drag in d3plot:
        EQ.0:	Off
        GE.1 : On.
        """ # nopep8
        return self._cards[0].get_value("perout")

    @perout.setter
    def perout(self, value: int) -> None:
        """Set the perout property."""
        self._cards[0].set_value("perout", value)

    @property
    def divi(self) -> int:
        """Get or set the Number of drag divisions for PEROUT. The default is 10 which means the contributions will be grouped in 10 deciles
        """ # nopep8
        return self._cards[0].get_value("divi")

    @divi.setter
    def divi(self, value: int) -> None:
        """Set the divi property."""
        self._cards[0].set_value("divi", value)

    @property
    def elout(self) -> int:
        """Get or set the Flag for outputting the drag value of each element in the d3plot:
        EQ.0:	Off
        GE.1 : On
        """ # nopep8
        return self._cards[0].get_value("elout")

    @elout.setter
    def elout(self, value: int) -> None:
        """Set the elout property."""
        self._cards[0].set_value("elout", value)

