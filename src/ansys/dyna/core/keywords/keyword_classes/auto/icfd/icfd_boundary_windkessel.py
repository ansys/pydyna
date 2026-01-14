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

"""Module providing the IcfdBoundaryWindkessel class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_ICFDBOUNDARYWINDKESSEL_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("wtype", int, 10, 10, None),
    FieldSchema("r1", float, 20, 10, 0.0),
    FieldSchema("c1", float, 30, 10, 0.0),
    FieldSchema("r2", float, 40, 10, 0.0),
    FieldSchema("l1", float, 50, 10, 0.0),
)

class IcfdBoundaryWindkessel(KeywordBase):
    """DYNA ICFD_BOUNDARY_WINDKESSEL keyword"""

    keyword = "ICFD"
    subkeyword = "BOUNDARY_WINDKESSEL"
    _link_fields = {
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the IcfdBoundaryWindkessel class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDBOUNDARYWINDKESSEL_CARD0,
                **kwargs,
            ),        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the PID for a fluid surface.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def wtype(self) -> typing.Optional[int]:
        """Get or set the Circuit type (See Remarks) :
        EQ.1:	Windkessel circuit
        EQ.2:	Windkessel circuit with inverted flux
        EQ.3:	CV type circuit
        EQ.4:	CV type circuit with inverted flux.
        """ # nopep8
        return self._cards[0].get_value("wtype")

    @wtype.setter
    def wtype(self, value: int) -> None:
        """Set the wtype property."""
        self._cards[0].set_value("wtype", value)

    @property
    def r1(self) -> float:
        """Get or set the Parameters (Resistances, inductances, capacities) for the different circuits.
        """ # nopep8
        return self._cards[0].get_value("r1")

    @r1.setter
    def r1(self, value: float) -> None:
        """Set the r1 property."""
        self._cards[0].set_value("r1", value)

    @property
    def c1(self) -> float:
        """Get or set the Parameters (Resistances, inductances, capacities) for the different circuits.
        """ # nopep8
        return self._cards[0].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        """Set the c1 property."""
        self._cards[0].set_value("c1", value)

    @property
    def r2(self) -> float:
        """Get or set the Parameters (Resistances, inductances, capacities) for the different circuits.
        """ # nopep8
        return self._cards[0].get_value("r2")

    @r2.setter
    def r2(self, value: float) -> None:
        """Set the r2 property."""
        self._cards[0].set_value("r2", value)

    @property
    def l1(self) -> float:
        """Get or set the Parameters (Resistances, inductances, capacities) for the different circuits.
        """ # nopep8
        return self._cards[0].get_value("l1")

    @l1.setter
    def l1(self, value: float) -> None:
        """Set the l1 property."""
        self._cards[0].set_value("l1", value)

    @property
    def pid_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

