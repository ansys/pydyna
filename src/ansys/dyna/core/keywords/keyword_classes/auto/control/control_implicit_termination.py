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

"""Module providing the ControlImplicitTermination class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLIMPLICITTERMINATION_CARD0 = (
    FieldSchema("deltau", float, 0, 10, 0.0),
    FieldSchema("delta1", float, 10, 10, 0.0),
    FieldSchema("ketol", float, 20, 10, 0.0),
    FieldSchema("ietol", float, 30, 10, 0.0),
    FieldSchema("tetol", float, 40, 10, 0.0),
    FieldSchema("nstep", int, 50, 10, 3),
)

class ControlImplicitTermination(KeywordBase):
    """DYNA CONTROL_IMPLICIT_TERMINATION keyword"""

    keyword = "CONTROL"
    subkeyword = "IMPLICIT_TERMINATION"

    def __init__(self, **kwargs):
        """Initialize the ControlImplicitTermination class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLIMPLICITTERMINATION_CARD0,
                **kwargs,
            ),        ]
    @property
    def deltau(self) -> float:
        """Get or set the Alternate termination criteria for implicit transient simulation.
        EQ.0.0:  terminate based on ENDTIM (default)
        NE.0.0:  terminate when displacement for last time step relative to the total displacement is less than DELTAU.
        """ # nopep8
        return self._cards[0].get_value("deltau")

    @deltau.setter
    def deltau(self, value: float) -> None:
        """Set the deltau property."""
        self._cards[0].set_value("deltau", value)

    @property
    def delta1(self) -> float:
        """Get or set the If _| max displacement for single dof |_characteristic length of model is less than DELTA1 then implicit will terminate
        """ # nopep8
        return self._cards[0].get_value("delta1")

    @delta1.setter
    def delta1(self, value: float) -> None:
        """Set the delta1 property."""
        self._cards[0].set_value("delta1", value)

    @property
    def ketol(self) -> float:
        """Get or set the If Kinetic Energy drops below KETOL for STEP  consecutive implicit time steps then implicit will terminate
        """ # nopep8
        return self._cards[0].get_value("ketol")

    @ketol.setter
    def ketol(self, value: float) -> None:
        """Set the ketol property."""
        self._cards[0].set_value("ketol", value)

    @property
    def ietol(self) -> float:
        """Get or set the If Internal Energy drops below IETOL for STEP consecutive implicit time steps then implicit will terminate
        """ # nopep8
        return self._cards[0].get_value("ietol")

    @ietol.setter
    def ietol(self, value: float) -> None:
        """Set the ietol property."""
        self._cards[0].set_value("ietol", value)

    @property
    def tetol(self) -> float:
        """Get or set the If Total Energy drops below TETOL for NSTEP  consecutive implicit time steps then implicit will terminate
        """ # nopep8
        return self._cards[0].get_value("tetol")

    @tetol.setter
    def tetol(self, value: float) -> None:
        """Set the tetol property."""
        self._cards[0].set_value("tetol", value)

    @property
    def nstep(self) -> int:
        """Get or set the Number of time steps used in tests for Kinetic Energy, Internal Energy, and/or Total Energy.
        """ # nopep8
        return self._cards[0].get_value("nstep")

    @nstep.setter
    def nstep(self, value: int) -> None:
        """Set the nstep property."""
        self._cards[0].set_value("nstep", value)

