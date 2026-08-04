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

"""Module providing the EfvControlSolver class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVCONTROLSOLVER_CARD0 = (
    FieldSchema("denup", int, 0, 10, 1),
    FieldSchema("strup", int, 10, 10, 1),
    FieldSchema("prsup", int, 20, 10, 2),
)

class EfvControlSolver(KeywordBase):
    """DYNA EFV_CONTROL_SOLVER keyword"""

    keyword = "EFV"
    subkeyword = "CONTROL_SOLVER"

    def __init__(self, **kwargs):
        """Initialize the EfvControlSolver class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVCONTROLSOLVER_CARD0,
                **kwargs,
            ),
        ]
    @property
    def denup(self) -> int:
        """Get or set the Method to update the density:
        EQ.1: Automatic update determined by Efv.This update is either an incremental or a total update.
        EQ.2: Incremental update.The density is computed from the change in volume relative to the last cycle.
        EQ.3: Total update.The density is based on the current element mass and volume.
        """ # nopep8
        return self._cards[0].get_value("denup")

    @denup.setter
    def denup(self, value: int) -> None:
        """Set the denup property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""denup must be `None` or one of {1,2,3}.""")
        self._cards[0].set_value("denup", value)

    @property
    def strup(self) -> int:
        """Get or set the Method to update the strain rate for Eulerian materials with strength. The strain rate is derived from the velocity gradients, which are obtained by an element surface integration of the velocity. The face velocity used in this integration is either density-weighted or a simple average of the element velocity on both sides of the face.
        EQ.1: Weighted method
        EQ.2: Averaged method
        """ # nopep8
        return self._cards[0].get_value("strup")

    @strup.setter
    def strup(self, value: int) -> None:
        """Set the strup property."""
        if value not in [1, 2, None]:
            raise Exception("""strup must be `None` or one of {1,2}.""")
        self._cards[0].set_value("strup", value)

    @property
    def prsup(self) -> int:
        """Get or set the Method to update Eulerian pressures:
        EQ.1: Volume - weighted average.
        EQ.2: Equilibrium.An iterative procedure is used for each material in an Eulerian cell to establish a single pressure consistent with the equation of stateand conditions of each material in the cell.This method is only applied if the cell contains two materialsand these materials are ideal or fully burnt JWL gases.If these conditions are not met, the pressure is averaged accounting for the volume fraction of each material(volume - weighted average).
        """ # nopep8
        return self._cards[0].get_value("prsup")

    @prsup.setter
    def prsup(self, value: int) -> None:
        """Set the prsup property."""
        if value not in [2, 1, None]:
            raise Exception("""prsup must be `None` or one of {2,1}.""")
        self._cards[0].set_value("prsup", value)

