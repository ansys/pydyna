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

"""Module providing the DualceseSolverSelection class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DUALCESESOLVERSELECTION_CARD0 = (
    FieldSchema("eqna", str, 0, 10, "EULER"),
)

class DualceseSolverSelection(KeywordBase):
    """DYNA DUALCESE_SOLVER_SELECTION keyword"""

    keyword = "DUALCESE"
    subkeyword = "SOLVER_SELECTION"

    def __init__(self, **kwargs):
        """Initialize the DualceseSolverSelection class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESESOLVERSELECTION_CARD0,
                **kwargs,
            ),
        ]
    @property
    def eqna(self) -> str:
        """Get or set the Select the equations being solved with the dual CESE solver:
        EQ.CAVITATION: Cavitation solver
        EQ.EULER: Euler equations
        EQ.HYBRID: Hybrid multiphase model
        EQ.NS: Navier - Stokes equations
        EQ.PHASE - CHNG: Phase change model
        EQ.TWO - PHASE: Two - phase multiphase model
        EQ.CHEM_REACT:	Chemically-reacting flow solver
        """ # nopep8
        return self._cards[0].get_value("eqna")

    @eqna.setter
    def eqna(self, value: str) -> None:
        """Set the eqna property."""
        if value not in ["EULER", "CAVITATION", "HYBRID", "NS", "PHASE-CHNG", "TWO-PHASE", "CHEM_REACT", None]:
            raise Exception("""eqna must be `None` or one of {"EULER","CAVITATION","HYBRID","NS","PHASE-CHNG","TWO-PHASE","CHEM_REACT"}.""")
        self._cards[0].set_value("eqna", value)

