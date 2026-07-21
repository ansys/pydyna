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

"""Module providing the DualceseSolverChemicallyReacting class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DUALCESESOLVERCHEMICALLYREACTING_CARD0 = (
    FieldSchema("id_chem", int, 0, 10, None),
    FieldSchema("flow_type", str, 10, 10, "NS"),
    FieldSchema("mix_flag", int, 20, 10, 0),
)

class DualceseSolverChemicallyReacting(KeywordBase):
    """DYNA DUALCESE_SOLVER_CHEMICALLY_REACTING keyword"""

    keyword = "DUALCESE"
    subkeyword = "SOLVER_CHEMICALLY_REACTING"

    def __init__(self, **kwargs):
        """Initialize the DualceseSolverChemicallyReacting class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESESOLVERCHEMICALLYREACTING_CARD0,
                **kwargs,
            ),
        ]
    @property
    def id_chem(self) -> typing.Optional[int]:
        """Get or set the Equation of state (EOS) ID that must refer to an EOS defined with *DUALCESE_EOS_CAV_HOMOG_EQUILIB.
        """ # nopep8
        return self._cards[0].get_value("id_chem")

    @id_chem.setter
    def id_chem(self, value: int) -> None:
        """Set the id_chem property."""
        self._cards[0].set_value("id_chem", value)

    @property
    def flow_type(self) -> str:
        """Get or set the Flow physics:
        EQ.NS: Navier-Stokes solver (default);
        EQ.EULER: Euler equations solver
        """ # nopep8
        return self._cards[0].get_value("flow_type")

    @flow_type.setter
    def flow_type(self, value: str) -> None:
        """Set the flow_type property."""
        if value not in ["NS", "EULER", None]:
            raise Exception("""flow_type must be `None` or one of {"NS","EULER"}.""")
        self._cards[0].set_value("flow_type", value)

    @property
    def mix_flag(self) -> int:
        """Get or set the Flag for determining whether only mixing is studied:
        EQ.0:	Perform the full calculation(chemical reactions and mixing).
        EQ.1 : Perform only the mixing(transport) aspect of the calculation, meaning that the chemical reactions are ignored(not computed).
        """ # nopep8
        return self._cards[0].get_value("mix_flag")

    @mix_flag.setter
    def mix_flag(self, value: int) -> None:
        """Set the mix_flag property."""
        if value not in [0, 1, None]:
            raise Exception("""mix_flag must be `None` or one of {0,1}.""")
        self._cards[0].set_value("mix_flag", value)

