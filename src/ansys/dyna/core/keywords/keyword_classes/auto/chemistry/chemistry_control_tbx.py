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

"""Module providing the ChemistryControlTbx class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CHEMISTRYCONTROLTBX_CARD0 = (
    FieldSchema("idchem", int, 0, 10, None),
    FieldSchema("usepar", int, 10, 10, 1),
)

class ChemistryControlTbx(KeywordBase):
    """DYNA CHEMISTRY_CONTROL_TBX keyword"""

    keyword = "CHEMISTRY"
    subkeyword = "CONTROL_TBX"

    def __init__(self, **kwargs):
        """Initialize the ChemistryControlTbx class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CHEMISTRYCONTROLTBX_CARD0,
                **kwargs,
            ),        ]
    @property
    def idchem(self) -> typing.Optional[int]:
        """Get or set the Identifier for this chemistry solver.
        """ # nopep8
        return self._cards[0].get_value("idchem")

    @idchem.setter
    def idchem(self, value: int) -> None:
        """Set the idchem property."""
        self._cards[0].set_value("idchem", value)

    @property
    def usepar(self) -> int:
        """Get or set the Coupling flag indicating if a *STOCHASTIC_TBX_PARTICLES card is provided for this model:
        EQ.1:uses a *STOCHASTIC_TBX_PARTICLES card (default).
        EQ.0: does not use such a card.
        """ # nopep8
        return self._cards[0].get_value("usepar")

    @usepar.setter
    def usepar(self, value: int) -> None:
        """Set the usepar property."""
        if value not in [1, 0, None]:
            raise Exception("""usepar must be `None` or one of {1,0}.""")
        self._cards[0].set_value("usepar", value)

