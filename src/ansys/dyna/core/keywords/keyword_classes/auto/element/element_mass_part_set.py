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

"""Module providing the ElementMassPartSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ELEMENTMASSPARTSET_CARD0 = (
    FieldSchema("psid", int, 0, 8, None),
    FieldSchema("addmass", float, 8, 16, 0.0),
    FieldSchema("finmass", float, 24, 16, 0.0),
)

class ElementMassPartSet(KeywordBase):
    """DYNA ELEMENT_MASS_PART_SET keyword"""

    keyword = "ELEMENT"
    subkeyword = "MASS_PART_SET"

    def __init__(self, **kwargs):
        """Initialize the ElementMassPartSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ELEMENTMASSPARTSET_CARD0,
                **kwargs,
            ),        ]
    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part set id, a unique number must be used.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[0].set_value("psid", value)

    @property
    def addmass(self) -> float:
        """Get or set the Added translational mass to be distributed to nodes of PID.
        """ # nopep8
        return self._cards[0].get_value("addmass")

    @addmass.setter
    def addmass(self, value: float) -> None:
        """Set the addmass property."""
        self._cards[0].set_value("addmass", value)

    @property
    def finmass(self) -> float:
        """Get or set the Final translational mass of the part set ID.  The total mass of PSID is computed and subtracted from the final mass of the part or part set to obtain the added translational mass, which must exceed zero.  Set FINMASS to zero if ADDMASS is nonzero.  FINMASS is available in the R3 release of version 971.
        """ # nopep8
        return self._cards[0].get_value("finmass")

    @finmass.setter
    def finmass(self, value: float) -> None:
        """Set the finmass property."""
        self._cards[0].set_value("finmass", value)

