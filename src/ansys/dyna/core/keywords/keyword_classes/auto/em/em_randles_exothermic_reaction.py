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

"""Module providing the EmRandlesExothermicReaction class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EMRANDLESEXOTHERMICREACTION_CARD0 = (
    FieldSchema("areatype", int, 0, 10, 2),
    FieldSchema("funcid", int, 10, 10, None),
)

class EmRandlesExothermicReaction(KeywordBase):
    """DYNA EM_RANDLES_EXOTHERMIC_REACTION keyword"""

    keyword = "EM"
    subkeyword = "RANDLES_EXOTHERMIC_REACTION"

    def __init__(self, **kwargs):
        """Initialize the EmRandlesExothermicReaction class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMRANDLESEXOTHERMICREACTION_CARD0,
                **kwargs,
            ),
        ]
    @property
    def areatype(self) -> int:
        """Get or set the Works the same way as RDLAREA in *EM_RANDLES_SOLID or in *EM_RANDLES_TSHELL:
        EQ.1: The heat source in FUNCTID is per unit area so that, for each local Randles circuit, the result returned by FUNCTID is multiplied by a factor areaLoc.areaLoc is the local area associated with each Randles circuit while areaGlob is the area of the whole cell.Unit consistency in SI:W*m**2.
        EQ.2: The heat source in FUNCTID is for the whole cell(the whole cell is shorted), so that, for each Randles circuit, the result returned by FUNCTID is multiplied by a factor areaLoc / areaGlob(default).Unit consistency in SI:w.
        EQ.3:The heat source returned by FUNCTID is taken as is in each Randles circuit.Unit consistency in SI:W.
        EQ.11: Same as 1, except the parameters are defined for the whole cell and are scaled in each Randles circuit by a factor
        depending on the local volume of the circuit and the global volume of the cell.
        EQ.22: Same as 2, except the parameters are defined for the whole cell and are scaled in each Randles circuit by a factor
        depending on the local volume of the circuit and the global volume of the cell.
        """ # nopep8
        return self._cards[0].get_value("areatype")

    @areatype.setter
    def areatype(self, value: int) -> None:
        """Set the areatype property."""
        if value not in [2, 1, 3, 11, 22, None]:
            raise Exception("""areatype must be `None` or one of {2,1,3,11,22}.""")
        self._cards[0].set_value("areatype", value)

    @property
    def funcid(self) -> typing.Optional[int]:
        """Get or set the DEFINE_FUNCTION ID giving the local heat source as a function of local parameters for the local Randles circuit. See Table Error! Reference source not found. in *EM_RANDLES
        """ # nopep8
        return self._cards[0].get_value("funcid")

    @funcid.setter
    def funcid(self, value: int) -> None:
        """Set the funcid property."""
        self._cards[0].set_value("funcid", value)

