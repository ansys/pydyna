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

import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class EmRandlesExothermicReaction(KeywordBase):
    """DYNA EM_RANDLES_EXOTHERMIC_REACTION keyword"""

    keyword = "EM"
    subkeyword = "RANDLES_EXOTHERMIC_REACTION"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "areatype",
                        int,
                        0,
                        10,
                        kwargs.get("areatype", 2)
                    ),
                    Field(
                        "funcid",
                        int,
                        10,
                        10,
                        kwargs.get("funcid")
                    ),
                ],
            ),
        ]

    @property
    def areatype(self) -> int:
        """Get or set the Works the same way as RDLAREA in *EM_RANDLES_SOLID or in *EM_RANDLES_TSHELL:
        EQ.1:The heat source in FUNCTID is per unit area.
        EQ.2:Default. The heat source in FUNCTID is for the whole cell(the whole cell is shorted).
        EQ.3:The heat source returned by FUNCTID is taken as is in each Randles circuit.
        """ # nopep8
        return self._cards[0].get_value("areatype")

    @areatype.setter
    def areatype(self, value: int) -> None:
        if value not in [2, 1, 3]:
            raise Exception("""areatype must be one of {2,1,3}""")
        self._cards[0].set_value("areatype", value)

    @property
    def funcid(self) -> typing.Optional[int]:
        """Get or set the DEFINE_FUNCTION ID giving the local heat source function of local parameters for the local randle circuit.
        """ # nopep8
        return self._cards[0].get_value("funcid")

    @funcid.setter
    def funcid(self, value: int) -> None:
        self._cards[0].set_value("funcid", value)

