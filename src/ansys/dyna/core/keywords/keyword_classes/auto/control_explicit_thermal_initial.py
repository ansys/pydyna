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

class ControlExplicitThermalInitial(KeywordBase):
    """DYNA CONTROL_EXPLICIT_THERMAL_INITIAL keyword"""

    keyword = "CONTROL"
    subkeyword = "EXPLICIT_THERMAL_INITIAL"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "id",
                        int,
                        0,
                        10,
                        kwargs.get("id")
                    ),
                    Field(
                        "idtyp",
                        int,
                        10,
                        10,
                        kwargs.get("idtyp", 1)
                    ),
                    Field(
                        "tempini",
                        float,
                        20,
                        10,
                        kwargs.get("tempini", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Set ID :
        GT.0:	ID is a set
        LT.0 : | ID | is an element.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def idtyp(self) -> int:
        """Get or set the Type of ID:
        EQ.1: solid
        EQ.2: shell
        EQ.3: beam.
        EQ.4:	thick shell
        """ # nopep8
        return self._cards[0].get_value("idtyp")

    @idtyp.setter
    def idtyp(self, value: int) -> None:
        if value not in [1, 2, 3, 4]:
            raise Exception("""idtyp must be one of {1,2,3,4}""")
        self._cards[0].set_value("idtyp", value)

    @property
    def tempini(self) -> float:
        """Get or set the Initial temperature (see Remark 1).
        """ # nopep8
        return self._cards[0].get_value("tempini")

    @tempini.setter
    def tempini(self, value: float) -> None:
        self._cards[0].set_value("tempini", value)

