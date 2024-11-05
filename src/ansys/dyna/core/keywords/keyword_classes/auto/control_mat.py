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

class ControlMat(KeywordBase):
    """DYNA CONTROL_MAT keyword"""

    keyword = "CONTROL"
    subkeyword = "MAT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "maef",
                        int,
                        0,
                        10,
                        kwargs.get("maef", 0)
                    ),
                    Field(
                        "unused",
                        int,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "umchk",
                        int,
                        20,
                        10,
                        kwargs.get("umchk", 0)
                    ),
                ],
            ),
        ]

    @property
    def maef(self) -> int:
        """Get or set the EQ.0:	failure of  *MAT_ADD_EROSION definitions are active.
        EQ.1:	switch off all *MAT_ADD_EROSION definitions globally. This replaces the need to remove every *MAT_ADD_EROSION card in large models.
        """ # nopep8
        return self._cards[0].get_value("maef")

    @maef.setter
    def maef(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""maef must be one of {0,1}""")
        self._cards[0].set_value("maef", value)

    @property
    def umchk(self) -> int:
        """Get or set the User material check. Initially in the first calculation cycle, it is checked if true user-defined material models are applied or whether only the default, unmodified subroutines already present in the native dyn21 files are called.
        EQ.0:	Warning is issued.
        EQ.1 : Error termination occurs.
        """ # nopep8
        return self._cards[0].get_value("umchk")

    @umchk.setter
    def umchk(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""umchk must be one of {0,1}""")
        self._cards[0].set_value("umchk", value)

