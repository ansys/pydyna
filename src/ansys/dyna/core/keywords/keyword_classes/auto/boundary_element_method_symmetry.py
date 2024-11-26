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

class BoundaryElementMethodSymmetry(KeywordBase):
    """DYNA BOUNDARY_ELEMENT_METHOD_SYMMETRY keyword"""

    keyword = "BOUNDARY"
    subkeyword = "ELEMENT_METHOD_SYMMETRY"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "bemsym",
                        int,
                        0,
                        10,
                        kwargs.get("bemsym", 0)
                    ),
                ],
            ),
        ]

    @property
    def bemsym(self) -> int:
        """Get or set the Defines a symmetry plane for boundary element method.
        EQ.0: no symmetry plane is defined,
        EQ.1: x=0 is a symmetry plane,
        EQ.2: y=0 is a symmetry plane,
        EQ.3: z=0 is a symmetry plane.
        """ # nopep8
        return self._cards[0].get_value("bemsym")

    @bemsym.setter
    def bemsym(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""bemsym must be one of {0,1,2,3}""")
        self._cards[0].set_value("bemsym", value)

