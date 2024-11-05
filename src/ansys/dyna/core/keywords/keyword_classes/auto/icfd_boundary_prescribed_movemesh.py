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

class IcfdBoundaryPrescribedMovemesh(KeywordBase):
    """DYNA ICFD_BOUNDARY_PRESCRIBED_MOVEMESH keyword"""

    keyword = "ICFD"
    subkeyword = "BOUNDARY_PRESCRIBED_MOVEMESH"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "dofx",
                        int,
                        10,
                        10,
                        kwargs.get("dofx", 1)
                    ),
                    Field(
                        "dofy",
                        int,
                        20,
                        10,
                        kwargs.get("dofy", 1)
                    ),
                    Field(
                        "dofz",
                        int,
                        30,
                        10,
                        kwargs.get("dofz", 1)
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the PID for a fluid surface.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def dofx(self) -> int:
        """Get or set the Degrees of freedom in the X,Y and Z directions :
        EQ.0: degree of freedom left free (Surface nodes can translate in the
        chosen direction)
        EQ. 1: prescribed degree of freedom (Surface nodes are blocked).
        """ # nopep8
        return self._cards[0].get_value("dofx")

    @dofx.setter
    def dofx(self, value: int) -> None:
        if value not in [1, 0]:
            raise Exception("""dofx must be one of {1,0}""")
        self._cards[0].set_value("dofx", value)

    @property
    def dofy(self) -> int:
        """Get or set the Degrees of freedom in the X,Y and Z directions :
        EQ.0: degree of freedom left free (Surface nodes can translate in the
        chosen direction)
        EQ. 1: prescribed degree of freedom (Surface nodes are blocked).
        """ # nopep8
        return self._cards[0].get_value("dofy")

    @dofy.setter
    def dofy(self, value: int) -> None:
        if value not in [1, 0]:
            raise Exception("""dofy must be one of {1,0}""")
        self._cards[0].set_value("dofy", value)

    @property
    def dofz(self) -> int:
        """Get or set the Degrees of freedom in the X,Y and Z directions :
        EQ.0: degree of freedom left free (Surface nodes can translate in the
        chosen direction)
        EQ. 1: prescribed degree of freedom (Surface nodes are blocked).
        """ # nopep8
        return self._cards[0].get_value("dofz")

    @dofz.setter
    def dofz(self, value: int) -> None:
        if value not in [1, 0]:
            raise Exception("""dofz must be one of {1,0}""")
        self._cards[0].set_value("dofz", value)

