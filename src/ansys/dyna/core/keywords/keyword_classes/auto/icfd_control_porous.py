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

class IcfdControlPorous(KeywordBase):
    """DYNA ICFD_CONTROL_POROUS keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_POROUS"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "pmstype",
                        int,
                        0,
                        10,
                        kwargs.get("pmstype", 0)
                    ),
                ],
            ),
        ]

    @property
    def pmstype(self) -> int:
        """Get or set the Indicates the porous media solve type.
        EQ.0: Anisotropic Generalized Navier-Stokes model for porous media (See *ICFD_MODEL_POROUS) using Fractional step method.
        Anisotropic Darcy-Forcheimer model using a Monolithic approach for the solve. This method is better suited for very low Reynolds flows through porous media.
        """ # nopep8
        return self._cards[0].get_value("pmstype")

    @pmstype.setter
    def pmstype(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""pmstype must be one of {0,1}""")
        self._cards[0].set_value("pmstype", value)

