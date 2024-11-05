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

class IcfdControlSurfmesh(KeywordBase):
    """DYNA ICFD_CONTROL_SURFMESH keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_SURFMESH"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "rsrf",
                        int,
                        0,
                        10,
                        kwargs.get("rsrf", 0)
                    ),
                    Field(
                        "sadapt",
                        int,
                        10,
                        10,
                        kwargs.get("sadapt", 0)
                    ),
                ],
            ),
        ]

    @property
    def rsrf(self) -> int:
        """Get or set the Indicates whether or not to perform a surface re-meshing.
        EQ.0: no re-meshing is applied..
        EQ.1: Laplacian smoothing surface remeshing
        EQ.2: Curvature preserving surface remeshing.

        """ # nopep8
        return self._cards[0].get_value("rsrf")

    @rsrf.setter
    def rsrf(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""rsrf must be one of {0,1,2}""")
        self._cards[0].set_value("rsrf", value)

    @property
    def sadapt(self) -> int:
        """Get or set the Indicates whether or not to trigger adaptive surface remeshing.
        EQ.0: no adaptive surface re-meshing is applied.
        EQ.1: automatic surface remeshing when quality deteriorates (3D only).

        """ # nopep8
        return self._cards[0].get_value("sadapt")

    @sadapt.setter
    def sadapt(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""sadapt must be one of {0,1}""")
        self._cards[0].set_value("sadapt", value)

