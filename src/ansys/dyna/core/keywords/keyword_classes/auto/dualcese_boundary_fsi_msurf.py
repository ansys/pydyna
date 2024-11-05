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

class DualceseBoundaryFsiMsurf(KeywordBase):
    """DYNA DUALCESE_BOUNDARY_FSI_MSURF keyword"""

    keyword = "DUALCESE"
    subkeyword = "BOUNDARY_FSI_MSURF"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "mspid",
                        int,
                        0,
                        10,
                        kwargs.get("mspid")
                    ),
                    Field(
                        "ref_p",
                        float,
                        10,
                        10,
                        kwargs.get("ref_p", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def mspid(self) -> typing.Optional[int]:
        """Get or set the Mesh surface part ID that is referenced by *MESH_SURFACE_ELEMENT cards
        """ # nopep8
        return self._cards[0].get_value("mspid")

    @mspid.setter
    def mspid(self, value: int) -> None:
        self._cards[0].set_value("mspid", value)

    @property
    def ref_p(self) -> float:
        """Get or set the Ambient/reference pressure of the fluid domain on the side opposite this structural interface to the fluid simulation domain.  This ambient pressure only needs to be specified in the case where the FSI structural part(s) connected with this FSI interface are not immersed in the dual CESE mesh.  This reference pressure defaults to 0.0 since moving mesh FSI calculations most often involve structures surrounded by the dual CESE mesh, and there is no need for a reference pressure in that case
        """ # nopep8
        return self._cards[0].get_value("ref_p")

    @ref_p.setter
    def ref_p(self, value: float) -> None:
        self._cards[0].set_value("ref_p", value)

