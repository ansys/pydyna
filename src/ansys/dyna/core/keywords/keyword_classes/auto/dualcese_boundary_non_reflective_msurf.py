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

"""Module providing the DualceseBoundaryNonReflectiveMsurf class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DualceseBoundaryNonReflectiveMsurf(KeywordBase):
    """DYNA DUALCESE_BOUNDARY_NON_REFLECTIVE_MSURF keyword"""

    keyword = "DUALCESE"
    subkeyword = "BOUNDARY_NON_REFLECTIVE_MSURF"

    def __init__(self, **kwargs):
        """Initialize the DualceseBoundaryNonReflectiveMsurf class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "mspid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "dirx",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "diry",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "dirz",
                        float,
                        30,
                        10,
                        **kwargs,
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
        """Set the mspid property."""
        self._cards[0].set_value("mspid", value)

    @property
    def dirx(self) -> typing.Optional[float]:
        """Get or set the If this vector is non-zero, then it is used as the prescribed flow direction
        """ # nopep8
        return self._cards[0].get_value("dirx")

    @dirx.setter
    def dirx(self, value: float) -> None:
        """Set the dirx property."""
        self._cards[0].set_value("dirx", value)

    @property
    def diry(self) -> typing.Optional[float]:
        """Get or set the If this vector is non-zero, then it is used as the prescribed flow direction
        """ # nopep8
        return self._cards[0].get_value("diry")

    @diry.setter
    def diry(self, value: float) -> None:
        """Set the diry property."""
        self._cards[0].set_value("diry", value)

    @property
    def dirz(self) -> typing.Optional[float]:
        """Get or set the If this vector is non-zero, then it is used as the prescribed flow direction
        """ # nopep8
        return self._cards[0].get_value("dirz")

    @dirz.setter
    def dirz(self, value: float) -> None:
        """Set the dirz property."""
        self._cards[0].set_value("dirz", value)

