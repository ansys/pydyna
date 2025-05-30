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

"""Module providing the AleStructuredMeshRefine class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class AleStructuredMeshRefine(KeywordBase):
    """DYNA ALE_STRUCTURED_MESH_REFINE keyword"""

    keyword = "ALE"
    subkeyword = "STRUCTURED_MESH_REFINE"

    def __init__(self, **kwargs):
        """Initialize the AleStructuredMeshRefine class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "mshid",
                        int,
                        0,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "ifx,",
                        int,
                        10,
                        10,
                        1,
                        **kwargs,
                    ),
                    Field(
                        "ify,",
                        int,
                        20,
                        10,
                        1,
                        **kwargs,
                    ),
                    Field(
                        "ifz,",
                        int,
                        30,
                        10,
                        1,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def mshid(self) -> int:
        """Get or set the S-ALE Mesh ID. The ID of the Structured ALE mesh to be refined.
        """ # nopep8
        return self._cards[0].get_value("mshid")

    @mshid.setter
    def mshid(self, value: int) -> None:
        """Set the mshid property."""
        self._cards[0].set_value("mshid", value)

    @property
    def ifx_(self) -> int:
        """Get or set the Refinement factor at each local direction. Please see remark 1.
        """ # nopep8
        return self._cards[0].get_value("ifx,")

    @ifx_.setter
    def ifx_(self, value: int) -> None:
        """Set the ifx_ property."""
        self._cards[0].set_value("ifx,", value)

    @property
    def ify_(self) -> int:
        """Get or set the Refinement factor at each local direction. Please see remark 1.
        """ # nopep8
        return self._cards[0].get_value("ify,")

    @ify_.setter
    def ify_(self, value: int) -> None:
        """Set the ify_ property."""
        self._cards[0].set_value("ify,", value)

    @property
    def ifz_(self) -> int:
        """Get or set the Refinement factor at each local direction. Please see remark 1.
        """ # nopep8
        return self._cards[0].get_value("ifz,")

    @ifz_.setter
    def ifz_(self, value: int) -> None:
        """Set the ifz_ property."""
        self._cards[0].set_value("ifz,", value)

