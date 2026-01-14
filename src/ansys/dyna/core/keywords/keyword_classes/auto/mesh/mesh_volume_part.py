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

"""Module providing the MeshVolumePart class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MESHVOLUMEPART_CARD0 = (
    FieldSchema("volprt", int, 0, 10, None),
    FieldSchema("solprt", int, 10, 10, None),
    FieldSchema("solver", str, 20, 10, None),
)

class MeshVolumePart(KeywordBase):
    """DYNA MESH_VOLUME_PART keyword"""

    keyword = "MESH"
    subkeyword = "VOLUME_PART"

    def __init__(self, **kwargs):
        """Initialize the MeshVolumePart class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MESHVOLUMEPART_CARD0,
                **kwargs,
            ),        ]
    @property
    def volprt(self) -> typing.Optional[int]:
        """Get or set the Part ID of a volume part created by a *MESH_VOLUME card.
        """ # nopep8
        return self._cards[0].get_value("volprt")

    @volprt.setter
    def volprt(self, value: int) -> None:
        """Set the volprt property."""
        self._cards[0].set_value("volprt", value)

    @property
    def solprt(self) -> typing.Optional[int]:
        """Get or set the Part ID of a part created using SOLVERs part card.
        """ # nopep8
        return self._cards[0].get_value("solprt")

    @solprt.setter
    def solprt(self, value: int) -> None:
        """Set the solprt property."""
        self._cards[0].set_value("solprt", value)

    @property
    def solver(self) -> typing.Optional[str]:
        """Get or set the Name of a solver using a mesh created with *MESH cards.
        """ # nopep8
        return self._cards[0].get_value("solver")

    @solver.setter
    def solver(self, value: str) -> None:
        """Set the solver property."""
        self._cards[0].set_value("solver", value)

