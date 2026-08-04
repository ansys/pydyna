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

"""Module providing the ControlMppDecompositionDistributeAleElement class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLMPPDECOMPOSITIONDISTRIBUTEALEELEMENT_CARD0 = (
    FieldSchema("overlap", int, 0, 10, None),
)

class ControlMppDecompositionDistributeAleElement(KeywordBase):
    """DYNA CONTROL_MPP_DECOMPOSITION_DISTRIBUTE_ALE_ELEMENT keyword"""

    keyword = "CONTROL"
    subkeyword = "MPP_DECOMPOSITION_DISTRIBUTE_ALE_ELEMENT"

    def __init__(self, **kwargs):
        """Initialize the ControlMppDecompositionDistributeAleElement class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLMPPDECOMPOSITIONDISTRIBUTEALEELEMENT_CARD0,
                **kwargs,
            ),
        ]
    @property
    def overlap(self) -> typing.Optional[int]:
        """Get or set the For FSI models where structures are inside ALE meshes (see *CONSTRAINED_LAGRANGE_IN_SOLID), flag to decompose the structure and ALE domains together instead of first the structure and then the ALE (see Remark 2).
        EQ.0 : Off
        EQ.1 : On
        """ # nopep8
        return self._cards[0].get_value("overlap")

    @overlap.setter
    def overlap(self, value: int) -> None:
        """Set the overlap property."""
        self._cards[0].set_value("overlap", value)

