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

"""Module providing the AleStructuredMeshMotion class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ALESTRUCTUREDMESHMOTION_CARD0 = (
    FieldSchema("mshid", int, 0, 10, None),
    FieldSchema("option", str, 10, 10, "FOLLOW_GC"),
    FieldSchema("ammgsid", int, 20, 10, 0),
    FieldSchema("explim", float, 30, 10, 1.0),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("symcod", int, 70, 10, 0),
)

class AleStructuredMeshMotion(KeywordBase):
    """DYNA ALE_STRUCTURED_MESH_MOTION keyword"""

    keyword = "ALE"
    subkeyword = "STRUCTURED_MESH_MOTION"

    def __init__(self, **kwargs):
        """Initialize the AleStructuredMeshMotion class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ALESTRUCTUREDMESHMOTION_CARD0,
                **kwargs,
            ),        ]
    @property
    def mshid(self) -> typing.Optional[int]:
        """Get or set the S-ALE Mesh ID.  A unique number must be specified.
        """ # nopep8
        return self._cards[0].get_value("mshid")

    @mshid.setter
    def mshid(self, value: int) -> None:
        """Set the mshid property."""
        self._cards[0].set_value("mshid", value)

    @property
    def option(self) -> str:
        """Get or set the FOLLOW_GC/COVER_LAG
        """ # nopep8
        return self._cards[0].get_value("option")

    @option.setter
    def option(self, value: str) -> None:
        """Set the option property."""
        if value not in ["FOLLOW_GC", "COVER_LAG", None]:
            raise Exception("""option must be `None` or one of {"FOLLOW_GC","COVER_LAG"}.""")
        self._cards[0].set_value("option", value)

    @property
    def ammgsid(self) -> int:
        """Get or set the The set of ALE multi-material group list IDs which the mesh follows.
        Please refer to *SET_MULTI-MATERIAL_GROUP_LIST card for details.
        """ # nopep8
        return self._cards[0].get_value("ammgsid")

    @ammgsid.setter
    def ammgsid(self, value: int) -> None:
        """Set the ammgsid property."""
        self._cards[0].set_value("ammgsid", value)

    @property
    def explim(self) -> float:
        """Get or set the Limit ratio for mesh expansion and contraction. The distance between the nodes is not allowed to increase by
        more than a factor EXPLIM or decrease to less than a factor 1/EXPLIM.  Default value of 1.0 means no expansion/contraction.
        """ # nopep8
        return self._cards[0].get_value("explim")

    @explim.setter
    def explim(self, value: float) -> None:
        """Set the explim property."""
        self._cards[0].set_value("explim", value)

    @property
    def symcod(self) -> int:
        """Get or set the A three digit number to define symmetry. Each digit specifies one direction (local x,y,z defined in *ALE_STRUCTURED_MESH) and can be of 0,1 or 2. Code 0 means no symmetry; 1 symmetry defined at minus face; 2 plus face
        """ # nopep8
        return self._cards[0].get_value("symcod")

    @symcod.setter
    def symcod(self, value: int) -> None:
        """Set the symcod property."""
        self._cards[0].set_value("symcod", value)

