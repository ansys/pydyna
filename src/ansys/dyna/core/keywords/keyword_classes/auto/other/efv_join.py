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

"""Module providing the EfvJoin class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVJOIN_CARD0 = (
    FieldSchema("psid1", int, 0, 10, None),
    FieldSchema("psid2", int, 10, 10, None),
)

class EfvJoin(KeywordBase):
    """DYNA EFV_JOIN keyword"""

    keyword = "EFV"
    subkeyword = "JOIN"

    def __init__(self, **kwargs):
        """Initialize the EfvJoin class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVJOIN_CARD0,
                **kwargs,
            ),
        ]
    @property
    def psid1(self) -> typing.Optional[int]:
        """Get or set the IDs of meshes to join along their boundary element faces with common nodes. The mesh IDs are part sets defined in *EFV_STRUCTURED_MESH or *EFV_BOX_MESH.
        """ # nopep8
        return self._cards[0].get_value("psid1")

    @psid1.setter
    def psid1(self, value: int) -> None:
        """Set the psid1 property."""
        self._cards[0].set_value("psid1", value)

    @property
    def psid2(self) -> typing.Optional[int]:
        """Get or set the IDs of meshes to join along their boundary element faces with common nodes. The mesh IDs are part sets defined in *EFV_STRUCTURED_MESH or *EFV_BOX_MESH.
        """ # nopep8
        return self._cards[0].get_value("psid2")

    @psid2.setter
    def psid2(self, value: int) -> None:
        """Set the psid2 property."""
        self._cards[0].set_value("psid2", value)

