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
from ansys.dyna.core.lib.variable_card import VariableCard
from ansys.dyna.core.lib.keyword_base import KeywordBase

class MeshEmbedshell(KeywordBase):
    """DYNA MESH_EMBEDSHELL keyword"""

    keyword = "MESH"
    subkeyword = "EMBEDSHELL"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "volid",
                        int,
                        0,
                        10,
                        kwargs.get("volid")
                    ),
                ],
            ),
            VariableCard(
                "elements",
                8,
                10,
                int,
                None,
                data = kwargs.get("elements")),
        ]

    @property
    def volid(self) -> typing.Optional[int]:
        """Get or set the ID assigned to the new volume in the keyword *MESH_VOLUME. The size meshes will be applied to this volume
        """ # nopep8
        return self._cards[0].get_value("volid")

    @volid.setter
    def volid(self, value: int) -> None:
        self._cards[0].set_value("volid", value)

    @property
    def elements(self) -> VariableCard:
        """dynamic array of surface element ids."""
        return self._cards[1]

