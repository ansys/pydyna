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

"""Module providing the MeshEmbedshell class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.series_card import SeriesCard
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_MESHEMBEDSHELL_CARD0 = (
    FieldSchema("volid", int, 0, 10, None),
)

class MeshEmbedshell(KeywordBase):
    """DYNA MESH_EMBEDSHELL keyword"""

    keyword = "MESH"
    subkeyword = "EMBEDSHELL"
    _link_fields = {
        "pid1": LinkType.PART,
        "pid2": LinkType.PART,
        "pid3": LinkType.PART,
        "pid4": LinkType.PART,
        "pid5": LinkType.PART,
        "pid6": LinkType.PART,
        "pid7": LinkType.PART,
        "pid8": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the MeshEmbedshell class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MESHEMBEDSHELL_CARD0,
                **kwargs,
            ),            SeriesCard(
                "elements",
                8,
                10,
                int,
                None,
                data = kwargs.get("elements")),        ]
    @property
    def volid(self) -> typing.Optional[int]:
        """Get or set the ID assigned to the new volume in the keyword *MESH_VOLUME. The size meshes will be applied to this volume
        """ # nopep8
        return self._cards[0].get_value("volid")

    @volid.setter
    def volid(self, value: int) -> None:
        """Set the volid property."""
        self._cards[0].set_value("volid", value)

    @property
    def elements(self) -> SeriesCard:
        """dynamic array of surface element ids.."""
        return self._cards[1]

    @elements.setter
    def elements(self, value: typing.List) -> None:
        self._cards[1].data = value

    @property
    def pid1_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pid1."""
        return self._get_link_by_attr("PART", "pid", self.pid1, "parts")

    @property
    def pid2_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pid2."""
        return self._get_link_by_attr("PART", "pid", self.pid2, "parts")

    @property
    def pid3_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pid3."""
        return self._get_link_by_attr("PART", "pid", self.pid3, "parts")

    @property
    def pid4_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pid4."""
        return self._get_link_by_attr("PART", "pid", self.pid4, "parts")

    @property
    def pid5_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pid5."""
        return self._get_link_by_attr("PART", "pid", self.pid5, "parts")

    @property
    def pid6_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pid6."""
        return self._get_link_by_attr("PART", "pid", self.pid6, "parts")

    @property
    def pid7_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pid7."""
        return self._get_link_by_attr("PART", "pid", self.pid7, "parts")

    @property
    def pid8_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pid8."""
        return self._get_link_by_attr("PART", "pid", self.pid8, "parts")

