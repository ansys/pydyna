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

"""Module providing the ControlExplicitThermalAleCoupling class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_CONTROLEXPLICITTHERMALALECOUPLING_CARD0 = (
    FieldSchema("partset", int, 0, 10, None),
    FieldSchema("mmgset", int, 10, 10, None),
)

class ControlExplicitThermalAleCoupling(KeywordBase):
    """DYNA CONTROL_EXPLICIT_THERMAL_ALE_COUPLING keyword"""

    keyword = "CONTROL"
    subkeyword = "EXPLICIT_THERMAL_ALE_COUPLING"
    _link_fields = {
        "partset": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ControlExplicitThermalAleCoupling class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLEXPLICITTHERMALALECOUPLING_CARD0,
                **kwargs,
            ),        ]
    @property
    def partset(self) -> typing.Optional[int]:
        """Get or set the Part set ID (See *SET_PART).
        """ # nopep8
        return self._cards[0].get_value("partset")

    @partset.setter
    def partset(self, value: int) -> None:
        """Set the partset property."""
        self._cards[0].set_value("partset", value)

    @property
    def mmgset(self) -> typing.Optional[int]:
        """Get or set the Multi-material set ID (see *SET_MULTI-MATERIAL_GROUP_LIST)
        """ # nopep8
        return self._cards[0].get_value("mmgset")

    @mmgset.setter
    def mmgset(self, value: int) -> None:
        """Set the mmgset property."""
        self._cards[0].set_value("mmgset", value)

    @property
    def partset_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for partset."""
        return self._get_set_link("PART", self.partset)

    @partset_link.setter
    def partset_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for partset."""
        self.partset = value.sid

