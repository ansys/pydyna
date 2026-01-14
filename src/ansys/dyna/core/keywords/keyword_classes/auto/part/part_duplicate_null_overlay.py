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

"""Module providing the PartDuplicateNullOverlay class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_PARTDUPLICATENULLOVERLAY_CARD0 = (
    FieldSchema("ptype", str, 0, 10, "PART"),
    FieldSchema("typeid", int, 10, 10, None),
    FieldSchema("idpoff", int, 20, 10, 0),
    FieldSchema("ideoff", int, 30, 10, 0),
    FieldSchema("density", float, 40, 10, 0.0),
    FieldSchema("e", float, 50, 10, 0.0),
    FieldSchema("pr", float, 60, 10, 0.0),
)

class PartDuplicateNullOverlay(KeywordBase):
    """DYNA PART_DUPLICATE_NULL_OVERLAY keyword"""

    keyword = "PART"
    subkeyword = "DUPLICATE_NULL_OVERLAY"

    def __init__(self, **kwargs):
        """Initialize the PartDuplicateNullOverlay class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _PARTDUPLICATENULLOVERLAY_CARD0,
                **kwargs,
            ),        ]
    @property
    def ptype(self) -> str:
        """Get or set the Set to "PART" to duplicate a single part or "PSET" to duplicate a part set.
        """ # nopep8
        return self._cards[0].get_value("ptype")

    @ptype.setter
    def ptype(self, value: str) -> None:
        """Set the ptype property."""
        if value not in ["PART", "PSET", None]:
            raise Exception("""ptype must be `None` or one of {"PART","PSET"}.""")
        self._cards[0].set_value("ptype", value)

    @property
    def typeid(self) -> typing.Optional[int]:
        """Get or set the ID of part or part set to be duplicated.
        """ # nopep8
        return self._cards[0].get_value("typeid")

    @typeid.setter
    def typeid(self, value: int) -> None:
        """Set the typeid property."""
        self._cards[0].set_value("typeid", value)

    @property
    def idpoff(self) -> int:
        """Get or set the ID offset of newly created parts
        """ # nopep8
        return self._cards[0].get_value("idpoff")

    @idpoff.setter
    def idpoff(self, value: int) -> None:
        """Set the idpoff property."""
        self._cards[0].set_value("idpoff", value)

    @property
    def ideoff(self) -> int:
        """Get or set the ID offset of newly created elements.
        """ # nopep8
        return self._cards[0].get_value("ideoff")

    @ideoff.setter
    def ideoff(self, value: int) -> None:
        """Set the ideoff property."""
        self._cards[0].set_value("ideoff", value)

    @property
    def density(self) -> float:
        """Get or set the Density.
        """ # nopep8
        return self._cards[0].get_value("density")

    @density.setter
    def density(self, value: float) -> None:
        """Set the density property."""
        self._cards[0].set_value("density", value)

    @property
    def e(self) -> float:
        """Get or set the Youngs modulus
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> float:
        """Get or set the Poissons ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

