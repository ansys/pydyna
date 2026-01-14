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

"""Module providing the PartDuplicate class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_transformation import DefineTransformation

_PARTDUPLICATE_CARD0 = (
    FieldSchema("ptype", str, 0, 10, "PART"),
    FieldSchema("typeid", int, 10, 10, None),
    FieldSchema("idpoff", int, 20, 10, 0),
    FieldSchema("ideoff", int, 30, 10, 0),
    FieldSchema("idnoff", int, 40, 10, 0),
    FieldSchema("tranid", int, 50, 10, 0),
    FieldSchema("boxid", int, 60, 10, 0),
    FieldSchema("zmin", float, 70, 10, 0.0),
)

class PartDuplicate(KeywordBase):
    """DYNA PART_DUPLICATE keyword"""

    keyword = "PART"
    subkeyword = "DUPLICATE"
    _link_fields = {
        "tranid": LinkType.DEFINE_TRANSFORMATION,
    }

    def __init__(self, **kwargs):
        """Initialize the PartDuplicate class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _PARTDUPLICATE_CARD0,
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
    def idnoff(self) -> int:
        """Get or set the ID offset of newly created nodes
        """ # nopep8
        return self._cards[0].get_value("idnoff")

    @idnoff.setter
    def idnoff(self, value: int) -> None:
        """Set the idnoff property."""
        self._cards[0].set_value("idnoff", value)

    @property
    def tranid(self) -> int:
        """Get or set the ID of *DEFINE_TRANSFORMATION to transform the existing nodes in a part or part set..
        """ # nopep8
        return self._cards[0].get_value("tranid")

    @tranid.setter
    def tranid(self, value: int) -> None:
        """Set the tranid property."""
        self._cards[0].set_value("tranid", value)

    @property
    def boxid(self) -> int:
        """Get or set the ID of box defining the boundary of the transformed nodal coordinates; see Remark 6.
        """ # nopep8
        return self._cards[0].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        """Set the boxid property."""
        self._cards[0].set_value("boxid", value)

    @property
    def zmin(self) -> float:
        """Get or set the ID of box defining the boundary of the transformed nodal coordinates; see Remark 6.
        """ # nopep8
        return self._cards[0].get_value("zmin")

    @zmin.setter
    def zmin(self, value: float) -> None:
        """Set the zmin property."""
        self._cards[0].set_value("zmin", value)

    @property
    def tranid_link(self) -> DefineTransformation:
        """Get the DefineTransformation object for tranid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "TRANSFORMATION"):
            if kwd.tranid == self.tranid:
                return kwd
        return None

    @tranid_link.setter
    def tranid_link(self, value: DefineTransformation) -> None:
        """Set the DefineTransformation object for tranid."""
        self.tranid = value.tranid

