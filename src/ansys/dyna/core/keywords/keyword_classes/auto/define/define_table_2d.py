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

"""Module providing the DefineTable2D class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_DEFINETABLE2D_CARD0 = (
    FieldSchema("tbid", int, 0, 10, None),
    FieldSchema("sfa", float, 10, 10, 1.0),
    FieldSchema("offa", float, 20, 10, 0.0),
)

_DEFINETABLE2D_CARD1 = (
    FieldSchema("value", float, 0, 20, 0.0),
    FieldSchema("lcid", int, 20, 20, None),
)

_DEFINETABLE2D_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineTable2D(KeywordBase):
    """DYNA DEFINE_TABLE_2D keyword"""

    keyword = "DEFINE"
    subkeyword = "TABLE_2D"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineTable2D class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINETABLE2D_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINETABLE2D_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineTable2D.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINETABLE2D_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def tbid(self) -> typing.Optional[int]:
        """Get or set the Table ID. Tables and Load curves may not share common ID's.  LS-DYNA3D allows load curve ID's and table ID's to be used interchangeably.
        """ # nopep8
        return self._cards[0].get_value("tbid")

    @tbid.setter
    def tbid(self, value: int) -> None:
        """Set the tbid property."""
        self._cards[0].set_value("tbid", value)

    @property
    def sfa(self) -> float:
        """Get or set the Scale factor for value.
        """ # nopep8
        return self._cards[0].get_value("sfa")

    @sfa.setter
    def sfa(self, value: float) -> None:
        """Set the sfa property."""
        self._cards[0].set_value("sfa", value)

    @property
    def offa(self) -> float:
        """Get or set the Offset for values.
        """ # nopep8
        return self._cards[0].get_value("offa")

    @offa.setter
    def offa(self, value: float) -> None:
        """Set the offa property."""
        self._cards[0].set_value("offa", value)

    @property
    def value(self) -> float:
        """Get or set the Load curve will be defined corresponding to this value, e.g., this value could be a strain rate, see purpose above.
        """ # nopep8
        return self._cards[1].get_value("value")

    @value.setter
    def value(self, value: float) -> None:
        """Set the value property."""
        self._cards[1].set_value("value", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID used by this value.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[1].set_value("lcid", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[2].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def lcid_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid:
                return kwd
        return None

    @lcid_link.setter
    def lcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid."""
        self.lcid = value.lcid

