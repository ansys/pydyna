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

"""Module providing the DefineGroundMotion class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_DEFINEGROUNDMOTION_CARD0 = (
    FieldSchema("gmid", int, 0, 10, None),
    FieldSchema("alcid", int, 10, 10, None),
    FieldSchema("vlcid", int, 20, 10, None),
)

_DEFINEGROUNDMOTION_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineGroundMotion(KeywordBase):
    """DYNA DEFINE_GROUND_MOTION keyword"""

    keyword = "DEFINE"
    subkeyword = "GROUND_MOTION"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "alcid": LinkType.DEFINE_CURVE,
        "vlcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineGroundMotion class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEGROUNDMOTION_CARD0,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineGroundMotion.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEGROUNDMOTION_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def gmid(self) -> typing.Optional[int]:
        """Get or set the Ground motion ID. A unique number has to be defined
        """ # nopep8
        return self._cards[0].get_value("gmid")

    @gmid.setter
    def gmid(self, value: int) -> None:
        """Set the gmid property."""
        self._cards[0].set_value("gmid", value)

    @property
    def alcid(self) -> typing.Optional[int]:
        """Get or set the Load Curve ID of ground acceleration history
        """ # nopep8
        return self._cards[0].get_value("alcid")

    @alcid.setter
    def alcid(self, value: int) -> None:
        """Set the alcid property."""
        self._cards[0].set_value("alcid", value)

    @property
    def vlcid(self) -> typing.Optional[int]:
        """Get or set the Load Curve ID of ground velocity history
        """ # nopep8
        return self._cards[0].get_value("vlcid")

    @vlcid.setter
    def vlcid(self, value: int) -> None:
        """Set the vlcid property."""
        self._cards[0].set_value("vlcid", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[1].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def alcid_link(self) -> DefineCurve:
        """Get the DefineCurve object for alcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.alcid:
                return kwd
        return None

    @alcid_link.setter
    def alcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for alcid."""
        self.alcid = value.lcid

    @property
    def vlcid_link(self) -> DefineCurve:
        """Get the DefineCurve object for vlcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.vlcid:
                return kwd
        return None

    @vlcid_link.setter
    def vlcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for vlcid."""
        self.vlcid = value.lcid

