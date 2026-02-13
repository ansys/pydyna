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

"""Module providing the MatS05 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MATS05_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("lcdr", int, 10, 10, None),
)

_MATS05_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatS05(KeywordBase):
    """DYNA MAT_S05 keyword"""

    keyword = "MAT"
    subkeyword = "S05"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lcdr": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatS05 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATS05_CARD0,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatS05.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATS05_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def lcdr(self) -> typing.Optional[int]:
        """Get or set the Load curve identification describing force versus rate-of-displacement relationship or a moment versus rate-of-rotation relationship. The load curve must define the response in the negative and positive quadrants and pass through point (0,0).
        """ # nopep8
        return self._cards[0].get_value("lcdr")

    @lcdr.setter
    def lcdr(self, value: int) -> None:
        """Set the lcdr property."""
        self._cards[0].set_value("lcdr", value)

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
    def lcdr_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcdr."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcdr:
                return kwd
        return None

    @lcdr_link.setter
    def lcdr_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcdr."""
        self.lcdr = value.lcid

