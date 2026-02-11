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

"""Module providing the LoadDensityDepth class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_LOADDENSITYDEPTH_CARD0 = (
    FieldSchema("psid", int, 0, 10, 0),
    FieldSchema("gc", float, 10, 10, 0.0),
    FieldSchema("dir", int, 20, 10, 1),
    FieldSchema("lcid", int, 30, 10, None),
)

class LoadDensityDepth(KeywordBase):
    """DYNA LOAD_DENSITY_DEPTH keyword"""

    keyword = "LOAD"
    subkeyword = "DENSITY_DEPTH"
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
        "psid": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the LoadDensityDepth class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADDENSITYDEPTH_CARD0,
                **kwargs,
            ),        ]
    @property
    def psid(self) -> int:
        """Get or set the Part set ID, see *SET_PART.
        EQ.0: all parts are initialized.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[0].set_value("psid", value)

    @property
    def gc(self) -> float:
        """Get or set the Gravitational acceleration value.
        """ # nopep8
        return self._cards[0].get_value("gc")

    @gc.setter
    def gc(self, value: float) -> None:
        """Set the gc property."""
        self._cards[0].set_value("gc", value)

    @property
    def dir(self) -> int:
        """Get or set the Direction of loading:
        EQ.1: global x (default),
        EQ.2: global y,
        EQ.3: global z.
        """ # nopep8
        return self._cards[0].get_value("dir")

    @dir.setter
    def dir(self, value: int) -> None:
        """Set the dir property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""dir must be `None` or one of {1,2,3}.""")
        self._cards[0].set_value("dir", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining density versus depth, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

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

    @property
    def psid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for psid."""
        return self._get_set_link("PART", self.psid)

    @psid_link.setter
    def psid_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psid."""
        self.psid = value.sid

