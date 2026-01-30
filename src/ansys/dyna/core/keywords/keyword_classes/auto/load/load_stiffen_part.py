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

"""Module providing the LoadStiffenPart class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_LOADSTIFFENPART_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("lc", int, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("stga", int, 30, 10, None),
    FieldSchema("stgr", int, 40, 10, None),
)

class LoadStiffenPart(KeywordBase):
    """DYNA LOAD_STIFFEN_PART keyword"""

    keyword = "LOAD"
    subkeyword = "STIFFEN_PART"
    _link_fields = {
        "lc": LinkType.DEFINE_CURVE,
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the LoadStiffenPart class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADSTIFFENPART_CARD0,
                **kwargs,
            ),        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def lc(self) -> typing.Optional[int]:
        """Get or set the Direction: enter 1, 2 or 3 for X, Y or Z
        """ # nopep8
        return self._cards[0].get_value("lc")

    @lc.setter
    def lc(self, value: int) -> None:
        """Set the lc property."""
        self._cards[0].set_value("lc", value)

    @property
    def stga(self) -> typing.Optional[int]:
        """Get or set the Construction stage at which part is added (optional)
        """ # nopep8
        return self._cards[0].get_value("stga")

    @stga.setter
    def stga(self, value: int) -> None:
        """Set the stga property."""
        self._cards[0].set_value("stga", value)

    @property
    def stgr(self) -> typing.Optional[int]:
        """Get or set the Construction stage at which part is removed (optional)
        """ # nopep8
        return self._cards[0].get_value("stgr")

    @stgr.setter
    def stgr(self, value: int) -> None:
        """Set the stgr property."""
        self._cards[0].set_value("stgr", value)

    @property
    def lc_link(self) -> DefineCurve:
        """Get the DefineCurve object for lc."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc:
                return kwd
        return None

    @lc_link.setter
    def lc_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc."""
        self.lc = value.lcid

    @property
    def pid_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

