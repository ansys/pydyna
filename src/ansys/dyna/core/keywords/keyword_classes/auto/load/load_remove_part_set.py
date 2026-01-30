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

"""Module providing the LoadRemovePartSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_LOADREMOVEPARTSET_CARD0 = (
    FieldSchema("psid", int, 0, 10, None),
    FieldSchema("time0", float, 10, 10, None),
    FieldSchema("time1", float, 20, 10, None),
    FieldSchema("stgr", int, 30, 10, None),
)

class LoadRemovePartSet(KeywordBase):
    """DYNA LOAD_REMOVE_PART_SET keyword"""

    keyword = "LOAD"
    subkeyword = "REMOVE_PART_SET"
    _link_fields = {
        "psid": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the LoadRemovePartSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADREMOVEPARTSET_CARD0,
                **kwargs,
            ),        ]
    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part set ID for deletion.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[0].set_value("psid", value)

    @property
    def time0(self) -> typing.Optional[float]:
        """Get or set the Time at which stress reduction starts
        """ # nopep8
        return self._cards[0].get_value("time0")

    @time0.setter
    def time0(self, value: float) -> None:
        """Set the time0 property."""
        self._cards[0].set_value("time0", value)

    @property
    def time1(self) -> typing.Optional[float]:
        """Get or set the Time at which stresses become zero and elements are deleted
        """ # nopep8
        return self._cards[0].get_value("time1")

    @time1.setter
    def time1(self, value: float) -> None:
        """Set the time1 property."""
        self._cards[0].set_value("time1", value)

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
    def psid_link(self) -> KeywordBase:
        """Get the SET_PART_* keyword for psid."""
        return self._get_set_link("PART", self.psid)

    @psid_link.setter
    def psid_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psid."""
        self.psid = value.sid

