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

"""Module providing the EmEpCellmodelTomek class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_EMEPCELLMODELTOMEK_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("phiend", float, 10, 10, None),
    FieldSchema("phimyo", float, 20, 10, None),
)

class EmEpCellmodelTomek(KeywordBase):
    """DYNA EM_EP_CELLMODEL_TOMEK keyword"""

    keyword = "EM"
    subkeyword = "EP_CELLMODEL_TOMEK"
    _link_fields = {
        "mid": LinkType.MAT,
    }

    def __init__(self, **kwargs):
        """Initialize the EmEpCellmodelTomek class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMEPCELLMODELTOMEK_CARD0,
                **kwargs,
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material ID: refers to MID in the *PART card. If a negative value is entered,  the cell model is defined on a node set instead of a part, and -MID is the node set where the cell model is defined.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def phiend(self) -> typing.Optional[float]:
        """Get or set the Value between 0.0 and 1.0 giving the ratio of the cardiac tissue to be considered endocardial in the ToR-Ord cell model.
        """ # nopep8
        return self._cards[0].get_value("phiend")

    @phiend.setter
    def phiend(self, value: float) -> None:
        """Set the phiend property."""
        self._cards[0].set_value("phiend", value)

    @property
    def phimyo(self) -> typing.Optional[float]:
        """Get or set the Value between 0.0 and 1.0 giving the ratio of the cardiac tissue to be considered myocardial in the ToR-Ord cell model.
        """ # nopep8
        return self._cards[0].get_value("phimyo")

    @phimyo.setter
    def phimyo(self, value: float) -> None:
        """Set the phimyo property."""
        self._cards[0].set_value("phimyo", value)

    @property
    def mid_link(self) -> typing.Optional[KeywordBase]:
        """Get the MAT_* keyword for mid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_type("MAT"):
            if kwd.mid == self.mid:
                return kwd
        return None

    @mid_link.setter
    def mid_link(self, value: KeywordBase) -> None:
        """Set the MAT_* keyword for mid."""
        self.mid = value.mid

