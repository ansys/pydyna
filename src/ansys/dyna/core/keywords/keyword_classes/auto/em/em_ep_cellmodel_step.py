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

"""Module providing the EmEpCellmodelStep class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_EMEPCELLMODELSTEP_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("duration", float, 10, 10, None),
    FieldSchema("restp", float, 20, 10, None),
    FieldSchema("depolp", float, 30, 10, None),
)

class EmEpCellmodelStep(KeywordBase):
    """DYNA EM_EP_CELLMODEL_STEP keyword"""

    keyword = "EM"
    subkeyword = "EP_CELLMODEL_STEP"
    _link_fields = {
        "mid": LinkType.MAT,
    }

    def __init__(self, **kwargs):
        """Initialize the EmEpCellmodelStep class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMEPCELLMODELSTEP_CARD0,
                **kwargs,
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material ID defined in *MAT
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def duration(self) -> typing.Optional[float]:
        """Get or set the Duration of the depolarization potential
        """ # nopep8
        return self._cards[0].get_value("duration")

    @duration.setter
    def duration(self, value: float) -> None:
        """Set the duration property."""
        self._cards[0].set_value("duration", value)

    @property
    def restp(self) -> typing.Optional[float]:
        """Get or set the Resting potential
        """ # nopep8
        return self._cards[0].get_value("restp")

    @restp.setter
    def restp(self, value: float) -> None:
        """Set the restp property."""
        self._cards[0].set_value("restp", value)

    @property
    def depolp(self) -> typing.Optional[float]:
        """Get or set the Depolarization potential
        """ # nopep8
        return self._cards[0].get_value("depolp")

    @depolp.setter
    def depolp(self, value: float) -> None:
        """Set the depolp property."""
        self._cards[0].set_value("depolp", value)

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

