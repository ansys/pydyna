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

"""Module providing the EmControlSwitch class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_EMCONTROLSWITCH_CARD0 = (
    FieldSchema("lcid", int, 0, 10, 0),
    FieldSchema("femcomp", int, 10, 10, 0),
    FieldSchema("bemcomp", int, 20, 10, 0),
)

class EmControlSwitch(KeywordBase):
    """DYNA EM_CONTROL_SWITCH keyword"""

    keyword = "EM"
    subkeyword = "CONTROL_SWITCH"
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the EmControlSwitch class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMCONTROLSWITCH_CARD0,
                **kwargs,
            ),        ]
    @property
    def lcid(self) -> int:
        """Get or set the Load Curve ID.Negative values switch the solver off, positive values switch it back on.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def femcomp(self) -> int:
        """Get or set the Determines if FEM matrices are recomputed each time the EM solver is turned back on:
        EQ.0: FEM matrices are recomputed
        EQ.1: FEM matrices are not recomputed
        """ # nopep8
        return self._cards[0].get_value("femcomp")

    @femcomp.setter
    def femcomp(self, value: int) -> None:
        """Set the femcomp property."""
        if value not in [0, 1, None]:
            raise Exception("""femcomp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("femcomp", value)

    @property
    def bemcomp(self) -> int:
        """Get or set the Determines if BEM matrices are recomputed each time the EM solver is turned back on:
        EQ.0 : BEM matrices are recomputed
        EQ.1 : BEM matrices are not recomputed
        """ # nopep8
        return self._cards[0].get_value("bemcomp")

    @bemcomp.setter
    def bemcomp(self, value: int) -> None:
        """Set the bemcomp property."""
        if value not in [0, 1, None]:
            raise Exception("""bemcomp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("bemcomp", value)

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

