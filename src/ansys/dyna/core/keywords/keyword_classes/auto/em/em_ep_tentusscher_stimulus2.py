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

"""Module providing the EmEpTentusscherStimulus2 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_EMEPTENTUSSCHERSTIMULUS2_CARD0 = (
    FieldSchema("stimid", int, 0, 10, None),
    FieldSchema("tettype", int, 10, 10, 1),
    FieldSchema("setid", int, 20, 10, None),
    FieldSchema("lcid", int, 3, 10, None),
)

class EmEpTentusscherStimulus2(KeywordBase):
    """DYNA EM_EP_TENTUSSCHER_STIMULUS2 keyword"""

    keyword = "EM"
    subkeyword = "EP_TENTUSSCHER_STIMULUS2"
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the EmEpTentusscherStimulus2 class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMEPTENTUSSCHERSTIMULUS2_CARD0,
                **kwargs,
            ),
        ]
    @property
    def stimid(self) -> typing.Optional[int]:
        """Get or set the Id of the stimulation
        """ # nopep8
        return self._cards[0].get_value("stimid")

    @stimid.setter
    def stimid(self, value: int) -> None:
        """Set the stimid property."""
        self._cards[0].set_value("stimid", value)

    @property
    def tettype(self) -> int:
        """Get or set the Set type:
        EQ.1: Segment set
        EQ.2: Node set
        """ # nopep8
        return self._cards[0].get_value("tettype")

    @tettype.setter
    def tettype(self, value: int) -> None:
        """Set the tettype property."""
        if value not in [1, 2, None]:
            raise Exception("""tettype must be `None` or one of {1,2}.""")
        self._cards[0].set_value("tettype", value)

    @property
    def setid(self) -> typing.Optional[int]:
        """Get or set the Node set or segment set id to be stimulated.
        """ # nopep8
        return self._cards[0].get_value("setid")

    @setid.setter
    def setid(self, value: int) -> None:
        """Set the setid property."""
        self._cards[0].set_value("setid", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for defining the stimulation current as a function of time
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

