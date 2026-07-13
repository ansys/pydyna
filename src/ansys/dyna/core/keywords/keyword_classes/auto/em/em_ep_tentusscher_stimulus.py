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

"""Module providing the EmEpTentusscherStimulus class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_EMEPTENTUSSCHERSTIMULUS_CARD0 = (
    FieldSchema("stimid", int, 0, 10, None),
    FieldSchema("tettype", int, 10, 10, 1),
    FieldSchema("setid", int, 20, 10, None),
)

_EMEPTENTUSSCHERSTIMULUS_CARD1 = (
    FieldSchema("stimstart", int, 0, 10, None),
    FieldSchema("stimt", int, 10, 10, None),
    FieldSchema("stimdur", int, 20, 10, None),
    FieldSchema("stimamp", int, 30, 10, None),
)

class EmEpTentusscherStimulus(KeywordBase):
    """DYNA EM_EP_TENTUSSCHER_STIMULUS keyword"""

    keyword = "EM"
    subkeyword = "EP_TENTUSSCHER_STIMULUS"
    _link_fields = {
        "stimamp": LinkType.NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the EmEpTentusscherStimulus class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMEPTENTUSSCHERSTIMULUS_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EMEPTENTUSSCHERSTIMULUS_CARD1,
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
    def stimstart(self) -> typing.Optional[int]:
        """Get or set the Starting time of the stimulation
        """ # nopep8
        return self._cards[1].get_value("stimstart")

    @stimstart.setter
    def stimstart(self, value: int) -> None:
        """Set the stimstart property."""
        self._cards[1].set_value("stimstart", value)

    @property
    def stimt(self) -> typing.Optional[int]:
        """Get or set the Stimulation period
        """ # nopep8
        return self._cards[1].get_value("stimt")

    @stimt.setter
    def stimt(self, value: int) -> None:
        """Set the stimt property."""
        self._cards[1].set_value("stimt", value)

    @property
    def stimdur(self) -> typing.Optional[int]:
        """Get or set the Stimulation duration
        """ # nopep8
        return self._cards[1].get_value("stimdur")

    @stimdur.setter
    def stimdur(self, value: int) -> None:
        """Set the stimdur property."""
        self._cards[1].set_value("stimdur", value)

    @property
    def stimamp(self) -> typing.Optional[int]:
        """Get or set the Stimulation amplitude (picoA/picoF)
        """ # nopep8
        return self._cards[1].get_value("stimamp")

    @stimamp.setter
    def stimamp(self, value: int) -> None:
        """Set the stimamp property."""
        self._cards[1].set_value("stimamp", value)

    @property
    def stimamp_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given stimamp."""
        return self._get_link_by_attr("NODE", "nid", self.stimamp, "parts")

