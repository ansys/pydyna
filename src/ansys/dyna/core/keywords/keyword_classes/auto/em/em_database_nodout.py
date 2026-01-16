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

"""Module providing the EmDatabaseNodout class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_EMDATABASENODOUT_CARD0 = (
    FieldSchema("outlv", int, 0, 10, 0),
    FieldSchema("dtout", float, 10, 10, 0.0),
)

_EMDATABASENODOUT_CARD1 = (
    FieldSchema("nsid", int, 0, 10, None),
)

class EmDatabaseNodout(KeywordBase):
    """DYNA EM_DATABASE_NODOUT keyword"""

    keyword = "EM"
    subkeyword = "DATABASE_NODOUT"
    _link_fields = {
        "nsid": LinkType.SET_NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the EmDatabaseNodout class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMDATABASENODOUT_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EMDATABASENODOUT_CARD1,
                **kwargs,
            ),        ]
    @property
    def outlv(self) -> int:
        """Get or set the Determines if the output file should be dumped.
        EQ.0: No output file is generated.
        EQ.1: The output file is generated.
        """ # nopep8
        return self._cards[0].get_value("outlv")

    @outlv.setter
    def outlv(self, value: int) -> None:
        """Set the outlv property."""
        if value not in [0, 1, None]:
            raise Exception("""outlv must be `None` or one of {0,1}.""")
        self._cards[0].set_value("outlv", value)

    @property
    def dtout(self) -> float:
        """Get or set the Time interval to print the output. If DTOUT is equal to 0.0, then the EM timestep will be used.
        """ # nopep8
        return self._cards[0].get_value("dtout")

    @dtout.setter
    def dtout(self, value: float) -> None:
        """Set the dtout property."""
        self._cards[0].set_value("dtout", value)

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node Set ID.
        """ # nopep8
        return self._cards[1].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        """Set the nsid property."""
        self._cards[1].set_value("nsid", value)

    @property
    def nsid_link(self) -> KeywordBase:
        """Get the SET_NODE_* keyword for nsid."""
        return self._get_set_link("NODE", self.nsid)

    @nsid_link.setter
    def nsid_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for nsid."""
        self.nsid = value.sid

