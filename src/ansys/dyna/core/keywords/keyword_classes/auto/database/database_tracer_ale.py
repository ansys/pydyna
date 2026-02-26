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

"""Module providing the DatabaseTracerAle class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_DATABASETRACERALE_CARD0 = (
    FieldSchema("nid", int, 0, 10, 0),
    FieldSchema("track", int, 10, 10, 0),
    FieldSchema("ammgid", int, 20, 10, 0),
    FieldSchema("hvbeg", int, 30, 10, 0),
    FieldSchema("hvend", int, 40, 10, 0),
    FieldSchema("time", float, 50, 10, 0.0),
)

class DatabaseTracerAle(KeywordBase):
    """DYNA DATABASE_TRACER_ALE keyword"""

    keyword = "DATABASE"
    subkeyword = "TRACER_ALE"
    _link_fields = {
        "nid": LinkType.NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the DatabaseTracerAle class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DATABASETRACERALE_CARD0,
                **kwargs,
            ),        ]
    @property
    def nid(self) -> int:
        """Get or set the Node ID defining the initial position of a tracer particle. See Remark 1
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[0].set_value("nid", value)

    @property
    def track(self) -> int:
        """Get or set the Tracking option:
        EQ.0:	particle follows material
        EQ.1: particle is fixed in space.
        """ # nopep8
        return self._cards[0].get_value("track")

    @track.setter
    def track(self, value: int) -> None:
        """Set the track property."""
        if value not in [0, 1, None]:
            raise Exception("""track must be `None` or one of {0,1}.""")
        self._cards[0].set_value("track", value)

    @property
    def ammgid(self) -> int:
        """Get or set the The AMMG ID (ALE multi-material group) of the material being tracked in a multi-material ALE element. See Remark 2
        """ # nopep8
        return self._cards[0].get_value("ammgid")

    @ammgid.setter
    def ammgid(self, value: int) -> None:
        """Set the ammgid property."""
        self._cards[0].set_value("ammgid", value)

    @property
    def hvbeg(self) -> int:
        """Get or set the The beginning index of element history variables to be output. See Remark 3
        """ # nopep8
        return self._cards[0].get_value("hvbeg")

    @hvbeg.setter
    def hvbeg(self, value: int) -> None:
        """Set the hvbeg property."""
        self._cards[0].set_value("hvbeg", value)

    @property
    def hvend(self) -> int:
        """Get or set the The ending index of element history variables to be output. The number of extra history variables must be no more than 15, meaning HVEND-HVBEG=15.
        """ # nopep8
        return self._cards[0].get_value("hvend")

    @hvend.setter
    def hvend(self, value: int) -> None:
        """Set the hvend property."""
        self._cards[0].set_value("hvend", value)

    @property
    def time(self) -> float:
        """Get or set the Start time for tracer particle activation
        """ # nopep8
        return self._cards[0].get_value("time")

    @time.setter
    def time(self, value: float) -> None:
        """Set the time property."""
        self._cards[0].set_value("time", value)

    @property
    def nid_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid."""
        return self._get_link_by_attr("NODE", "nid", self.nid, "parts")

