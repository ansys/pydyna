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

"""Module providing the ElementShellSourceSink class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_ELEMENTSHELLSOURCESINK_CARD0 = (
    FieldSchema("nssr", int, 0, 10, None),
    FieldSchema("nssk", int, 10, 10, None),
    FieldSchema("pid", int, 20, 10, None),
)

class ElementShellSourceSink(KeywordBase):
    """DYNA ELEMENT_SHELL_SOURCE_SINK keyword"""

    keyword = "ELEMENT"
    subkeyword = "SHELL_SOURCE_SINK"
    _link_fields = {
        "nssr": LinkType.SET_NODE,
        "nssk": LinkType.SET_NODE,
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ElementShellSourceSink class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ELEMENTSHELLSOURCESINK_CARD0,
                **kwargs,
            ),        ]
    @property
    def nssr(self) -> typing.Optional[int]:
        """Get or set the Node set at source. Provide an ordered set of nodes between corner nodes, which include the corner nodes
        """ # nopep8
        return self._cards[0].get_value("nssr")

    @nssr.setter
    def nssr(self, value: int) -> None:
        """Set the nssr property."""
        self._cards[0].set_value("nssr", value)

    @property
    def nssk(self) -> typing.Optional[int]:
        """Get or set the Node set at sink. Provide an ordered set of nodes between corner nodes, which include the corner nodes
        """ # nopep8
        return self._cards[0].get_value("nssk")

    @nssk.setter
    def nssk(self, value: int) -> None:
        """Set the nssk property."""
        self._cards[0].set_value("nssk", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of work piece.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def nssr_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for nssr."""
        return self._get_set_link("NODE", self.nssr)

    @nssr_link.setter
    def nssr_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for nssr."""
        self.nssr = value.sid

    @property
    def nssk_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for nssk."""
        return self._get_set_link("NODE", self.nssk)

    @nssk_link.setter
    def nssk_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for nssk."""
        self.nssk = value.sid

    @property
    def pid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

