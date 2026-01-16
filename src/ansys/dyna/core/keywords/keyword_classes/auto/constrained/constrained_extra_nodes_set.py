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

"""Module providing the ConstrainedExtraNodesSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_CONSTRAINEDEXTRANODESSET_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("nsid", int, 10, 10, None),
    FieldSchema("iflag", int, 20, 10, 0),
)

class ConstrainedExtraNodesSet(KeywordBase):
    """DYNA CONSTRAINED_EXTRA_NODES_SET keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "EXTRA_NODES_SET"
    _link_fields = {
        "nsid": LinkType.SET_NODE,
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ConstrainedExtraNodesSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDEXTRANODESSET_CARD0,
                **kwargs,
            ),        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of rigid body to which the nodes will be added, see *PART.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID of added nodes, see *SET_NODE.
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        """Set the nsid property."""
        self._cards[0].set_value("nsid", value)

    @property
    def iflag(self) -> int:
        """Get or set the This flag is meaningful if and only if the inertia properties of the Part ID
        are defined in PART_INERTIA. If set to unity, the center-of-gravity, the
        translational mass, and the inertia matrix of the PID will be updated to reflect the
        merged nodal masses of the node or node set. If IFLAG is defaulted to zero, the merged nodes will not affect the properties defined in
        PART_INERTIA since it is assumed the properties already account for merged nodes.
        """ # nopep8
        return self._cards[0].get_value("iflag")

    @iflag.setter
    def iflag(self, value: int) -> None:
        """Set the iflag property."""
        self._cards[0].set_value("iflag", value)

    @property
    def nsid_link(self) -> KeywordBase:
        """Get the SET_NODE_* keyword for nsid."""
        return self._get_set_link("NODE", self.nsid)

    @nsid_link.setter
    def nsid_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for nsid."""
        self.nsid = value.sid

    @property
    def pid_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

